package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import hyperflux.annotation._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.MutableList
import scala.reflect.internal.Flags._

abstract class HyperfluxEarlyProxifierComponent extends PluginComponent {

  import global._

  val phaseName = "hf-eproxifier"
  
  val hf: HyperfluxEarlyStorage[TermName, ValDef, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxEarlyProxifierPhase(prev)
  
  class HyperfluxEarlyProxifierPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description = "rewrites calls from client to server"
    
    override def run() {
      createProxyDefPrototypes()
      super.run()
    }
    
    override def apply(unit: CompilationUnit) {
      unit.body = rewrite(unit.body)
    }
    
    def createProxyDefPrototypes() {
      val usedMethods = hf.methodUsages.values.flatten.toSet
      hf.serverMethods foreach { case (oName, meths) =>
        meths foreach { case (mName, (paramss, tpe)) =>
          if (usedMethods contains (oName, mName)) {
            
            // here, we create a proxy method by building up the program tree
            // from scratch depending on the signature of the original method
            
            hf.proxyDefs += (((oName, mName), new HashSet[Tree]))
          
            // extract the list of argument names from the method signature
            val (argNames, argVals) = if (paramss.size > 0) {
              if (paramss.size > 1)
                warning(s"Hyperflux does not yet support multiple parameter lists")
              (paramss.head map { vd => 
                (new Ident(vd.name), new ValDef(ccParamMods, vd.name, vd.tpt, EmptyTree))
              }).unzip
            } else (Nil, Nil)
            
            val proxMethName = createProxyMethodName(oName, mName)
            val proxDataName = createProxyDataName(oName, mName)
            val methBody = new MutableList[Tree]
            
            // create data carrier class if necessary
            if (argNames.size > 0) {
              hf.proxyDefs((oName, mName)) += new ClassDef(
                caseClassMods,
                proxDataName,
                Nil,
                new Template(
                  new Select(scalaIdent, productTypeName) ::
                    new Select(scalaIdent, serializableTypeName) ::
                    Nil,
                  noSelfType,
                  argVals ++ (
                    new DefDef(
                      nullMods,
                      initTermName,
                      Nil,
                      paramss,
                      new TypeTree(),
                      new Block(
                        new Apply(
                          new Select(
                            new Super(new This(emptyTypeName), emptyTypeName),
                            initTermName
                          ),
                          Nil
                        ) :: Nil,
                        new Literal(new Constant(()))
                      )
                    ) :: Nil
                  )
                )
              )
              
              // also create the call to the constructor
              methBody += new ValDef(
                nullMods,
                dataTermName,
                new TypeTree(), // please infer this type!
                new Apply(
                  new Select(new New(new Ident(proxDataName)), initTermName),
                  argNames
                )
              )
            }
            
            // the call to the central server proxy method
            methBody += new ValDef(
              nullMods,
              retTermName,
              new TypeTree(),
              new Apply(
                new Ident(callServerTermName),
                List(
                  new Literal(new Constant(oName.toString())),
                  new Literal(new Constant(mName.toString())),
                  if (argNames.size > 0) {
                    new Apply(
                      new Ident(writeTermName),
                      new Ident(dataTermName) :: Nil
                    )
                  } else {
                    new Literal(new Constant(""))
                  }
                )
              )
            )
            
            // the extraction of the result
            val extract = new Apply(
                new TypeApply(new Ident(readTermName), tpe :: Nil),
                new Ident(retTermName) :: Nil
            )
            
            hf.proxyDefs((oName, mName)) += new DefDef(
              nullMods,
              proxMethName,
              Nil,
              paramss,
              tpe,
              new Block(methBody.toList, extract)
            )
            hf.proxyAliases += (((oName, mName), proxMethName))
          }
        }
      }
      
      def createProxyMethodName(oName: TermName, mName: TermName): TermName = 
        newTermName(s"HF_PROXY__${oName}__${mName}")
      
      def createProxyDataName(oName: TermName, mName: TermName): TypeName = 
        newTypeName(s"${createProxyMethodName(oName, mName)}__DATA")
      
      // some constant names that will be used in every proxy method
      lazy val readTermName = newTermName("read")
      lazy val writeTermName = newTermName("write")
      lazy val callServerTermName = newTermName("callServer")
      lazy val initTermName = newTermName("<init>")
      lazy val dataTermName = newTermName("data")
      lazy val retTermName = newTermName("ret")
      lazy val scalaTermName = newTermName("scala")
      lazy val productTypeName = newTypeName("Product")
      lazy val serializableTypeName = newTypeName("Serializable")
      lazy val emptyTypeName = newTypeName("")
      lazy val nullMods = new Modifiers(NoFlags, tpnme.EMPTY, Nil)
      lazy val caseClassMods = new Modifiers(CASE, tpnme.EMPTY, Nil)
      lazy val ccParamMods = new Modifiers(
          PARAMACCESSOR | CASEACCESSOR, tpnme.EMPTY, Nil)
      lazy val scalaIdent = new Ident(scalaTermName)
    }
    
    lazy val hyperfluxName = newTermName("hyperflux")
    lazy val importHfProtocol = new Import(
        new Select(new Ident(hyperfluxName), newTermName("protocol_helpers")),
        ImportSelector.wildList
    )
    lazy val importUPickle = new Import(
        new Select(new Ident(newTermName("upickle")), newTermName("default")),
        ImportSelector.wildList
    )
    
    def rewrite(t: Tree): Tree = t match {
      case PackageDef(pid, stats) => new PackageDef(
          pid, importHfProtocol :: importUPickle :: (stats map rewrite))
      case m @ ModuleDef(mods, name, impl)
      if (
        (hf.clientObjects contains name) &&
        !(hf.methodUsages(name).isEmpty)
      ) => rewriteClientObject(m)
      // anything else on this level should not be transformed
      case e => e
    }
    
    def rewriteClientObject(m: ModuleDef): ModuleDef = {
      
      def rt[T <: Tree](tree: T): T = (tree match {
        // TermTree types:
        case Block(stats, expr) => new Block(rts(stats), rt(expr))
        case Alternative(ts) => new Alternative(rts(ts))
        case Star(elem) => new Star(rt(elem))
        case UnApply(fun, args) => new UnApply(rt(fun), rts(args))
        case ArrayValue(elemtpt, elems) =>
          new ArrayValue(rt(elemtpt), rts(elems))
        case Assign(lhs, rhs) => new Assign(rt(lhs), rt(rhs))
        case AssignOrNamedArg(lhs, rhs) => new AssignOrNamedArg(rt(lhs), rt(rhs))
        case If(cond, thenp, elsep) => new If(rt(cond), rt(thenp), rt(elsep))
        case Match(sel, cases) => new Match(rt(sel), rts(cases))
        case Try(block, catches, finalizer) =>
          new Try(rt(block), rts(catches), rt(finalizer))
        case Throw(expr) => new Throw(rt(expr))
        case New(tpt) => new New(rt(tpt))
        case Typed(expr, tpt) => new Typed(rt(expr), rt(tpt))
        case TypeApply(fun, args) => new TypeApply(rt(fun), rts(args))
        
        // the essential part happens here:
        case Apply(fun, args) => {
          new Apply(fun match {
            case Select(Ident(oName), mName)
            if (
              hf.proxyDefs contains ((oName.toTermName, mName.toTermName))
            ) => {
              new Ident(
                  hf.proxyAliases((oName.toTermName, mName.toTermName)))
            }
            case _ => rt(fun)
          },
          rts(args))
        }
        // *******************
        
        case Super(qual, mix) => new Super(rt(qual), mix)
        case ReferenceToBoxed(ident) => new ReferenceToBoxed(rt(ident))
        case Literal(const) => new Literal(const)
        case EmptyTree => EmptyTree
        // TypTree types:
        case SingletonTypeTree(ref) => new SingletonTypeTree(rt(ref))
        case CompoundTypeTree(templ) => new CompoundTypeTree(rt(templ))
        case AppliedTypeTree(tpt, args) =>
          new AppliedTypeTree(rt(tpt), rts(args))
        case TypeBoundsTree(lo, hi) => new TypeBoundsTree(rt(lo), rt(hi))
        case ExistentialTypeTree(tpt, wheres) =>
          new ExistentialTypeTree(rt(tpt), rts(wheres))
        case tt @ TypeTree() => {
          TypeTree(tt.tpe)
        }
        // SymTree types:
        case Select(qual, name) => new Select(rt(qual), name)
        case Ident(name) => new Ident(name)
        case SelectFromTypeTree(qual, name) =>
          new SelectFromTypeTree(rt(qual), name)
        case PackageDef(pid, stats) => new PackageDef(rt(pid), rts(stats))
        case ClassDef(Modifiers(flags, pw, ann), name, tparams, impl) =>
          new ClassDef(
              new Modifiers(flags, pw, rts(ann)),
              name,
              rts(tparams),
              rt(impl)
          )
        case ModuleDef(Modifiers(flags, pw, ann), name, impl) =>
          new ModuleDef(new Modifiers(flags, pw, rts(ann)), name, rt(impl))
        case ValDef(Modifiers(flags, pw, ann), name, tpt, rhs) =>
          new ValDef(
              new Modifiers(flags, pw, rts(ann)),
              name,
              rt(tpt),
              rt(rhs)
          )
        case DefDef(Modifiers(flags, pw, ann), name, tps, vpss, tpt, rhs) =>
          new DefDef(
              new Modifiers(flags, pw, rts(ann)),
              name,
              rts(tps),
              vpss map rts,
              rt(tpt),
              rt(rhs)
          )
        case TypeDef(Modifiers(flags, pw, ann), name, tps, rhs) =>
          new TypeDef(
              new Modifiers(flags, pw, rts(ann)),
              name, 
              rts(tps),
              rt(rhs)
          )
        case LabelDef(name, params, rhs) =>
          new LabelDef(name, rts(params), rt(rhs))
        case Bind(name, body) => new Bind(name, rt(body))
        case Import(expr, sels) => new Import(rt(expr), sels)
        case Template(parents, self, body) =>
          new Template(rts(parents), rt(self), rts(body))
        case Function(vps, body) => new Function(rts(vps), rt(body))
        case Return(expr) => new Return(rt(expr))
        case ApplyDynamic(qual, args) => new ApplyDynamic(rt(qual), rts(args))
        case This(qual) => new This(qual)
        case CaseDef(pat, guard, body) =>
          new CaseDef(rt(pat), rt(guard), rt(body))
        case Annotated(ann, arg) => new Annotated(rt(ann), rt(arg))
      }).asInstanceOf[T]
      def rts[T <: Tree](ts: List[T]): List[T] = ts map rt
      
      val usedProxyDefs = (hf.proxyDefs filter {
        case (k, _) => hf.methodUsages(m.name) contains k
      }).values.flatten
      
      new ModuleDef(
        m.mods,
        m.name,
        new Template(m.impl.parents, m.impl.self, rts(m.impl.body) ++ usedProxyDefs)
      )
    }
  }
}