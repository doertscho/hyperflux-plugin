package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.MutableList
import scala.reflect.internal.Flags._

abstract class HyperfluxProxifierComponent extends PluginComponent {

  import global._

  val phaseName = "hf-proxifier"
  
  val hf: HyperfluxStorage[TermName, ValDef, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxProxifierPhase(prev)
  
  class HyperfluxProxifierPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description = "rewrites calls from client to server"
    
    override def run() {
      
      // if this compilation run targets the server side, there is no need
      // for this phase
      if (!(hf.compilesToJS)) {
        return
      }
      
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
            val methBody = new MutableList[Tree]
            
            // create data tuple if necessary
            if (argNames.size > 0) {
              methBody += new ValDef(
                nullMods,
                dataTermName,
                new TypeTree(), // please infer this type!
                if (argNames.size == 1) { // create either a single value or a tuple
                  argNames.head
                } else {
                  new Apply(
                    new Select(scalaIdent, newTermName(s"Tuple${argNames.size}")),
                    argNames
                  )
                }
              )
            }
            
            // the call to the central server proxy method
            methBody += new ValDef(
              nullMods,
              retTermName,
              new AppliedTypeTree(
                new Ident(newTypeName("Future")),
                List(new Ident(newTypeName("String")))
              ),
              new Apply(
                new Ident(callServerTermName),
                List(
                  new Literal(new Constant(hf.serverURL(oName))),
                  new Literal(new Constant(oName.toString())),
                  new Literal(new Constant(mName.toString())),
                  if (argNames.size > 0) {
                    new Apply(
                      new Ident(writeTermName),
                      List(new Ident(dataTermName))
                    )
                  } else {
                    new Literal(new Constant(""))
                  }
                )
              )
            )
            
            // the extraction of the result
            val extract = new Apply(
              new TypeApply(new Ident(readTermName), List(tpe)),
              List(new Ident(strTermName))
            )
            // the Future mapping
            val mapping = new Apply(
              new Select(new Ident(retTermName), newTermName("map")),
              List(
                new Match(
                  EmptyTree,
                  List(
                    new CaseDef(
                      new Bind(strTermName, new Ident(nme.WILDCARD)),
                      EmptyTree,
                      extract
                    )
                  )
                )
              )
            )
            
            hf.proxyDefs((oName, mName)) += new DefDef(
              nullMods,
              proxMethName,
              Nil,
              paramss,
              new AppliedTypeTree(new Ident(newTypeName("Future")), List(tpe)),
              new Block(methBody.toList, mapping)
            )
            hf.proxyAliases += (((oName, mName), proxMethName))
          }
        }
      }
    }
      
    val HF_PROXY_PREFIX = "HF_PROXY__"
    def createProxyMethodName(oName: TermName, mName: TermName): TermName = 
      newTermName(s"$HF_PROXY_PREFIX${oName}__$mName")
      
    // some constant names that will be used in every proxy method
    lazy val readTermName = newTermName("read")
    lazy val writeTermName = newTermName("write")
    lazy val callServerTermName = newTermName("callServer")
    lazy val initTermName = newTermName("<init>")
    lazy val dataTermName = newTermName("data")
    lazy val retTermName = newTermName("ret")
    lazy val strTermName = newTermName("str")
    lazy val scalaTermName = newTermName("scala")
    lazy val productTypeName = newTypeName("Product")
    lazy val serializableTypeName = newTypeName("Serializable")
    lazy val emptyTypeName = newTypeName("")
    lazy val nullMods = new Modifiers(NoFlags, tpnme.EMPTY, Nil)
    lazy val caseClassMods = new Modifiers(CASE, tpnme.EMPTY, Nil)
    lazy val ccParamMods = new Modifiers(
        PARAMACCESSOR | CASEACCESSOR, tpnme.EMPTY, Nil)
    lazy val scalaIdent = new Ident(scalaTermName)
    
    lazy val hyperfluxName = newTermName("hyperflux")
    lazy val importHfProtocol = new Import(
        new Select(new Ident(hyperfluxName), newTermName("protocol_helpers")),
        ImportSelector.wildList
    )
    lazy val importUPickle = new Import(
        new Select(new Ident(newTermName("upickle")), newTermName("default")),
        ImportSelector.wildList
    )
    lazy val importJSApp = new Import(
      new Select(
        new Select(new Ident(newTermName("scala")), newTermName("scalajs")),
        newTermName("js")
      ),
      ImportSelector.wildList
    )
    lazy val imports = List(importHfProtocol, importJSApp, importUPickle)
    
    def rewrite(t: Tree): Tree = t match {
      case PackageDef(pid, stats) => new PackageDef(
          pid, imports ++ (stats map rewrite))
      case m @ ModuleDef(mods, name, impl)
      if (
        (hf.clientObjects contains name) &&
        !(hf.methodUsages(name).isEmpty)
      ) => rewriteClientObject(m)
      // anything else on this level should not be transformed
      case e => e
    }
    
    /**
     * This method replaces all occurrences of server methods
     * by the corresponding proxy method and tries to "asynchronize"
     * the control flow.
     */
    def rewriteClientObject(m: ModuleDef): ModuleDef = {
      
      inform(s"Rewriting client object ${m.name}")
      
      val newMeths = new HashSet[Tree]
      var mainMissing = true
      
      /**
       * Replaces server method calls by the corresponding proxy method
       */
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
          val rFun = fun match {
            case Select(Ident(oName), mName)
            if (
              hf.proxyDefs contains ((oName.toTermName, mName.toTermName))
            ) => {
              new Ident(hf.proxyAliases((oName.toTermName, mName.toTermName)))
            }
            case _ => rt(fun)
          }
          new Apply(rFun, rts(args))
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
        case tt: TypeTree => tt
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
          async(
            new ValDef(
              new Modifiers(flags, pw, rts(ann)),
              name,
              rt(tpt),
              rt(rhs)
            )
          )
        case c @ DefDef(_, name, _, _, _, _) if (name == nme.CONSTRUCTOR) => c
        case DefDef(Modifiers(flags, pw, ann), name, tps, vpss, tpt, rhs) => {
          if (name.toString == "main") mainMissing = false
          async(
            new DefDef(
              new Modifiers(flags, pw, addExport(name, rts(ann))),
              name,
              rts(tps),
              vpss map rts,
              rt(tpt),
              rt(rhs)
            )
          )
        }
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
        case _ => {
          warning(s"don't know the type of $tree")
          new Literal(new Constant("<error>"))
        }
      }).asInstanceOf[T]
      def rts[T <: Tree](ts: List[T]): List[T] = ts map rt
      
      /**
       * Adds an @JSExport annotation to methods such that they are callable
       * by their Scala name in the final JavaScript file
       */
      def addExport(name: Name, ann: List[Tree]): List[Tree] = {
        if (ann exists {
          case Apply(Select(New(Ident(tpe)), nme.CONSTRUCTOR), _) =>
            tpe.toString == "JSExport"
          case _ => false
        }) {
          ann
        } else {
          new Apply(
            new Select(
              new New(new Ident(newTypeName("JSExport"))),
              nme.CONSTRUCTOR
            ),
            List(new Literal(new Constant(name.toString)))
          ) :: ann
        }
      }
      
      
      /**
       * Tries to "async" methods if necessary
       */
      def async[D <: ValOrDefDef](d: D): D = {
        // first of all, check whether there is a proxy call in this one
        // (as always, might be tested more elegantly ..)
        if (d.rhs.toString contains HF_PROXY_PREFIX) {
          
          inform(s"Asyncing ${d.name}")
          
          var valID = 0
          var funID = 0
          def getNewValName() = {
            valID = valID + 1
            newTermName(s"v_${d.name}_$valID")
          }
          def getNewFunName() = {
            funID = funID + 1
            newTermName(s"f_${d.name}_$funID")
          }
          
          def a(t: Tree): Tree = t match {
            
            case Apply(fun, args)
            // rewrite is only necessary if the proxy call is in the arguments 
            if (args.toString contains HF_PROXY_PREFIX) => {
              val (wo, w) = args span { a =>
                !(a.toString contains HF_PROXY_PREFIX) }
              val valName = getNewValName()
              new Apply(
                new Select(a(w.head), newTermName("onSuccess")),
                List(
                  new Match(
                    EmptyTree,
                    List(
                      new CaseDef(
                        new Bind(valName, wildCardIdent),
                        EmptyTree,
                        a(
                          new Apply(
                            fun,
                            wo ++ List(new Ident(valName)) ++ w.tail
                          )
                        )
                      )
                    )
                  )
                )
              )
            }
            
            case b @ Block(es, e)
            if (b.toString contains HF_PROXY_PREFIX) => {
              // identify the point to split the sequence
              val (wo, w) = (es ++ List(e)) span { t =>
                !(t.toString contains HF_PROXY_PREFIX) }
              
              w.head match {
                // flattened form: create a new method with the rest of the
                // sequence and execute it on success
                case ValDef(mods, name, tpt, Apply(fun, args))
                if (fun.toString contains HF_PROXY_PREFIX) => {
                  
                  new Block(
                    wo,
                    if (w.size == 1) {
                      new Apply(fun, args)
                    } else {
                      new Apply(
                        new Select(new Apply(fun, args), newTermName("onSuccess")),
                        List(
                          new Match(
                            EmptyTree,
                            List(
                              new CaseDef(
                                new Bind(name, wildCardIdent),
                                EmptyTree,
                                a(
                                  if (w.tail.size == 1) {
                                    w.last
                                  } else {
                                    new Block(
                                      w.tail.dropRight(1),
                                      w.last
                                    )
                                  }
                                )
                              )
                            )
                          )
                        )
                      )
                    }
                  )
                  
                }
                
                // fallback: return input
                case _ => new Block(es, e)
              }
            }
            
            case _ => t
          }
          
          (d match {
            case ValDef(mods, name, tpt, rhs) =>
              new ValDef(mods, name, tpt, a(rhs))
            case DefDef(mods, name, tps, vpss, tpt, rhs) =>
              new DefDef(mods, name, tps, vpss, tpt, a(rhs))
          }).asInstanceOf[D]
          
        } else {
          d
        }
      }
      
      lazy val wildCardIdent = new Ident(nme.WILDCARD)
      
      
      val usedProxyDefs = (hf.proxyDefs filter {
        case (k, _) => hf.methodUsages(m.name) contains k
      }).values.flatten
      
      val parents = if (m.impl.parents exists { 
          _.toString() contains "JSApp"
        }) {
        m.impl.parents
      } else {
        new Ident(newTypeName("JSApp")) ::
          (m.impl.parents filterNot { _.toString() contains "AnyRef" })
      }
      
      
      val rewrittenBody = rts(m.impl.body)
      if (mainMissing) {
        newMeths += new DefDef(
          nullMods,
          newTermName("main"),
          List(),
          List(List()),
          new Select(scalaIdent, newTypeName("Unit")),
          new Literal(new Constant(()))
        )
      }
      
      new ModuleDef(
        new Modifiers(
          m.mods.flags,
          m.mods.privateWithin,
          addExport(m.name, m.mods.annotations)
        ),
        m.name,
        new Template(
          parents,
          m.impl.self,
          rewrittenBody ++ newMeths ++ usedProxyDefs
        )
      )
    }
  }
}