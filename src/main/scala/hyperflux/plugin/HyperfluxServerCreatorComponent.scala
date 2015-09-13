package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import hyperflux.annotation._
import scala.collection.mutable.HashMap
import scala.reflect.internal.Flags._

abstract class HyperfluxServerCreatorComponent extends PluginComponent {

  import global._

  val phaseName = "hf-server-creator"
  
  val hf: HyperfluxStorage[TermName, ValDef, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxServerCreatorPhase(prev)

  class HyperfluxServerCreatorPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description =
      "creates the server-side post office"
    
    override def run() {
      
      // no changes are required on server objects if we are compiling
      // towards JavaScript
      if (hf.compilesToJS) {
        return
      }
      
      super.run()
    }
    
    override def apply(unit: CompilationUnit) {
      unit.body = rewrite(unit.body)
    }
    
    def rewrite(t: Tree): Tree = t match {
      
      case PackageDef(pid, stats) => new PackageDef(pid, rewritePackage(stats))
      
      case m @ ModuleDef(_, name, _)
      if (hf.serverObjects contains name) => rewriteServerObject(m)
      
      case m @ ModuleDef(mods, name, impl)
      if (hf.interfaceObjects contains name) => 
        new ModuleDef(mods, name, rewriteInterfaceObject(impl))
      
      // anything else on this level should not be transformed
      case _ => t
    }
    
    def rewritePackage(ts: List[Tree]): List[Tree] = {
      imports ++ (ts filter (_ match {
        case ModuleDef(_, name, _) => !(hf.clientObjects contains name)
        case _ => true
      })) map {
        // again, not the most elegant test
        case i: Import
        if (i.toString() contains "scalatags.JsDom") => rsti(i)
        case x => x 
      } map rewrite
    }
    
    // additional import required for the server components
    val imports = List()
    
    /**
     * Rewrites imports of the scalatags.JsDom object to the more
     * server-friendly version scalatags.Text
     */
    def rsti(t: Tree): Tree = t match {
      case Import(e, sels) => new Import(rsti(e), sels)
      case Select(q, n) =>
        new Select(
          rsti(q),
          if (n.toString == "JsDom") newTermName("Text") else n
        )
      case _ => t
    }
    
    /**
     * Adds the actual server functionality to a server object. It uses HTTP4S
     * by creating to define a corresponding action for every method it
     * provides and adding a main method that starts a HTTP server.  
     */
    def rewriteServerObject(m: ModuleDef): ModuleDef = {
      
      val rpcMethodCases = hf.serverMethods(m.name).toList map {
        case (mName, (mArgs, _)) =>
          if (mArgs.size == 0 || mArgs.head.size == 0) {
            
            // PARAMETERLESS METHOD
            
            new CaseDef(
              new Apply(
                mgIdent,
                List(
                  postIdent,
                  new Apply(
                    divIdent,
                    List(rootIdent, new Literal(new Constant(mName.toString)))
                  )
                )
              ),
              EmptyTree,
              new Block(
                List(
                  new ValDef(
                    nullMods,
                    retName,
                    new TypeTree(),
                    new Apply(new Ident(mName), List())
                  ),
                  retPickledVal
                ),
                okApply
              )
            )
          } else if (mArgs.head.size == 1) {
            
            // SINGLE PARAMETER METHOD
            
            val argType = mArgs.head.head.tpt            
            new CaseDef(
              new Bind(
                reqName,
                new Apply(
                  mgIdent,
                  List(
                    postIdent,
                    new Apply(
                      divIdent,
                      List(rootIdent, new Literal(new Constant(mName.toString)))
                    )
                  )
                )
              ),
              EmptyTree,
              new Try(
                new Block(
                  List(
                    new ValDef(
                      nullMods,
                      argName,
                      new TypeTree(),
                      new Apply(
                        new TypeApply(
                          readSelect,
                          List(argType)
                        ),
                        List(reqBody)
                      )
                    ),
                    new ValDef(
                      nullMods,
                      retName,
                      new TypeTree(),
                      new Apply(new Ident(mName), List(new Ident(argName)))
                    ),
                    retPickledVal
                  ),
                  okApply
                ),
                reqCatches,
                EmptyTree
              )
            )
          } else {
            
            // MULTIPLE PARAMETER METHOD
            
            // TODO: maybe someday support multiple parameter lists
            val argNames = (for (i <- 1 to mArgs.head.size)
              yield newTermName(s"arg$i")
            ).toList
            val argIdents = argNames map { n => new Ident(n) }
            val argTypes = mArgs.head map (_.tpt)
            
            new CaseDef(
              new Bind(
                reqName,
                new Apply(
                  mgIdent,
                  List(
                    postIdent,
                    new Apply(
                      divIdent,
                      List(rootIdent, new Literal(new Constant(mName.toString)))
                    )
                  )
                )
              ),
              EmptyTree,
              new Try(
                new Block(
                  List(
                    new ValDef(
                      nullMods,
                      tupName,
                      new TypeTree(),
                      new Apply(
                        new TypeApply(
                          readSelect,
                          List(
                            new AppliedTypeTree(
                              new Select(scalaIdent, newTypeName(s"Tuple${argTypes.size}")),
                              argTypes
                            )
                          )
                        ),
                        List(reqBody)
                      )
                    )
                  ) ++ (for (i <- 1 to argNames.size)
                    yield new ValDef(
                      nullMods,
                      argNames(i-1),
                      new TypeTree(),
                      new Select(new Ident(tupName), newTermName(s"_$i"))
                    )
                  ).toList ++
                  List(
                    new ValDef(
                      nullMods,
                      retName,
                      new TypeTree(),
                      new Apply(new Ident(mName), argIdents)
                    ),
                    retPickledVal
                  ),
                  okApply
                ),
                reqCatches,
                EmptyTree
              )
            )
          }
      }
      
      val rpcService = new ValDef(
        nullMods,
        getRPCServiceName(m.name),
        new TypeTree(),
        new Apply(
          httpServiceIdent,
          List(
            new Match(
              EmptyTree,
              rpcMethodCases
            )
          )
        )
      )
      
      // TODO: for the case of multiple server objects, think of rules for the
      // association of pages to server objects
      val htmlPageCases = hf.pages.toList map { case ((oName, vName), pURL) =>
        new CaseDef(
          new Apply(
            mgIdent,
            List(
              getIdent,
              if (pURL == "main")
                rootIdent
              else new Apply(
                divIdent,
                List(rootIdent, new Literal(new Constant(pURL)))
              )
            )
          ),
          EmptyTree,
          new Block(
            List(
              new ValDef(
                nullMods,
                htmlDataName,
                new TypeTree(),
                new Select(
                  new Select(new Ident(oName), vName),
                  newTermName("toString")
                )
              )
            ),
            okPage
          )
        )
      }
      val htmlService = new ValDef(
        nullMods,
        getHTMLServiceName(m.name),
        new TypeTree(),
        new Apply(
          httpServiceIdent,
          List(
            new Match(
              EmptyTree,
              htmlPageCases
            )
          )
        )
      )
      
      // TODO: for the future - find a better way to create synthetic trees
      val mainMethod = new DefDef(
        nullMods,
        mainName,
        List(),
        List(List(mainArgs)),
        unitType,
        new Apply(
          new Select(
            new Select(
              new Apply(
                new Select(
                  new Apply(
                    new Select(
                      new Apply(
                        blazeBuilderBindHttp,
                        List(new Literal(new Constant(hf.serverPort(m.name))))
                      ),
                      mountServiceName
                    ),
                    List(
                      new Ident(getHTMLServiceName(m.name)),
                      new Literal(new Constant(""))
                    )
                  ),
                  mountServiceName
                ),
                List(
                  new Ident(newTermName("service")),//getRPCServiceName(m.name)),
                  new Literal(new Constant(m.name.toString)))
              ),
              newTermName("run")
            ),
            newTermName("awaitShutdown")
          ),
          List()
        )
      )
      
      new ModuleDef(
        m.mods,
        m.name,
        new Template(
          m.impl.parents,
          m.impl.self,
          m.impl.body ++ List(rpcService, htmlService, mainMethod)
        )
      )
    }
    
    def getRPCServiceName(oName: TermName) =
      newTermName(s"HF_RPC_SERVICE__$oName")
    def getHTMLServiceName(oName: TermName) =
      newTermName(s"HF_HTML_SERVICE__$oName")
    
    /*
     * some elements that are used in several places
     */    
    lazy val nullMods = new Modifiers(NoFlags, tpnme.EMPTY, Nil)
    lazy val mainName = newTermName("main")
    lazy val scalaIdent = new Ident(newTermName("scala"))
    lazy val unitType = new Select(scalaIdent, newTypeName("Unit"))
    lazy val mainArgs = new ValDef(
      nullMods,
      newTermName("args"),
      new AppliedTypeTree(
        new Select(scalaIdent, newTypeName("Array")),
        List(new Ident(newTypeName("String")))
      ),
      EmptyTree
    )
    lazy val mountServiceName = newTermName("mountService")
    lazy val blazeBuilderBindHttp = new Select(
      new Ident(newTermName("BlazeBuilder")),
      newTermName("bindHttp")
    )
    lazy val httpServiceIdent = new Ident(newTermName("HttpService"))
    lazy val mgIdent = new Ident(newTermName("$minus$greater"))
    lazy val postIdent = new Ident(newTermName("POST"))
    lazy val getIdent = new Ident(newTermName("GET"))
    lazy val divIdent = new Ident(newTermName("$div"))
    lazy val rootIdent = new Ident(newTermName("Root"))
    lazy val reqName = newTermName("req")
    lazy val tupName = newTermName("tup")
    
    // TODO:
    lazy val reqBody = new Literal(new Constant(""))
    
    lazy val argName = newTermName("arg")
    lazy val upickleDefaultSelect = new Select(
      new Ident(newTermName("upickle")),
      newTermName("default")
    )
    lazy val readSelect = new Select(upickleDefaultSelect, newTermName("read"))
    lazy val retName = newTermName("ret")
    lazy val retPickledName = newTermName("retPickled")
    lazy val retPickledVal = new ValDef(
      nullMods,
      retPickledName,
      new TypeTree(),
      new Ident(retName)
      /*new Apply(
        new Select(upickleDefaultSelect, newTermName("write")),
        List(new Ident(retName))
      )*/
    )
    lazy val okApply = new Apply(
      new Ident(newTermName("Ok")), List(new Ident(retPickledName)))
    lazy val reqCatches = List(
      new CaseDef(
        new Typed(
          new Ident(nme.WILDCARD),
          new Select(new Ident(newTermName("upickle")), newTypeName("Invalid"))
        ),
        EmptyTree,
        new Apply(
          new Ident(newTermName("BadRequest")),
          List(new Literal(new Constant("")))
        )
      ),
      new CaseDef(
        new Typed(
          new Ident(nme.WILDCARD),
          new Ident(newTypeName("Throwable"))
        ),
        EmptyTree,
        new Apply(
          new Ident(newTermName("InternalServerError")),
          List(new Literal(new Constant("")))
        )
      )
    )
    lazy val htmlDataName = newTermName("htmlData")
    lazy val okPage = new Apply(
      new Ident(newTermName("Ok")), List(new Ident(htmlDataName)))
    
    /**
     * Prepares interface definitions for JVM compilation by replacing calls
     * to Scala-defined client methods by their export names for JavaScript
     */
    def rewriteInterfaceObject(t: Template): Template = {
      
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
        case Apply(fun, args) => new Apply(rt(fun), rts(args))
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
        
        // the interesting part:
        // TODO: there may be more. for now, use this way, like in:
        // onclick := ClientObject.method
        
        case Select(Ident(oName), mName)
        if (hf.clientObjects contains oName.toTermName) =>
          new Literal(new Constant(createJSExportName(oName, mName)))
        
        // other cases:
        case Select(qual, name) => new Select(rt(qual), name)
        
        // *********************
        
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
      
      def createJSExportName(oName: Name, mName: Name) =
        s"$oName().$mName()"
      
      new Template(t.parents, t.self, t.body map rt)
    }
  }
}