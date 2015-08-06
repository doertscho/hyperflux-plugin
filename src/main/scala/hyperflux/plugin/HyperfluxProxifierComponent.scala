package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.transform._
import scala.collection.mutable._
import scala.collection.immutable.Nil
import scala.reflect.internal.Flags
import scala.tools.nsc._
import scala.reflect.internal.util.NoSourceFile
import scala.tools.nsc.ast.TreeBrowsers
import scala.reflect.internal.Flags._

abstract class HyperfluxProxifierComponent
    extends PluginComponent with Transform {

  import global._

  val phaseName = "hf-proxifier"

  override def newPhase(prev: nsc.Phase): StdPhase =
      new HyperfluxProxifierPhase(prev)
  
  val hf: HyperfluxStorage[Symbol, Template, DefDef, RefTree, Tree]
  
  class HyperfluxProxyTransformer(unit: CompilationUnit)
      extends global.Transformer {
    
    override def transform(tree: Tree): Tree = {
      var rw = tree
      tree match {
        // for package definitions, march down the tree recursively
        case PackageDef(pid, stats) =>
          rw = new PackageDef(pid, transformTrees(stats))
        // for modules with a @Client annotation, do the actual transformation,
        // for others, do nothing
        case ModuleDef(mods, name, impl) 
        if (hf.clientObjects contains impl.symbol.safeOwner) =>
          rw = new ModuleDef(
              mods,
              name,
              rewriteClientComponent(impl.symbol.owner, impl)
          )
        // change nothing on anything else on this level
        case _ =>
      }
      rw.symbol = tree.symbol
      rw.pos = tree.pos
      rw
    }
    
    def rewriteClientComponent(cSym: Symbol, tIn: Template): Template = {
      
      def rt[T <: Tree](tree: T): T = {
        val rw = (tree match {
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
            if (hf.serverObjects contains fun.symbol.safeOwner) {
              // rewrite this call to make it a call to the proxy function
              val proxSym = hf.proxyAliases(cSym)(fun.symbol.owner, fun.symbol)
              val proxMethod = new Select(
                  new This(cSym.name.toTypeName),
                  proxSym.name
              )
              new Apply(proxMethod, rts(args))
            } else {
              new Apply(rt(fun), rts(args))
            }
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
            var typeName = tt.tpe.toString()
            if (typeName startsWith "scala.this.") {
              typeName = typeName substring 11
            }
            if (typeName contains '.') {
              // TODO: this is not correct, a select chain needs to be created here
              new Ident(newTypeName(typeName))
            } else {
              new Ident(newTypeName(typeName))
            }
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
        
        if (tree.hasSymbolField) {
          // this test also excludes Applys because they have no symbol field
          // of their own
          rw.symbol = tree.symbol
        }
        rw.pos = tree.pos
        rw
      }
      def rts[T <: Tree](ts: List[T]): List[T] = ts map rt
      
      val tOut = new Template(
          tIn.parents, 
          tIn.self, 
          rts(tIn.body) ++ hf.proxyDefs(cSym)
      )
      tOut.symbol = tIn.symbol
      tOut.pos = tIn.pos
      tOut
    }
  }
  
  override def newTransformer(unit: CompilationUnit): global.Transformer =
    new HyperfluxProxyTransformer(unit)

  class HyperfluxProxifierPhase(prev: nsc.Phase) extends Phase(prev) {
    override def name = phaseName
    override def description =
      "rewrites from client to server"
    
    override def run() {
      
      // at this point, we have identified the server objects and the methods
      // they provide, and we have determined which of them are actually used
      // by which client components. we now want to create a suitable proxy
      // method for each of those usages.
      
      // initialize dictionaries
      hf.clientObjects.foreach { sym =>
        hf.proxyDefs += ((sym, new HashSet[Tree]))
        hf.proxyAliases += ((sym, new HashMap[(Symbol, Symbol), Symbol]))
      }
      
      hf.packageIDs foreach { case (pSym, pid) =>
        // create a new compilation unit for the proxy methods
        val proxyUnit = new CompilationUnit(NoSourceFile)
        proxyUnit.body = buildProxyTree(pSym, pid)
        
        //treeBrowser.browse("proxy-pre-compile", proxyUnit :: Nil)
        
        // run namer phase
        val proxNamer = analyzer.namerFactory.newPhase(NoPhase)
        proxNamer(proxyUnit)
        //treeBrowser.browse("proxy-namer", proxyUnit :: Nil)
        
        // run package objects phase
        val proxPOer = analyzer.packageObjects.newPhase(proxNamer)
        proxPOer(proxyUnit)
        //treeBrowser.browse("proxy-po", proxyUnit :: Nil)
        
        // run typer phase
        val proxTyper = analyzer.typerFactory.newPhase(proxPOer)
        proxTyper(proxyUnit)
        treeBrowser.browse("proxy-typer", proxyUnit :: Nil)
        
        // extract proxy methods
        proxyUnit.body match {
          case PackageDef(_, stats) => stats foreach {
            case m @ ModuleDef(_, _, impl) => {  
              impl.body foreach { t =>
                // the proxy method definitions need not only be remembered for
                // later addition but also associated to the right server method
                val proxObjName = t.symbol.name.toString()
                /*var proxObjName = ""
                t match {
                  case DefDef(_, name, _, _, _, _) => proxObjName = name.toString()
                  case ClassDef(_, name, _, _) => proxObjName = name.toString()
                  case ModuleDef(_, name, _) => proxObjName = name.toString()
                  case _ =>
                }*/
                var usingClientObjects: List[Symbol] = Nil
                val matchingPair = hf.methodUsages.values.flatten find {
                  case (objSym, methSym) => {
                    proxObjName startsWith
                      createProxyMethodName(objSym, methSym)
                  }
                } match {
                  case Some(pair) => {
                    println("pair " + pair + " matches " + proxObjName)
                    usingClientObjects = (hf.methodUsages.keys filter {
                      sym => hf.methodUsages(sym) contains pair
                    }).toList
                    pair
                  }
                  case None => {
                    println("no pair matches " + proxObjName)
                    warning("proxy creator created an unused method")
                    (null, null)
                  }
                }
                
                t match {
                  case d @ DefDef(_, name, _, _, _, _) =>
                    usingClientObjects foreach { co =>
                      copyToClientDefList(co, d)
                      hf.proxyAliases(co) += ((matchingPair, t.symbol))
                    }
                
                  // the data carrier case classes only need to be remembered
                  case c @ ClassDef(_, _, _, _) =>
                    usingClientObjects foreach { co =>
                      copyToClientDefList(co, c)
                    }
                  case m @ ModuleDef(_, _, _) =>
                    usingClientObjects foreach { co =>
                      copyToClientDefList(co, m)
                    }
                  case x @ _ => warning(
                      "the proxy creator created something strange: " + x)
                }
              }
            }
            // imports can be skipped
            case _ =>
          }
        }
      }
      
      // at this point, a proxy method has been created and compiled for every
      // server method used, and for every client component, an alias table
      // has been created. we can now start rewriting the client components.
            
      // apply transformations from HFProxyTransformer
      super.run()
    }
    
    /**
     * copies a proxy method or class definition to the dictionary of a client
     * component, rewriting symbol ownerships accordingly
     */
    def copyToClientDefList(co: Symbol, proxyDef: Tree) {
      val proxyObjSym = proxyDef.symbol.owner;
      
      def cc[T <: Tree](tree: T): T = {
        val rw = (tree match {
          // TermTree types:
          case Block(stats, expr) => new Block(ccs(stats), cc(expr))
          case Alternative(ts) => new Alternative(ccs(ts))
          case Star(elem) => new Star(cc(elem))
          case UnApply(fun, args) => new UnApply(cc(fun), ccs(args))
          case ArrayValue(elemtpt, elems) =>
            new ArrayValue(cc(elemtpt), ccs(elems))
          case Assign(lhs, rhs) => new Assign(cc(lhs), cc(rhs))
          case AssignOrNamedArg(lhs, rhs) => new AssignOrNamedArg(cc(lhs), cc(rhs))
          case If(cond, thenp, elsep) => new If(cc(cond), cc(thenp), cc(elsep))
          case Match(sel, cases) => new Match(cc(sel), ccs(cases))
          case Try(block, catches, finalizer) =>
            new Try(cc(block), ccs(catches), cc(finalizer))
          case Throw(expr) => new Throw(cc(expr))
          case New(tpt) => new New(cc(tpt))
          case Typed(expr, tpt) => new Typed(cc(expr), cc(tpt))
          case TypeApply(fun, args) => new TypeApply(cc(fun), ccs(args))
          case Apply(fun, args) => new Apply(cc(fun), ccs(args))
          case Super(qual, mix) => new Super(cc(qual), mix)
          case ReferenceToBoxed(ident) => new ReferenceToBoxed(cc(ident))
          case Literal(const) => new Literal(const)
          case EmptyTree => EmptyTree
          // TypTree types:
          case SingletonTypeTree(ref) => new SingletonTypeTree(cc(ref))
          case CompoundTypeTree(templ) => new CompoundTypeTree(cc(templ))
          case AppliedTypeTree(tpt, args) =>
            new AppliedTypeTree(cc(tpt), ccs(args))
          case TypeBoundsTree(lo, hi) => new TypeBoundsTree(cc(lo), cc(hi))
          case ExistentialTypeTree(tpt, wheres) =>
            new ExistentialTypeTree(cc(tpt), ccs(wheres))
          case tt @ TypeTree() => {
            var typeName = tt.tpe.toString()
            if (typeName startsWith "scala.this.") {
              typeName = typeName substring 11
            }
            if (typeName contains '.') {
              // TODO: this is not correct, a select chain needs to be created here
              new Ident(newTypeName(typeName))
            } else {
              new Ident(newTypeName(typeName))
            }
          }
          // SymTree types:
          case Select(qual, name) => new Select(cc(qual), name)
          case Ident(name) => new Ident(name)
          case SelectFromTypeTree(qual, name) =>
            new SelectFromTypeTree(cc(qual), name)
          case PackageDef(pid, stats) => new PackageDef(cc(pid), ccs(stats))
          case ClassDef(Modifiers(flags, pw, ann), name, tparams, impl) =>
            new ClassDef(
                new Modifiers(flags, pw, ccs(ann)),
                name,
                ccs(tparams),
                cc(impl)
            )
          case ModuleDef(Modifiers(flags, pw, ann), name, impl) =>
            new ModuleDef(new Modifiers(flags, pw, ccs(ann)), name, cc(impl))
          case ValDef(Modifiers(flags, pw, ann), name, tpt, rhs) =>
            new ValDef(
                new Modifiers(flags, pw, ccs(ann)),
                name,
                cc(tpt),
                cc(rhs)
            )
          case DefDef(Modifiers(flags, pw, ann), name, tps, vpss, tpt, rhs) =>
            new DefDef(
                new Modifiers(flags, pw, ccs(ann)),
                name,
                ccs(tps),
                vpss map ccs,
                cc(tpt),
                cc(rhs)
            )
          case TypeDef(Modifiers(flags, pw, ann), name, tps, rhs) =>
            new TypeDef(
                new Modifiers(flags, pw, ccs(ann)),
                name, 
                ccs(tps),
                cc(rhs)
            )
          case LabelDef(name, params, rhs) =>
            new LabelDef(name, ccs(params), cc(rhs))
          case Bind(name, body) => new Bind(name, cc(body))
          case Import(expr, sels) => new Import(cc(expr), sels)
          case Template(parents, self, body) =>
            new Template(ccs(parents), cc(self), ccs(body))
          case Function(vps, body) => new Function(ccs(vps), cc(body))
          case Return(expr) => new Return(cc(expr))
          case ApplyDynamic(qual, args) => new ApplyDynamic(cc(qual), ccs(args))
          case This(qual) => new This(qual)
          case CaseDef(pat, guard, body) =>
            new CaseDef(cc(pat), cc(guard), cc(body))
          case Annotated(ann, arg) => new Annotated(cc(ann), cc(arg))
        }).asInstanceOf[T]
        
        // copy symbol, change to client module where necessary
        if (tree.hasSymbolField) {
          rw.symbol = tree.symbol
          if (tree.symbol.safeOwner == proxyObjSym) {
            rw.symbol.owner = co
          }
        }
        
        // copy type, adjust ownership where necessary
        // TODO: this is probably not the most elegant test
        if (tree.tpe.toString() startsWith "PROXY__") { 
          // TODO
        } else {
          rw setType tree.tpe
        }
        
        rw
      }
      def ccs[T <: Tree](trees: List[T]): List[T] = trees map cc
      
      hf.proxyDefs(co) += cc(proxyDef)
    }
    
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
        PARAMACCESSOR | CASEACCESSOR,
        tpnme.EMPTY,
        Nil
    )
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
    
    def buildProxyTree(pSym: Symbol, pid: RefTree): Tree = {

      val pBody = new MutableList[Tree]
      pBody += importHfProtocol
      pBody += importUPickle
      hf.serverMethods foreach { case (objSym, methodMap) =>
        if (objSym.safeOwner == pSym) {
          val mBody = new MutableList[Tree]
          methodMap foreach { case (methSym, methDef) =>
            
            if (hf.methodUsages exists {
              case (_, pairs) => pairs contains ((objSym, methSym))
              case _ => false
            }) {
            
              // here, we create a proxy method by building up the program tree
              // from scratch depending on the signature of the original method
              
              val proxName = createProxyMethodName(objSym, methSym)
              val proxMethTermName = newTermName(proxName)
              val proxDataTypeName = newTypeName(proxName + "__DATA")
              val methBody = new MutableList[Tree]
              
              // extract the list of argument names from the method signature
              val argNames = {
                if (methDef.vparamss.size > 0) {
                  methDef.vparamss.head map { vd => new Ident(vd.name) }
                } else {
                  Nil
                }
              }
              
              val argVals = {
                if (methDef.vparamss.size > 0) {
                  methDef.vparamss.head map { vd =>
                    new ValDef(ccParamMods, vd.name, ct(vd.tpt), EmptyTree)
                  }
                } else {
                  Nil
                }
              }
              
              // the case class that will be used as a data carrier
              // this is only needed if there are parameters to be sent
              if (argNames.size > 0) {
                val proxyDataCC = new ClassDef(
                    caseClassMods,
                    proxDataTypeName,
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
                              methDef.vparamss map cts,
                              new TypeTree(),
                              new Block(
                                  new Apply(
                                      new Select(
                                          new Super(
                                              new This(emptyTypeName),
                                              emptyTypeName
                                          ),
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
                mBody += proxyDataCC
              
                // the instantiation of the data carrier class
                val createData = new ValDef(
                    nullMods,
                    dataTermName,
                    new TypeTree(), // please infer this type!
                    new Apply(
                        new Select(
                            new New(new Ident(proxDataTypeName)),
                            initTermName
                        ),
                        argNames
                    )
                )
                methBody += createData
              }
              
              // the call to the central server proxy method
              val call = new ValDef(
                  nullMods,
                  retTermName,
                  new TypeTree(),
                  new Apply(
                      new Ident(callServerTermName),
                      List(
                          new Literal(new Constant(objSym.name.toString())),
                          new Literal(new Constant(methSym.name.toString())),
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
              methBody += call
              
              // the extraction of the result
              val extract = new Apply(
                  new TypeApply(
                      new Ident(readTermName),
                      ct(methDef.tpt) :: Nil
                  ),
                  new Ident(retTermName) :: Nil
              )
              
              val proxMeth = new DefDef(
                  nullMods,
                  newTermName(proxName),
                  Nil,
                  methDef.vparamss map cts,
                  ct(methDef.tpt),
                  new Block(methBody.toList, extract)
              )
              mBody += proxMeth
            }
          }
          
          pBody += new ModuleDef(
              nullMods,
              newTermName(createProxyObjectName(objSym)),
              new Template(Nil, noSelfType, mBody.toList)
          )
        }
      }
      new PackageDef(pid, pBody.toList)
    }
    
    def createProxyObjectName(objSym: Symbol): String =
      "PROXY__" + objSym.name.toString().replace('.', '_')
        
    def createProxyMethodName(objSym: Symbol, methSym: Symbol): String = {
      val objName = objSym.name.toString().replace('.', '_')
      val methName = methSym.name.toString().replace('.', '_')
      "PROXY__" + objName + "_" + methName
    }
    
    /**
     * copies a tree without copying the symbols
     */
    def ct[T <: Tree](tree: T): T = (tree match {
      // TermTree types:
      case Block(stats, expr) => new Block(cts(stats), ct(expr))
      case Alternative(ts) => new Alternative(cts(ts))
      case Star(elem) => new Star(ct(elem))
      case UnApply(fun, args) => new UnApply(ct(fun), cts(args))
      case ArrayValue(elemtpt, elems) =>
        new ArrayValue(ct(elemtpt), cts(elems))
      case Assign(lhs, rhs) => new Assign(ct(lhs), ct(rhs))
      case AssignOrNamedArg(lhs, rhs) => new AssignOrNamedArg(ct(lhs), ct(rhs))
      case If(cond, thenp, elsep) => new If(ct(cond), ct(thenp), ct(elsep))
      case Match(sel, cases) => new Match(ct(sel), cts(cases))
      case Try(block, catches, finalizer) =>
        new Try(ct(block), cts(catches), ct(finalizer))
      case Throw(expr) => new Throw(ct(expr))
      case New(tpt) => new New(ct(tpt))
      case Typed(expr, tpt) => new Typed(ct(expr), ct(tpt))
      case TypeApply(fun, args) => new TypeApply(ct(fun), cts(args))
      case Apply(fun, args) => new Apply(ct(fun), cts(args))
      case Super(qual, mix) => new Super(ct(qual), mix)
      case ReferenceToBoxed(ident) => new ReferenceToBoxed(ct(ident))
      case Literal(const) => new Literal(const)
      case EmptyTree => EmptyTree
      // TypTree types:
      case SingletonTypeTree(ref) => new SingletonTypeTree(ct(ref))
      case CompoundTypeTree(templ) => new CompoundTypeTree(ct(templ))
      case AppliedTypeTree(tpt, args) =>
        new AppliedTypeTree(ct(tpt), cts(args))
      case TypeBoundsTree(lo, hi) => new TypeBoundsTree(ct(lo), ct(hi))
      case ExistentialTypeTree(tpt, wheres) =>
        new ExistentialTypeTree(ct(tpt), cts(wheres))
      case tt @ TypeTree() => {
        var typeName = tt.tpe.toString()
        if (typeName startsWith "scala.this.") {
          typeName = typeName substring 11
        }
        if (typeName contains '.') {
          // TODO: this is not correct, a select chain needs to be created here
          new Ident(newTypeName(typeName))
        } else {
          new Ident(newTypeName(typeName))
        }
      }
      // SymTree types:
      case Select(qual, name) => new Select(ct(qual), name)
      case Ident(name) => new Ident(name)
      case SelectFromTypeTree(qual, name) =>
        new SelectFromTypeTree(ct(qual), name)
      case PackageDef(pid, stats) => new PackageDef(ct(pid), cts(stats))
      case ClassDef(Modifiers(flags, pw, ann), name, tparams, impl) =>
        new ClassDef(
            new Modifiers(flags, pw, cts(ann)),
            name,
            cts(tparams),
            ct(impl)
        )
      case ModuleDef(Modifiers(flags, pw, ann), name, impl) =>
        new ModuleDef(new Modifiers(flags, pw, cts(ann)), name, ct(impl))
      case ValDef(Modifiers(flags, pw, ann), name, tpt, rhs) =>
        new ValDef(
            new Modifiers(flags, pw, cts(ann)),
            name,
            ct(tpt),
            ct(rhs)
        )
      case DefDef(Modifiers(flags, pw, ann), name, tps, vpss, tpt, rhs) =>
        new DefDef(
            new Modifiers(flags, pw, cts(ann)),
            name,
            cts(tps),
            vpss map cts,
            ct(tpt),
            ct(rhs)
        )
      case TypeDef(Modifiers(flags, pw, ann), name, tps, rhs) =>
        new TypeDef(
            new Modifiers(flags, pw, cts(ann)),
            name, 
            cts(tps),
            ct(rhs)
        )
      case LabelDef(name, params, rhs) =>
        new LabelDef(name, cts(params), ct(rhs))
      case Bind(name, body) => new Bind(name, ct(body))
      case Import(expr, sels) => new Import(ct(expr), sels)
      case Template(parents, self, body) =>
        new Template(cts(parents), ct(self), cts(body))
      case Function(vps, body) => new Function(cts(vps), ct(body))
      case Return(expr) => new Return(ct(expr))
      case ApplyDynamic(qual, args) => new ApplyDynamic(ct(qual), cts(args))
      case This(qual) => new This(qual)
      case CaseDef(pat, guard, body) =>
        new CaseDef(ct(pat), ct(guard), ct(body))
      case Annotated(ann, arg) => new Annotated(ct(ann), ct(arg))
    }).asInstanceOf[T]
    
    def cts[T <: Tree](trees: List[T]): List[T] = trees map ct
  }
}


