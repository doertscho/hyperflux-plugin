package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import hyperflux.annotation._
import scala.collection.mutable.HashMap
import scala.reflect.internal.Flags._

abstract class HyperfluxInterfaceAnalyzerComponent extends PluginComponent {

  import global._

  val phaseName = "hf-i-analyzer"
  
  val hf: HyperfluxStorage[TermName, ValDef, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxInterfaceAnalyzerPhase(prev)

  class HyperfluxInterfaceAnalyzerPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description =
      "gathers information and tells client from server parts"
    
    override def run() {
      identifyCompilationTarget()
      super.run()
    }
    
    /*
     * In the first compiler phase, we need to identify whether we are
     * currently compiling the client parts to JavaScript or the server
     * parts to JVM bytecode
     */
    def identifyCompilationTarget() {
      // probably not the most elegant test
      hf.compilesToJS = 
        currentSettings.plugin.value exists (_ contains "scalajs-compiler")
      inform("Compilation target: " + (if (hf.compilesToJS) "JS" else "JVM"))
    }
    
    override def apply(unit: CompilationUnit) = findAnn(unit.body)
    
    val HF_SERVER_ANN = "Server"
    val HF_CLIENT_ANN = "Client"
    val HF_INTERFACE_ANN = "Interface"
    def findAnn(t: Tree): Unit = t match {
      case PackageDef(pid, stats) => stats foreach findAnn
      case ModuleDef(Modifiers(_, _, anns), oName, impl) => {
        anns find { 
          case Apply(Select(New(tpt), _), args) => tpt.toString() match {
            case HF_SERVER_ANN => {
              if (args.size == 2 || args.size == 3) {
                val argsSeq = args.toIndexedSeq
                argsSeq(0) match {
                  case Literal(Constant(url: String)) =>
                    argsSeq(1) match {
                      case Literal(Constant(port: Int)) =>
                        hf.serverURL += ((oName, s"$url:$port"))
                        hf.serverPort += ((oName, port))
                      case _ => error(
                        "second argument of @Server annotation must be an Int")
                    }
                  case _ => error(
                      "first argument of @Server annotation must be a String")
                }
              } else {
                error("wrong number of arguments for @Server annotation")
              }
              hf.serverObjects += oName
              analyzeServerObject(oName, impl.body)
              true
            }
            case HF_CLIENT_ANN => {
              hf.clientObjects += oName 
              true
            }
            case HF_INTERFACE_ANN => {
              hf.interfaceObjects += oName
              analyzeInterfaceObject(oName, impl.body)
              true
            }
            case _ => false
          }
          case _ => false
        }
      }
      case _ =>
    }
    
    def analyzeServerObject(oName: TermName, ts: List[Tree]) {
      hf.serverMethods +=
        ((oName, new HashMap[TermName, (List[List[ValDef]], Tree)]))
      ts foreach {
        case DefDef(mods, mName, _, pss, tpt, _)
        if (
          // private methods should of course not be exposed
          !(mods.hasAccessBoundary) &&
          !(mods.isPrivate) &&
          // same for the constructor
          mName != nme.CONSTRUCTOR &&
          // furthermore, we want to exclude methods that are mere accessors to
          // values and variables (this may be integrated later)
          !(mods.hasAccessorFlag)
        ) => hf.serverMethods(oName) += ((mName, (pss, tpt)))
        case _ => 
      }
    }
    
    val HF_PAGE_ANN = "Page"
    val HF_ELEMENT_ANN = "Element"
    def analyzeInterfaceObject(oName: TermName, ts: List[Tree]): Unit =
      ts foreach (_ match {
        case ValDef(Modifiers(_, _, anns), vName, _, rhs) => {
          anns find { 
            case Apply(Select(New(tpt), _), args) => tpt.toString match {
              case HF_PAGE_ANN => {
                val pageURL = if (
                  (args.size == 0) ||
                  (args.head.toString == "__HF_AUTO")
                ) {
                  vName.toString
                } else {
                  args.head.toString
                }
                hf.pages += (((oName, vName), pageURL))
                true
              }
              case HF_ELEMENT_ANN => {
                rhs match {
                  case Select(Apply(Ident(eFun), _), selM)
                  if (selM.toString == "render") =>
                    hf.elements += (((oName, vName), eFun.toTermName))
                  case _ =>
                    hf.elements += (((oName, vName), newTermName("???")))
                }
                true
              }
              case _ => false
            }
          }
        }
        case _ =>
      })
  }
}