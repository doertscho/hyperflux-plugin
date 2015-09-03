package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import hyperflux.annotation._
import scala.collection.mutable.HashMap
import scala.reflect.internal.Flags._

abstract class HyperfluxEarlyInterfaceAnalyzerComponent extends PluginComponent {

  import global._

  val phaseName = "hf-ei-analyzer"
  
  val hf: HyperfluxEarlyStorage[TermName, ValDef, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxEarlyInterfaceAnalyzerPhase(prev)

  class HyperfluxEarlyInterfaceAnalyzerPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description =
      "gathers information and tells client from server parts"
    
    override def apply(unit: CompilationUnit) {
      findAnn(unit.body)
    }
    
    val HF_SERVER_ANN = "Server"
    val HF_CLIENT_ANN = "Client"
    def findAnn(t: Tree): Unit = t match {
      case PackageDef(pid, stats) => stats foreach findAnn
      case ModuleDef(Modifiers(_, _, anns), oName, impl) => {
        anns find { 
          case Apply(Select(New(tpt), _), _) => tpt.toString() match {
            case HF_SERVER_ANN => {
              hf.serverObjects += oName
              analyzeServerObject(oName, impl.body)
              true
            }
            case HF_CLIENT_ANN => {
              hf.clientObjects += oName 
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
  }
}