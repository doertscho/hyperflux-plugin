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
  
  val hf: HyperfluxStorage[Symbol, DefDef, RefTree, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxInterfaceAnalyzerPhase(prev)

  class HyperfluxInterfaceAnalyzerPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description =
      "gathers information and tells client from server parts"
    
    override def apply(unit: CompilationUnit) {
      println("actual unit root symbol: " + unit.body.symbol)
      findAnnotations(unit.body)
    }
    
    // TODO: find a way to get the class names type-safely at compile time
    val CLIENT_CLASS_NAME = "hyperflux.annotation.Client"
    val SERVER_CLASS_NAME = "hyperflux.annotation.Server"
    var currentPackageID: RefTree = _
    def findAnnotations(tree: Tree) {
      tree match {
        case PackageDef(pid, stats) => {
          currentPackageID = pid
          stats foreach findAnnotations
        }
        case ModuleDef(_, _, impl) => {
          tree.symbol.annotations foreach {
            _.atp.baseClasses foreach {
              _.fullName match {
                case CLIENT_CLASS_NAME =>
                  hf.packageIDs += ((tree.symbol.safeOwner, currentPackageID))
                  hf.clientObjects += impl.symbol.safeOwner
                case SERVER_CLASS_NAME => {
                  hf.packageIDs += ((tree.symbol.safeOwner, currentPackageID))
                  hf.serverObjects += impl.symbol.safeOwner
                  analyzeServerObject(impl)
                }
                case _ =>
              }
            }
          }
          // for now, we only allow those annotations on top-level objects
          //impl.children foreach findAnnotations
        }
        case Import(_, _) => // imports can be skipped, no need to go deeper
        // TODO: optimizations for several node types
        case _ => //tree.children foreach findAnnotations
      }
    }
    
    /*
    def debugTree(tree: Tree) {
      println("__ debug info")
      
      def debugInt(tree: Tree, lvl: Int) {
        println("  " * lvl + tree.symbol)
        tree.children foreach { debugInt(_, lvl + 1) }
      }
      debugInt(tree, 0)
    }*/
    
    def analyzeServerObject(impl: Template) {
      hf.serverMethods +=
        ((impl.symbol.safeOwner, new HashMap[Symbol, DefDef]))
      // for the sake of simplicity (and in order to enforce a clear design)
      // only top-level method definitions in our server object are registered
      // as remotely callable functions
      impl.children foreach {
        case d @ DefDef(mods, name, _, _, _, _)
        if (
          // private methods should of course not be exposed
          !(mods.hasAccessBoundary) &&
          !(mods.hasFlag(PRIVATE)) &&
          // same for the constructor
          !(name containsName "<init>") &&
          // furthermore, we want to exclude methods that are mere accessors to
          // values and variables (this may be integrated later)
          !(mods.hasFlag(ACCESSOR))
        ) =>
          hf.serverMethods(impl.symbol.safeOwner) += ((d.symbol, d))
        case _ =>
      }
    }
  }
}
