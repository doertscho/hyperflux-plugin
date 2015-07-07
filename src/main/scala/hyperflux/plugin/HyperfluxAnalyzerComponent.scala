package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import hyperflux.annotation._

abstract class HyperfluxAnalyzerComponent extends PluginComponent {

  import global._

  val phaseName = "hf-analyzer"

  override def newPhase(prev: nsc.Phase): StdPhase = new HyperfluxAnalyzerPhase(prev)

  class HyperfluxAnalyzerPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description =
      "gathers information and tells client from server parts"
    
    override def run() {
      println("hyperflux analyzer phase starts running now!")
      super.run()
    }
    
    override def apply(unit: CompilationUnit) {
      var body = unit.body
      findAnnotations(body)
    }
    
    // TODO: find a way to get the class names type-safely at compile time
    val CLIENT_CLASS_NAME = "hyperflux.annotation.Client"
    val SERVER_CLASS_NAME = "hyperflux.annotation.Server"
    def findAnnotations(tree: Tree) {
      tree match {
        case ModuleDef(_, name, _) => {
          tree.symbol.annotations foreach {
            _.atp.baseClasses foreach {
              _.fullName match {
                case CLIENT_CLASS_NAME => println("client module: " + name)
                case SERVER_CLASS_NAME => println("server module: " + name)
                case _                 =>
              }
            }
          }
          tree.children foreach findAnnotations
        }
        case Import(_, _) => // imports can be skipped, no need to go deeper
        // TODO: optimizations for several node types
        case _ => tree.children foreach findAnnotations
      }
    }
  }
}
