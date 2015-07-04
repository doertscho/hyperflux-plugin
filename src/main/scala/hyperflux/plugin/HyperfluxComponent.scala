package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._

abstract class HyperfluxComponent extends PluginComponent {

  import global._

  val phaseName = "hyperflux"

  override def newPhase(prev: nsc.Phase): StdPhase = new HyperfluxPhase(prev)

  class HyperfluxPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name: String = phaseName
    override def description: String =
      "Hyperflux magic: create client-server links, change annotations"
    override def run() {
      println("hyperflux phase starts running now!")
      super.run()
    }
    override def apply(unit: CompilationUnit) {
      // dummy task: find @Server and @Client annotations
      var body = unit.body
      findAnnotations(body)
    }
    def findAnnotations(tree: Tree) {
      tree match {
        case PackageDef(_, _) => tree.children foreach findAnnotations
        case Import(_, _) => // no need to go deeper
        case ModuleDef(_, name, _) => {
          tree.symbol.annotations foreach { a =>
            println("found annotation " + a.atp + " on module " + name)
          }
          tree.children foreach findAnnotations
        }
        case Template(_, _, body) => body foreach findAnnotations
        case _ => println("unhandled tree type: " + tree.symbol)
      }
    }
  }
}
