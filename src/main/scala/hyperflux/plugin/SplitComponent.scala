package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._

abstract class SplitComponent extends PluginComponent {
  
  import global._

  val phaseName = "c-s-splitter"
  
  override def newPhase(prev: nsc.Phase): StdPhase = new SplitPhase(prev)
  
  class SplitPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name: String = phaseName
    override def description: String = "Split compilation units into client and server parts"
    override def run() {
      
      super.run()
    }
    override def apply(unit: CompilationUnit) {
      
      println(unit.body)
    }
  }
}