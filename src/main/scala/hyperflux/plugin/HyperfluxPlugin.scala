package hyperflux.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins._

class HyperfluxPlugin(val global: Global) extends Plugin {
  import global._

  val name = "hyperflux"
  val description = "Creates client-server-structure"
  val components = List[PluginComponent](
    SplitComponent
  )
  
  object SplitComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("typer")
    override val runsBefore = List("jsinterop")
  } with SplitComponent
}