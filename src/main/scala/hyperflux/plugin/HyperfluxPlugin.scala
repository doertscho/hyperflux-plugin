package hyperflux.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.collection.mutable.TreeSet
import scala.collection.mutable.HashSet

class HyperfluxPlugin(val global: Global) extends Plugin {
  import global._

  val name = "hyperflux"
  val description = "Creates client-server structure"

  // some storage objects that are used across the phases
  val ehf = new HyperfluxStorage[TermName, ValDef, Tree]
  
  val components = List[PluginComponent](
    HyperfluxInterfaceAnalyzerComponent,
    HyperfluxUsageAnalyzerComponent,
    HyperfluxProxifierComponent,
    HyperfluxServerCreatorComponent
  )
  
  /*
   * 
   * EARLY
   * 
   */
  object HyperfluxInterfaceAnalyzerComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("parser")
    override val runsRightAfter = Some("parser")
    override val runsBefore = List("hf-u-analyzer")
    override val hf = ehf
  } with HyperfluxInterfaceAnalyzerComponent
  
  object HyperfluxUsageAnalyzerComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("hf-i-analyzer")
    override val runsRightAfter = Some("hf-i-analyzer")
    override val runsBefore = List("hf-proxifier")
    override val hf = ehf
  } with HyperfluxUsageAnalyzerComponent
  
  object HyperfluxProxifierComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("hf-u-analyzer")
    override val runsRightAfter = Some("hf-u-analyzer")
    override val runsBefore = List("hf-server-creator")
    override val hf = ehf
  } with HyperfluxProxifierComponent
  
  object HyperfluxServerCreatorComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("hf-proxifier")
    override val runsRightAfter = Some("hf-proxifier")
    override val runsBefore = List("namer")
    override val hf = ehf
  } with HyperfluxServerCreatorComponent
  
}
