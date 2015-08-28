package hyperflux.plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.collection.mutable.TreeSet

class HyperfluxPlugin(val global: Global) extends Plugin {
  import global._

  val name = "hyperflux"
  val description = "Creates client-server structure"

  // some storage objects that are used across the phases
  val hf: HyperfluxStorage[Symbol, DefDef, RefTree, Tree] = 
    new HyperfluxStorage[Symbol, DefDef, RefTree, Tree]
  
  val components = List[PluginComponent](
    HyperfluxInterfaceAnalyzerComponent,
    HyperfluxUsageAnalyzerComponent,
    HyperfluxProxifierComponent
  )

  // the analysis part has to be done in two phases because there might well
  // be references across compilation units, so before looking out for usages
  // of server methods, we first need to collect the available server methods
  // from ALL units
  
  object HyperfluxInterfaceAnalyzerComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    override val runsBefore = List("hf-u-analyer")
    override val hf = HyperfluxPlugin.this.hf
  } with HyperfluxInterfaceAnalyzerComponent

  object HyperfluxUsageAnalyzerComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("hf-i-analyzer")
    override val runsRightAfter = Some("hf-i-analyzer")
    override val runsBefore = List("hf-proxyfier")
    override val hf = HyperfluxPlugin.this.hf
  } with HyperfluxUsageAnalyzerComponent

  object HyperfluxProxifierComponent extends {
    val global: HyperfluxPlugin.this.global.type = HyperfluxPlugin.this.global
    override val runsAfter = List("hf-u-analyzer")
    override val runsRightAfter = Some("hf-u-analyzer")
    override val runsBefore = List("jsinterop")
    override val hf = HyperfluxPlugin.this.hf
  } with HyperfluxProxifierComponent
  
}
