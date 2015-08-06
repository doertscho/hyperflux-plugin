package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.collection.mutable.HashSet

abstract class HyperfluxUsageAnalyzerComponent extends PluginComponent {

  import global._

  val phaseName = "hf-u-analyzer"
  
  val hf: HyperfluxStorage[Symbol, Template, DefDef, RefTree, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxUsageAnalyzerPhase(prev)

  class HyperfluxUsageAnalyzerPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description =
      "examines which server methods are actually used in client parts"
    
    override def apply(unit: CompilationUnit) {
      findUsages(unit.body)
    }
    
    def findUsages(tree: Tree) {
      tree match {
        case PackageDef(_, stats) =>
          stats foreach findUsages
        case ModuleDef(mods, name, impl) 
        if (hf.clientObjects contains impl.symbol.safeOwner) =>
          analyzeClientComponent(impl)
        // change nothing on anything else on this level
        case _ =>
      }
    }
    
    def analyzeClientComponent(impl: Template) {
      val compSym = impl.symbol.safeOwner
      hf.methodUsages += ((compSym, new HashSet[(Symbol, Symbol)]))
      
      def accInt(tree: Tree) {
        tree match {
          // an actual method call
          case Apply(fun, args) => {
            if (hf.serverObjects contains fun.symbol.safeOwner) {
              // TODO: at this point, it should not happen that the called
              // server method is not defined or private, because otherwise
              // the compiler would have complained in an earlier phase.
              // however, this should be tested
              hf.methodUsages(compSym) += ((fun.symbol.safeOwner, fun.symbol))
            }
            args foreach accInt
          }
          case Select(qualifier, _) => accInt(qualifier)
          case DefDef(_, _, _, _, _, rhs) => accInt(rhs)
          case Block(stats, expr) => { stats foreach accInt ; accInt(expr) }
          case ValDef(_, _, _, rhs) => accInt(rhs)
          case If(cond, thenp, elsep) => {
            accInt(cond)
            accInt(thenp)
            accInt(elsep)
          }
          // TODO: add further cases
          case _ =>
        }
      }
      impl.children foreach accInt
    }
  }
}