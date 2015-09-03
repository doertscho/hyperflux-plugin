package hyperflux.plugin

import scala.tools.nsc
import scala.tools.nsc._
import scala.tools.nsc.plugins._
import hyperflux.annotation._
import scala.collection.mutable.HashSet
import scala.reflect.internal.Flags._

abstract class HyperfluxEarlyUsageAnalyzerComponent extends PluginComponent {

  import global._

  val phaseName = "hf-eu-analyzer"
  
  val hf: HyperfluxEarlyStorage[TermName, ValDef, Tree]

  override def newPhase(prev: nsc.Phase): StdPhase =
    new HyperfluxEarlyUsageAnalyzerPhase(prev)

  class HyperfluxEarlyUsageAnalyzerPhase(prev: nsc.Phase) extends StdPhase(prev) {
    override def name = phaseName
    override def description =
      "examines which server methods are actually used in client parts"
    
    override def apply(unit: CompilationUnit) {
      findUsages(unit.body)
      println(hf.methodUsages)
    }
    
    def findUsages(t: Tree): Unit = t match {
      case PackageDef(pid, stats) => stats foreach findUsages
      case ModuleDef(Modifiers(_, _, anns), oName, impl)
      if (hf.clientObjects contains oName) => analyzeClientObject(oName, impl.body)
      case _ =>
    }
    
    def analyzeClientObject(coName: TermName, ts: List[Tree]) {
      hf.methodUsages += ((coName, new HashSet[(TermName, TermName)]))
      ts foreach acc
      
      def acc(t: Tree): Unit = t match {
        // TermTree types:
        case Block(es, e) => { accs(es) ; acc(e) }
        case Alternative(ts) => accs(ts)
        case Star(e) => acc(e)
        case UnApply(f, as) => { acc(f) ; accs(as) }
        case ArrayValue(t, es) => { acc(t) ; accs(es) }
        case Assign(l, r) => { acc(l) ; acc(r) }
        case AssignOrNamedArg(l, r) => { acc(l) ; acc(r) }
        case If(c, t, e) => { acc(c) ; acc(t) ; acc(e) }
        case Match(s, cs) => { acc(s) ; accs(cs) }
        case Try(b, cs, f) => { acc(b) ; accs(cs) ; acc(f) }
        case Throw(e) => acc(e)
        case New(t) => acc(t)
        case Typed(e, t) => { acc(e) ; acc(t) }
        case TypeApply(f, as) => { acc(f) ; accs(as) }
        
        /*
         * the interesting part:
         */
        case Apply(f, as) => {
          f match {
            case Select(Ident(oName), mName)
            if (
              (hf.serverObjects contains oName.toTermName) &&
              (hf.serverMethods(oName.toTermName) contains mName.toTermName)
            ) =>
              hf.methodUsages(coName) += ((oName.toTermName, mName.toTermName))
            case _ =>
          }
          accs(as)
        }
        
        case Super(q, m) => acc(q)
        case _: ReferenceToBoxed =>
        case _: Literal =>
        case EmptyTree =>
        // TypTree types:
        case SingletonTypeTree(r) => acc(r)
        case CompoundTypeTree(t) => acc(t)
        case AppliedTypeTree(t, as) => { acc(t) ; accs(as) }
        case TypeBoundsTree(l, h) => { acc(l) ; acc(h) }
        case ExistentialTypeTree(_, ws) => accs(ws)
        case _: TypeTree =>
        // SymTree types:
        case Select(q, n) => acc(q)
        case _: Ident =>
        case SelectFromTypeTree(q, _) => acc(q)
        case _: PackageDef => // should not occur
        case ClassDef(_, _, _, i) => acc(i)
        case ModuleDef(_, _, i) => acc(i)
        case ValDef(_, _, _, r) => acc(r)
        case DefDef(_, _, _, _, _, r) => acc(r)
        case TypeDef(_, _, _, r) => acc(r)
        case LabelDef(_, _, r) => acc(r)
        case Bind(_, b) => acc(b)
        case _: Import =>
        case Template(_, _, b) => accs(b)
        case Function(_, b) => acc(b)
        case Return(e) => acc(e)
        case ApplyDynamic(q, as) => { acc(q) ; accs(as) }
        case _: This => 
        case CaseDef(p, g, b) => { acc(p) ; acc(g) ; acc(b) }
        case Annotated(an, ar) => { acc(an) ; acc(ar) }
      }
      def accs(ts: List[Tree]) = ts foreach acc
    }
  }
}