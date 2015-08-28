package hyperflux.plugin

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

/**
 * Instances of this class serve as a cross-phase data storage for the
 * Hyperflux plugin. It is necessary because all phases depend on analyses
 * and transformations made be previous phases.
 * 
 * In this case, S is supposed to represent the symbol type,
 * D is for DefDef, R for RefTree and T for Tree.
 * They have to be passed as parameters, however, due to complex
 * cross-linkings in the levels above.
 */
class HyperfluxStorage[S, D, R, T] {

  /**
   * which package definitions exist in the current run?
   * each package name (S) is stored along the matching qualifier (R)
   */
  val packageIDs = new HashMap[S, R]
  
  /**
   * which objects with a Server annotation are there?
   * each object's name (S) is stored 
   */
  val serverObjects = new HashSet[S]
  
  /**
   * which (public) methods do those server objects provide?
   * for each object, this map stores its name (first S) along with
   * a map that holds all public methods, identified by their names
   * (second S) and accompanied by their implementation (D)
   */
  val serverMethods = new HashMap[S, HashMap[S, D]]
  
  /**
   * which client objects are there? this is simple a set of their names
   */
  val clientObjects = new HashSet[S]
  
  /**
   * which server methods are used in which client object?
   * for each client object, we store its name (first S) along with a set
   * of (server object name, method name) pairs (second and third S)
   */
  val methodUsages = new HashMap[S, HashSet[(S, S)]]
  
  /**
   * which proxy methods have been created for this run?
   * for each client component (S), there is a set of proxy methods, case
   * classes and case objects (T) that are required for the methods of this
   * component
   */
  val proxyDefs = new HashMap[S, HashSet[T]]
  
  /**
   * which proxy methods tunnel to which server objects?
   * as proxy method are added to each client object individually, this map
   * holds a seperate map for each client object (first S) which maps
   * server methods (second and third S, for server object name and method
   * name) to the associated proxy method (fourth S)
   */
  val proxyAliases = new HashMap[S, HashMap[(S, S), S]]
  
}
