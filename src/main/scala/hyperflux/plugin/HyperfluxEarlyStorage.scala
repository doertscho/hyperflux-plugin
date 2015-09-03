package hyperflux.plugin

import scala.tools.nsc._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class HyperfluxEarlyStorage[TermName, ValDef, Tree] {

  /**
   * The names of all identified server and client objects.
   */
  val serverObjects = new HashSet[TermName]
  val clientObjects = new HashSet[TermName]
  
  /**
   * Each server object has a set of methods, identified by their name,
   * associated to the param lists and their return type.
   */
  val serverMethods = new HashMap[
    TermName,
    HashMap[TermName, (List[List[ValDef]], Tree)]
  ]
  
  
  val methodUsages = new HashMap[TermName, HashSet[(TermName, TermName)]]
  
  val proxyDefs = new HashMap[(TermName, TermName), HashSet[Tree]]
  
  val proxyAliases = new HashMap[(TermName, TermName), TermName]
}