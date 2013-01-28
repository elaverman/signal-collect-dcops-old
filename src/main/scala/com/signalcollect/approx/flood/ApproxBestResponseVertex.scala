package com.signalcollect.approx.flood

import com.signalcollect._
//import scala.collection.immutable._
//import scala.Int
//import collection.JavaConversions._

trait ApproxBestResponseVertex[Id, Int] extends DataGraphVertex[Id, Int] {

  def neighbourConfigs: Map[Any, Int]
  
  def constraints: Iterable[Constraint]

  def possibleValues: Array[Int]
  
  def utility: Double
  
  def computeUtility(ownConfig: Int): Double

  /**
   * Returns true if this vertex cannot change its state and have a higher utility
   * (weak NE condition if fulfilled for all vertices in the graph).
   */
  def existsBetterStateUtility: Boolean

  def computeIfBetterStatesExist(currentState: Int, currentStateUtility: Double): Boolean 

  override def toString = {
    val stringResult = "Vertex ID: " + id + ", State: " + state + " Utility: " + utility + "/" + constraints.size +
      "\n Edges: " + this.outgoingEdges.size + //values + 
      "\n Possible values: " + possibleValues.mkString("; ") +
      "\n Constraints: " + constraints.mkString("; ")

    stringResult
  }

}