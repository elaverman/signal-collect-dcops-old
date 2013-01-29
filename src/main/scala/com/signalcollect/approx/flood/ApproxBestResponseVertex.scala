package com.signalcollect.approx.flood

import com.signalcollect._
import java.util.Random
//import scala.collection.immutable._
//import scala.Int
//import collection.JavaConversions._

trait ApproxBestResponseVertex[Id, Int] extends DataGraphVertex[Id, Int] {

  def neighbourConfig: Map[Any, Int]

  def constraints: Iterable[Constraint]

  def possibleValues: Array[Int]

  def utility: Double

  def computeUtility(ownConfig: Int): Double

  /**
   * Returns true if this vertex cannot change its state and have a higher utility
   * (weak NE condition if fulfilled for all vertices in the graph).
   */
  def existsBetterStateUtility: Boolean

 
  def computeMaxUtilityState(utilityFunction: Int => Double): Int = { //TODO is this ok as a generalization?
    val utilities = possibleValues map (value => (value, utilityFunction(value)))
    val maxUtility = utilities map (_._2) max
    val maxUtilityStates = utilities filter (_._2 == maxUtility)
    val r = new Random
    val resultPos = r.nextInt(maxUtilityStates.size)
    maxUtilityStates(resultPos)._1
  }

  def computeIfBetterStatesExist(currentState: Int, currentStateUtility: Double, utilityFunction: Int => Double = computeUtility): Boolean = {
    var maxState = computeMaxUtilityState(utilityFunction)
    (utilityFunction(maxState) > utilityFunction(currentState))		
    //(utilityFunction(maxState) >= utilityFunction(currentState))&&(maxState!=currentState)&&.... //for strict NE?	
  }
  
//  def computeIfBetterStatesExist(currentState: Int, currentStateUtility: Double): Boolean = {
//    var existsBetterThanCurrentStateUtility: Boolean = false
//    var i: Int = 0
//    while (!(existsBetterThanCurrentStateUtility) && (i < possibleValues.size)) {
//      val candidateState = possibleValues(i)
//      val possibleStatesConfig = neighbourConfig + (id -> candidateState)
//      val possibleStatesConfigsUtility = constraints.foldLeft(0.0)((a, b) => a + b.utility(possibleStatesConfig))
//      if ((candidateState != currentState) && (possibleStatesConfigsUtility > currentStateUtility)) // strict NE when >= TODO change back to >=
//        existsBetterThanCurrentStateUtility = true
//      i = i + 1
//    }
//    existsBetterThanCurrentStateUtility
//  }
  
  

  override def toString = {
    val stringResult = "Vertex ID: " + id + ", State: " + state + " Utility: " + utility + "/" + constraints.size +
      "\n Edges: " + this.outgoingEdges.size + //values + 
      "\n Possible values: " + possibleValues.mkString("; ") +
      "\n Constraints: " + constraints.mkString("; ")

    stringResult
  }

}