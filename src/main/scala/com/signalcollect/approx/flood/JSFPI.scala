/*
 *  @author Philip Stutz, Mihaela Verman
 *  
 *  Copyright 2012 University of Zurich
 *      
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *         http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  
 */

/*
 * Implementation of Joint Strategy Fictitious Play with Inertia
 * ( Marden, Arslan, Shamma, 2009. "Joint Strategy Fictitious Play With Inertia for Potential Games". 
 *  Automatic Control, IEEE Transactions on , vol.54, no.2, pp.208-220, Feb. 2009 doi: 10.1109/TAC.2008.2010885 )
 */

package com.signalcollect.approx.flood
import com.signalcollect._
import com.signalcollect.configuration._
import com.signalcollect.configuration.LoggingLevel._
import scala.util._
import java.util.Random
import com.signalcollect.interfaces.MessageBus

/**
 * Marden, Jason R., Gürdal Arslan, and Jeff S. Shamma. "Joint strategy fictitious play with inertia for potential games." Automatic Control, IEEE Transactions on 54.2 (2009): 208-220.
 *
 * Represents an Agent
 *
 *  @param id: the identifier of this vertex
 *  @param constraints: the set of constraints in which it is involved
 *  @param possibleValues: which values can the state take
 */

class JSFPIVertexBuilder(algorithmDescription: String, fadingMemory: Double = 0.03, inertia: Double = 0.5) extends ConstraintVertexBuilder {
  def apply(id: Int, constraints: Iterable[Constraint], domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val v = new JSFPIVertex(id, domain(r.nextInt(domain.size)), constraints, domain, fadingMemory, inertia)

    for (ctr <- constraints) {
      for (variable <- ctr.variablesList) {
        if (variable != id) {
          v.addEdge(new StateForwarderEdge(variable), null.asInstanceOf[GraphEditor[Any, Any]])
        }
      }
    }

    v
  }

  override def toString = "JSFPI - " + algorithmDescription
}


class JSFPIVertex(
  id: Int,
  initialState: Int,
  var constraints: Iterable[Constraint],
  val possibleValues: Array[Int],
  fadingMemory: Double,
  inertia: Double)
  extends DataGraphVertex(id, 0)
  with ApproxBestResponseVertex[Int, Int] {

  type Signal = Int
  var weightedAvgUtilities: Array[Double] = Array.fill[Double](possibleValues.size)(0)
  var utility: Double = 0
  var oldStateWeightedAvgUtility: Double = 0
  var neighbourConfig: Map[Any, Int] = _



  var existsBetterStateUtility: Boolean = false

  def computeUtility(ownConfig: Int): Double = {
    //Calculate utility and number of satisfied constraints for the current value
    val config = neighbourConfig + (id -> ownConfig)
    constraints.foldLeft(0.0)((a, b) => a + b.utility(config))
  }

  
  /**
   * The collect function chooses a new random state and chooses it if it improves over the old state,
   * or, if it doesn't it still chooses it (for exploring purposes) with probability decreasing with time
   */
  def collect: Int = {

    neighbourConfig = mostRecentSignalMap.map(x => (x._1, x._2)).toMap
    utility = computeUtility(state)
    
    //Update the weighted average utilities for each action 
    var currentEvalState = 0
    for (i <- 0 to (possibleValues.size - 1)) {
      currentEvalState = possibleValues(i)
      val possibleStatesConfigsUtility = computeUtility(currentEvalState)
      weightedAvgUtilities(i) = fadingMemory * possibleStatesConfigsUtility + (1 - fadingMemory) * weightedAvgUtilities(i)
    }

    //Select one of the actions with maximum weighted average utilities as candidate State
    var candidateState = computeMaxUtilityState(weightedAvgUtilities)


    //existsBetterStateUtility = weightedAvgUtilities(candidateState) > weightedAvgUtilities(state)
    existsBetterStateUtility = computeUtility(candidateState) > computeUtility(state) //||((maxUtility==utility)&&(maximumsCount != 1)) //for strict NE. only STRICT NE are absorbing!


    // With some inertia we keep the last state even if it's not the best. Else, we update to the best new state

    val probability: Double = (new Random).nextDouble()

    if ((probability > inertia) && (candidateState != state)) { // we adopt the new maximum state, else we do not change state
      println("Vertex: " + id + "; changed to state: " + candidateState + " of new WAU/utility " + weightedAvgUtilities(candidateState) + "/" + computeUtility(candidateState) + " instead of old state " + state + " with WAU/utility " + weightedAvgUtilities(state) + "/" + computeUtility(state) + "; prob = " + probability + " > inertia =  " + inertia)
      existsBetterStateUtility = false
      utility = computeUtility(candidateState)
      return candidateState
    } else {
      if (candidateState != state)
        println("Vertex: " + id + "; NOT changed to state: " + candidateState + " of new WAU/utility " + weightedAvgUtilities(candidateState) + "/" + computeUtility(candidateState) + " instead of old state " + state + " with WAU/utility " + weightedAvgUtilities(state) + "/" + computeUtility(state) +  "; prob = " + probability + " < inertia =  " + inertia)
      return state
    }

  } //end collect function

  def isStateUnchanged = {
    lastSignalState match {
      case Some(oldState) => state == oldState
      case None => false
    }
  }
  

  override def scoreSignal: Double = {
    lastSignalState match {
      case Some(oldState) => //TODO: see if i would add that there are no other possibilities of moving into a same utility state
        //        if (/*(oldState == state) &&*/ (!canBeImproved || (utility == constraints.size))) { //computation is allowed to stop only if state has not changed and the utility is maximized - TODO: utility maximized should be expressed differently
        //          0                       //before it was numberSatisfied == constraints.size
        //        } else {
        1
      //     }
      case other => 1

    }

  } //end scoreSignal

} //end JSFPIVertex class

/** Builds an agents graph and executes the computation */
object JSFPI extends App {

  val graph = GraphBuilder.withLoggingLevel(LoggingLevel.Debug).build

  println("From client: Graph built")

  //Simple graph with 2 vertices

  //    val c12:  Constraint = Variable(1) != Variable(2)
  //    
  //    graph.addVertex(new JSFPIVertex(1, Array(c12), Array(0, 1)))
  //   	graph.addVertex(new JSFPIVertex(2, Array(c12), Array(0, 1)))
  //   
  //   	graph.addEdge(new StateForwarderEdge(1, 2))
  //   	graph.addEdge(new StateForwarderEdge(2, 1))

  //Graph with 6 nodes 

  var constraints: List[Constraint] = List()

  constraints = (Variable(1) != Variable(2)) :: constraints
  constraints = (Variable(1) != Variable(3)) :: constraints
  constraints = (Variable(3) != Variable(2)) :: constraints
  constraints = (Variable(3) != Variable(4)) :: constraints
  constraints = (Variable(5) != Variable(4)) :: constraints
  constraints = (Variable(5) != Variable(3)) :: constraints
  constraints = (Variable(5) != Variable(6)) :: constraints
  constraints = (Variable(6) != Variable(2)) :: constraints

  val fm: Double = 0.03
  val in: Double = 0.5
  for (i <- 1 to 6) {
    graph.addVertex(new JSFPIVertex(i, 0, constraints.filter(s => s.variablesList().contains(i)).toArray: Array[Constraint], Array(0, 1, 2), fm, in))
  }

  for (ctr <- constraints) {
    for (i <- ctr.variablesList()) {
      for (j <- ctr.variablesList()) {
        if (i != j)
          graph.addEdge(i, new StateForwarderEdge(j))
      }
    }
  }

  println("Begin")

  val stats = graph.execute(ExecutionConfiguration().withExecutionMode(ExecutionMode.Synchronous))
  println(stats)
  graph.foreachVertex(println(_))
  graph.shutdown
}
