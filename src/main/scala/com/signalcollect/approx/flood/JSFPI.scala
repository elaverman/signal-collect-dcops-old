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


//(id: Any, initialState: Int, csts: Iterable[Constraint], possibleValues: Array[Int], explorationProbability: (Int, Double) => Double)

class JSFPIVertex(id: Any, initialState: Int, csts: Iterable[Constraint], possibleValues: Array[Int], fadingMemory: Double, inertia: Double) extends DataGraphVertex(id, 0) {

  type Signal = Int
 // var inertia: Double = 0.5 //time counter used for calculating temperature
  var weightedAvgUtilities: Array[Double] = Array.fill[Double](possibleValues.size)(0)
  var utility: Double = 0
  val constraints: Iterable[Constraint] = csts
  var canBeImproved: Boolean = false
  var oldStateWeightedAvgUtility: Double = 0
 // var numberSatisfied: Int = 0 //number of satisfied constraints
//  val fadingMemory: Double = 0.03 //constant rho for fading memory  - is 1 if we do not take into account memory and only current utility

  /**
   * The collect function chooses a new random state and chooses it if it improves over the old state,
   * or, if it doesn't it still chooses it (for exploring purposes) with probability decreasing with time
   */
  def collect(oldState: Int, mostRecentSignals: Iterable[Int]): Int = {

    //Update the weighted average utilities for each action 
    val neighbourConfigs = mostRecentSignalMap.map(x => (x._1,x._2)).toMap //neighbourConfigs must be immutable and mostRecentSignalMap is mutable, so we convert

    for (i <- 0 to (possibleValues.size - 1)) {
      state = possibleValues(i)
      val possibleStatesConfigs = neighbourConfigs + (id -> state)
      val possibleStatesConfigsUtility = constraints.foldLeft(0.0)((a, b) => a + b.utility(possibleStatesConfigs))
        //(constraints map (_.utility(possibleStatesConfigs)) sum)
      weightedAvgUtilities(i) = fadingMemory * possibleStatesConfigsUtility + (1 - fadingMemory) * weightedAvgUtilities(i)
    }
    //end Update weighted average utilities and select candidate state

    //Select one of the actions with maximum weighted average utilities as candidate State

    //we initialize the candidate state and the maximumUtility to the first possible state and to its weightedAverageUtility 
    var candidateState = possibleValues(0)
    var maxUtility = weightedAvgUtilities(0)
    var maximumsCount = 1 //how many states with the maximumUtility

    for (i <- 0 to (possibleValues.size - 1)) {
      if (weightedAvgUtilities(i) > maxUtility) {
        candidateState = possibleValues(i)
        maxUtility = weightedAvgUtilities(i)
        maximumsCount = 1
      } else { 
        if (weightedAvgUtilities(i) == maxUtility) {
          maximumsCount += 1
        }
      }
    }

    
    var str: String = "vertex "+id+" WAU vector: "
    for (i <- 0 to (possibleValues.size - 1)) 
      str = str + weightedAvgUtilities(i).toString +" "
      
    println(str)
    
    //if we have more than 1 states with the same maximum utility we have to select randomly from them
    val r = new Random()
    if (maximumsCount != 1) {
      var nThMaximum: Int = r.nextInt(maximumsCount) + 1 //random between 1 and the number of maximum utility states

      for (i <- 0 to (possibleValues.size - 1)) {
        if (weightedAvgUtilities(i) == maxUtility) {
          if (nThMaximum == 0) {
            candidateState = possibleValues(i)
          } else {
            nThMaximum -= 1
          }
        }
      }
    }
    //end Selection of the candidate State

    //Calculate utility and number of satisfied constraints for the candidate state
    val candidateStateConfigs = neighbourConfigs + (id -> candidateState)
    val candidateStateUtility = constraints.foldLeft(0.0)((a, b) => a + b.utility(candidateStateConfigs))
    
    //Calculate utility and number of satisfied constraints for the old state
    oldStateWeightedAvgUtility = weightedAvgUtilities(oldState) 
    
    val configs = neighbourConfigs + (id -> oldState)
    utility = constraints.foldLeft(0.0)((a, b) => a + b.utility(configs)) //TODO: should use weightedAvg utilities!!!! 
      //(constraints map (_.utility(oldStateConfigs)) sum)

    
    canBeImproved = (maxUtility > oldStateWeightedAvgUtility)//||((maxUtility==utility)&&(maximumsCount != 1)) //only STRICT NE are absorbing!
      
    
    // With some inertia we keep the last state even if it's not the best. Else, we update to the best new state
    
    val probability: Double = r.nextDouble()

    if ((probability > inertia) && (candidateState != oldState)) { // we adopt the new maximum state, else we do not change state
      println("Vertex: " + id + "; changed to state: " + candidateState + " of new WAU/utility " + maxUtility + "/" + candidateStateUtility+" instead of old state " + oldState + " with WAU/utility " + oldStateWeightedAvgUtility + "/" + utility+"; prob = " + probability + " > inertia =  " + inertia)
      //numberSatisfied = constraints.foldLeft(0)((a, b) => a + b.satisfiesInt(candidateStateConfigs))
        //constraints map (_.satisfiesInt(candidateStateConfigs)) sum;
      canBeImproved = false
      utility = candidateStateUtility
      return candidateState
    } else {
      if (candidateState!=oldState)
      println("Vertex: " + id + "; NOT changed to state: " + candidateState + " of new utility " + maxUtility + " instead of old state " + oldState + " with utility " + utility + "; prob = " + probability + " < inertia =  " + inertia)
      //numberSatisfied = constraints.foldLeft(0)((a, b) => a + b.satisfiesInt(configs))
        //constraints map (_.satisfiesInt(oldStateConfigs)) sum;
      return oldState
    }

  } //end collect function

  
  //TODO: write functions for NE, global optimum
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
