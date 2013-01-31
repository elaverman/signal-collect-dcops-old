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
 * Implementation of Distributed Simulated Annealing 
 * ( Arshad, Silaghi, 2003. "Distributed Simulated Annealing and comparison to DSA". 
 *  In Proceedings of the 4th International Workshop on Distributed Contraint Reasoning, Acapulco, Mexico)
 */

package com.signalcollect.approx.flood

import com.signalcollect._
import com.signalcollect.configuration._
import com.signalcollect.configuration.LoggingLevel._
import scala.math
import com.signalcollect.interfaces.MessageBus
import com.signalcollect.interfaces.SignalMessage
import com.signalcollect.dcopgraphproviders.AdoptFileGraphGenerator
import com.signalcollect.interfaces.AggregationOperation
import java.io.File
import java.util.Random
import collection.JavaConversions._
import com.signalcollect.approx.performance.LowMemoryDsa
/**
 * Represents an Agent
 *
 *  @param id: the identifier of this vertex
 *  @param constraints: the set of constraints in which it is involved
 *  @param possibleValues: which values can the state take
 */

//Takes vertex id, constraints for the vertex with that id and the domain of the variable represented by the vertex.
trait ConstraintVertexBuilder extends Function3[Int, Iterable[Constraint], Array[Int], Vertex[Any, _]] with Serializable {
  def apply(id: Int, constraints: Iterable[Constraint], domain: Array[Int]): Vertex[Any, _]
}

class DSANVertexBuilder(algorithmDescription: String, explorationProbability: (Int, Double) => Double) extends ConstraintVertexBuilder {
  def apply(id: Int, constraints: Iterable[Constraint], domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val v = new DSANVertex(id, domain(r.nextInt(domain.size)), constraints, domain, explorationProbability)

    for (ctr <- constraints) {
      for (variable <- ctr.variablesList) {
        if (variable != id) {
          v.addEdge(new StateForwarderEdge(variable), null.asInstanceOf[GraphEditor[Any, Any]])
        }
      }
    }

    v
  }

  override def toString = "DSAN - " + algorithmDescription
}

class GoogleDSANVertexBuilder(algorithmDescription: String, explorationProbability: (Int, Double) => Double) extends ConstraintVertexBuilder {
  def apply(id: Int, constraints: Iterable[Constraint], domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val v = new DSANVertex(id, domain(r.nextInt(domain.size)), constraints, domain, explorationProbability)
    v
  }

  override def toString = "Google DSAN - " + algorithmDescription
}

//TODO Converged not when global optimum is obtained but when temperature reaches 0 and i'm not in a NE

class DSANVertex(
  id: Int,
  initialState: Int,
  var constraints: Iterable[Constraint],
  val possibleValues: Array[Int],
  explorationProbability: (Int, Double) => Double)
  extends DataGraphVertex(id, initialState)
  with ApproxBestResponseVertex[Int, Int] {

  type Signal = Int
  val r = new Random

  var time: Int = 0

  var neighbourConfig: Map[Any, Int] = _
  val maxDelta: Double = (-1 * constraints.size).toDouble
  var utility: Double = 0
  var existsBetterStateUtility = false


  def computeUtility(ownConfig: Int): Double = {
    //Calculate utility and number of satisfied constraints for the current value
    val config = neighbourConfig + (id -> ownConfig)
    constraints.foldLeft(0.0)((a, b) => a + b.utility(config))
  }

  def getRandomState = possibleValues(r.nextInt(possibleValues.size))

  /**
   * The collect function chooses a new random state and chooses it if it improves over the old state,
   * or, if it doesn't it still chooses it (for exploring purposes) with probability decreasing with time
   *
   * Selects randomly a value and adopt it with probability (e(delta/t_i)) when delta<=0 (to explore)
   * or with probability 1 otherwise.
   */
  def collect: Int = {
    neighbourConfig = mostRecentSignalMap.map(x => (x._1, x._2)).toMap
    time += 1
    utility = computeUtility(state) 

    //Calculate utility and number of satisfied constraints for the new value

    val newState = getRandomState
    val newStateUtility = computeUtility(newState) 

    // Delta is the difference between the utility of the new randomly selected state and the utility of the old state. 
    // It is > 0 if the new state would lead to improvements
    val delta: Double = newStateUtility - utility

    /**
     * The actual algorithm:
     * 	If new state does not improve the utility (delta<=0), select it for exploring purposes with probability (e(delta/t_i)) or else keep the old state
     * 	If new state does improve the utility (delta>0), select it instead of the old state
     * t_i is the temperature which has to be a decreasing function of time. For this particular case we chose t_i = constTemp/(time*time)
     */

    if (delta <= 0) { //The new state does not improve utility
      val adopt = r.nextDouble
      if (adopt < explorationProbability(time, delta)) {
        // We choose the new state (to explore) over the old state with probability (e(delta/t_i))
        utility = newStateUtility
        existsBetterStateUtility = computeIfBetterStatesExist(newState)
        //println("Vertex: " + id + " utility " + utility + " at time " + time + "; Case DELTA=" + delta + "<= 0 and changed to state: " + newState + " instead of " + state + " with Adoption of new state prob =" + explorationProbability(time, delta) + " ")
        newState
      } else {
        //With probability 1 - (e(delta/t_i)) we keep the old state which is better
        existsBetterStateUtility = computeIfBetterStatesExist(state)
        state
      }
    } else {
      //The new state improves utility (delta>0), so we adopt the new state
      utility = newStateUtility
      //println("Vertex: " + id + " at time " + time + "; Case DELTA=" + delta + "> 0 and changed to state: " + newState + " instead of " + state)
      existsBetterStateUtility = computeIfBetterStatesExist(newState)
      newState
    }
  }



  def isFrozen = explorationProbability(time, maxDelta) < 0.000001

  /**
   * When should it stop signaling?
   * (When the temperature reaches 0 (almost) = no exploration and
   * there are no better states to which it can go into.)
   * OR
   * (when it has satisfied all the local constraints)
   */
  override def scoreSignal: Double = {
    if ((isStateUnchanged)&&(areAllLocalConstraintsSatisfied)) {
          // This vertex is happy, no need to signal.
          0
        } else {
          // Things could still be better, let's signal.
          1
        }
  }
}

class GlobalUtility extends AggregationOperation[(Int, Double)] with Serializable {
  val neutralElement = (0, 0.0)
  def extract(v: Vertex[_, _]): (Int, Double) = v match {
    case vertex: ApproxBestResponseVertex[_,_] => (vertex.edgeCount, vertex.utility)
    case vertex: LowMemoryDsa =>  (vertex.edgeCount, vertex.utility)
    case other => neutralElement
  }
  def reduce(elements: Stream[(Int, Double)]) = elements.foldLeft(neutralElement)(aggregate)
  def aggregate(a: (Int, Double), b: (Int, Double)): (Int, Double) = (a._1 + b._1, a._2 + b._2)
}

class NashEquilibrium extends AggregationOperation[Boolean] {
  val neutralElement = true
  def extract(v: Vertex[_, _]): Boolean = v match {
    case vertex: ApproxBestResponseVertex[_, _] => !vertex.existsBetterStateUtility
    case vertex: LowMemoryDsa => !vertex.existsBetterStateUtility
    case other => {
      //throw new Exception("No ApproxBestResponseVertex")
      neutralElement
    }
  }
  def reduce(elements: Stream[Boolean]) = elements.foldLeft(neutralElement)(aggregate)
  def aggregate(a: Boolean, b: Boolean): Boolean = a && b
}

class DSANGlobalTerminationCondition(
  /*f: java.io.FileWriter,
  g: java.io.FileWriter,*/
  startTime: Long,
  aggregationOperation: AggregationOperation[(Int, Double)],
  aggregationInterval: Long) extends GlobalTerminationCondition[(Int, Double)](aggregationOperation, aggregationInterval)
  with Serializable {
  def shouldTerminate(aggregate: (Int, Double)): Boolean = {
    if (aggregate._1 - aggregate._2 < 0.001) true
    else {
//      f.write(aggregate._1 - aggregate._2 + " ")
//      g.write((System.nanoTime() - startTime).toString + " ")
//      print(aggregate._1 - aggregate._2 + " " + (System.nanoTime() - startTime).toString + "; ")

      false
    }
  }
}

/** Builds an agents graph and executes the computation */
object DSAN extends App {

  val myDirectory = new java.io.File("data-sets/problems/")
  val outS = new java.io.FileWriter("resultsS.txt")
  val outTimeS = new java.io.FileWriter("resultsTimeS.txt")
  val outA = new java.io.FileWriter("resultsA.txt")
  val outTimeA = new java.io.FileWriter("resultsTimeA.txt")

  //  for (file <- myDirectory.listFiles) {
  //
  //    //Synchronous Execution
  //    val graphGenS = new AdoptFileGraphGenerator(file.getAbsolutePath())
  //    val graphS = graphGenS.constraintGraph
  //
  //    println("From client -  Sync: Graph built from file: " + file.getName())
  //    outS.write(file.getName() + "S ")
  //    outTimeS.write(file.getName() + "S ")
  //    var startTime = System.nanoTime()
  //    val terminationConditionS = new DSANGlobalTerminationCondition(outS, outTimeS, startTime)
  //    val statsS = graphS.execute(ExecutionConfiguration().withExecutionMode(ExecutionMode.Synchronous).withGlobalTerminationCondition(terminationConditionS).withStepsLimit(1000))
  //    outS.write(statsS.aggregatedWorkerStatistics.numberOfOutgoingEdges.toString + "\n")
  //    outTimeS.write("\n")
  //    println("\n" + statsS.executionStatistics.computationTime)
  //    graphS.shutdown
  //
  //    //Asynchronous Execution
  //    val graphGenA = new AdoptFileGraphGenerator(file.getAbsolutePath())
  //    val graphA = graphGenA.constraintGraph
  //
  //    println("From client - Async: Graph built from file: " + file.getName())
  //    outA.write(file.getName() + "A ")
  //    outTimeA.write(file.getName() + "A ")
  //    startTime = System.nanoTime()
  //    val terminationConditionA = new DSANGlobalTerminationCondition(outA, outTimeA, startTime)
  //    val statsA = graphA.execute(ExecutionConfiguration().withExecutionMode(ExecutionMode.OptimizedAsynchronous).withGlobalTerminationCondition(terminationConditionA).withTimeLimit(500))
  //    outA.write(statsA.aggregatedWorkerStatistics.numberOfOutgoingEdges.toString + "\n")
  //    outTimeA.write("\n")
  //    println("\n" + statsA.executionStatistics.computationTime)
  //    graphA.shutdown

  // }

  outS.close
  outTimeS.close
  outA.close
  outTimeA.close

}

