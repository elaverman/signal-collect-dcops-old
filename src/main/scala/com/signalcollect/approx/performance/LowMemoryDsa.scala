/*
 *  @author Mihaela Verman
 *  @author Philip Stutz
 *  
 *  Copyright 2013 University of Zurich
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

package com.signalcollect.approx.performance

import com.signalcollect._
import scala.util.Random
import com.signalcollect.graphproviders.synthetic.Grid
import scala.collection.mutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import com.signalcollect.interfaces._
import scala.collection.mutable.HashMap
import com.signalcollect.graphproviders.synthetic.Chain
import com.signalcollect.approx.flood.Constraint
import com.signalcollect.approx.flood.ConstraintVertexBuilder
import com.signalcollect.evaluation.algorithms.CompactIntSet
import collection.JavaConversions._

class LowMemoryDsaVertexBuilder(algorithmDescription: String) extends ConstraintVertexBuilder {
  def apply(id: Int, constraints: Iterable[Constraint], domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val colors: Set[Byte] = (0 until domain.size).toSet map ((color: Int) => color.asInstanceOf[Byte])
    val v = new LowMemoryDsa(id, domain.size.asInstanceOf[Byte], domain(r.nextInt(domain.size)).asInstanceOf[Byte])
    val targetIdArray = ((constraints flatMap (_.variablesList filter (_ != id))).toSet.asInstanceOf[Set[Int]]).toArray[Int]
    val compact = CompactIntSet.create(targetIdArray)
    v.setTargetIdArray(compact)
    v
  }

  override def toString = "LowMemoryDsa - " + algorithmDescription
}

class LowMemoryDsa(val id: Int, numColors: Byte, var state: Byte) extends Vertex[Int, Byte] {

  type Signal = Byte

  var lastSignalState: Byte = -1

  def setState(s: Byte) {
    state = s
  }

  protected var targetIdArray = Array[Byte]()

  override def addEdge(e: Edge[_], graphEditor: GraphEditor[Any, Any]): Boolean = {
    throw new Exception("Adding edges is not currently supported, but easy to implement.")
    false
  }

  def setTargetIdArray(links: Array[Byte]) = targetIdArray = links

  def executeSignalOperation(graphEditor: GraphEditor[Any, Any]) {
    CompactIntSet.foreach(targetIdArray, graphEditor.sendSignal(state, _, Some(id)))
    lastSignalState = state
  }

  protected val mostRecentSignalMap = new java.util.HashMap[Int, Byte](4)

  def deliverSignal(signal: Any, sourceId: Option[Any]): Boolean = {
    mostRecentSignalMap.put(sourceId.get.asInstanceOf[Int], signal.asInstanceOf[Byte])
    false
  }

  override def executeCollectOperation(graphEditor: GraphEditor[Any, Any]) {
    state = collect
  }

  def scoreCollect = 1

  override def edgeCount = {
    var noEdges = 0
    CompactIntSet.foreach(targetIdArray, { targetId => noEdges += 1 })
    noEdges
  }

  var currentConflicts = Int.MaxValue

  def utility = edgeCount - currentConflicts

  def existsBetterStateUtility: Boolean = {
    val stateCostTable = new Array[Int](numColors)
    val signalIterator = mostRecentSignalMap.values.iterator
    while (signalIterator.hasNext) {
      val color = signalIterator.next
      stateCostTable(color) += 1
    }
    var i = 0
    var leastConflicts = Int.MaxValue
    var leastConflictsCount = 0 // How many states would produce this least number of conflicts.
    while (i < stateCostTable.length) {
      if (stateCostTable(i) == leastConflicts) {
        leastConflictsCount += 1
      } else if (stateCostTable(i) < leastConflicts) {
        leastConflicts = stateCostTable(i)
        leastConflictsCount = 1
      }
      i += 1
    }
    val currentConflictCount = stateCostTable(state)
    currentConflictCount == leastConflicts
  }

  def afterInitialization(graphEditor: GraphEditor[Any, Any]) = {}
  def beforeRemoval(graphEditor: GraphEditor[Any, Any]) = {}

  override def removeEdge(targetId: Any, graphEditor: GraphEditor[Any, Any]): Boolean = {
    throw new UnsupportedOperationException
  }

  override def removeAllEdges(graphEditor: GraphEditor[Any, Any]): Int = {
    throw new UnsupportedOperationException
  }

  /** Returns a random color */
  def getRandomColor: Byte = Random.nextInt(numColors).asInstanceOf[Byte]

  def collect: Byte = {
    val stateCostTable = new Array[Int](numColors)
    val signalIterator = mostRecentSignalMap.values.iterator
    while (signalIterator.hasNext) {
      val color = signalIterator.next
      stateCostTable(color) += 1
    }
    var i = 0
    var leastConflicts = Int.MaxValue
    var leastConflictsCount = 0 // How many states would produce this least number of conflicts.
    while (i < stateCostTable.length) {
      if (stateCostTable(i) == leastConflicts) {
        leastConflictsCount += 1
      } else if (stateCostTable(i) < leastConflicts) {
        leastConflicts = stateCostTable(i)
        leastConflictsCount = 1
      }
      i += 1
    }
    currentConflicts = stateCostTable(state)
    val maxDelta = currentConflicts - leastConflicts
    val probability = Random.nextDouble
    val inertia = 0.5
    if (((maxDelta > 0) || ((maxDelta == 0) && (currentConflicts > 0))) && (probability > inertia)) {
      val returnIndex = Random.nextInt(leastConflictsCount)
      var currentLeastConflictIndex = 0
      i = 0
      while (i < numColors) {
        if (stateCostTable(i) == leastConflicts) {
          if (returnIndex == currentLeastConflictIndex) {
            currentConflicts = leastConflicts
            return i.asInstanceOf[Byte]
          } else {
            currentLeastConflictIndex += 1
          }
        }
        i += 1
      }
      throw new Exception("This case should be impossible")
    }
    state
  }

  /**
   * The signal score is 1 if this vertex hasn't signaled before or if it has
   *  changed its color (kept track of by informNeighbors). Else it's 0.
   */
  override def scoreSignal = {
    if (lastSignalState != state) {
      1
    } else {
      if (currentConflicts > 0) {
        1
      } else {
        0
      }
    }
  }

  override def toString = "LowMemoryDsaVertex(id=" + id + ",state=" + state + ")"

}

/**
 * Builds a Vertex Coloring compute graph and executes the computation
 *
 * StateForwarderEdge is a built-in edge type that simply sends the state
 * of the source vertex as the signal, which means that this algorithm does
 * not require a custom edge type.
 */
object LowMemoryDsaExecutable extends App {
  //  val grid = new Grid(100, 100)
  val graph = new GraphBuilder[Int, Any].build
  val chainBuilder = new Chain(5, true)
  chainBuilder.populate(graph, (id: Int) => new LowMemoryDsa(id, 2, state = 1), (sourceId, targetId) => new StateForwarderEdge(targetId))
  //  graph.addVertex(new BrFast(1, numColors = 2, state = 1.asInstanceOf[Byte]))
  //  graph.addVertex(new BrFast(2, numColors = 2, state = 1.asInstanceOf[Byte]))
  //  graph.addVertex(new BrFast(3, numColors = 2, state = 1.asInstanceOf[Byte]))
  //  graph.addEdge(1, new StateForwarderEdge(2))
  //  graph.addEdge(2, new StateForwarderEdge(1))
  //  graph.addEdge(2, new StateForwarderEdge(3))
  //  graph.addEdge(3, new StateForwarderEdge(2))

  val stats = graph.execute(ExecutionConfiguration.withTimeLimit(10000))
  graph.foreachVertex(println(_))
  println(stats)
  graph.shutdown
}