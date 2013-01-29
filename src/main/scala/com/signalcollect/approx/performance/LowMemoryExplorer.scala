/*
 *  @author Philip Stutz
 *  
 *  Copyright 2010 University of Zurich
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
import scala.collection.mutable.OpenHashMap
import com.signalcollect.approx.flood.Constraint
import com.signalcollect.approx.flood.ConstraintVertexBuilder
import com.signalcollect.evaluation.algorithms.CompactIntSet

class LowMemoryExplorerVertexBuilder(algorithmDescription: String) extends ConstraintVertexBuilder {
  def apply(id: Int, constraints: Iterable[Constraint], domain: Array[Int]): Vertex[Any, _] = {
    val r = new Random
    val colors: Set[Byte] = (0 until domain.size).toSet map ((color: Int) => color.asInstanceOf[Byte])
    val v = new LowMemoryExplorer(id, domain.size.asInstanceOf[Byte], domain(r.nextInt(domain.size)).asInstanceOf[Byte])
    val targetIdArray = ((constraints flatMap (_.variablesList filter (_ != id))).toSet.asInstanceOf[Set[Int]]).toArray[Int]
    v.setTargetIdArray(CompactIntSet.create(targetIdArray))
    v
  }

  override def toString = "DSAN - " + algorithmDescription
}

/**
 * 	This algorithm attempts to find a vertex coloring.
 * A valid vertex coloring is defined as an assignment of labels (colors)
 * 	to vertices such that no two vertices that share an edge have the same label.
 *
 * Usage restriction: this implementation *ONLY* works on *UNDIRECTED* graphs.
 * In Signal/Collect this means that there is either no edge between 2 vertices
 * or one in each direction.
 *
 * @param id: the vertex id
 * @param numColors: the number of colors (labels) used to color the graph, from 0 to numColors-1
 */
class LowMemoryExplorer(val id: Int, numColors: Byte, var state: Byte) extends Vertex[Int, Byte] {

  type Signal = Byte

  var lastSignalState: Byte = -1

  def setState(s: Byte) {
    state = s
  }

  protected var targetIdArray = Array[Byte]()

  override def addEdge(e: Edge[_], graphEditor: GraphEditor[Any, Any]): Boolean = {
    throw new Exception("Adding edges is not curently supported, but easy to implement.")
  }

  def setTargetIdArray(links: Array[Byte]) = targetIdArray = links

  def executeSignalOperation(graphEditor: GraphEditor[Any, Any]) {
    CompactIntSet.foreach(targetIdArray, graphEditor.sendSignal(state, _, Some(id)))
    lastSignalState = state
  }

  protected val mostRecentSignalMap = new OpenHashMap[Int, Byte](1)

  def deliverSignal(signal: Any, sourceId: Option[Any]): Boolean = {
    mostRecentSignalMap.put(sourceId.get.asInstanceOf[Int], signal.asInstanceOf[Byte])
    false
  }

  override def executeCollectOperation(graphEditor: GraphEditor[Any, Any]) {
    state = collect
  }

  def scoreCollect = 1

  def edgeCount = targetIdArray.size

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

  def isInConflict: Boolean = {
    var conflict = false
    var i = 0
    val signalIterator = mostRecentSignalMap.values.iterator
    while (!conflict && signalIterator.hasNext) {
      val signal = signalIterator.next
      if (signal == state) {
        conflict = true
      }
    }
    conflict
  }

  /**
   * Checks if one of the neighbors shares the same color. If so, the state is
   *  set to a random color and the neighbors are informed about this vertex'
   *  new color. If no neighbor shares the same color, we stay with the old color.
   */
  def collect: Byte = {
    //    println(id + " collecting " + state)
    if (isInConflict) {
      val freeColors = ((0 until numColors).toSet map ((color: Int) => color.asInstanceOf[Byte])) -- mostRecentSignalMap.values.toSet
      val numberOfFreeColors = freeColors.size
      if (numberOfFreeColors > 0) {
        freeColors.toSeq(Random.nextInt(numberOfFreeColors))
      } else {
        getRandomColor
      }
    } else {
      state
    }
  }

  /**
   * The signal score is 1 if this vertex hasn't signaled before or if it has
   *  changed its color (kept track of by informNeighbors). Else it's 0.
   */
  override def scoreSignal = {
    if (lastSignalState != state) {
      1
    } else {
      if (isInConflict) {
        1
      } else {
        0
      }
    }
  }

  override def toString = "ColoredVertex(id=" + id + ",state=" + state + ")"

}

/**
 * Builds a Vertex Coloring compute graph and executes the computation
 *
 * StateForwarderEdge is a built-in edge type that simply sends the state
 * of the source vertex as the signal, which means that this algorithm does
 * not require a custom edge type.
 */
object LowMemoryExplorerExecutable extends App {
  //  val grid = new Grid(100, 100)
  val graph = new GraphBuilder[Int, Any].build
  val chainBuilder = new Chain(5, true)
  chainBuilder.populate(graph, (id: Int) => new LowMemoryExplorer(id, 2, state = 1), (sourceId, targetId) => new StateForwarderEdge(targetId))
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