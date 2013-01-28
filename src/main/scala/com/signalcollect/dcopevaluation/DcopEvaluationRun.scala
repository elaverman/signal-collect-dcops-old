/*
 *  @author Daniel Strebel
 *  @author Philip Stutz
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
 *  distributed under the License is distributed on an "AS IS" BASIS
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  
 */
package com.signalcollect.dcopevaluation

import com.signalcollect.GraphBuilder
import com.signalcollect.ExecutionInformation
import com.signalcollect.graphproviders.synthetic._
import com.signalcollect.ExecutionConfiguration
import com.signalcollect.Graph
import com.signalcollect.evaluation.algorithms._
import com.signalcollect.configuration.ExecutionMode
import com.signalcollect.graphproviders.GraphProvider
import com.signalcollect.approx.flood._
import com.signalcollect.StateForwarderEdge
import com.signalcollect.dcopgraphproviders._
import com.signalcollect.GraphEditor
import com.signalcollect.Vertex
import com.signalcollect.Vertex
import com.signalcollect.interfaces.AggregationOperation
import com.signalcollect.ExecutionStatistics
import com.signalcollect.Edge
import collection.JavaConversions._

class DcopEvaluationRun(
  val algorithmName: String,
  graphBuilder: GraphBuilder[Any, Any] = new GraphBuilder[Any, Any](),
  edgeBuilder: (Int, Int) => Edge[Int],
  vertexBuilder: ConstraintVertexBuilder,
  graphProvider: ConstraintGraphProvider[Any, Any],
  executionConfiguration: ExecutionConfiguration = ExecutionConfiguration(ExecutionMode.Synchronous).withSignalThreshold(0.01),
  jvmParams: String = "",
  reportMemoryStats: Boolean = false) extends EvaluationAlgorithmRun[Any, Any] {

  var stats: ExecutionInformation = null

  def loadGraph = {
    graphProvider.populate(graph, vertexBuilder, edgeBuilder)
    graph.awaitIdle
//    println("Printing vertex states")
//    graph.foreachVertex(println(_))
//    println("Done printing vertex states")
    readLine
  }

  //TODO: Cut the AdoptFileGraphGenerator: creating a graph instance to use in buildGraph, and adding edges and vertices for loadGraph

  def buildGraph = {
    graph = graphBuilder.build

  }

  def execute = {
    println("Before aggregate")
   // println("Printing vertex states")
   // graph.foreachVertex(println(_))
   // println("Done printing vertex states")
    
    vertexBuilder match {
      case vb: GoogleDSANVertexBuilder =>
        graph.foreachVertex(v => v match {
          case vertex: DSANVertex => {
            val targetIds = vertex.outgoingEdges.keys
           // println("In aggregation operation add ctr to DSANVertex " + vertex.id + " with " + vertex.edgeCount + " edges: " + targetIds)

            var constraints: List[SimpleDiffConstraint] = List()
            for (targetId <- targetIds) {
              constraints = SimpleDiffConstraint(List(vertex.id, targetId.asInstanceOf[Int])) :: constraints

            }
            vertex.constraints = constraints
            //println("vertex "+vertex.id +" Constraints: "+constraints)
          }
          case vertex: JSFPIVertex => println("This is not the vertex you are looking for " + vertex)
          case other => println("This is not even a vertex " + other)
        })
      case other => graph
    }

    stats = graph.execute(executionConfiguration)
    stats
  }

  override def postExecute: List[(String, String)] = {
    val pseudoAggregate = graph.aggregate(new GlobalUtility)
    val nashEquilibrium = graph.aggregate(new NashEquilibrium)

    List[(String, String)](("isNE", nashEquilibrium.toString), ("maxUtility", pseudoAggregate._1.toString), ("utility", pseudoAggregate._2.toString), ("domainSize", graphProvider.domainSize.toString), ("graphSize", graphProvider.graphSize.toString), ("debug", if (stats.aggregatedWorkerStatistics.numberOfVertices < 101) graph.aggregate(new Visualizer).toString else " "))

  }

  def graphStructure = graphProvider.toString

  override def jvmParameters = jvmParams

  override def memoryStatsEnabled = reportMemoryStats

}

class Visualizer extends AggregationOperation[List[(Any, String)]] {
  val neutralElement: List[(Any, String)] = List()
  def extract(v: Vertex[_, _]): List[(Any, String)] = v match {
    case vertex: ApproxBestResponseVertex[_,_] =>  List((vertex.id, vertex.state.toString+" "+vertex.existsBetterStateUtility.toString))
    case other => List((v.id, v.state.toString))
  }
  def reduce(elements: Stream[List[(Any, String)]]): List[(Any, String)] = elements.foldLeft(neutralElement)(aggregate)
  def aggregate(a: List[(Any, String)], b: List[(Any, String)]): List[(Any, String)] = a ++ b

}
