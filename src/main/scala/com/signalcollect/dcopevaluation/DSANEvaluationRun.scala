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

class DSANEvaluationRun(
  val algorithmName: String,
  graphBuilder: GraphBuilder[Any, Any] = new GraphBuilder[Any, Any](),
  vertexBuilder: ConstraintVertexBuilder,
  graphProvider: ConstraintGraphProvider[Any, Any],
  executionConfiguration: ExecutionConfiguration = ExecutionConfiguration(ExecutionMode.Synchronous).withSignalThreshold(0.01),
  jvmParams: String = "",
  reportMemoryStats: Boolean = false) extends EvaluationAlgorithmRun[Any, Any] {

  var stats: ExecutionInformation = null
  
  def loadGraph = {
    graphProvider.populate(graph, vertexBuilder)
  }

  //TODO: Cut the AdoptFileGraphGenerator: creating a graph instance to use in buildGraph, and adding edges and vertices for loadGraph

  def buildGraph = {
    graph = graphBuilder.build
  }

  def execute = {
    //    println("Printing vertex states")
    //    graph.foreachVertex(println(_))
    //    println("Done printing vertex states")
    stats = graph.execute(executionConfiguration)
    stats
  }

  override def postExecute: List[(String, String)] = {
    val pseudoAggregate = graph.aggregate(new GlobalUtility)
    
    List[(String, String)](("maxUtility", pseudoAggregate._1.toString), ("utility", pseudoAggregate._2.toString), ("domainSize", graphProvider.domainSize.toString), ("graphSize", graphProvider.graphSize.toString), ("debug", if (stats.aggregatedWorkerStatistics.numberOfVertices < 101) graph.aggregate(new Visualizer).toString else " "))

  }

  def graphStructure = graphProvider.toString

  override def jvmParameters = jvmParams

  override def memoryStatsEnabled = reportMemoryStats

}

class Visualizer extends AggregationOperation[List[(Any, String)]] {
  val neutralElement: List[(Any, String)] = List()
  def extract(v: Vertex[_, _]): List[(Any, String)] = List((v.id, v.state.toString))
  def reduce(elements: Stream[List[(Any, String)]]): List[(Any, String)] = elements.foldLeft(neutralElement)(aggregate)
  def aggregate(a: List[(Any, String)], b: List[(Any, String)]): List[(Any, String)] = a ++ b

}
