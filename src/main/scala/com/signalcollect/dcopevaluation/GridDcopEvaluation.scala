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
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  
 */

package com.signalcollect.dcopevaluation

import com.signalcollect.evaluation.jobsubmission._
import com.signalcollect.nodeprovisioning.torque._
import com.signalcollect.evaluation.resulthandling._
import com.signalcollect.configuration._
import com.signalcollect._
import com.signalcollect.nodeprovisioning.torque.TorqueNodeProvisioner
import com.signalcollect.graphproviders.synthetic._
import com.signalcollect.nodeprovisioning.local.LocalNodeProvisioner
import com.signalcollect.nodeprovisioning.Node
import com.signalcollect.nodeprovisioning.local.LocalNode
import com.signalcollect.evaluation.algorithms.ChineseWhispersEvaluationRun
import com.signalcollect.evaluation.util.ParallelFileGraphLoader
import com.typesafe.config.Config
import com.signalcollect.approx.flood._
import com.signalcollect.dcopgraphproviders._
import com.signalcollect.StateForwarderEdge
import com.signalcollect.approx.performance.GreedyExplorerVertexBuilder
import com.signalcollect.approx.performance.LowMemoryExplorerVertexBuilder
import com.signalcollect.approx.flood.DSAVariant
import com.signalcollect.approx.performance.BalancedExplorerVertexBuilder

//TODO replace anything you can with ints and arrays instead of lists
//TODO function for Nash Equilibrium!

/**
 * Runs a PageRank algorithm on a graph of a fixed size
 * for different numbers of worker threads.
 *
 * Evaluation is set to execute on a 'Kraken'-node.
 */
object GridDcopEvaluation extends App {

  val evalName = "JSFPI and WRMI grid" //"DSAN and DSA grid"
  val jvmParameters = "-Xmx64000m -XX:+UseNUMA -XX:+UseCondCardMark -XX:+UseParallelGC"


    
    
    
  val kraken = new TorqueHost(
    jobSubmitter = new TorqueJobSubmitter(username = System.getProperty("user.name"), hostname = "kraken.ifi.uzh.ch"),
    localJarPath = "./target/signal-collect-dcops-assembly-2.0.0-SNAPSHOT.jar", priority = TorquePriority.superfast)

  val fastEval = new EvaluationSuiteCreator(evaluationName = evalName,
    executionHost =
      // new LocalHost 
      kraken
      )
  val out = new java.io.FileWriter("results.txt")

  val outTime = new java.io.FileWriter("resultsTime.txt")
  var startTime = System.nanoTime()
  val terminationCondition = new DSANGlobalTerminationCondition(/*out, outTime,*/ startTime, aggregationOperation = new GlobalUtility, aggregationInterval = 5l)

  
  val executionConfigSync = ExecutionConfiguration(ExecutionMode.Synchronous).withSignalThreshold(0.01).withGlobalTerminationCondition(terminationCondition).withTimeLimit(420000) //(36000000)

  val repetitions = 10
  val executionConfigurations = List(executionConfigSync)
  val graphSizes = List(10, 100, 1000)
  val algorithmsList = List(
     new JSFPIVertexBuilder("Weighted rho=0.5", fadingMemory = 0.5),
    // new JSFPIVertexBuilder("Weighted"),
     // new BalancedExplorerVertexBuilder("Balanced"),
      //new GreedyExplorerVertexBuilder("Greedy"),
    //  new DSAVertexBuilder("final", DSAVariant.B, 0.5)
    new WRMIVertexBuilder("first trial fm=0.5", fadingMemory = 0.5)
      
     // new DSANVertexBuilder("ela-special", ((time, delta) => if (delta * delta <= 0.01) 0.001 else math.exp(delta * time * time / 1000))) //,
    //new DSANVertexBuilder(" - 0.001 exploration", (time, delta) => 0.001)
    )

  
  
  for (i <- 0 until repetitions) {
    for (executionConfig <- executionConfigurations) {
      for (numberOfColors <- List(8, 7, 6, 5, 4)) {
        for (graphSize <- graphSizes) {
          for (graphProvider <- /*googleGraphProviderList */ List(new ConstraintGridProvider(graphSize, graphSize, numberOfColors)/*, new ConstraintLatinSquareProvider(graphSize, graphSize, numberOfColors)*/))
            for (algorithm <- algorithmsList) {
              val graphBuilder = new GraphBuilder[Any, Any]()//.withConsole(true)
              fastEval.addJobForEvaluationAlgorithm(new DcopEvaluationRun(algorithm.toString, graphBuilder = graphBuilder, vertexBuilder = algorithm, edgeBuilder = (x: Int, y: Int) => new StateForwarderEdge(y), graphProvider = graphProvider, executionConfiguration = executionConfig, jvmParams = jvmParameters, reportMemoryStats = true))
            }
        }
      }
    }
  }

  fastEval.setResultHandlers(List(new ConsoleResultHandler(true), new GoogleDocsResultHandler(args(0), args(1), "evaluation_ela", "data")))
  fastEval.runEvaluation
}