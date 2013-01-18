package com.signalcollect.dcopgraphproviders

import com.signalcollect.graphproviders.synthetic._
import com.signalcollect._
import scala.util.Random
import scala.math._
import com.signalcollect.graphproviders._
import java.io.PrintWriter
import java.io.FileWriter

//class DcopLogNormalGraph[Signal](graphSize: Int, seed: Long = 0, sigma: Double = 1, mu: Double = 3, colors: Int = 4) extends com.signalcollect.graphproviders.synthetic.LogNormalGraph[Signal](graphSize, seed, sigma, mu){
//	
//
//  
//  override def populate(graphEditor: GraphEditor[Int, Signal], vertexBuilder: Int => Vertex[Int, _], edgeBuilder: (Int, Int) => Edge[Int]) {
//    
//    val simpleLogNormalGraph = new LogNormalGraph(graphSize, seed, sigma, mu)
//    
//  for (i <- 0 until graphSize)
//    
//    
//    
//   simpleLogNormalGraph.foreach(tuple => {
//    tuple match {
//      case (source, target) => {
//        edgeWriter.write(source + "," + target + "\n")
//      }
//    }
//  })
//    
//    
//    for (id <- (0 until graphSize).par) {
//      graphEditor.addVertex(vertexBuilder(id))
//    }
//
//    val r = new Random(seed)
//
//    for (i <- 0 until graphSize) {
//      val from = i
//      val outDegree: Int = exp(mu + sigma * (r.nextGaussian)).round.toInt //log-normal
//      var j = 0
//      while (j < outDegree) {
//        val to = ((r.nextDouble * (graphSize - 1))).round.toInt
//        if (from != to) {
//          graphEditor.addEdge(from, edgeBuilder(from, to))
//          j += 1
//        }
//      }
//    }
//  }
//}