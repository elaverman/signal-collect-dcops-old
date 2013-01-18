package com.signalcollect.dcopgraphproviders

import com.signalcollect.graphproviders.synthetic.LogNormalGraph
import com.signalcollect.graphproviders.synthetic.Grid
import java.io.PrintWriter
import java.io.FileWriter

//class GraphsToAdoptFile {
//
//  def getTextFromLogNormalGraph(fileName: String = "", graphSize: Int, seed: Long = 0, sigma: Double = 1, mu: Double = 3, colors: Int = 4): String = {
//    val graph = new LogNormalGraph(graphSize, seed, sigma, mu)
//    val graphWriter = new PrintWriter(new FileWriter(if (fileName=="") "./lognormal" + graphSize + "-sigma" + sigma + "-mu" + mu else fileName))
//    
//    graphWriter.write("AGENT 1\n")
//    
//    for (i <- 0 until graphSize) {
//      graphWriter.write("VARIABLE "+ i + " 1 "+colors+ "\n") //1 is AGENT id
//    }
// 
//   
//    graph.foreach(tuple => {
//      tuple match {
//        case (source, target) => {
//          graphWriter.write("CONSTRAINT "+ source + " " + target + "\n")
//          for (i <- 0 until colors) 
//            graphWriter.write("NOGOOD "+i+" "+i+"\n")
//        }
//      }
//    })
//    graphWriter.close
//    
//    fileName
//  }
//  
//  
//  def getTextFromGridGraph(fileName: String = "", graphWidth: Int, graphHeight: Int, colors: Int = 4): String = {
//    val graph = new Grid(graphWidth, graphHeight)
//    val graphWriter = new PrintWriter(new FileWriter(if (fileName=="") "./grid-width" + graphWidth + "-height" + graphHeight else fileName))
//    
//    val graphSize = graphWidth*graphHeight
//    
//    graphWriter.write("AGENT 1\n")
//    
//    for (i <- 0 until graphSize) {
//      graphWriter.write("VARIABLE "+ i + " 1 "+colors+ "\n") //1 is AGENT id
//    }
// 
//   
//    graph.foreach(tuple => {
//      tuple match {
//        case (source, target) => {
//          graphWriter.write("CONSTRAINT "+ source + " " + target + "\n")
//          for (i <- 0 until colors) 
//            graphWriter.write("NOGOOD "+i+" "+i+"\n")
//        }
//      }
//    })
//    graphWriter.close
//    
//    fileName
//  }
//
//}