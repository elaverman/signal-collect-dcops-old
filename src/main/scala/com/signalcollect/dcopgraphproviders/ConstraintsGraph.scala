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

package com.signalcollect.dcopgraphproviders

import com.signalcollect._
//import com.signalcollect.examples._
import com.signalcollect.approx.flood._
import com.signalcollect.graphproviders._

class ConstraintsGraph(val graphSize: Int, constraints: List[Constraint]) extends GraphProvider[Any, Double] {

  def populate(graphEditor: GraphEditor[Any, Double], vertexBuilder: Any => Vertex[Any, _], edgeBuilder: (Any, Any) => Edge[Any]) = {
    
    for (id <- (0 to graphSize).par) {
      graphEditor.addVertex(vertexBuilder(id))
    }
    
    
   for (ctr <- constraints) {
	  for (i <- ctr.variablesList()){
	    for (j <- ctr.variablesList()){
	      if ( i != j )
	        graphEditor.addEdge(i, edgeBuilder(i, j))
	        //graph.addEdge(edgeBuilder(j,i))
	    }
	  }
   }
   
  }

}