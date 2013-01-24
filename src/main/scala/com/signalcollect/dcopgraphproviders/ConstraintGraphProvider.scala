package com.signalcollect.dcopgraphproviders

import com.signalcollect.approx.flood.ConstraintVertexBuilder
import com.signalcollect.GraphEditor
import com.signalcollect.approx.flood.Constraint
import com.signalcollect.approx.flood.SimpleNoGoodConstraint
import com.signalcollect.approx.flood.SimpleDiffConstraint
import com.signalcollect.Edge

trait ConstraintGraphProvider[Id, Signal] extends Serializable {
  def populate(graphEditor: GraphEditor[Id, Signal], constraintVertexBuilder: ConstraintVertexBuilder, constraintEdgeBuilder: (Int, Int) => Edge[Int])
  def domainSize: Int
  def graphSize: Int
}

//different implementations for different graph structures, different constraintVertexBuilder for different algorithms
class ConstraintGridProvider(val width: Int = 10, height: Int = 10, numberOfColors: Int = 4) extends ConstraintGraphProvider[Any, Any] {

  //constraintVertexBuilder: Takes vertex id, constraints for the vertex with that id and the domain of the variable represented by the vertex.  
  def populate(graphEditor: GraphEditor[Any, Any], constraintVertexBuilder: ConstraintVertexBuilder, constraintEdgeBuilder: (Int, Int) => Edge[Int]) {
    val max = width * height
    val domain = (0 until numberOfColors).toArray

    // Returns all the neighboring cells of the cell with the given row/column
    def neighbors(column: Int, row: Int): List[(Int, Int)] = {
      List(
        (column - 1, row - 1), (column, row - 1), (column + 1, row - 1),
        (column - 1, row), (column + 1, row),
        (column - 1, row + 1), (column, row + 1), (column + 1, row + 1))
    }

    // Tests if a cell is within the grid boundaries
    def inGrid(column: Int, row: Int): Boolean = {
      column >= 0 && row >= 0 && column < width && row < height
    }

    def constraintsForVertex(id: Int): Iterable[Constraint] = {

      val column: Int = id % width
      val row: Int = id / width

      val result = neighbors(column, row).filter(coordinate => inGrid(coordinate._1, coordinate._2)) map
        //  (coordinate => SimpleNoGoodConstraint(List(id, (coordinate._2 * width + coordinate._1)), (domain map (color => List(color, color))).toList))
        (coordinate => SimpleDiffConstraint(List(id, (coordinate._2 * width + coordinate._1))))

      result
    }

    for (id <- 0 to max - 1) {
      val constraints = constraintsForVertex(id)
      graphEditor.addVertex(constraintVertexBuilder(id, constraints, domain))
    }

  }

  def domainSize = numberOfColors

  def graphSize = width

  override def toString() = {
    "Grid"
  }

}

//different implementations for different graph structures, different constraintVertexBuilder for different algorithms
class ConstraintLatinSquareProvider(val width: Int = 10, height: Int = 10, numberOfColors: Int = 4) extends ConstraintGraphProvider[Any, Any] {

  //constraintVertexBuilder: Takes vertex id, constraints for the vertex with that id and the domain of the variable represented by the vertex.  
  def populate(graphEditor: GraphEditor[Any, Any], constraintVertexBuilder: ConstraintVertexBuilder, constraintEdgeBuilder: (Int, Int) => Edge[Int]) {
    val max = width * height
    val domain = (0 until numberOfColors).toArray

    // Returns all the neighboring cells of the cell with the given row/column
    def neighbors(column: Int, row: Int): List[(Int, Int)] = {
      (0 until column).map(i => (i, row)).toList ++
        (column + 1 until width).map(i => (i, row)).toList ++
        (0 until row).map(i => (column, i)).toList ++
        (row + 1 until height).map(i => (column, i)).toList
    }

    // Tests if a cell is within the grid boundaries
    def inSquare(column: Int, row: Int): Boolean = {
      column >= 0 && row >= 0 && column < width && row < height
    }

    def constraintsForVertex(id: Int): Iterable[Constraint] = {

      val column: Int = id % width
      val row: Int = id / width

      val result = neighbors(column, row).filter(coordinate => inSquare(coordinate._1, coordinate._2)) map
        //  (coordinate => SimpleNoGoodConstraint(List(id, (coordinate._2 * width + coordinate._1)), (domain map (color => List(color, color))).toList))
        (coordinate => SimpleDiffConstraint(List(id, (coordinate._2 * width + coordinate._1))))

      result
    }

    for (id <- 0 to max - 1) {
      val constraints = constraintsForVertex(id)
      graphEditor.addVertex(constraintVertexBuilder(id, constraints, domain))
    }

  }

  def domainSize = numberOfColors

  def graphSize = width

  override def toString() = {
    "Grid"
  }

}

class ConstraintGoogleGraphLoader(numberOfWorkers: Int, edgeFilename: String = "web-Google.txt", directed: Boolean = true, numberOfColors: Int = 4) extends ConstraintGraphProvider[Any, Any] {

  def populate(graphEditor: GraphEditor[Any, Any], constraintVertexBuilder: ConstraintVertexBuilder, constraintEdgeBuilder: (Int, Int) => Edge[Int]) {
    val domain = (0 until numberOfColors).toArray

    println("Populating Google Graph")
    for (i <- (0 until numberOfWorkers).par) {
      graphEditor.modifyGraph(graph => {
        val edgeSource = scala.io.Source.fromFile(edgeFilename)
        edgeSource.getLines.foreach({ line =>
          if (!line.startsWith("#")) {
            val ids = line.split("	")
            val sourceId = ids(0).toInt
            if (sourceId % numberOfWorkers == i) {
              val targetId = ids(1).toInt
              graph.addVertex(constraintVertexBuilder(targetId, List(), domain))
              graph.addVertex(constraintVertexBuilder(sourceId, List(), domain))
              graph.addEdge(sourceId, constraintEdgeBuilder(sourceId, targetId))
              if (!directed) {
                graph.addEdge(targetId, constraintEdgeBuilder(targetId, sourceId))
              }
            }
          }
        })
      }, Some(i))
    }
  }

  def domainSize = numberOfColors

  def graphSize = 875713

  override def toString = "GoogleFileGraphLoader" + edgeFilename

}