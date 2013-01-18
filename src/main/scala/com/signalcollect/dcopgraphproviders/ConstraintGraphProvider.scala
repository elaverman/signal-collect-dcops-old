package com.signalcollect.dcopgraphproviders

import com.signalcollect.approx.flood.ConstraintVertexBuilder
import com.signalcollect.GraphEditor
import com.signalcollect.approx.flood.Constraint
import com.signalcollect.approx.flood.SimpleNoGoodConstraint
import com.signalcollect.approx.flood.SimpleAllDiffConstraint

trait ConstraintGraphProvider[Id, Signal] extends Serializable {
  def populate(graphEditor: GraphEditor[Id, Signal], constraintVertexBuilder: ConstraintVertexBuilder)
  def domainSize: Int
  def graphSize: Int
}

//different implementations for different graph structures, different constraintVertexBuilder for different algorithms
class ConstraintGridProvider(val width: Int = 10, height: Int = 10, numberOfColors: Int = 4) extends ConstraintGraphProvider[Any, Any] {

  //constraintVertexBuilder: Takes vertex id, constraints for the vertex with that id and the domain of the variable represented by the vertex.  
  def populate(graphEditor: GraphEditor[Any, Any], constraintVertexBuilder: ConstraintVertexBuilder) {
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

      neighbors(column, row).filter(coordinate => inGrid(coordinate._1, coordinate._2)) map
        //  (coordinate => SimpleNoGoodConstraint(List(id, (coordinate._2 * width + coordinate._1)), (domain map (color => List(color, color))).toList))
        (coordinate => SimpleAllDiffConstraint(List(id, (coordinate._2 * width + coordinate._1))))

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