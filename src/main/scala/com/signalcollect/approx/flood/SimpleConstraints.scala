package com.signalcollect.approx.flood

//import scala.collection.mutable._

case class SimpleUtilityConstraint(constraintVariables: List[Any], utilityFunction: Map[List[Int], Double],
  hard: Boolean = false) extends Constraint {

  def satisfies(configuration: Map[Any, Int]): Boolean = {
    utilityFunction.contains(constraintVariables map (variableName => configuration(variableName)))
  }

  def satisfiesInt(configuration: Map[Any, Int]): Int = if (satisfies(configuration)) 1 else 0

  def utility(configuration: Map[Any, Int]): Double = {
    utilityFunction(constraintVariables map (variableName => configuration(variableName)))
  }

  def variablesList(): List[Any] = {
    constraintVariables
  }

  def hardInt = if (hard) 1 else 0

}

//TODO: put inside the permitted values!
//TODO: verify the call for satisfies if the config is empty or doesn't have something 

case class SimpleNoGoodConstraint(constraintVariables: List[Any], noGoodFunction: List[List[Int]],
  utilitySatisfied: Double = 1, utilityNonSatisfied: Double = 0, hard: Boolean = false) extends Constraint {

  def satisfies(configuration: Map[Any, Int]): Boolean =
    !noGoodFunction.contains(constraintVariables map (variableName => configuration.getOrElse(variableName, false)))
  // !noGoodFunction.contains(constraintVariables map(variableName => configuration(variableName)))

  def satisfiesInt(configuration: Map[Any, Int]): Int = if (satisfies(configuration)) 1 else 0

  def utility(configuration: Map[Any, Int]): Double = {
    if (satisfies(configuration)) utilitySatisfied else utilityNonSatisfied
  }

  def variablesList(): List[Any] = {
    constraintVariables
  }

  def hardInt = if (hard) 1 else 0

  override def toString = {
    "[\n   Variables = " + constraintVariables +
      "\n   NoGoodFunction =  " + noGoodFunction.mkString(",") +
      " Utility satisfied = " + utilitySatisfied +
      "\n]"

  }

}

case class SimpleDiffConstraint(constraintVariables: List[Any],
  utilitySatisfied: Double = 1, utilityNonSatisfied: Double = 0, hard: Boolean = false) extends Constraint {

  def diff(listOfValues: List[Int]): Boolean = {
    if (listOfValues(0) == listOfValues(1)) false
    else
      true
  }

  def satisfies(configuration: Map[Any, Int]): Boolean = {
    val listOfValues = constraintVariables map (variableName => configuration.getOrElse(variableName, return false))
    diff(listOfValues)
  }

  def satisfiesInt(configuration: Map[Any, Int]): Int = if (satisfies(configuration)) 1 else 0

  def utility(configuration: Map[Any, Int]): Double = {
    if (satisfies(configuration)) utilitySatisfied else utilityNonSatisfied
  }

  def variablesList(): List[Any] = {
    constraintVariables
  }

  def hardInt = if (hard) 1 else 0

  override def toString = {
    "[\n   Variables = " + constraintVariables +
      " Utility satisfied = " + utilitySatisfied +
      "\n]"

  }

}

//case class SimpleAllDiffConstraint(constraintVariables: List[Any],
//  utilitySatisfied: Double = 1, utilityNonSatisfied: Double = 0, hard: Boolean = false) extends Constraint {
//
//  def allDiff(listOfValues: List[Int]): Boolean = {
//    var setOfValues: Set[Int] = Set()
//    for (i <- listOfValues) {
//      if (setOfValues.contains(i))
//        return false
//      else
//        setOfValues += i
//    }
//    true
//  }
//
//  def satisfies(configuration: Map[Any, Int]): Boolean = {
//    val listOfValues = constraintVariables map (variableName => configuration.getOrElse(variableName, return false))
//    allDiff(listOfValues)
//  }
//
//  def satisfiesInt(configuration: Map[Any, Int]): Int = if (satisfies(configuration)) 1 else 0
//
//  def utility(configuration: Map[Any, Int]): Double = {
//    if (satisfies(configuration)) utilitySatisfied else utilityNonSatisfied
//  }
//
//  def variablesList(): List[Any] = {
//    constraintVariables
//  }
//
//  def hardInt = if (hard) 1 else 0
//
//  override def toString = {
//    "[\n   Variables = " + constraintVariables +
//      " Utility satisfied = " + utilitySatisfied +
//      "\n]"
//
//  }
//
//}