package org.plan_explorer

import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, RelTypeId}
import org.neo4j.cypher.internal.ir.v3_3.Cardinality

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

case class StatisticsCreator(labelCardinality: Map[LabelId, StatisticsValue],
                             allNodes: StatisticsValue,
                             edgeCardinality: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), StatisticsValue]) {

  def getStatisticsFor(x: Double, y: Double): StoredStatistics = {
    val allNodes = this.allNodes.evaluate(x, y)
    val labels = labelCardinality.mapValues(_.evaluate(x, y))
    val edges = edgeCardinality.mapValues(_.evaluate(x, y))

    StoredStatistics(labels, allNodes, edges)
  }

}

trait StatisticsValue {
  def evaluate(x: Double, y: Double): Cardinality
}

case class Static(cardinality: Cardinality) extends StatisticsValue {
  override def evaluate(x: Double, y: Double): Cardinality = cardinality
}

case class Dynamic(f: (Double, Double) => Double) extends StatisticsValue {
  override def evaluate(x: Double, y: Double): Cardinality = Cardinality(f(x, y))
}

object Dynamic {
  // Take a string that is a Scala expression using arithmetics, and having the two variables x and y available in scope
  def createFunction(input: String): Dynamic = {
    val f = locally {
      val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

      tb.eval(tb.parse(s"""locally { (x:Double, y: Double) => $input }""")).asInstanceOf[(Double, Double) => Double]
    }
    Dynamic(f)
  }
}