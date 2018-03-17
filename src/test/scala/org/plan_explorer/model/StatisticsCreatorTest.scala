package org.plan_explorer.model

import org.neo4j.cypher.internal.frontend.v3_3.LabelId
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.scalatest.{FunSuite, Matchers}

class StatisticsCreatorTest extends FunSuite with Matchers {
  test("given all static input, we get a stored stats that make sense back") {
    val creator =
      StatisticsCreator(
        labelCardinality = Map(LabelId(0) -> Static(Cardinality(10))),
        allNodes = Static(Cardinality(10)),
        edgeCardinality = Map.empty
      )

    val stats = creator.getStatisticsFor(0.5, 0.5)

    stats.allNodes should equal(Cardinality(10))
  }

  test("create and use dynamic value") {
    val dynamic1 = Dynamic.createFunction("x + y")
    val dynamic2 = Dynamic.createFunction("x * y")
    val creator =
      StatisticsCreator(
        labelCardinality = Map(
          LabelId(0) -> dynamic1,
          LabelId(1) -> dynamic2
        ),
        allNodes = Static(Cardinality(10)),
        edgeCardinality = Map.empty
      )

    val stats = creator.getStatisticsFor(0.3, 0.7)

    stats.labelCardinality(LabelId(0)) should equal(Cardinality(.3 + .7))
    stats.labelCardinality(LabelId(1)) should equal(Cardinality(.3 * .7))
  }
}
