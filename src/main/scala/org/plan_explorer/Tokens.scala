package org.plan_explorer

case class Tokens(labels: Map[String, Int],
                  types: Map[String, Int],
                  propKeys: Map[String, Int]) {
  val reverseLabels = labels.map(_.swap)
  val reverseTypes = types.map(_.swap)
  val reverseProps = propKeys.map(_.swap)
}
