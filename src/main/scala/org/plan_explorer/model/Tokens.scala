package org.plan_explorer.model

import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, PropertyKeyId, RelTypeId}

case class Tokens(labels: Map[String, LabelId],
                  types: Map[String, RelTypeId],
                  propKeys: Map[String, PropertyKeyId]) {
  val reverseLabels: Map[LabelId, String] = labels.map(_.swap)
  val reverseTypes: Map[RelTypeId, String] = types.map(_.swap)
  val reverseProps: Map[PropertyKeyId, String] = propKeys.map(_.swap)

  def maybeTokenToString[T <: org.neo4j.cypher.internal.frontend.v3_3.NameId](in: Option[T]): String = in match {
    case Some(labelId) => tokenToString(labelId)
    case None => ""
  }

  def tokenToString[T <: org.neo4j.cypher.internal.frontend.v3_3.NameId](in: T): String = in match {
    case labelId: LabelId => ":" + reverseLabels(labelId)
    case typeId: RelTypeId => ":" + reverseTypes(typeId)
    case typeId: PropertyKeyId => reverseProps(typeId)
  }

}

object Tokens {
  def empty = Tokens(Map.empty, Map.empty, Map.empty)
}