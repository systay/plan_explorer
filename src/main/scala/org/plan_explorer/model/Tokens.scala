package org.plan_explorer.model

import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, PropertyKeyId, RelTypeId}

case class Tokens(labels: Map[String, LabelId],
                  types: Map[String, RelTypeId],
                  propKeys: Map[String, PropertyKeyId]) {
  val reverseLabels: Map[LabelId, String] = labels.map(_.swap)
  val reverseTypes: Map[RelTypeId, String] = types.map(_.swap)
  val reverseProps: Map[PropertyKeyId, String] = propKeys.map(_.swap)

  def tokenToString[T <: org.neo4j.cypher.internal.frontend.v3_3.NameId](in: Option[T]): String = in match {
    case Some(labelId: LabelId) => ":" + reverseLabels(labelId)
    case Some(typeId: RelTypeId) => ":" + reverseTypes(typeId)
    case Some(typeId: PropertyKeyId) => reverseProps(typeId)
    case None => ""
  }

}
