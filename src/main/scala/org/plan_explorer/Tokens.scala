package org.plan_explorer

import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, PropertyKeyId, RelTypeId}

case class Tokens(labels: Map[String, Int],
                  types: Map[String, Int],
                  propKeys: Map[String, Int]) {
  val reverseLabels = labels.map(_.swap)
  val reverseTypes = types.map(_.swap)
  val reverseProps = propKeys.map(_.swap)

  def tokenToString[T <: org.neo4j.cypher.internal.frontend.v3_3.NameId](in: Option[T]): String = in match {
    case Some(labelId: LabelId) => ":" + reverseLabels(labelId.id)
    case Some(typeId: RelTypeId) => ":" + reverseTypes(typeId.id)
    case Some(typeId: PropertyKeyId) => reverseProps(typeId.id)
    case None => ""
  }

}
