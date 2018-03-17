package org.plan_explorer.model

import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, PropertyKeyId}

case class IndexUse(label: LabelId, props: Seq[PropertyKeyId], unique: Boolean) {
  def toString(tokens: Tokens): String = {
    val uniqueS = if (unique) "UNIQUE " else ""
    val labelName = tokens.reverseLabels(label)
    val propNames = props.map(tokens.reverseProps).mkString(", ")
    s"${uniqueS}INDEX ON :$labelName($propNames)"
  }
}

case class IndexPossibility(label: LabelId, props: Seq[PropertyKeyId]) {
  def toString(tokens: Tokens): String = s"(:$label {${props.mkString(",")}})"
}
