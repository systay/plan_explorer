package org.plan_explorer

import org.neo4j.cypher.internal.compiler.v3_3.IndexDescriptor
import org.neo4j.cypher.internal.compiler.v3_3.spi.GraphStatistics
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, RelTypeId}
import org.neo4j.cypher.internal.ir.v3_3.{Cardinality, Selectivity}

case class StoredStatistics(labelCardinality: Map[LabelId, Cardinality],
                            allNodes: Cardinality,
                            edgeCardinality: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), Cardinality])
  extends GraphStatistics {

  override def nodesWithLabelCardinality(labelId: Option[LabelId]): Cardinality =
    if (labelId.isEmpty)
      allNodes
    else
      labelCardinality(labelId.get)

  override def nodesAllCardinality(): Cardinality = allNodes

  override def cardinalityByLabelsAndRelationshipType(fromLabel: Option[LabelId],
                                                      relTypeId: Option[RelTypeId],
                                                      toLabel: Option[LabelId]): Cardinality =
    edgeCardinality((fromLabel, relTypeId, toLabel))

  override def indexSelectivity(index: IndexDescriptor): Option[Selectivity] = Some(Selectivity(0.5))

  override def indexPropertyExistsSelectivity(index: IndexDescriptor): Option[Selectivity] = Some(Selectivity(0.5))

  def toString(tokens: Tokens): String = {

    val y = tokens.tokenToString _

    val labels = labelCardinality.map {
      case (id, card) => s"  :${tokens.reverseLabels(id.id)} ${card.amount}"
    }.mkString("\n")

    val edges = edgeCardinality.map {
      case ((fromLabel, relType, toLabel), cardinality) =>
        s"(${y(fromLabel)})-[${y(relType)}]->(${y(toLabel)}) : ${cardinality.amount}"
    }.toSeq.sorted.mkString("\n")

    s"""Node count
       |  ${allNodes.amount}
       |
       |Label cardinality
       |$labels
       |
       |Edge cardinality
       |$edges
       |""".stripMargin
  }
}
