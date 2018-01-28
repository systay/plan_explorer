package org.plan_explorer

import org.neo4j.cypher.internal.compiler.v3_3.IndexDescriptor
import org.neo4j.cypher.internal.compiler.v3_3.spi.GraphStatistics
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, RelTypeId}
import org.neo4j.cypher.internal.ir.v3_3.{Cardinality, Selectivity}

class RecordedStatistics(nodes: Map[LabelId, Cardinality],
                         allNodes: Cardinality,
                         edgeCardinality: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), Cardinality])
  extends GraphStatistics {

  override def nodesWithLabelCardinality(labelId: Option[LabelId]): Cardinality =
    if (labelId.isEmpty)
      allNodes
    else
      nodes(labelId.get)

  override def nodesAllCardinality(): Cardinality = allNodes

  override def cardinalityByLabelsAndRelationshipType(fromLabel: Option[LabelId],
                                                      relTypeId: Option[RelTypeId],
                                                      toLabel: Option[LabelId]): Cardinality =
    edgeCardinality((fromLabel, relTypeId, toLabel))

  override def indexSelectivity(index: IndexDescriptor): Option[Selectivity] = Some(Selectivity(0.5))

  override def indexPropertyExistsSelectivity(index: IndexDescriptor): Option[Selectivity] = Some(Selectivity(0.5))
}
