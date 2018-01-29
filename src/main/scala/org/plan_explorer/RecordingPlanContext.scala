package org.plan_explorer

import org.neo4j.cypher.internal.compiler.v3_3.IndexDescriptor
import org.neo4j.cypher.internal.compiler.v3_3.spi.{GraphStatistics, PlanContext}
import org.neo4j.cypher.internal.frontend.v3_3.phases.{InternalNotificationLogger, devNullLogger}
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, RelTypeId}
import org.neo4j.cypher.internal.ir.v3_3.{Cardinality, Selectivity}
import org.neo4j.cypher.internal.v3_3.logical.plans.{ProcedureSignature, QualifiedName, UserFunctionSignature}

import scala.collection.mutable

class RecordingPlanContext extends PlanContext {
  val labels = mutable.HashMap.empty[String, Int]
  val types = mutable.HashMap.empty[String, Int]
  val propKeys = mutable.HashMap.empty[String, Int]
  var tokenCounter = 0

  override def indexGet(labelName: String, propertyKeys: Seq[String]): Option[IndexDescriptor] = {
    Some(IndexDescriptor(labels(labelName), propertyKeys.map(propKeys)))
  }

  override def indexExistsForLabel(labelName: String): Boolean = true

  override def uniqueIndexesGetForLabel(labelId: Int): Iterator[IndexDescriptor] = indexesGetForLabel(labelId)

  override def indexesGetForLabel(labelId: Int): Iterator[IndexDescriptor] = {
    val descriptors = (for {
      propsSet: Set[Int] <- propKeys.values.toSet.subsets()
      props: Seq[Int] <- propsSet.toList.permutations
      if props.nonEmpty
    } yield {
      IndexDescriptor(labelId, props)
    }).toList
    descriptors.toIterator
  }

  override def uniqueIndexGet(labelName: String, propertyKeys: Seq[String]): Option[IndexDescriptor] = {
    Some(IndexDescriptor(labels(labelName), propertyKeys.map(propKeys)))
  }

  override def hasPropertyExistenceConstraint(labelName: String, propertyKey: String): Boolean = true

  override def checkNodeIndex(idxName: String): Unit = {}

  override def checkRelIndex(idxName: String): Unit = {}

  override def getOrCreateFromSchemaState[T](key: Any, f: => T): T = f

  override def txIdProvider: () => Long = () => 0L

  override val statistics: RecordingStatistics = new RecordingStatistics

  override def notificationLogger(): InternalNotificationLogger = devNullLogger

  override def procedureSignature(name: QualifiedName): ProcedureSignature = ???

  override def functionSignature(name: QualifiedName): Option[UserFunctionSignature] = None

  override def getLabelName(id: Int): String = labels.find(p => p._2 == id).get._1

  override def getOptLabelId(labelName: String): Option[Int] = Some(getLabelId(labelName))

  override def getLabelId(labelName: String): Int = {
    labels.getOrElseUpdate(labelName, nextToken())
  }

  override def getPropertyKeyName(id: Int): String = propKeys.find(p => p._2 == id).get._1

  override def getOptPropertyKeyId(propertyKeyName: String): Option[Int] =
    Some(getPropertyKeyId(propertyKeyName))

  override def getPropertyKeyId(propertyKeyName: String): Int = {
    propKeys.getOrElseUpdate(propertyKeyName, nextToken())
  }

  override def getRelTypeName(id: Int): String = types.find(p => p._2 == id).get._1

  override def getOptRelTypeId(relType: String): Option[Int] =
    Some(getRelTypeId(relType))

  override def getRelTypeId(relType: String): Int = {
    types.getOrElseUpdate(relType, nextToken())
  }

  private def nextToken() = {
    val id = tokenCounter
    tokenCounter += 1
    id
  }
}

class RecordingStatistics extends GraphStatistics with InterestingStats {

  private val interestingLabels = new collection.mutable.HashSet[LabelId]()
  private val interestingEdges = new collection.mutable.HashSet[(Option[LabelId], Option[RelTypeId], Option[LabelId])]()
  private val interestingIndexes = new collection.mutable.HashSet[IndexDescriptor]()

  override def labels: Set[LabelId] = interestingLabels.toSet

  override def edges: Set[(Option[LabelId], Option[RelTypeId], Option[LabelId])] = interestingEdges.toSet

  override def indexes: Set[IndexDescriptor] = interestingIndexes.toSet

  override def nodesWithLabelCardinality(labelId: Option[LabelId]): Cardinality = {
    labelId.foreach(interestingLabels.add)
    Cardinality(1)
  }

  override def nodesAllCardinality(): Cardinality = Cardinality(1)

  override def cardinalityByLabelsAndRelationshipType(fromLabel: Option[LabelId], relTypeId: Option[RelTypeId], toLabel: Option[LabelId]): Cardinality = {
    interestingEdges.add((fromLabel, relTypeId, toLabel))
    Cardinality(1)
  }

  override def indexPropertyExistsSelectivity(index: IndexDescriptor): Option[Selectivity] = indexSelectivity(index)

  override def indexSelectivity(index: IndexDescriptor): Option[Selectivity] = {
    interestingIndexes.add(index)
    Some(Selectivity(1))
  }

}

trait InterestingStats {
  def labels: Set[LabelId]

  def edges: Set[(Option[LabelId], Option[RelTypeId], Option[LabelId])]

  def indexes: Set[IndexDescriptor]
}