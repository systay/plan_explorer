package org.plan_explorer

import org.neo4j.cypher.internal.compiler.v3_3.IndexDescriptor
import org.neo4j.cypher.internal.compiler.v3_3.spi.{GraphStatistics, PlanContext}
import org.neo4j.cypher.internal.frontend.v3_3.phases.{InternalNotificationLogger, devNullLogger}
import org.neo4j.cypher.internal.spi.v3_3.HardcodedGraphStatistics
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

  override def statistics: GraphStatistics = HardcodedGraphStatistics

  override def notificationLogger(): InternalNotificationLogger = devNullLogger

  override def procedureSignature(name: QualifiedName): ProcedureSignature = ???

  override def functionSignature(name: QualifiedName): Option[UserFunctionSignature] = None

  override def getLabelName(id: Int): String = labels.find(p => p._2 == id).get._1

  override def getOptLabelId(labelName: String): Option[Int] = Some(getLabelId(labelName))

  override def getLabelId(labelName: String): Int = {
    labels.getOrElseUpdate(labelName, nextToken())
  }

  private def nextToken() = {
    val id = tokenCounter
    tokenCounter += 1
    id
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
}