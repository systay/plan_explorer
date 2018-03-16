package org.plan_explorer

import java.io.File

import org.neo4j.cypher.internal.compiler.v3_3.spi.PlanContext
import org.neo4j.cypher.internal.frontend.v3_3.phases.devNullLogger
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, PropertyKeyId, RelTypeId}
import org.neo4j.cypher.internal.spi.v3_3.{TransactionBoundPlanContext, TransactionalContextWrapper}
import org.neo4j.cypher.javacompat.internal.GraphDatabaseCypherService
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.{GraphDatabaseFactory, GraphDatabaseSettings}
import org.neo4j.kernel.api.KernelTransaction
import org.neo4j.kernel.api.exceptions.KernelException
import org.neo4j.kernel.api.security.SecurityContext.AUTH_DISABLED
import org.neo4j.kernel.impl.coreapi.{InternalTransaction, PropertyContainerLocker}
import org.neo4j.kernel.impl.query.Neo4jTransactionalContextFactory
import org.neo4j.kernel.impl.query.clientconnection.ClientConnectionInfo
import org.neo4j.kernel.internal.GraphDatabaseAPI
import org.neo4j.values.virtual.VirtualValues.EMPTY_MAP

object LoadFromDatabase {

  def loadFromDatabase(path: String,
                       query: String,
                       oldTokens: Tokens,
                       recordingStatistics: InterestingStats): StateFromDb = {

    val file = new File(path)
    if (!file.exists())
      throw new RuntimeException("No such db exists")

    if (!file.isDirectory)
      throw new RuntimeException("Found file instead of directory")

    val dbService = new GraphDatabaseFactory().
      newEmbeddedDatabaseBuilder(file).
      setConfig(GraphDatabaseSettings.read_only, "true").
      newGraphDatabase()

    try {

      val dbApi = dbService.asInstanceOf[GraphDatabaseAPI]
      val tx = dbApi.beginTransaction(KernelTransaction.Type.`implicit`, AUTH_DISABLED)
      val planContext = createPlanContext(query, dbService, tx)
      val newTokens = getTokensFromDb(oldTokens, planContext)
      val newIndexes = getIndexes(newTokens, planContext)
      val statistics = loadStatistics(oldTokens, newTokens, recordingStatistics, planContext)

      StateFromDb(newIndexes, statistics, newTokens)
    } finally {
      dbService.shutdown()
    }
  }

  private def loadStatistics(oldTokens: Tokens,
                             newTokens: Tokens,
                             recordingStatistics: InterestingStats,
                             planContext: PlanContext) = {
    val old2loadedLabels: Map[LabelId, LabelId] = oldTokens.labels.map {
      case (label, oldLabelId) => oldLabelId -> newTokens.labels(label)
    }
    val old2loadedTypes = oldTokens.types.map {
      case (relType, oldToken) => oldToken -> newTokens.types(relType)
    }

    val statistics = planContext.statistics
    val labelCardinality = recordingStatistics.labels.map {
      oldLabelId =>
        val loadedId = old2loadedLabels(oldLabelId)
        val labelId = LabelId(loadedId)
        labelId -> statistics.nodesWithLabelCardinality(Some(labelId))
    }.toMap

    val allNodes = statistics.nodesAllCardinality()

    val edgeCardinality = recordingStatistics.edges.map {
      case (fromOldLabel, oldRelType, toOldLabel) =>
        val fromLabel = fromOldLabel.map(oldId => LabelId(old2loadedLabels(oldId)))
        val toLabel = toOldLabel.map(oldId => LabelId(old2loadedLabels(oldId)))
        val relType = oldRelType.map(oldId => RelTypeId(old2loadedTypes(oldId)))
        val cardinality = statistics.cardinalityByLabelsAndRelationshipType(fromLabel, relType, toLabel)

        (fromLabel, relType, toLabel) -> cardinality
    }.toMap

    StoredStatistics(labelCardinality, allNodes, edgeCardinality)
  }

  private def getIndexes(newTokens: Tokens, planContext: PlanContext) = {

    val newIndexes = newTokens.labels.flatMap {
      case (_, labelId) =>
        val normalIndexes = planContext.indexesGetForLabel(labelId).map {
          descriptor =>
            IndexUse(labelId, descriptor.properties, unique = false)
        }
        val uniqueIndexes = planContext.uniqueIndexesGetForLabel(labelId).map {
          descriptor =>
            IndexUse(labelId, descriptor.properties, unique = true)
        }
        normalIndexes ++ uniqueIndexes
    }.toSet
    newIndexes
  }

  private def getTokensFromDb(tokens: Tokens, planContext: PlanContext) = {

    def trackSource[T](l: String, f: => T): T = try {
      f
    } catch {
      case e: KernelException =>
        throw new RuntimeException(l, e)
    }

    val labels = tokens.labels.keySet.map {
      l => l -> LabelId(trackSource(l, planContext.getLabelId(l)))
    }.toMap

    val propKeys = tokens.propKeys.keySet.map {
      p => p -> PropertyKeyId(trackSource(p, planContext.getPropertyKeyId(p)))
    }.toMap

    val types = tokens.types.keySet.map {
      p => p -> RelTypeId(trackSource(p, planContext.getRelTypeId(p)))
    }.toMap

    val newTokens = Tokens(labels, types, propKeys)
    newTokens
  }

  private def createPlanContext(query: String,
                                dbService: GraphDatabaseService,
                                tx: InternalTransaction): PlanContext = {
    val queryService = new GraphDatabaseCypherService(dbService)
    val contextFactory = Neo4jTransactionalContextFactory.create(queryService, new PropertyContainerLocker)
    val context = contextFactory.newContext(ConnectionInfo, tx, query, EMPTY_MAP)
    new TransactionBoundPlanContext(TransactionalContextWrapper(context), devNullLogger)
  }
}

case class StateFromDb(indexes: Set[IndexUse], statistics: StoredStatistics, tokens: Tokens)

object ConnectionInfo extends ClientConnectionInfo {
  override def protocol(): String = "w00t"

  override def asConnectionDetails(): String = "w00t"
}
