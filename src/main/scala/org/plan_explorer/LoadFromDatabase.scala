package org.plan_explorer

import java.io.File

import org.neo4j.cypher.internal.compiler.v3_3.spi.PlanContext
import org.neo4j.cypher.internal.frontend.v3_3.phases.devNullLogger
import org.neo4j.cypher.internal.spi.v3_3.{TransactionBoundPlanContext, TransactionalContextWrapper}
import org.neo4j.cypher.javacompat.internal.GraphDatabaseCypherService
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.graphdb.factory.{GraphDatabaseFactory, GraphDatabaseSettings}
import org.neo4j.kernel.api.KernelTransaction
import org.neo4j.kernel.api.security.SecurityContext.AUTH_DISABLED
import org.neo4j.kernel.impl.coreapi.{InternalTransaction, PropertyContainerLocker}
import org.neo4j.kernel.impl.query.Neo4jTransactionalContextFactory
import org.neo4j.kernel.impl.query.clientconnection.ClientConnectionInfo
import org.neo4j.kernel.internal.GraphDatabaseAPI
import org.neo4j.values.virtual.VirtualValues.EMPTY_MAP
import org.plan_explorer.Main.IndexUse

object LoadFromDatabase {

  def loadFromDatabase(path: String, query: String, tokens: Tokens): StateFromDb = {

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
      val newTokens = getTokensFromDb(tokens, planContext)
      val newIndexes = getIndexes(newTokens, planContext)

      StateFromDb(newIndexes, null, newTokens)
    } finally {
      dbService.shutdown()
    }
  }

  private def getIndexes(newTokens: Tokens, planContext: PlanContext) = {
    val propLookup = newTokens.propKeys.map(_.swap)

    val newIndexes = newTokens.labels.flatMap {
      case (label, labelId) =>
        val normalIndexes = planContext.indexesGetForLabel(labelId).map {
          descriptor =>
            val props = descriptor.properties.map(pki => propLookup(pki.id))
            IndexUse(label, props, unique = false)
        }
        val uniqueIndexes = planContext.uniqueIndexesGetForLabel(labelId).map {
          descriptor =>
            val props = descriptor.properties.map(pki => propLookup(pki.id))
            IndexUse(label, props, unique = true)
        }
        normalIndexes ++ uniqueIndexes
    }.toSet
    newIndexes
  }

  private def getTokensFromDb(tokens: Tokens, planContext: PlanContext) = {
    val labels = tokens.labels.keySet.map {
      l => l -> planContext.getLabelId(l)
    }.toMap

    val propKeys = tokens.propKeys.keySet.map {
      p => p -> planContext.getPropertyKeyId(p)
    }.toMap

    val types = tokens.types.keySet.map {
      p => p -> planContext.getRelTypeId(p)
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

  case class StateFromDb(indexes: Set[IndexUse], statistics: RecordedStatistics, tokens: Tokens)

}

object ConnectionInfo extends ClientConnectionInfo {
  override def protocol(): String = "w00t"

  override def asConnectionDetails(): String = "w00t"
}
