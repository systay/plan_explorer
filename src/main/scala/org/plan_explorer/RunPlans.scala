package org.plan_explorer

import java.io.File
import java.time.Clock

import org.neo4j.cypher.internal.InternalExecutionResult
import org.neo4j.cypher.internal.compatibility.v3_3.WrappedMonitors
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.executionplan.ExecutionPlan
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.helpers.simpleExpressionEvaluator
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.{CommunityRuntimeBuilder, CommunityRuntimeContext, CommunityRuntimeContextCreator, ProfileMode}
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.{ExpressionEvaluator, MetricsFactory, QueryGraphSolver, SimpleMetricsFactory}
import org.neo4j.cypher.internal.compiler.v3_3.spi.PlanContext
import org.neo4j.cypher.internal.compiler.v3_3.{CypherCompilerConfiguration, IndexDescriptor, UpdateStrategy, defaultUpdateStrategy}
import org.neo4j.cypher.internal.frontend.v3_3.InputPosition
import org.neo4j.cypher.internal.frontend.v3_3.phases.CompilationPhaseTracer.NO_TRACING
import org.neo4j.cypher.internal.frontend.v3_3.phases._
import org.neo4j.cypher.internal.spi.v3_3.TransactionBoundQueryContext.IndexSearchMonitor
import org.neo4j.cypher.internal.spi.v3_3.{TransactionBoundPlanContext, TransactionBoundQueryContext, TransactionalContextWrapper}
import org.neo4j.cypher.internal.v3_3.logical.plans.LogicalPlan
import org.neo4j.cypher.javacompat.internal.GraphDatabaseCypherService
import org.neo4j.graphdb.factory.{GraphDatabaseFactory, GraphDatabaseSettings}
import org.neo4j.kernel.api.KernelTransaction
import org.neo4j.kernel.api.security.SecurityContext.AUTH_DISABLED
import org.neo4j.kernel.impl.coreapi.PropertyContainerLocker
import org.neo4j.kernel.impl.query.clientconnection.ClientConnectionInfo.EMBEDDED_CONNECTION
import org.neo4j.kernel.impl.query.{Neo4jTransactionalContextFactory, TransactionalContextFactory}
import org.neo4j.kernel.monitoring.{Monitors => KernelMonitors}
import org.neo4j.values.virtual.VirtualValues
import org.neo4j.values.virtual.VirtualValues.EMPTY_MAP

case class ExecutionReport(dbHits: Int, totalIntermediateRows: Int)

object RunPlans {

  def runThese(directory: String,
               baseState: LogicalPlanState,
               plans: Set[LogicalPlan]): Set[(LogicalPlan, ExecutionReport)] = {
    val dir = new File(directory)

    if (!dir.exists()) {
      println("No such database exists")
      return Set.empty
    }

    val dbService = new GraphDatabaseFactory().
      newEmbeddedDatabaseBuilder(dir).
      setConfig(GraphDatabaseSettings.read_only, "true").
      newGraphDatabase()

    try {

      val cypherService = new GraphDatabaseCypherService(dbService)
      val contextFactory = Neo4jTransactionalContextFactory.create(cypherService, new PropertyContainerLocker)
      val execPlans = createExecutionPlans(baseState, plans, cypherService, contextFactory)

      execPlans map {
        case (lp, execPlan) =>
          val tx = cypherService.beginTransaction(KernelTransaction.Type.`implicit`, AUTH_DISABLED)
          try {
            val txContext = contextFactory.newContext(EMBEDDED_CONNECTION, tx, baseState.queryText, EMPTY_MAP)
            val queryContext = new TransactionBoundQueryContext(TransactionalContextWrapper(txContext))(NullSearchMonitor)
            val result = execPlan.run(queryContext, ProfileMode, VirtualValues.EMPTY_MAP)
            emptyResults(result)

            val dbHits = result.executionPlanDescription().totalDbHits.get.toInt
            val allIntermediateRows = result.executionPlanDescription().flatten.map {
              x =>
                val map = x.getArguments()
                map.get("Rows").asInstanceOf[Long]
            }.sum.toInt
            (lp, ExecutionReport(dbHits, allIntermediateRows))
          } finally {
            tx.success()
            tx.close()
          }
      }
    } finally {
      dbService.shutdown()
    }
  }

  private def emptyResults(result: InternalExecutionResult) = {
    while (result.nonEmpty) result.next()
  }

  object NullSearchMonitor extends IndexSearchMonitor {
    override def indexSeek(index: IndexDescriptor, values: Seq[Any]): Unit = {}

    override def lockingUniqueIndexSeek(index: IndexDescriptor, values: Seq[Any]): Unit = {}
  }

  private def createExecutionPlans(baseState: LogicalPlanState,
                                   plans: Set[LogicalPlan],
                                   cypherService: GraphDatabaseCypherService,
                                   contextFactory: TransactionalContextFactory): Set[(LogicalPlan, ExecutionPlan)] = {
    val tx = cypherService.beginTransaction(KernelTransaction.Type.`implicit`, AUTH_DISABLED)
    try {
      val txContext = contextFactory.newContext(EMBEDDED_CONNECTION, tx, baseState.queryText, EMPTY_MAP)
      val txWrapper = TransactionalContextWrapper(txContext)
      val communityRuntimeContext = createContext(txWrapper)
      val runtimeBuilder = CommunityRuntimeBuilder.create(None, useErrorsOverWarnings = false)

      plans map { plan =>
        val logicalPlanState = baseState.withMaybeLogicalPlan(Some(plan))
        val compilationState = runtimeBuilder.transform(logicalPlanState, communityRuntimeContext)
        val execPlan = compilationState.maybeExecutionPlan.getOrElse(throw new RuntimeException("Failed to construct execution plan"))
        (plan, execPlan)
      }
    } finally {
      tx.success()
      tx.close()
    }
  }

  private def createContext(transactionalContextWrapper: TransactionalContextWrapper): CommunityRuntimeContext = {
    val planContext: PlanContext = new TransactionBoundPlanContext(transactionalContextWrapper, devNullLogger)
    val queryText: String = "apa"
    val debugOptions: Set[String] = Set.empty
    val offset: Option[InputPosition] = None
    val monitors = WrappedMonitors(new KernelMonitors)
    val metricsFactory: MetricsFactory = SimpleMetricsFactory
    val queryGraphSolver: QueryGraphSolver = null
    val cypherCompilerConfig: CypherCompilerConfiguration = CypherCompilerConfiguration(100, StatsDivergenceNoDecayCalculator(1, 1000), false, 1000, 1000, false, false, false, 1000)
    val updateStrategy: UpdateStrategy = defaultUpdateStrategy
    val clock: Clock = Clock.systemUTC()
    val evaluator: ExpressionEvaluator = simpleExpressionEvaluator

    CommunityRuntimeContextCreator.create(
      tracer = NO_TRACING,
      notificationLogger = devNullLogger,
      planContext = planContext,
      queryText = queryText,
      debugOptions = debugOptions,
      offset = offset,
      monitors = monitors,
      metricsFactory = metricsFactory,
      queryGraphSolver = queryGraphSolver,
      config = cypherCompilerConfig,
      updateStrategy = updateStrategy,
      clock = clock,
      evaluator = evaluator)
  }

}
