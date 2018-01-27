package org.plan_explorer

import java.time.Clock

import org.neo4j.cypher.internal.compatibility.v3_3.WrappedMonitors
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.CommunityRuntimeContextCreator
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.helpers.simpleExpressionEvaluator
import org.neo4j.cypher.internal.compiler.v3_3.defaultUpdateStrategy
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.Metrics.{CardinalityModel, CostModel, QueryGraphCardinalityModel}
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.idp._
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.{ExpressionEvaluator, MetricsFactory, SimpleMetricsFactory}
import org.neo4j.cypher.internal.compiler.v3_3.spi.GraphStatistics
import org.neo4j.cypher.internal.frontend.v3_3.phases.CompilationPhaseTracer.NO_TRACING
import org.neo4j.cypher.internal.frontend.v3_3.phases.{BaseState, devNullLogger}
import org.neo4j.kernel.monitoring.Monitors

object Planning {
  def plan(query: String, baseState: BaseState): LogicalPlanState = {

    val metricsFactory = new MyMetricsFactory
    val config = ParseAndSemanticAnalysis.config
    val monitors = WrappedMonitors(new Monitors)
    val monitor = monitors.newMonitor[IDPQueryGraphSolverMonitor]()
    val solverConfig = new ConfigurableIDPSolverConfig(
      maxTableSize = config.idpMaxTableSize,
      iterationDurationLimit = config.idpIterationDuration
    )
    val singleComponentPlanner = SingleComponentPlanner(monitor, solverConfig)
    val queryGraphSolver = IDPQueryGraphSolver(singleComponentPlanner, cartesianProductsOrValueJoins, monitor)
    val planContext = new RecordingPlanContext

    val context = CommunityRuntimeContextCreator.create(
      tracer = NO_TRACING,
      notificationLogger = devNullLogger,
      planContext = planContext,
      queryText = query,
      debugOptions = Set.empty,
      offset = None,
      monitors = monitors,
      metricsFactory = metricsFactory,
      queryGraphSolver = queryGraphSolver,
      config = config,
      updateStrategy = defaultUpdateStrategy,
      clock = Clock.systemDefaultZone(),
      evaluator = simpleExpressionEvaluator)

    val compiler = ParseAndSemanticAnalysis.createCompiler
    val result = compiler.normalizeQuery(baseState, context)
    val finalResult = compiler.planPreparedQuery(result, context)

    println("...")
    println(s"labels: ${planContext.labels.keySet.mkString(", ")}")
    println(s"propkeys: ${planContext.propKeys.keySet.mkString(", ")}")
    println(s"rel-types: ${planContext.types.keySet.mkString(", ")}")
    println(s"interesting indexes:\n  ${metricsFactory._costModel.interestingIndexes.mkString("\n  ")}")

    finalResult
  }
}

class MyMetricsFactory extends MetricsFactory {

  val _costModel = new RecordingCostModel(SimpleMetricsFactory.newCostModel())

  override def newCardinalityEstimator(queryGraphCardinalityModel: QueryGraphCardinalityModel,
                                       expressionEvaluator: ExpressionEvaluator): CardinalityModel =
    SimpleMetricsFactory.newCardinalityEstimator(queryGraphCardinalityModel, expressionEvaluator)

  override def newCostModel(): CostModel = _costModel

  override def newQueryGraphCardinalityModel(statistics: GraphStatistics): QueryGraphCardinalityModel =
    SimpleMetricsFactory.newQueryGraphCardinalityModel(statistics)
}
