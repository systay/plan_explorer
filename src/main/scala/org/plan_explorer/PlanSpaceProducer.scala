package org.plan_explorer

import java.time.Clock

import org.neo4j.cypher.internal.compatibility.v3_3.WrappedMonitors
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.CommunityRuntimeContextCreator
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.helpers.simpleExpressionEvaluator
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.SimpleMetricsFactory
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.idp._
import org.neo4j.cypher.internal.compiler.v3_3.spi.{GraphStatistics, PlanContext}
import org.neo4j.cypher.internal.compiler.v3_3.{IndexDescriptor, defaultUpdateStrategy}
import org.neo4j.cypher.internal.frontend.v3_3.phases.CompilationPhaseTracer.NO_TRACING
import org.neo4j.cypher.internal.frontend.v3_3.phases.{BaseState, InternalNotificationLogger, devNullLogger}
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, PropertyKeyId, RelTypeId}
import org.neo4j.cypher.internal.v3_3.logical.plans.{LogicalPlan, ProcedureSignature, QualifiedName, UserFunctionSignature}
import org.neo4j.kernel.monitoring.Monitors

object PlanSpaceProducer {
  def produce(steps: Int,
              labels: Map[LabelId, StatisticsValue],
              edges: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), StatisticsValue],
              allNodes: StatisticsValue,
              baseState: BaseState,
              tokens: Tokens,
              indexes: Set[IndexUse]): Array[Array[LogicalPlan]] = {
    val step = 1.0 / steps
    val results: Array[Array[LogicalPlan]] = Array.ofDim[LogicalPlan](steps, steps)
    for {
      xStep <- 0 until steps
      yStep <- 0 until steps
    } {
      val x = xStep * step
      val y = yStep * step
      val labelStats = labels.mapValues(v => v.evaluate(x, y))
      val edgeStats = edges.mapValues(v => v.evaluate(x, y))
      val nodeCount = allNodes.evaluate(x, y)
      val stats = StoredStatistics(labelStats, nodeCount, edgeStats)

      results(xStep)(yStep) = plan(baseState, stats, tokens, indexes)
    }

    results
  }

  private def plan(baseState: BaseState, stats: GraphStatistics, tokens: Tokens, indexes: Set[IndexUse]): LogicalPlan = {

    val config = ParseAndSemanticAnalysis.config
    val monitors = WrappedMonitors(new Monitors)
    val monitor = monitors.newMonitor[IDPQueryGraphSolverMonitor]()
    val solverConfig = new ConfigurableIDPSolverConfig(
      maxTableSize = config.idpMaxTableSize,
      iterationDurationLimit = config.idpIterationDuration
    )
    val singleComponentPlanner = SingleComponentPlanner(monitor, solverConfig)
    val queryGraphSolver = IDPQueryGraphSolver(singleComponentPlanner, cartesianProductsOrValueJoins, monitor)
    val planContext = new MyPlanContext(stats, tokens, indexes)

    val context = CommunityRuntimeContextCreator.create(
      tracer = NO_TRACING,
      notificationLogger = devNullLogger,
      planContext = planContext,
      queryText = baseState.queryText,
      debugOptions = Set.empty,
      offset = None,
      monitors = monitors,
      metricsFactory = SimpleMetricsFactory,
      queryGraphSolver = queryGraphSolver,
      config = config,
      updateStrategy = defaultUpdateStrategy,
      clock = Clock.systemDefaultZone(),
      evaluator = simpleExpressionEvaluator)

    val compiler = ParseAndSemanticAnalysis.createCompiler
    val result = compiler.normalizeQuery(baseState, context)
    val planState = compiler.planPreparedQuery(result, context)
    planState.logicalPlan
  }

  class MyPlanContext(stats: GraphStatistics, tokens: Tokens, indexes: Set[IndexUse]) extends PlanContext {
    override def indexesGetForLabel(labelId: Int): Iterator[IndexDescriptor] = apa(labelId, false)

    private def apa(labelId: Int, wantedUnique: Boolean) = (indexes collect {
      case IndexUse(l, props, unique) if unique == wantedUnique && l.id == labelId =>
        IndexDescriptor(LabelId(labelId), props)
    }).iterator

    override def indexGet(labelName: String, propertyKeys: Seq[String]): Option[IndexDescriptor] = ???

    override def indexExistsForLabel(labelName: String): Boolean = ???

    override def uniqueIndexesGetForLabel(labelId: Int): Iterator[IndexDescriptor] = apa(labelId, wantedUnique = true)

    override def uniqueIndexGet(labelName: String, propertyKey: Seq[String]): Option[IndexDescriptor] = ???

    override def hasPropertyExistenceConstraint(labelName: String, propertyKey: String): Boolean = ???

    override def checkNodeIndex(idxName: String): Unit = ???

    override def checkRelIndex(idxName: String): Unit = ???

    override def getOrCreateFromSchemaState[T](key: Any, f: => T): T = ???

    override def txIdProvider: () => Long = ???

    override def statistics: GraphStatistics = stats

    override def notificationLogger(): InternalNotificationLogger = ???

    override def getLabelName(id: Int): String = tokens.reverseLabels(LabelId(id))

    override def getLabelId(labelName: String): Int = getOptLabelId(labelName).get

    override def getOptLabelId(labelName: String): Option[Int] = tokens.labels.get(labelName).map(_.id)

    override def getPropertyKeyName(id: Int): String = tokens.reverseProps(PropertyKeyId(id))

    override def getPropertyKeyId(key: String): Int = getOptPropertyKeyId(key).get

    override def getOptPropertyKeyId(propertyKeyName: String): Option[Int] = tokens.propKeys.get(propertyKeyName).map(_.id)

    override def getRelTypeName(id: Int): String = tokens.reverseTypes(RelTypeId(id))

    override def getRelTypeId(relType: String): Int = getOptRelTypeId(relType).get

    override def getOptRelTypeId(relType: String): Option[Int] = tokens.types.get(relType).map(_.id)

    override def procedureSignature(name: QualifiedName): ProcedureSignature = ???

    override def functionSignature(name: QualifiedName): Option[UserFunctionSignature] = ???
  }

}
