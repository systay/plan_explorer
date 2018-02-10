package org.plan_explorer

import java.time.Clock

import org.jline.reader.LineReader
import org.neo4j.cypher.internal.compatibility.v3_3.WrappedMonitors
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.CommunityRuntimeContextCreator
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.helpers.simpleExpressionEvaluator
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.SimpleMetricsFactory
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.idp._
import org.neo4j.cypher.internal.compiler.v3_3.spi.{GraphStatistics, PlanContext}
import org.neo4j.cypher.internal.compiler.v3_3.{IndexDescriptor, defaultUpdateStrategy}
import org.neo4j.cypher.internal.frontend.v3_3.phases.CompilationPhaseTracer.NO_TRACING
import org.neo4j.cypher.internal.frontend.v3_3.phases.{BaseState, InternalNotificationLogger, devNullLogger}
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, RelTypeId}
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.neo4j.cypher.internal.v3_3.logical.plans.{LogicalPlan, ProcedureSignature, QualifiedName, UserFunctionSignature}
import org.neo4j.kernel.monitoring.Monitors
import org.plan_explorer.Main.createReader

object PlanExplorer {
  def explore(reader: LineReader, storedStatistics: StoredStatistics, mainMenu: Action): Unit = {

    var labels: Map[LabelId, StatisticsValue] = storedStatistics.labelCardinality.mapValues(Static.apply)
    var allNodes: StatisticsValue = Static(storedStatistics.allNodes)
    var edges: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), StatisticsValue] =
      storedStatistics.edgeCardinality.mapValues(Static.apply)

    def mainExplorerMenu(): Action = {

      def y[T <: org.neo4j.cypher.internal.frontend.v3_3.NameId](in: Option[T]): String =
        in.map(_.id.toString).getOrElse("")

      def createMenuOption(updater: StatisticsValue => Unit)(): Action = {
        val input = reader.readLine(
          """|New cardinality (the variables x and y are
             |    available, and they will have double
             |    values between 0 and 1)
             | (x: Double, y: Double) =>""".stripMargin)

        val newValue = maybeInt(input).map[StatisticsValue] {
          // If we are dealing with a literal number, just turn it into a static
          amount => Static(Cardinality(amount))
        } getOrElse {
          // otherwise, compile it to a function
          Dynamic.createFunction(input)
        }

        updater(newValue)
        mainExplorerMenu()
      }

      val labelEdits: Seq[(String, () => Action)] =
        labels.toSeq.map {
          case (id, statisticsValue) =>
            val action = createMenuOption((newValue: StatisticsValue) => labels = labels + (id -> newValue)) _
            s"Label :${id.id} $statisticsValue" -> action
        }
      val edgeEdits: Seq[(String, () => Action)] =
        edges.toSeq.map {
          case (key@(fromLabel, relType, toLabel), statisticsValue) =>
            val action = createMenuOption((newValue: StatisticsValue) => edges = edges + (key -> newValue)) _
            s"(${y(fromLabel)})-[${y(relType)}]->(${y(toLabel)}) : $statisticsValue" -> action
        }
      val allNodeEdit = s"All Nodes count: $allNodes" -> createMenuOption(allNodes = _) _
      //      val plot = "Plot plan space" -> (() -> )
      val exit: (String, () => Action) = "Exit!" -> (() => Quit)
      NumberedMenu(labelEdits ++ edgeEdits :+ allNodeEdit :+ exit: _*)
    }

    var current: Action = mainExplorerMenu()

    while (current != Quit) {
      current = current.chooseOptionFromReader(createReader())
    }
  }

  private def maybeInt(in: String): Option[Int] =
    try {
      Some(in.toInt)
    } catch {
      case _: NumberFormatException => None
    }
}

object PlanSpaceProducer {
  def produce(steps: Int,
              labels: Map[LabelId, StatisticsValue],
              edges: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), StatisticsValue],
              allNodes: StatisticsValue): Array[Array[LogicalPlan]] = {
    val step = 1.0 / steps
    val results: Array[Array[LogicalPlan]] = Array.ofDim[LogicalPlan](steps, steps)
    for {
      xStep <- 0 until steps //0D.until(1D, 1 / steps)
      yStep <- 0 until steps //D.until(1D, 1 / steps)
    } {
      val x = xStep * step
      val y = yStep * step
      val labelStats = labels.mapValues(v => v.evaluate(x, y))
      val edgeStats = edges.mapValues(v => v.evaluate(x, y))
      val nodeCount = allNodes.evaluate(x, y)
      val stats = StoredStatistics(labelStats, nodeCount, edgeStats)

    }
    ???
  }

  def plan(query: String, baseState: BaseState): LogicalPlan = {

    val config = ParseAndSemanticAnalysis.config
    val monitors = WrappedMonitors(new Monitors)
    val monitor = monitors.newMonitor[IDPQueryGraphSolverMonitor]()
    val solverConfig = new ConfigurableIDPSolverConfig(
      maxTableSize = config.idpMaxTableSize,
      iterationDurationLimit = config.idpIterationDuration
    )
    val singleComponentPlanner = SingleComponentPlanner(monitor, solverConfig)
    val queryGraphSolver = IDPQueryGraphSolver(singleComponentPlanner, cartesianProductsOrValueJoins, monitor)
    val planContext = new MyPlanContext

    val context = CommunityRuntimeContextCreator.create(
      tracer = NO_TRACING,
      notificationLogger = devNullLogger,
      planContext = planContext,
      queryText = query,
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

  class MyPlanContext extends PlanContext {
    override def indexesGetForLabel(labelId: Int): Iterator[IndexDescriptor] = ???

    override def indexGet(labelName: String, propertyKeys: Seq[String]): Option[IndexDescriptor] = ???

    override def indexExistsForLabel(labelName: String): Boolean = ???

    override def uniqueIndexesGetForLabel(labelId: Int): Iterator[IndexDescriptor] = ???

    override def uniqueIndexGet(labelName: String, propertyKey: Seq[String]): Option[IndexDescriptor] = ???

    override def hasPropertyExistenceConstraint(labelName: String, propertyKey: String): Boolean = ???

    override def checkNodeIndex(idxName: String): Unit = ???

    override def checkRelIndex(idxName: String): Unit = ???

    override def getOrCreateFromSchemaState[T](key: Any, f: => T): T = ???

    override def txIdProvider: () => Long = ???

    override def statistics: GraphStatistics = ???

    override def notificationLogger(): InternalNotificationLogger = ???

    override def getLabelName(id: Int): String = ???

    override def getOptLabelId(labelName: String): Option[Int] = ???

    override def getLabelId(labelName: String): Int = ???

    override def getPropertyKeyName(id: Int): String = ???

    override def getOptPropertyKeyId(propertyKeyName: String): Option[Int] = ???

    override def getPropertyKeyId(propertyKeyName: String): Int = ???

    override def getRelTypeName(id: Int): String = ???

    override def getOptRelTypeId(relType: String): Option[Int] = ???

    override def getRelTypeId(relType: String): Int = ???

    override def procedureSignature(name: QualifiedName): ProcedureSignature = ???

    override def functionSignature(name: QualifiedName): Option[UserFunctionSignature] = ???
  }

}
