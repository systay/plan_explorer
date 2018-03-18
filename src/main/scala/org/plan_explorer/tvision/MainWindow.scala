package org.plan_explorer.tvision

import java.io.File

import jexer._
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.neo4j.cypher.internal.v3_3.logical.plans.{LogicalPlan, SingleRow}
import org.plan_explorer.model._
import org.plan_explorer.tvision.JexerScalaHelpers._

import scala.collection.JavaConverters._
import scala.io.Source

class MainWindow(app: TApplication)
  extends TWindow(app, "Interactive Plan Explorer", 80, 80, TWindow.NOCLOSEBOX) {

  private val statisticsPointer = new StatisticsPointer()

  // Widgets
  private val queryTest = new TEditorWidget(this, q, 0, 0, 42, 10)
  private val stats = new StatisticsWidget(this, 0, 13, 42, 10, statisticsPointer, () => statsHaveBeenUpdated())
  private var queryPlanLabels = Seq.empty[TLabel]

  // State
  private var query: String = _
  private var baseState: LogicalPlanState = _
  private var possibleIndexes: Set[IndexPossibility] = _
  private var selectedIndexes: Set[IndexUse] = Set.empty
  private var interestingStatistics: InterestingStats = _
  private var currentPlan: LogicalPlan = SingleRow()(null)

  // Initiliaze
  maximize()
  addButton("Update plan with query", 1, 11, () => planQuery())
  planQuery()

  private def q =
    """MATCH (a:A)-[:T]->(b:B)
      |RETURN *""".stripMargin

  private def planQuery(): Unit = {
    this.query = getQueryText()
    val (
      indexes: Set[IndexPossibility],
      tokens: Tokens,
      recordedStats: RecordingStatistics,
      baseState: LogicalPlanState) = ModelBuilder.prepareForQuery(query)
    statisticsPointer.setNewState(StoredStatistics(recordedStats), tokens)
    this.possibleIndexes = indexes
    this.interestingStatistics = recordedStats
    this.baseState = baseState
    stats.update()
    statsHaveBeenUpdated()
  }

  private def getQueryText(): String = {
    // There has got to be a better way of doing this...
    val tempFile = File.createTempFile("plan_explorer", ".cypher")
    queryTest.saveToFilename(tempFile.getAbsolutePath)
    val query = Source.fromFile(tempFile).getLines().mkString("\n")
    query
  }

  private def statsHaveBeenUpdated(): Unit = {
    val newPlan = PlanSpaceProducer.plan(baseState, statisticsPointer.storedStatistics, statisticsPointer.tokens, Set.empty)
    setPlan(newPlan)
  }

  private def setPlan(p: LogicalPlan): Unit = {
    this.getChildren.removeAll(queryPlanLabels.asJava)
    this.currentPlan = p
    val lines = p.toString().split(System.lineSeparator())
    queryPlanLabels = lines.zipWithIndex.map {
      case (line, row) =>
        addLabel(line, 44, 3 + row)
    }
  }
}

object JexerScalaHelpers {
  implicit def function2action[A](x: () => A): TAction = new TAction {
    override def DO(): Unit = x()
  }
}
