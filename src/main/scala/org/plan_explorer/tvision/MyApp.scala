package org.plan_explorer.tvision

import jexer.TApplication.BackendType
import jexer.event.TMenuEvent
import jexer.menu.TMenu
import jexer.{TAction, TApplication}
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.plan_explorer.model._

class MyApp
  extends TApplication(BackendType.SWING)
    with QueryWindowSPI
    with StatisticsWindowSPI {
  val ENTER_QUERY = 2000

  // State
  private val statisticsPointer = new StatisticsPointer()
  private val queryW = new QueryWindow(this, this)
  private val planW = new ShowPlanWindow(this)
  private val statsW = new StatisticsWindow(this, statisticsPointer, this)
  private var baseState: LogicalPlanState = _
  private var possibleIndexes: Set[IndexPossibility] = _
  private var interestingStatistics: InterestingStats = _

  // Init
  onMenu(new TMenuEvent(TMenu.MID_TILE))
  createMenuItems()
  queryHasBeenUpdated(queryW.getQueryText())

  override def queryHasBeenUpdated(newQuery: String): Unit = {
    val (
      indexes: Set[IndexPossibility],
      tokens: Tokens,
      recordedStats: RecordingStatistics,
      baseState: LogicalPlanState) = ModelBuilder.prepareForQuery(newQuery)
    statisticsPointer.setNewState(StoredStatistics(recordedStats), tokens)
    this.possibleIndexes = indexes
    this.interestingStatistics = recordedStats
    this.baseState = baseState
    statsW.showNewStats()
    signalNewStatisticsExist()
  }

  override def signalNewStatisticsExist(): Unit = {
    val newPlan = PlanSpaceProducer.plan(baseState, statisticsPointer.storedStatistics, statisticsPointer.tokens, Set.empty)
    planW.setPlan(newPlan)
  }

  override def currentQuery: String = queryW.getQueryText()

  private def createMenuItems() = {
    val fileMenu = addMenu("&Menu")
    fileMenu.addDefaultItem(TMenu.MID_TILE)
    fileMenu.addItem(2000, "Smart layout")
    fileMenu.addDefaultItem(TMenu.MID_EXIT)
    fileMenu
  }

  //  override def onMenu(menu: TMenuEvent): Boolean = {
  //    if(menu.getId == 2000) {
  //      val maxQuery = queryW.getQueryText().split("\n").map(_.length).max
  //
  //    }
  //    super.onMenu(menu)
  //  }
}

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val app = new MyApp
      new Thread(app).start()
    } catch {
      case t: Throwable =>
        t.printStackTrace()
    }
  }
}

object JexerScalaHelpers {
  implicit def function2action[A](x: () => A): TAction = new TAction {
    override def DO(): Unit = x()
  }
}
