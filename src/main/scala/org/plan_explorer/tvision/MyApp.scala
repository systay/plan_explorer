package org.plan_explorer.tvision

import jexer.TApplication.BackendType
import jexer.event.TMenuEvent
import jexer.menu.TMenu
import jexer.{TAction, TApplication, TKeypress}
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

  override def onMenu(menu: TMenuEvent): Boolean = {
    if (menu.getId == 2000) {
      smartLayout()
      return true
    }
    super.onMenu(menu)
  }

  private def smartLayout(): Unit = {
    // Take the size needed for the q and stats, but no more than one third of the window space available.
    val desktopWidth = getDesktop.getWidth
    val desktopHeight = getDesktop.getHeight
    val queryLines = queryW.getQueryText().split("\n")
    val maxQuerySize = queryLines.map(_.length).max + 3
    val maxStatisticSize = statsW.statisticsSize
    val qAndStatsMax = Math.max(maxQuerySize, maxStatisticSize)
    val half = desktopWidth / 2
    val qAndStats = Math.min(qAndStatsMax, half)

    val queryWindowHeight = queryLines.length + 4

    queryW.setX(0)
    queryW.setY(1)
    queryW.resizeTo(qAndStats, queryWindowHeight)

    statsW.setX(0)
    statsW.setY(queryWindowHeight + 1)
    statsW.resizeTo(qAndStats, desktopHeight - queryWindowHeight - 1)

    planW.setX(qAndStats + 1)
    planW.setY(1)
    planW.resizeTo(desktopWidth - qAndStats - 1, desktopHeight - 1)
  }

  private def createMenuItems() = {
    val fileMenu = addMenu("&Menu")
    val ctrlSpace = new TKeypress(
      /*isKey*/ true,
      /*fnKey*/ TKeypress.F2,
      /*ch*/ ' ',
      /*alt*/ false,
      /*ctrl*/ true,
      /*shift*/ false)
    fileMenu.addItem(2000, "Smart layout", ctrlSpace)
    fileMenu.addDefaultItem(TMenu.MID_EXIT)
    fileMenu
  }
}

trait Resizeable {
  def resizeTo(newWidth: Int, newHeight: Int): Unit
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
