package org.plan_explorer.tvision

import jexer.TApplication.BackendType
import jexer.event.TMenuEvent
import jexer.menu.TMenu
import jexer.{TAction, TApplication, TKeypress, TWindow}
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.plan_explorer.model._
import org.plan_explorer.tvision.MyApp.MenuEvents._
import org.plan_explorer.tvision.MyApp._

class MyApp extends TApplication(BackendType.SWING)
  with QueryWindowSPI
  with StatisticsWindowSPI
  with ViewCollector {
  // State
  private val statisticsPointer = new StatisticsPointer()
  private val queryW = new QueryWindow(this, this)
  private val statsW = new StatisticsWindow(this, statisticsPointer, this)
  private val viewWindows = new scala.collection.mutable.ArrayBuffer[TWindow with Resizeable]()
  private var baseState: LogicalPlanState = _
  private var possibleIndexes: Set[IndexPossibility] = _
  private var interestingStatistics: InterestingStats = _

  // Init
  createMenuItems()
  viewWindows.append(new ShowPlanWindow(this))
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

    viewWindows.foreach {
      case i: InformationConsumer => i.setData(newPlan)
      case _ =>
    }
  }

  override def currentQuery: String = queryW.getQueryText()

  override def onMenu(menu: TMenuEvent): Boolean = {
    val pf: PartialFunction[Int, Unit] = {
      case SMART_LAYOUT =>
        smartLayout()
      case LOGICALPLAN =>
        showOrCreate(_.isInstanceOf[ShowPlanWindow], new ShowPlanWindow(this))
      case SHOW_AST =>
        showOrCreate(_.isInstanceOf[ShowPlanTreeWindow], new ShowPlanTreeWindow(this))
    }

    if (pf.isDefinedAt(menu.getId)) {
      pf(menu.getId)
      true
    } else

      super.onMenu(menu)
  }

  private def showOrCreate(tester: TWindow with Resizeable => Boolean, creator: => TWindow with Resizeable): Unit = {
    val maybeWindow = viewWindows.find(tester)
    maybeWindow match {
      case None =>
        viewWindows.append(creator)
        queryHasBeenUpdated(queryW.getQueryText())

      case Some(view) =>
        view.activate()
    }
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
    statsW.setY(queryWindowHeight)
    statsW.resizeTo(qAndStats, desktopHeight - queryWindowHeight)

    val viewHeight = desktopHeight / viewWindows.size - 1
    val viewWidth = desktopWidth - qAndStats
    viewWindows.zipWithIndex.foreach {
      case (window, idx) =>
        val top = viewHeight * idx + 1
        window.setX(qAndStats)
        window.setY(top)
        window.resizeTo(viewWidth, viewHeight)
    }
  }

  override def viewClosed(view: TWindow with Resizeable): Unit = {
    println("closed view")
    val idx = viewWindows.indexOf(view)
    if (idx >= 0)
      viewWindows.remove(idx)
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
    fileMenu.addItem(MenuEvents.SMART_LAYOUT, "Smart layout", ctrlSpace)
    fileMenu.addDefaultItem(TMenu.MID_EXIT)

    val viewMenu = addMenu("&View")
    viewMenu.addItem(MenuEvents.LOGICALPLAN, "LogicalPlan.toString")
    viewMenu.addItem(MenuEvents.SHOW_AST, "LogicalPlan Tree")
  }
}

object MyApp {

  object MenuEvents {
    val SMART_LAYOUT = 2000
    val LOGICALPLAN = 2001
    val SHOW_AST = 2002
  }

}

trait Resizeable {
  def resizeTo(newWidth: Int, newHeight: Int): Unit
}

trait InformationConsumer {
  def setData(p: LogicalPlanState): Unit
}

trait ViewCollector {
  def viewClosed(view: TWindow with Resizeable): Unit
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
