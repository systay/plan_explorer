package org.plan_explorer.tvision

import jexer._
import jexer.event.TResizeEvent
import org.plan_explorer.model.{LoadFromDatabase, StateFromDb}
import org.plan_explorer.tvision.JexerScalaHelpers._

class StatisticsWindow(app: TApplication, statisticsPointer: StatisticsPointer, spi: StatisticsWindowSPI)
  extends TWindow(app, "Statistics", 80, 20, TWindow.NOCLOSEBOX | TWindow.RESIZABLE) with Resizeable {

  // Widgets
  private val stats = new StatisticsWidget(this, 0, 0, 80, 20, statisticsPointer, () => spi.signalNewStatisticsExist())

  def statisticsSize: Int = stats.statisticsSize

  def showNewStats(): Unit = {
    stats.showNewStatistics()
  }

  addButton("Load from database", 0, 0, () => useDb())

  private def useDb(): Unit = {
    val i = new TInputBox(app, "Load Statistics From Database", "Path", "/home/systay/panama")
    println(i.getText)
    val dir = i.getText
    if (dir != null) {
      try {
        val cx: StateFromDb = LoadFromDatabase.loadFromDatabase(dir, spi.currentQuery, statisticsPointer.tokens,
          statisticsPointer.storedStatistics.asInterestingStats)

        statisticsPointer.setNewState(cx.statistics, cx.tokens)
        stats.showNewStatistics()
        spi.signalNewStatisticsExist()
      } catch {
        case e: RuntimeException =>
          e.printStackTrace()
          app.messageBox("Error!", e.getMessage)
      }
    }
  }

  override def resizeTo(newWidth: Int, newHeight: Int): Unit = {
    stats.setWidth(newWidth)
    stats.setHeight(newHeight)
    this.setWidth(newWidth)
    this.setHeight(newHeight)
    onResize(new TResizeEvent(TResizeEvent.Type.WIDGET, newWidth, newHeight))
    showNewStats()
  }
}

trait StatisticsWindowSPI {
  def signalNewStatisticsExist(): Unit

  def currentQuery: String
}

