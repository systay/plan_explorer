package org.plan_explorer.tvision

import jexer._
import org.plan_explorer.model.{LoadFromDatabase, StateFromDb}
import org.plan_explorer.tvision.JexerScalaHelpers._

class StatisticsWindow(app: TApplication, statisticsPointer: StatisticsPointer, spi: StatisticsWindowSPI)
  extends TWindow(app, "Statistics", 80, 80, TWindow.NOCLOSEBOX | TWindow.RESIZABLE) {

  // Widgets
  private val stats = new StatisticsWidget(this, 0, 0, 42, 10, statisticsPointer, () => spi.signalNewStatisticsExist())

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

}

trait StatisticsWindowSPI {
  def signalNewStatisticsExist(): Unit

  def currentQuery: String
}

