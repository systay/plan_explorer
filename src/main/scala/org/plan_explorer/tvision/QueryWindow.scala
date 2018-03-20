package org.plan_explorer.tvision

import java.io.File

import jexer._
import jexer.event.TResizeEvent
import org.plan_explorer.tvision.JexerScalaHelpers._

import scala.io.Source

class QueryWindow(app: TApplication, spi: QueryWindowSPI)
  extends TWindow(app, "Query", 80, 80, TWindow.NOCLOSEBOX | TWindow.RESIZABLE) {

  // Widgets
  private val queryText = new TEditorWidget(this, q, 1, 2, 80 - 3, 80 - 4)

  // Initiliaze
  addButton("Update plan with query", 0, 0, () => spi.queryHasBeenUpdated(getQueryText()))

  def getQueryText(): String = {
    // There has got to be a better way of doing this...
    val tempFile = File.createTempFile("plan_explorer", ".cypher")
    queryText.saveToFilename(tempFile.getAbsolutePath)
    val query = Source.fromFile(tempFile).getLines().mkString("\n")
    query
  }

  override def onResize(resize: TResizeEvent): Unit = {
    super.onResize(resize)
    queryText.setWidth(resize.getWidth - 3)
    queryText.setHeight(resize.getHeight - 4)
  }

  private def q =
    """MATCH (o:Officer)-->(e:Entity)-[:INTERMEDIARY_OF]-(i:Intermediary)
      |WHERE o.name CONTAINS "Ross"
      |MATCH (e)--(o2:Officer)
      |RETURN *""".stripMargin
}

trait QueryWindowSPI {
  def queryHasBeenUpdated(newQuery: String): Unit
}