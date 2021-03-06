package org.plan_explorer.tvision

import jexer._
import jexer.event.TResizeEvent
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.plan_explorer.tvision.JexerScalaHelpers._

import scala.collection.JavaConverters._

class ShowPlanWindow(app: TApplication with ViewCollector)
  extends TWindow(app, "Show Plan", 80, 80, TWindow.RESIZABLE) with Resizeable with InformationConsumer {

  // Widgets
  private var queryPlanLabels = Seq.empty[TLabel]

  override def setData(p: LogicalPlanState): Unit = {
    val plan = p.logicalPlan
    this.getChildren.removeAll(queryPlanLabels.asJava)
    val lines = plan.toString().split(System.lineSeparator())
    queryPlanLabels = lines.zipWithIndex.map {
      case (line, row) =>
        addLabel(line, 0, 2 + row)
    }
  }

  override def resizeTo(newWidth: Int, newHeight: Int): Unit = {
    this.setWidth(newWidth)
    this.setHeight(newHeight)
    onResize(new TResizeEvent(TResizeEvent.Type.WIDGET, newWidth, newHeight))
  }

  override def onClose(): Unit = {
    app.viewClosed(this)
  }
}
