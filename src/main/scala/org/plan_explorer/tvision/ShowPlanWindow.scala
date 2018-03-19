package org.plan_explorer.tvision

import jexer._
import org.neo4j.cypher.internal.v3_3.logical.plans.LogicalPlan
import org.plan_explorer.tvision.JexerScalaHelpers._

import scala.collection.JavaConverters._

class ShowPlanWindow(app: TApplication) extends TWindow(app, "Show Plan", 80, 80, TWindow.NOCLOSEBOX | TWindow.RESIZABLE) {

  // Widgets
  private var queryPlanLabels = Seq.empty[TLabel]

  def setPlan(p: LogicalPlan): Unit = {
    this.getChildren.removeAll(queryPlanLabels.asJava)
    val lines = p.toString().split(System.lineSeparator())
    queryPlanLabels = lines.zipWithIndex.map {
      case (line, row) =>
        addLabel(line, 0, 2 + row)
    }
  }
}
