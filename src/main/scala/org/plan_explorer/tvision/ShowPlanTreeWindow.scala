package org.plan_explorer.tvision

import jexer._
import jexer.event.TResizeEvent
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.neo4j.cypher.internal.v3_3.logical.plans.LogicalPlan
import org.plan_explorer.tvision.JexerScalaHelpers._

class ShowPlanTreeWindow(app: TApplication with ViewCollector)
  extends TWindow(app, "Show Plan Tree", 80, 80, TWindow.RESIZABLE) with Resizeable with InformationConsumer {

  // Widgets
  private var queryTree = new TTreeView(this, 0, 0, 80, 80)

  override def setData(p: LogicalPlanState): Unit = {
    val plan = p.logicalPlan
    def planName(p: LogicalPlan): String = {
      s"${p.getClass.getSimpleName}(${p.availableSymbols.mkString(",")})"
    }

    def addChildren(tree: TTreeItem, p: LogicalPlan): Unit = {

      def addPlan(p: LogicalPlan): Unit = {
        val treeItem = tree.addChild(planName(p), true)
        addChildren(treeItem, p)
      }

      p.lhs.foreach(addPlan)
      p.rhs.foreach(addPlan)
    }

    val root = new TTreeItem(queryTree, planName(plan), true)
    addChildren(root, plan)
    queryTree.setTreeRoot(root, true)
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
