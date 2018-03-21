package org.plan_explorer.tvision

import jexer.event.TResizeEvent
import jexer.{TCheckbox, TScrollableWidget, TWidget}
import org.neo4j.cypher.internal.frontend.v3_3.LabelId
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.plan_explorer.model.StoredStatistics

import scala.util.Try

class StatisticsWidget(parent: TWidget,
                       x: Int,
                       y: Int,
                       width: Int,
                       height: Int,
                       pointer: StatisticsPointer,
                       onUpdatedStats: () => Unit)
  extends TScrollableWidget(parent, x, y, width, height) {

  // State
  private val fields = new collection.mutable.ArrayBuffer[StatisticValue]()
  private var biggestStatisticsName = 0

  def statisticsSize: Int = biggestStatisticsName + 6

  override def onResize(event: TResizeEvent): Unit = {
    super.onResize(event)
    showNewStatistics()
  }

  def showNewStatistics(): Unit = {
    this.getChildren.clear()
    this.fields.clear()
    var row = 0
    biggestStatisticsName = 0

    def addField(name: String, startValue: Cardinality, updatedStats: Long => StoredStatistics): Unit = {
      biggestStatisticsName = Math.max(biggestStatisticsName, name.length)
      val divider = DIVIDER
      val pos: Int = Math.max(divider - name.length - 6, 0)
      val checkBox: TCheckbox = addCheckbox(pos, row, name, false)
      val field = new TNumberField(
        parent = this,
        x = divider,
        y = row,
        width = 10,
        fixed = false,
        initialValue = startValue.amount.toLong,
        onUpdate = i => pointer.setNewState(updatedStats(i)),
        onEnter = onUpdatedStats)
      fields.append(new StatisticValue(checkBox, Try(field.getText.toLong).toOption, field.setNewValue))
      row = row + 1
    }

    def stats = pointer.storedStatistics

    addField("All Nodes", stats.allNodes, i => stats.copy(allNodes = Cardinality(i)))
    val f = pointer.tokens.maybeTokenToString _


    stats.labelCardinality.foreach {
      case (l: LabelId, card) =>
        val labelName = pointer.tokens.tokenToString(l)
        addField(labelName, card, i => stats.copy(labelCardinality = stats.labelCardinality + (l -> Cardinality(i))))
    }

    stats.edgeCardinality.foreach {
      case (key@(fromLabel, relType, toLabel), cardinality) =>
        val name = s"(${f(fromLabel)})-[${f(relType)}]->(${f(toLabel)})"
        addField(name, cardinality, i => stats.copy(edgeCardinality = stats.edgeCardinality + (key -> Cardinality(i))))
    }

    import JexerScalaHelpers._

    addButton("x0.1", 1, row, () => updateFieldsWith(_ / 10))
    addButton("x0.5", 10, row, () => updateFieldsWith(_ / 2))
    addButton("x2", 20, row, () => updateFieldsWith(_ * 2))
    addButton("x10", 30, row, () => updateFieldsWith(_ * 10))
  }

  // Where
  def DIVIDER = getWidth - 10

  private def updateFieldsWith(f: Long => Long): Unit =
    for {
      field <- fields if field.isChecked
      currentValue <- field.currentValue
      newValue = f(currentValue)
    } {
      field.setNewValue(newValue)
    }

  class StatisticValue(checkbox: TCheckbox, current: => Option[Long], newValue: Long => Unit) {
    def isChecked: Boolean = checkbox.isChecked

    def currentValue: Option[Long] = current

    def setNewValue(in: Long): Unit = newValue(in)
  }
}

// Field that only accepts numbers as input
class TNumberField(parent: TWidget,
                   x: Int,
                   y: Int,
                   width: Int,
                   fixed: Boolean,
                   initialValue: Long,
                   onUpdate: Long => Unit,
                   onEnter: () => Unit)
  extends jexer.TField(parent, x, y, width, fixed, initialValue.toString) {

  import JexerScalaHelpers.function2action

  def setNewValue(value: Long) = {
    setText(value.toString)
    onUpdate(value)
    onEnter()
  }

  this.enterAction = () => {
    onEnter()
  }

  this.updateAction = () => try {
    onUpdate(getText.toLong)
  } catch {
    case _: NumberFormatException => // Let's just ignore this field until we have a number again
  }

  override def appendChar(ch: Char): Unit =
    if (Character.isDigit(ch)) super.appendChar(ch)

  override def insertChar(ch: Char): Unit =
    if (Character.isDigit(ch)) super.insertChar(ch)
}