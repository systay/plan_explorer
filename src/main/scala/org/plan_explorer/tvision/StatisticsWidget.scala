package org.plan_explorer.tvision

import jexer.{TScrollableWidget, TWidget}
import org.neo4j.cypher.internal.frontend.v3_3.LabelId
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.plan_explorer.model.{StoredStatistics, Tokens}

class StatisticsWidget(parent: TWidget,
                       x: Int,
                       y: Int,
                       width: Int,
                       height: Int,
                       pointer: StatisticsPointer,
                       onUpdatedStats: () => Unit)
  extends TScrollableWidget(parent, x, y, width, height) {

  val DIVIDER = 25

  def update(): Unit = {
    this.getChildren.clear()
    var row = 0

    def addField(name: String, startValue: Cardinality, updatedStats: Long => StoredStatistics): Unit = {
      val pos: Int = Math.max(DIVIDER - name.length, 0)

      addLabel(name, pos, row)

      new TNumberField(this, DIVIDER + 2, row, 10, false, startValue.amount.toLong, i => pointer.setNewState(updatedStats(i)), onUpdatedStats)
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
  }

}

class StatisticsPointer() {
  private var _storedStatistics = StoredStatistics.empty
  private var _tokens = Tokens.empty

  def storedStatistics: StoredStatistics = _storedStatistics

  def tokens: Tokens = _tokens

  def setNewState(storedStatistics: StoredStatistics, tokens: Tokens): Unit = {
    this._storedStatistics = storedStatistics
    this._tokens = tokens
  }

  def setNewState(storedStatistics: StoredStatistics): Unit = {
    this._storedStatistics = storedStatistics
  }
}

// Field that only accepts numbers as input
class TNumberField(parent: TWidget,
                   x: Int,
                   y: Int,
                   width: Int,
                   fixed: Boolean,
                   initalValue: Long,
                   onUpdate: Long => Unit,
                   onEnter: () => Unit)
  extends jexer.TField(parent, x, y, width, fixed, initalValue.toString) {

  import JexerScalaHelpers.function2action

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