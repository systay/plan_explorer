package org.plan_explorer.tvision

import org.plan_explorer.model.{StoredStatistics, Tokens}

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
