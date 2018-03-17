package org.plan_explorer.model

case class StateFromDb(indexes: Set[IndexUse], statistics: StoredStatistics, tokens: Tokens)
