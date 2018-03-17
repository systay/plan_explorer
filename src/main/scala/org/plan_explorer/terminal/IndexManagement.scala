package org.plan_explorer.terminal

import org.jline.reader.LineReader
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, PropertyKeyId}
import org.plan_explorer.model.{IndexPossibility, IndexUse, Tokens}
import org.plan_explorer.terminal.menu.{Action, NumberedMenu}

object IndexManagement {
  def pickIndexes(input: Set[IndexUse],
                  possible: Set[IndexPossibility],
                  tokens: Tokens,
                  createReader: () => LineReader): Set[IndexUse] = {

    val indexPossibilities: Seq[IndexPossibility] = possible.toSeq.sortBy(i => i.label + i.props.mkString("|"))
    var current = input

    def menu() = {
      val newIndexes: Seq[(String, () => Action)] = (
        for {
        i@IndexPossibility(label, props) <- indexPossibilities
      } yield Seq(
        s"Create unique index on $i" -> (() => createIndexOn(label, props, unique = true)),
        s"Create index on $i" -> (() => createIndexOn(label, props, unique = false))
      )).flatten

      val removeIndexes = current.map { idx: IndexUse =>
        s"Remove $idx" -> (() => removeIndexOn(idx.label, idx.props))
      }

      NumberedMenu(newIndexes ++ removeIndexes :+ ("Exit index management" -> (() => SystemExit)): _*)
    }

    def createIndexOn(label: LabelId, props: Seq[PropertyKeyId], unique: Boolean): Action = {
      current = current.filterNot {
        x => x.label == label && x.props == props
      } + IndexUse(label, props, unique)
      menu()
    }

    def removeIndexOn(label: LabelId, props: Seq[PropertyKeyId]): Action = {
      current = current.filterNot {
        x => x.label == label && x.props == props
      }
      menu()
    }

    var currentAction: Action = menu()
    while (currentAction != SystemExit) {
      currentAction = currentAction.chooseOptionFromReader(createReader())
    }

    current
  }

}
