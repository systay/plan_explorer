package org.plan_explorer

import org.plan_explorer.Main.createReader

object IndexManagement {
  def pickIndexes(input: Set[IndexUse], possible: Set[IndexPossibility], tokens: Tokens): Set[IndexUse] = {

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

      NumberedMenu(newIndexes ++ removeIndexes :+ ("Exit index management" -> (() => Quit)): _*)
    }

    def createIndexOn(label: Int, props: Seq[Int], unique: Boolean): Action = {
      current = current.filterNot {
        case x => x.label == label && x.props == props
      } + IndexUse(label, props, unique)
      menu()
    }

    def removeIndexOn(label: Int, props: Seq[Int]): Action = {
      current = current.filterNot {
        case x => x.label == label && x.props == props
      }
      menu()
    }

    var currentAction: Action = menu()
    while (currentAction != Quit) {
      currentAction = currentAction.chooseOptionFromReader(createReader())
    }

    current
  }

}

case class IndexUse(label: Int, props: Seq[Int], unique: Boolean) {
  def toString(tokens: Tokens): String = {
    val uniqueS = if (unique) "UNIQUE " else ""
    val labelName = tokens.reverseLabels(label)
    val propNames = props.map(tokens.reverseLabels).mkString(", ")
    s"${uniqueS}INDEX ON :$labelName($propNames)"
  }
}

case class IndexPossibility(label: Int, props: Seq[Int]) {
  def toString(tokens: Tokens): String = s"(:$label {${props.mkString(",")}})"
}
