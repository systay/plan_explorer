package org.plan_explorer

import org.plan_explorer.Main.{IndexUse, getReader}

object IndexManagement {
  def pickIndexes(input: Set[IndexUse], possible: Set[IndexPossibility]): Set[IndexUse] = {

    val indexPossibilities: Seq[IndexPossibility] = possible.toSeq.sortBy(i => i.label + i.props.mkString("|"))
    var current = input

    def menu() = {
      val items: Seq[(String, () => Action)] = (for {
        i@IndexPossibility(label, props) <- indexPossibilities
        alreadExists = current.collectFirst {
          case IndexUse(l, p, unique) if label == l && props == p => unique
        }
      } yield alreadExists match {
        case None => Seq(
          s"Create unique index on $i" -> (() => createIndexOn(label, props, unique = true)),
          s"Create index on $i" -> (() => createIndexOn(label, props, unique = false))
        )
        case Some(unique) =>
          val x = if (unique)
            s"Change index on $i to non-unique" -> (() => createIndexOn(label, props, unique = false))
          else
            s"Change index on $i to unique" -> (() => createIndexOn(label, props, unique = true))

          Seq(s"Drop index on $i" -> (() => removeIndexOn(label, props))) :+ x

      }).flatten


      NumberedMenu(items :+ ("Exit index management" -> (() => Quit)): _*)
    }

    def createIndexOn(label: String, props: Seq[String], unique: Boolean): Action = {
      current = current.filterNot {
        case x => x.label == label && x.props == props
      } + IndexUse(label, props, unique)
      menu()
    }

    def removeIndexOn(label: String, props: Seq[String]): Action = ???

    var currentAction: Action = menu()
    while (currentAction != Quit) {
      currentAction = currentAction.chooseOptionFromReader(getReader())
    }

    current
  }

}
