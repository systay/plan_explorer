package org.plan_explorer

import org.jline.reader.LineReader
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, RelTypeId}
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.plan_explorer.Main.getReader

object PlanExplorer {
  def explore(reader: LineReader, storedStatistics: StoredStatistics, mainMenu: Action): Unit = {

    var labels: Map[LabelId, StatisticsValue] = storedStatistics.labelCardinality.mapValues(Static.apply)
    var allNodes: StatisticsValue = Static(storedStatistics.allNodes)
    var edges: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), StatisticsValue] =
      storedStatistics.edgeCardinality.mapValues(Static.apply)

    def mainExplorerMenu(): Action = {

      def y[T <: org.neo4j.cypher.internal.frontend.v3_3.NameId](in: Option[T]): String =
        in.map(_.id.toString).getOrElse("")

      def createMenuOption(updater: StatisticsValue => Unit)(): Action = {
        val input = reader.readLine(
          """|New cardinality (the variables x and y are
             |    available, and they will have double
             |    values between 0 and 1)
             | (x: Double, y: Double) =>""".stripMargin)

        val newValue = maybeInt(input).map[StatisticsValue] {
          // If we are dealing with a literal number, just turn it into a static
          amount => Static(Cardinality(amount))
        } getOrElse {
          // otherwise, compile it to a function
          Dynamic.createFunction(input)
        }

        updater(newValue)
        mainExplorerMenu()
      }

      val labelEdits: Seq[(String, () => Action)] =
        labels.toSeq.map {
          case (id, statisticsValue) =>
            val action = createMenuOption((newValue: StatisticsValue) => labels = labels + (id -> newValue)) _
            s"Label :${id.id} $statisticsValue" -> action
        }
      val edgeEdits: Seq[(String, () => Action)] =
        edges.toSeq.map {
          case (key@(fromLabel, relType, toLabel), statisticsValue) =>
            val action = createMenuOption((newValue: StatisticsValue) => edges = edges + (key -> newValue)) _
            s"(${y(fromLabel)})-[${y(relType)}]->(${y(toLabel)}) : $statisticsValue" -> action
        }
      val allNodeEdit = s"All Nodes count: $allNodes" -> createMenuOption(allNodes = _) _
      //      val plot = "Plot plan space" -> (() -> )
      val exit: (String, () => Action) = "Exit!" -> (() => Quit)
      NumberedMenu(labelEdits ++ edgeEdits :+ allNodeEdit :+ exit: _*)
    }

    var current: Action = mainExplorerMenu()

    while (current != Quit) {
      current = current.chooseOptionFromReader(getReader())
    }
  }

  private def maybeInt(in: String): Option[Int] =
    try {
      Some(in.toInt)
    } catch {
      case _: NumberFormatException => None
    }
}

