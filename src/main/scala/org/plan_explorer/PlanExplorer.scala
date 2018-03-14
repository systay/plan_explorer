package org.plan_explorer

import org.jline.reader.LineReader
import org.neo4j.cypher.internal.frontend.v3_3.phases.BaseState
import org.neo4j.cypher.internal.frontend.v3_3.{LabelId, RelTypeId}
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.neo4j.cypher.internal.v3_3.logical.plans.LogicalPlan
import org.plan_explorer.Main._

object PlanExplorer {
  def explore(reader: LineReader,
              storedStatistics: StoredStatistics,
              mainMenu: Action,
              tokens: Tokens,
              indexes: Set[IndexUse],
              baseState: BaseState): Unit = {

    var labels: Map[LabelId, StatisticsValue] = storedStatistics.labelCardinality.mapValues(Static.apply)
    var allNodes: StatisticsValue = Static(storedStatistics.allNodes)
    var edges: Map[(Option[LabelId], Option[RelTypeId], Option[LabelId]), StatisticsValue] =
      storedStatistics.edgeCardinality.mapValues(Static.apply)

    def plotIt(): Action = {
      val result: Array[Array[LogicalPlan]] = PlanSpaceProducer.produce(10, labels, edges, allNodes, baseState, tokens, indexes)
      val allPlans = new scala.collection.mutable.HashSet[LogicalPlan]()
      for {
        lvl1: Array[LogicalPlan] <- result
        plan: LogicalPlan <- lvl1
      } {
        allPlans.add(plan)
      }

      val plansWithId: Map[LogicalPlan, Int] = allPlans.zipWithIndex.toMap

      drawChart(result, plansWithId)

      val planLookup: Map[Int, LogicalPlan] = plansWithId.map(_.swap)
      var thisMenu: Action = null

      def showPlan(i: Int): Action = {
        val plan = planLookup(i)
        println(plan)
        thisMenu
      }

      val showPlanOptions = plansWithId.values.map {
        case x => s"${asChar(x)}" -> (() => showPlan(x))
      }.toSeq

      val allOptions = showPlanOptions :+ ("Back", () => mainExplorerMenu())

      thisMenu = Menu(allOptions: _*)

      thisMenu
    }


    def mainExplorerMenu(): Action = {

      val y = tokens.tokenToString _

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
            s"Label ${y(Some(id))} $statisticsValue" -> action
        }
      val edgeEdits: Seq[(String, () => Action)] =
        edges.toSeq.map {
          case (key@(fromLabel, relType, toLabel), statisticsValue) =>
            val action = createMenuOption((newValue: StatisticsValue) => edges = edges + (key -> newValue)) _
            s"(${y(fromLabel)})-[${y(relType)}]->(${y(toLabel)}) : $statisticsValue" -> action
        }
      val allNodeEdit = s"All Nodes count: $allNodes" -> createMenuOption(allNodes = _) _
      val plot: (String, () => Action) = "Plot plan space" -> (() => plotIt())
      val exit: (String, () => Action) = "Exit!" -> (() => Quit)
      NumberedMenu(labelEdits ++ edgeEdits :+ allNodeEdit :+ plot :+ exit: _*)
    }

    var current: Action = mainExplorerMenu()

    while (current != Quit) {
      current = current.chooseOptionFromReader(createReader())
    }
  }

  private def drawChart(result: Array[Array[LogicalPlan]], x: Map[LogicalPlan, Int]): Unit = {
    for (lvl1 <- result) {
      for (plan <- lvl1) {
        val id = x(plan)
        print(asChar(id))
      }
      println()
    }
  }

  def asChar(i: Int) = ('A' + i).asInstanceOf[Char]

  private def maybeInt(in: String): Option[Int] =
    try {
      Some(in.toInt)
    } catch {
      case _: NumberFormatException => None
    }
}
