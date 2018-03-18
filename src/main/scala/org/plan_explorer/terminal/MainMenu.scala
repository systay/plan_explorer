package org.plan_explorer.terminal

import org.jline.reader._
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.plan_explorer.model._
import org.plan_explorer.terminal.menu.{Action, Menu}

class MainMenu {

  private var knownTokens: Tokens = _

  private var query: String = _
  private var baseState: LogicalPlanState = _
  private var terminal: Terminal = TerminalBuilder.terminal()
  private var possibleIndexes: Set[IndexPossibility] = _
  private var selectedIndexes: Set[IndexUse] = Set.empty
  private var storedStats: Option[StoredStatistics] = None
  private var interestingStatistics: InterestingStats = _

  def mainLoop(): Unit = {
    var current: Action = enterQuery()

    while (current != SystemExit) {
      current = current.chooseOptionFromReader(createReader())
    }
  }

  def createReader(): LineReader = {
    terminal.close()
    terminal = TerminalBuilder.builder().build()
    LineReaderBuilder.builder().terminal(terminal).build()
  }

  private def mainMenu(): Action = {
    println()
    println(s"Current query: \n$query")
    println()
    Menu(
      ("View current schema & stats", viewState),
      ("Load schema and statistics from database", loadFromDatabase),
      ("Edit indexes", indexMgmt),
      ("Explore plan space", explorePlanSpace),
      ("Change query", enterQuery),
      ("Reset state", reset),
      ("Quit", () => SystemExit)
    )
  }

  private def viewState(): Action = {
    if (selectedIndexes.isEmpty)
      println("No indexes defined")
    else {
      println("Current indexes:")
      selectedIndexes.foreach(i => println(s"  $i"))
    }

    if (storedStats == null)
      println("No stats set")
    else
      println(interestingStatistics.toString(knownTokens))

    mainMenu()
  }

  private def reset(): Action = {
    println("Reset state")
    selectedIndexes = Set.empty
    storedStats = None
    possibleIndexes = null
    interestingStatistics = null
    enterQuery()
  }

  private def loadFromDatabase(): Action = {
    val path = createReader().readLine("Path to database: ")

    try {
      val dbState = LoadFromDatabase.loadFromDatabase(path, query, knownTokens, interestingStatistics)
      this.selectedIndexes = dbState.indexes
      this.storedStats = Some(dbState.statistics)
      this.interestingStatistics = this.interestingStatistics.translateBetween(this.knownTokens, dbState.tokens)
      this.knownTokens = dbState.tokens
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
    viewState()
  }

  private def indexMgmt(): Action = {
    selectedIndexes = IndexManagement.pickIndexes(selectedIndexes, possibleIndexes, knownTokens, createReader)
    mainMenu()
  }

  private def explorePlanSpace(): Action = {
    val stats = storedStats.getOrElse {
      val labels = interestingStatistics.labels.map(l => l -> Cardinality(0)).toMap
      val edges = interestingStatistics.edges.map(l => l -> Cardinality(0)).toMap
      val allNodes = Cardinality(0)
      StoredStatistics(labels, allNodes, edges)
    }

    PlanExplorer.explore(createReader(), stats, mainMenu(), knownTokens, selectedIndexes, baseState, createReader)
    mainMenu()
  }

  private def enterQuery(): Action = {
    println("Please enter query. Single line with . to finish input")
    try {
      val input =
        if (true)
          """MATCH (a:A)-->(x:X),
            |      (a:B)-->(x:X),
            |      (a:C)-->(x:X)
            |RETURN *
          """.stripMargin
        else
          multiLineInput()

      print("parsing, ast-rewriting, semantic analysis and initial planning to find interesting stats")
      val (indexes, tokens, recordedStats, baseState) = ModelBuilder.prepareForQuery(input)
      println("...")

      // Passed all steps = let's switch to the new values
      this.query = input
      this.baseState = baseState
      this.possibleIndexes = indexes
      this.knownTokens = tokens
      this.interestingStatistics = recordedStats

      mainMenu()
    } catch {
      case e: RuntimeException =>
        e.printStackTrace()
        enterQuery()
    }
  }

  private def multiLineInput(): String = {
    val builder = new StringBuilder
    val reader = createReader()
    while (true) {
      reader.readLine("") match {
        case null => return builder.toString()
        case "." => return builder.toString()
        case line => builder.append(line).append(System.lineSeparator())
      }
    }

    "This never happens"
  }
}
