package org.plan_explorer

import java.time.Clock

import org.jline.reader._
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.neo4j.cypher.internal.compatibility.v3_3.WrappedMonitors
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.CommunityRuntimeContextCreator
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.neo4j.cypher.internal.frontend.v3_3.phases.CompilationPhaseTracer.NO_TRACING
import org.neo4j.cypher.internal.frontend.v3_3.phases.{CompilationPhases, devNullLogger}
import org.neo4j.cypher.internal.ir.v3_3.Cardinality
import org.neo4j.kernel.internal.Version
import org.neo4j.kernel.monitoring.{Monitors => KernelMonitors}


object Main {

  private var knownTokens: Tokens = _

  private var query: String = _
  private var baseState: LogicalPlanState = _
  private var terminal: Terminal = TerminalBuilder.terminal()
  private var possibleIndexes: Set[IndexPossibility] = _
  private var selectedIndexes: Set[IndexUse] = Set.empty
  private var storedStats: Option[StoredStatistics] = None
  private var interestingStatistics: InterestingStats = _

  def main(args: Array[String]): Unit = {

    Runtime.getRuntime.exec("clear")

    println(
      s"""Welcome to plan explorer!
         |verbose ${java.lang.Boolean.getBoolean("pickBestPlan.VERBOSE")}

         |Using Neo4j ${Version.getNeo4jVersion}
         |*-*-*-*-*-*-*-*-*-*-*-*-*
         |""".stripMargin)

    var current: Action = enterQuery()

    while (current != Quit) {
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
      ("Quit", () => Quit)
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
    selectedIndexes = IndexManagement.pickIndexes(selectedIndexes, possibleIndexes, knownTokens)
    mainMenu()
  }

  private def explorePlanSpace(): Action = {
    val stats = storedStats.getOrElse {
      val labels = interestingStatistics.labels.map(l => l -> Cardinality(0)).toMap
      val edges = interestingStatistics.edges.map(l => l -> Cardinality(0)).toMap
      val allNodes = Cardinality(0)
      StoredStatistics(labels, allNodes, edges)
    }

    PlanExplorer.explore(createReader(), stats, mainMenu(), knownTokens, selectedIndexes, baseState)
    mainMenu()
  }

  private def enterQuery(): Action = {
    println("Please enter query. Single line with . to finish input")
    try {
      val input =
        if (true)
          """MATCH (a:A)-->(b:B)<--(c:C) RETURN *""".stripMargin
        else
          multiLineInput()

      print("parsing, ast-rewriting and semantic analysis")
      val maybeBaseState = ParseAndSemanticAnalysis.parsing_rewriting_and_semantics(input)
      val monitors = WrappedMonitors(new KernelMonitors)
      val context = CommunityRuntimeContextCreator.create(NO_TRACING, devNullLogger, null, query, Set.empty, None, monitors, new MyMetricsFactory, null, null, null, Clock.systemUTC(), evaluator = null)

      val finalBaseState = CompilationPhases.lateAstRewriting.transform(maybeBaseState, context).asInstanceOf[LogicalPlanState].copy(maybePeriodicCommit = Some(None))
      println("...")

      print("initial planning to find out interesting schema")
      val (indexes, tokens, recordedStats) = PreparatoryPlanning.plan(input, finalBaseState)
      println("...")

      // Passed all steps = let's switch to the new values
      this.query = input
      this.baseState = finalBaseState
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
