package org.plan_explorer

import org.jline.reader._
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.neo4j.cypher.internal.frontend.v3_3.phases.BaseState
import org.neo4j.kernel.internal.Version

object Main {

  private var knownTokens: Tokens = _

  private var query: String = _
  private var baseState: BaseState = _
  private var terminal: Terminal = TerminalBuilder.terminal()
  private var possibleIndexes: Set[IndexPossibility] = _
  private var selectedIndexes: Set[IndexUse] = Set.empty
  private var statistics: RecordedStatistics = _

  def main(args: Array[String]): Unit = {

    println(
      s"""Welcome to plan explorer!
         |Using Neo4j ${Version.getNeo4jVersion}
         |*-*-*-*-*-*-*-*-*-*-*-*-*
         |""".stripMargin)

    var current: Action = enterQuery()

    while (current != Quit) {
      current = current.chooseOptionFromReader(getReader())
    }
  }

  def getReader(): LineReader = {
    terminal.close()
    terminal = TerminalBuilder.builder().build()
    LineReaderBuilder.builder().terminal(terminal).build()
  }

  private def mainMenu(): Action = {
    println()
    Menu(
      ("View current schema", viewIndexes),
      ("Load schema and statistics from database", loadFromDatabase),
      //    ("Edit schema and statistics", mainMenu),
      ("Edit indexes", indexMgmt),
      //      ("Explore plan space", mainMenu),
      ("Change query", enterQuery),
      ("Reset state", reset),
      ("Quit", () => Quit)
    )
  }

  private def viewIndexes(): Action = {
    if (selectedIndexes.isEmpty)
      println("No indexes defined")
    else {
      println("Current indexes:")
      selectedIndexes.foreach(i => println(s"  $i"))
    }

    mainMenu()
  }

  private def reset(): Action = {
    println("Reset state")
    selectedIndexes = Set.empty
    enterQuery()
  }

  private def loadFromDatabase(): Action = {
    val path = getReader().readLine("Path to database: ")

    try {
      val dbState = LoadFromDatabase.loadFromDatabase(path, query, knownTokens)
      this.selectedIndexes = dbState.indexes
      this.statistics = dbState.statistics
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
    mainMenu()
  }

  private def indexMgmt(): Action = {
    selectedIndexes = IndexManagement.pickIndexes(selectedIndexes, possibleIndexes)
    mainMenu()
  }

  private def enterQuery(): Action = {
    println("Please enter query. Single line with . to finish input")
    try {
      val input =
        if (true)
          "MATCH (a:A:B) WHERE a.prop1 = 42 AND a.prop2 > 43 AND exists(a.prop3) RETURN *"
        else
          multiLineInput()

      print("parsing, ast-rewriting and semantic analysis")
      val maybeBaseState = ParseAndSemanticAnalysis.parsing_rewriting_and_semantics(input)
      println("...")

      print("initial planning to find out interesting schema")
      val (indexes, tokens) = PreparatoryPlanning.plan(input, maybeBaseState)
      println("...")

      // Passed all steps = let's switch to the new values
      query = input
      baseState = maybeBaseState
      possibleIndexes = indexes
      knownTokens = tokens
      mainMenu()
    } catch {
      case e: RuntimeException =>
        e.printStackTrace()
        enterQuery()
    }
  }

  case class IndexUse(label: String, props: Seq[String], unique: Boolean) {
    override def toString: String = {
      val uniqueS = if (unique) "UNIQUE " else ""
      s"${uniqueS}INDEX ON :$label(${props.mkString(", ")})"
    }
  }

  private def multiLineInput(): String = {
    val builder = new StringBuilder
    val reader = getReader()
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
