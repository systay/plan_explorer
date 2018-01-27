package org.plan_explorer

import org.jline.reader._
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.neo4j.cypher.internal.frontend.v3_3.phases.BaseState
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.kernel.internal.Version

object Main {


  case class IndexUse(label: String, props: Seq[String], unique: Boolean)

  private var db: GraphDatabaseService = _
  private var query: String = _
  private var baseState: BaseState = _
  private var terminal: Terminal = TerminalBuilder.terminal()
  private var possibleIndexes: Set[IndexPossibility] = _
  private var selectedIndexes: Set[IndexUse] = Set.empty

  def getReader(): LineReader = {
    terminal.close()
    terminal = TerminalBuilder.builder().build()
    LineReaderBuilder.builder().terminal(terminal).build()
  }

  def main(args: Array[String]): Unit = {
    registerCtrlCHook()

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

  private def mainMenu(): Action = Menu(
    ("View current schema and statistics", mainMenu),
    ("Load schema and statistics from database", mainMenu),
    ("Edit schema and statistics", mainMenu),
    ("Edit indexes", indexMgmt),
    ("Explore plan space", mainMenu),
    ("Change query", enterQuery),
    ("Quit", () => Quit)
  )

  private def indexMgmt(): Action = {
    selectedIndexes = IndexManagement.pickIndexes(selectedIndexes, possibleIndexes)
    mainMenu()
  }

  private def enterQuery(): Action = {
    println("Please enter query. Single line with . to finish input")
    try {
      val input = if (true)
        "MATCH (a:A:B) WHERE a.prop1 = 42 AND a.prop2 > 43 AND exists(a.prop3) RETURN *"
      else
        multiLineInput()

      print("parsing, ast-rewriting and semantic analysis")
      val maybeBaseState = ParseAndSemanticAnalysis.parsing_rewriting_and_semantics(input)
      println("...")

      print("initial planning to find out interesting schema")
      val result: Set[IndexPossibility] = Planning.plan(input, maybeBaseState)

      // Passed all steps = let's switch to the new values
      query = input
      baseState = maybeBaseState
      possibleIndexes = result
      mainMenu()
    } catch {
      case e: RuntimeException =>
        e.printStackTrace()
        enterQuery()
    }
  }

  private def close(): Unit = {
    if(db != null)
      db.shutdown()
  }

  private def multiLineInput(): String = {
    val builder = new StringBuilder
    val reader = getReader()
    while (true) {
      try {
        reader.readLine("") match {
          case null => return builder.toString()
          case "." => return builder.toString()
          case line => builder.append(line).append(System.lineSeparator())
        }
      } catch {
        case e: UserInterruptException =>
          close()
          throw e
        case e: EndOfFileException =>
          close()
          throw e
      }
    }

    "This never happens"
  }

  private def registerCtrlCHook(): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = close()
    })
  }
}
