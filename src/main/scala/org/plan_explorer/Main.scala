package org.plan_explorer

import org.jline.reader._
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.kernel.internal.Version

object Main {

  private var db: GraphDatabaseService = _
  private var query: String = _
  private var terminal: Terminal = TerminalBuilder.terminal()

  private def getReader(): LineReader = {
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
    ("Explore plan space", mainMenu),
    ("Change query", enterQuery),
    ("Quit", () => Quit)
  )

  private def enterQuery(): Action = {
    println("Please enter query. Single line with . to finish input")
    query = getMultiLineInput()
    mainMenu()
  }

  private def close(): Unit = {
    if(db != null)
      db.shutdown()
  }

  private def getMultiLineInput(): String = {
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
