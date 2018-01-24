package org.plan_explorer

import org.jline.reader._
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.kernel.internal.Version

object Main {

  private var db: GraphDatabaseService = _
  private var query: String = _
  private val reader = LineReaderBuilder.builder.build()

  def main(args: Array[String]): Unit = {
    registerCtrlCHook()

    println(
      s"""Welcome to plan explorer!
         |Using Neo4j ${Version.getNeo4jVersion}
         |*-*-*-*-*-*-*-*-*-*-*-*-*
         |
         |Please enter query. CTRL-D to finish input""".stripMargin)

    var current: Action = mainMenu()

    while (current != Quit) {
      current = current.chooseOptionFromReader(reader)
    }
  }

  private def mainMenu(): Action = Menu(
    ("View current schema and statistics", mainMenu),
    ("Load schema and statistics from database", mainMenu),
    ("Edit schema and statistics", mainMenu),
    ("Explore plan space", mainMenu),
    ("Change query", mainMenu),
    ("Quit", () => Quit)
  )

  private def enterQuery(): Action = {
    println("Please enter query. CTRL-D to finish input")

    mainMenu()
  }

  private def registerCtrlCHook(): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit =
        if (db != null)
          db.shutdown()

    })
  }
}
