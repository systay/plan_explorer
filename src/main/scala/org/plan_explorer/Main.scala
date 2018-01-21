package org.plan_explorer

import java.util

import org.jline.reader._
import org.neo4j.graphdb.GraphDatabaseService
import org.neo4j.kernel.internal.Version

object Main {

  var db: GraphDatabaseService = _

  def main(args: Array[String]): Unit = {
    registerCtrlCHook()

    println(
      s"""Welcome to plan explorer!
         |Using Neo4j ${Version.getNeo4jVersion}
         |*-*-*-*-*-*-*-*-*-*-*-*-*
         |
         |Please enter query. CTRL-D to finish input""".stripMargin)
    val reader = LineReaderBuilder.builder.parser(new Apa).build()
    val query = getMultiLineInput(reader)

    println(query)
  }

  private def getMultiLineInput(reader: LineReader): String = {
    val builder = new StringBuilder
    while (true) {
      try {
        val line = reader.readLine("")
        if (line == null)
          return builder.toString()
        builder.append(line).append(System.lineSeparator())
      } catch {
        case _: UserInterruptException =>
          println("UserInterruptException")
          println("UserInterruptException")
        case _: EndOfFileException =>
          println("ctrl-d")
          return builder.toString()
      }
    }

    "This never happens"
  }

  private def registerCtrlCHook(): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit =
        if (db != null)
          db.shutdown()

    })
  }
}

class Apa extends Parser {
  override def parse(line: String, cursor: Int, context: Parser.ParseContext): ParsedLine = {
    val words: util.List[String] = new util.LinkedList[String]

    new org.jline.reader.impl.DefaultParser.ArgumentList(line, words, 0, 0, 0)
  }
}