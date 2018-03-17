package org.plan_explorer

import org.neo4j.kernel.internal.Version

object Main {

  def main(args: Array[String]): Unit = {
    println(
      s"""Welcome to plan explorer!
         |verbose ${java.lang.Boolean.getBoolean("pickBestPlan.VERBOSE")}

         |Using Neo4j ${Version.getNeo4jVersion}
         |*-*-*-*-*-*-*-*-*-*-*-*-*
         |""".stripMargin)

    val mainMenu = new terminal.MainMenu
    mainMenu.mainLoop()
  }
}
