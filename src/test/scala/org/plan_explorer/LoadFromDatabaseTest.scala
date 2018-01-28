package org.plan_explorer

import java.io.File
import java.util.concurrent.TimeUnit

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{DynamicLabel, GraphDatabaseService}
import org.plan_explorer.Main.IndexUse
import org.scalatest.{FunSuite, Matchers}

class LoadFromDatabaseTest extends FunSuite with Matchers {
  test("create database with indexes and load schema & stats from it") {
    val file = File.createTempFile("apa", "affe")
    file.delete()
    file.mkdir()

    createDbWithIndexes(file, "A" -> "prop1", "A" -> "prop2", "B" -> "prop1", "B" -> "prop2")
    val oldTokens = Tokens(Map("A" -> 0, "B" -> 1), Map.empty, Map("prop1" -> 2, "prop2" -> 3))

    val result = LoadFromDatabase.loadFromDatabase(file.getAbsolutePath, "MATCH (a:A:B) WHERE a.prop1 = 42 AND a.prop2 > 43 RETURN *", oldTokens)

    result.indexes should equal(Set(
      IndexUse("A", Seq("prop1"), unique = false),
      IndexUse("A", Seq("prop2"), unique = false),
      IndexUse("B", Seq("prop1"), unique = false),
      IndexUse("B", Seq("prop2"), unique = false)
    ))
  }

  private def createDbWithIndexes(file: File, indexesOn: (String, String)*) = {
    val db = new GraphDatabaseFactory().
      newEmbeddedDatabaseBuilder(file).
      newGraphDatabase()

    inTx(db) {
      val node = db.createNode()
      node.setProperty("prop3", 42)
    }

    inTx(db) {
      indexesOn foreach {
        case (label, property) =>
          DynamicLabel.label(label)
          db.schema().indexFor(DynamicLabel.label(label)).on(property).create()
      }
    }

    inTx(db) {
      println("creating indexes")
      db.schema().awaitIndexesOnline(10, TimeUnit.DAYS)
    }

    db.shutdown()
    println("done")
  }

  def inTx(db: GraphDatabaseService)(action: => Unit): Unit = {
    val tx = db.beginTx()
    action
    tx.success()
    tx.close()
  }
}
