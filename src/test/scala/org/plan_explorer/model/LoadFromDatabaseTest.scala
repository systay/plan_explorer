package org.plan_explorer.model

import java.io.File
import java.util.concurrent.TimeUnit

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{DynamicLabel, DynamicRelationshipType, GraphDatabaseService, Node}
import org.scalatest.{FunSuite, Matchers}

import scala.util.Random

class LoadFromDatabaseTest extends FunSuite with Matchers {
  test("create database with indexes and load schema & stats from it") {
    val file = File.createTempFile("apa", "affe")
    file.delete()
    file.mkdir()

    createDbWithIndexes(file, "A" -> "prop1", "A" -> "prop2", "B" -> "prop1", "B" -> "prop2")

    val query = "MATCH (a:A)-[:T]->(b:B) WHERE a.prop1 = 42 AND a.prop2 > 43 RETURN *"
    val baseState = ParseAndSemanticAnalysis.parsing_rewriting_and_semantics(query)
    val (_, oldTokens, stats) = PreparatoryPlanning.plan(query, baseState)
    val result = LoadFromDatabase.loadFromDatabase(file.getAbsolutePath, query, oldTokens, stats)

    val namedIndexes = result.indexes.map {
      case IndexUse(labelId, propIds, unique) =>
        (result.tokens.reverseLabels(labelId), propIds.map(result.tokens.reverseProps), unique)
    }

    namedIndexes should equal(Set(
      ("A", Seq("prop1"), false),
      ("A", Seq("prop2"), false),
      ("B", Seq("prop1"), false),
      ("B", Seq("prop2"), false)
    ))

    println(result.statistics.toString(result.tokens))

    result.statistics.allNodes.amount should equal(1001)
  }

  private def createDbWithIndexes(file: File, indexesOn: (String, String)*) = {
    val db = new GraphDatabaseFactory().
      newEmbeddedDatabaseBuilder(file).
      newGraphDatabase()

    inTx(db) {
      println("creating data")
      val r = new Random()
      val aLabel = DynamicLabel.label("A")
      val bLabel = DynamicLabel.label("B")
      val nodes: Array[Node] = ((0 to 1000) map { _ =>
        val node = db.createNode()
        if (r.nextBoolean())
          node.addLabel(aLabel)

        if (r.nextBoolean())
          node.addLabel(bLabel)

        node
      }).toArray

      val t = DynamicRelationshipType.withName("T")

      (0 to 1000) foreach { _ =>

        val from = nodes(r.nextInt(1000))
        val to = nodes(r.nextInt(1000))
        from.createRelationshipTo(to, t)

      }
    }
    println("done")

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
