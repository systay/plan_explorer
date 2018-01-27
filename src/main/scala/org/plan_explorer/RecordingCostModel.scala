package org.plan_explorer

import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.Metrics
import org.neo4j.cypher.internal.compiler.v3_3.planner.logical.Metrics.CostModel
import org.neo4j.cypher.internal.frontend.v3_3.Foldable._
import org.neo4j.cypher.internal.frontend.v3_3.ast.LabelToken
import org.neo4j.cypher.internal.ir.v3_3.Cost
import org.neo4j.cypher.internal.v3_3.logical.plans._

import scala.collection.mutable

class RecordingCostModel(inner: CostModel) extends CostModel {


  val interestingIndexes = new mutable.HashSet[IndexPossibility]()

  override def apply(plan: LogicalPlan, queryGraphInput: Metrics.QueryGraphSolverInput): Cost = {

    plan.findByAllClass[NodeLogicalLeafPlan].foreach {
      case NodeUniqueIndexSeek(_, LabelToken(label, _), props, _, _) =>
        interestingIndexes.add(IndexPossibility(label, props.map(_.name)))
      case NodeIndexSeek(_, LabelToken(label, _), props, _, _) =>
        interestingIndexes.add(IndexPossibility(label, props.map(_.name)))
      case NodeIndexScan(_, LabelToken(label, _), props, _) =>
        interestingIndexes.add(IndexPossibility(label, Seq(props.name)))
      case NodeIndexContainsScan(_, LabelToken(label, _), props, _, _) =>
        interestingIndexes.add(IndexPossibility(label, Seq(props.name)))
      case NodeIndexEndsWithScan(_, LabelToken(label, _), props, _, _) =>
        interestingIndexes.add(IndexPossibility(label, Seq(props.name)))
      case _ =>
    }

    inner.apply(plan, queryGraphInput)
  }
}

case class IndexPossibility(label: String, props: Seq[String]) {
  override def toString: String = s"(:$label {${props.mkString(",")}})"
}
