package org.plan_explorer.model

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
      case NodeUniqueIndexSeek(_, LabelToken(_, id), props, _, _) =>
        interestingIndexes.add(IndexPossibility(id, props.map(_.nameId)))
      case NodeIndexSeek(_, LabelToken(_, id), props, _, _) =>
        interestingIndexes.add(IndexPossibility(id, props.map(_.nameId)))
      case NodeIndexScan(_, LabelToken(_, id), props, _) =>
        interestingIndexes.add(IndexPossibility(id, Seq(props.nameId)))
      case NodeIndexContainsScan(_, LabelToken(_, id), props, _, _) =>
        interestingIndexes.add(IndexPossibility(id, Seq(props.nameId)))
      case NodeIndexEndsWithScan(_, LabelToken(_, id), props, _, _) =>
        interestingIndexes.add(IndexPossibility(id, Seq(props.nameId)))
      case _ =>
    }

    inner.apply(plan, queryGraphInput)
  }
}

