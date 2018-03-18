package org.plan_explorer.model

import java.time.Clock

import org.neo4j.cypher.internal.compatibility.v3_3.WrappedMonitors
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.CommunityRuntimeContextCreator
import org.neo4j.cypher.internal.compiler.v3_3.phases.LogicalPlanState
import org.neo4j.cypher.internal.frontend.v3_3.phases.CompilationPhaseTracer.NO_TRACING
import org.neo4j.cypher.internal.frontend.v3_3.phases.{CompilationPhases, devNullLogger}
import org.neo4j.kernel.monitoring.{Monitors => KernelMonitors}

object ModelBuilder {
  def prepareForQuery(query: String) = {
    val maybeBaseState = ParseAndSemanticAnalysis.parsing_rewriting_and_semantics(query)
    val monitors = WrappedMonitors(new KernelMonitors)
    val context = CommunityRuntimeContextCreator.create(NO_TRACING, devNullLogger, null, query, Set.empty, None, monitors, new MyMetricsFactory, null, null, null, Clock.systemUTC(), evaluator = null)

    val finalBaseState = CompilationPhases.
      lateAstRewriting.
      transform(maybeBaseState, context).asInstanceOf[LogicalPlanState].
      copy(maybePeriodicCommit = Some(None), queryText = query)

    val (indexes, tokens, recordedStats) = PreparatoryPlanning.plan(query, finalBaseState)
    (indexes, tokens, recordedStats, finalBaseState)
  }
}
