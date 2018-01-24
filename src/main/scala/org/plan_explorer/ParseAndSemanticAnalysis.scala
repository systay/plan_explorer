package org.plan_explorer

import java.time.Clock

import org.neo4j.cypher.internal.compatibility.v3_3.WrappedMonitors
import org.neo4j.cypher.internal.compatibility.v3_3.runtime.CommunityRuntimeContextCreator
import org.neo4j.cypher.internal.compiler.v3_3.{CypherCompilerConfiguration, CypherCompilerFactory}
import org.neo4j.cypher.internal.frontend.v3_3.helpers.rewriting.RewriterStepSequencer
import org.neo4j.cypher.internal.frontend.v3_3.phases.CompilationPhaseTracer.NO_TRACING
import org.neo4j.cypher.internal.frontend.v3_3.phases.{BaseState, devNullLogger}
import org.neo4j.kernel.monitoring.Monitors

object ParseAndSemanticAnalysis {
  def parsing_rewriting_and_semantics(query: String): BaseState = {
    val config = CypherCompilerConfiguration(
      queryCacheSize = 0,
      statsDivergenceCalculator = null,
      useErrorsOverWarnings = true,
      idpMaxTableSize = 100,
      idpIterationDuration = 100000,
      errorIfShortestPathFallbackUsedAtRuntime = false,
      errorIfShortestPathHasCommonNodesAtRuntime = false,
      legacyCsvQuoteEscaping = false,
      nonIndexedLabelWarningThreshold = 1000)

    val compiler = new CypherCompilerFactory().costBasedCompiler(
      config,
      Clock.systemDefaultZone(),
      WrappedMonitors(new Monitors),
      RewriterStepSequencer.newPlain,
      None,
      None,
      CommunityRuntimeContextCreator)

    compiler.parseQuery(query, query, devNullLogger, "COST", Set.empty, None, NO_TRACING)
  }
}

