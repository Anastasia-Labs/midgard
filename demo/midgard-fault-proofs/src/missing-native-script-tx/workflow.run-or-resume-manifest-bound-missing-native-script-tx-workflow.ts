import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { resolveHistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import {
  historicalCorpusCells,
  type ManifestBoundMissingNativeScriptTxWorkflow,
} from "./workflow.missing-native-script-tx-family-definition.js";

export const runOrResumeManifestBoundMissingNativeScriptTxWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingNativeScriptTxWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  const throughPoint = await workflow.l1.observeBoundary?.({
    headerHash: workflow.binding.definition.headerHash,
  });
  if (throughPoint === undefined) {
    throw new Error(
      "missing-native-script-tx raw L1 boundary authority disappeared",
    );
  }
  const evidence = await fetchCanonicalBlockEvidence({
    observation,
    sources,
    minimumConfirmationDepth: 1,
  });
  const corpus = await resolveHistoricalNativeScriptCorpus({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    checkpointStore: workflow.historicalNativeScriptCheckpointStore,
    historySource: workflow.historicalNativeScriptHistorySource,
    currentEvidence: evidence,
    sources,
  });
  const cell = historicalCorpusCells.get(workflow);
  if (cell === undefined) {
    throw new Error(
      "missing-native-script-tx workflow was not created by its manifest-bound constructor",
    );
  }
  cell.throughPoint = throughPoint;
  if (
    cell.value !== undefined &&
    cell.value.corpusDigest !== corpus.corpusDigest
  ) {
    throw new Error(
      "missing-native-script-tx authenticated history changed across resume",
    );
  }
  cell.value = corpus;
  const decision =
    await MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer: MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY,
    decision,
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["missingNativeScriptTx"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
