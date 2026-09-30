import { type UTxO } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  createTransitionTraceCompleteCanonicalReplayFromRetainedHistory,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay.js";
import {
  defineFamily,
  type FamilyReferenceScripts,
} from "../workflow/family-definition.js";
import { resolveHistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import type { RetainedDaPayloadSource } from "./fetch.js";
import {
  captureTransitionTraceL1Events,
  requireTransitionTraceL1Events,
} from "./l1-events.js";
import { makeTransitionProofMaterial } from "./proof-material.js";
import {
  replayTransitionTraceFromRetainedHistory,
  transitionTraceDetectionId,
} from "./replay-authority.js";
import {
  TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
  transitionTraceFinalIndex,
} from "./submit.js";
import { runFor } from "./workflow.bind-run.js";
import {
  cells,
  type ManifestBoundTransitionTraceWorkflowConfig,
  type Prepared,
  type ReplayCell,
  TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
  type TransitionRuntime,
} from "./workflow.manifest-bound-transition-trace-workflow-config.js";
import { createTransitionTraceWorkflowArtifact } from "./workflow-artifact.js";
import { bindTransitionTraceProofEvent } from "./workflow-proof.js";
import { TRANSITION_TRACE_CURSOR_SPEC } from "./workflow-spec.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

export const TRANSITION_TRACE_FAMILY_DEFINITION = defineFamily<
  "transitionTrace",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  false,
  9,
  TransitionRuntime
>({
  category: "transitionTrace",
  stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
  ],
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: {
    ...Object.fromEntries(
      Object.values(TRANSITION_TRACE_YIELD_REFERENCES).map(({ entry }) => [
        entry,
        entry,
      ]),
    ),
    stateQueueSpend: "stateQueueSpend",
  },
  replayer: (context) =>
    createTransitionTraceCompleteCanonicalReplayFromRetainedHistory(
      () => {
        const corpus = context.runtime.cell.corpus;
        if (corpus === undefined)
          throw new Error(
            "Transition replay requires freshly derived retained history",
          );
        return corpus;
      },
      () => {
        const events = context.runtime.cell.l1Events;
        if (events === undefined)
          throw new Error(
            "Transition replay requires freshly authenticated L1 events",
          );
        return events;
      },
    ),
  adapter: {
    kind: "cursor",
    spec: TRANSITION_TRACE_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofTransitionTrace",
      ...TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
    ],
    createRefineAction: (context) => runFor(context).refineAction,
    transactionPort: (context) => runFor(context).transactions,
  },
  fieldCarriage: [
    {
      rawDatum: true,
      requirementForAction: (context, input) =>
        runFor(context).requirementForAction(input),
    },
  ],
  extend: (context) => ({
    references: runFor(context).references,
    witnesses: runFor(context).witnesses,
    config: context.runtime.config,
  }),
});

export const createManifestBoundTransitionTraceWorkflow = async (
  config: ManifestBoundTransitionTraceWorkflowConfig,
) => {
  const cell: ReplayCell = {};
  const steps = [
    "fraudProofTransitionTrace",
    ...TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
  ].map((name) => config.referenceScripts[name]!);
  const workflow = await assembleManifestBoundFamilyWorkflow(
    TRANSITION_TRACE_FAMILY_DEFINITION,
    {
      ...config,
      referenceScripts: {
        steps: steps as unknown as FamilyReferenceScripts<
          "transitionTrace",
          "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
          false,
          9
        >["steps"],
        witnesses: {
          computationThreadMint: config.referenceScripts.computationThreadMint!,
          fraudProofMint: config.referenceScripts.fraudProofMint!,
          phasMembershipWithdraw:
            config.referenceScripts.phasMembershipWithdraw!,
        },
      },
      auxiliaryReferenceScripts: config.referenceScripts,
    },
    { config, cell },
  );
  cells.set(workflow, cell);
  return workflow as typeof workflow & {
    readonly references: Readonly<Record<string, UTxO>>;
    readonly witnesses: FaultProofWitnessReferenceScripts;
    readonly config: ManifestBoundTransitionTraceWorkflowConfig;
  };
};

export type ManifestBoundTransitionTraceWorkflow = Awaited<
  ReturnType<typeof createManifestBoundTransitionTraceWorkflow>
>;

export const runOrResumeManifestBoundTransitionTraceWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundTransitionTraceWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  const cell = cells.get(workflow);
  if (cell === undefined)
    throw new Error(
      "Transition workflow was not created by its manifest-bound constructor",
    );
  if (workflow.l1.observeRetainedHeader === undefined)
    throw new Error(
      "Transition recovery requires authenticated retained header history",
    );
  const observation = await workflow.l1.observeRetainedHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  const evidence = await fetchCanonicalBlockEvidence({
    observation,
    sources,
    minimumConfirmationDepth: 1,
  });
  const corpus = await resolveHistoricalNativeScriptCorpus({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    checkpointStore: workflow.config.historicalNativeScriptCheckpointStore,
    historySource: workflow.config.historicalNativeScriptHistorySource,
    currentEvidence: evidence,
    sources,
  });
  if (
    cell.corpus !== undefined &&
    cell.corpus.corpusDigest !== corpus.corpusDigest
  )
    throw new Error("Transition authenticated history changed across resume");
  const l1Events = await captureTransitionTraceL1Events({
    binding: workflow.binding,
    authority: workflow.l1.rawL1!,
  });
  const replay = await replayTransitionTraceFromRetainedHistory({
    evidence,
    corpus,
    l1Events,
  });
  const raw = requireTransitionTraceL1Events(l1Events);
  const prepared = new Map<string, Prepared>();
  for (const [index, detection] of replay.detections.entries()) {
    if (!detection.buildable)
      throw new Error("Transition replay finding is not buildable");
    const finalIndex = transitionTraceFinalIndex(detection.proof);
    const bound = bindTransitionTraceProofEvent({
      proof: makeTransitionProofMaterial(
        evidence.reconstruction,
        detection.proof,
      ),
      events: replay.referencesByEvent,
    });
    const detectionId = transitionTraceDetectionId(index, detection.kind);
    const depositOpening =
      finalIndex === 5 && bound.event?.kind === "deposit"
        ? bound.event.history
        : null;
    prepared.set(detectionId, {
      proof: bound.proof,
      references: bound.event === null ? [] : [bound.event.utxo],
      depositPolicyId: raw.depositPolicyId,
      ...(depositOpening === null ? {} : { depositOpening }),
      artifact: createTransitionTraceWorkflowArtifact({
        evidence,
        corpus,
        proof: bound.proof,
        detectionId,
        l1Snapshot: replay.l1Snapshot,
        depositOpening,
        eventOutRef:
          bound.event === null ||
          (finalIndex === 6 && bound.event.kind !== "forcedTransaction")
            ? null
            : `${bound.event.utxo.txHash}#${bound.event.utxo.outputIndex}`,
      }),
    });
  }
  cell.evidence = evidence;
  cell.corpus = corpus;
  cell.l1Events = l1Events;
  cell.prepared = prepared;
  const replayer = workflow.replayer;
  const decision = await replayer.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
  });
  return runFraudProofWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["transitionTrace"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
