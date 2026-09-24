import { decodeMidgardTxOutput } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  createTransitionTraceCompleteCanonicalReplayFromRetainedHistory,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
  type FamilyReferenceScripts,
} from "../workflow/family-definition.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import type {
  FraudProofWorkflowJournalStore,
  JournalJsonObject,
} from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowAction,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import {
  createChunkedRawDatumPreimageRequirement,
  createStructuredDataPreimageRequirement,
} from "../workflow/raw-datum-preimage-prerequisite.js";
import { structuredDataPublicationPlan } from "../workflow/structured-data-preimage.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import type { RetainedDaPayloadSource } from "./fetch.js";
import { type TransitionDepositOpening } from "./history-opening.js";
import {
  captureTransitionTraceL1Events,
  requireTransitionTraceL1Events,
} from "./l1-events.js";
import {
  transitionTraceProofChunks,
  transitionTraceTimedProofNeedsChunks,
} from "./proof-carriage.js";
import {
  makeTransitionProofMaterial,
  type TransitionProofInput,
} from "./proof-material.js";
import {
  replayTransitionTraceFromRetainedHistory,
  transitionTraceDetectionId,
} from "./replay-authority.js";
import {
  submitTransitionTraceFinal,
  submitTransitionTraceRoute,
  TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
  transitionTraceFinalIndex,
} from "./submit.js";
import {
  createTransitionTraceWorkflowArtifact,
  requireTransitionTraceWorkflowArtifact,
} from "./workflow-artifact.js";
import { transitionDepositCheckpointRequiresQueueLease } from "./workflow-checkpoint.js";
import { bindTransitionTraceProofEvent } from "./workflow-proof.js";
import { TRANSITION_TRACE_CURSOR_SPEC } from "./workflow-spec.js";
import { transitionTraceYieldData } from "./yield-data.js";
import { TRANSITION_TRACE_YIELD_REFERENCES } from "./yield-references.js";

export const TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS = Object.freeze([
  SDK.FraudProofComputationThreadStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceProofCommitmentDatum,
  SDK.TransitionTraceProofCommitmentDatum,
  SDK.TransitionTraceStepDatum,
  SDK.TransitionTraceStepDatum,
] as const);
export const TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES = Object.freeze(
  [
    "fraudProofTransitionTrace",
    ...TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
    ...Object.values(TRANSITION_TRACE_YIELD_REFERENCES).map(
      (entry) => entry.entry,
    ),
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "stateQueueSpend",
  ],
);

type Prepared = Readonly<{
  proof: TransitionProofInput;
  artifact: JournalJsonObject;
  references: readonly UTxO[];
  depositPolicyId: string;
  depositOpening?: TransitionDepositOpening;
}>;
type ReplayCell = {
  evidence?: CanonicalBlockEvidence;
  corpus?: HistoricalNativeScriptCorpus;
  l1Events?: Awaited<ReturnType<typeof captureTransitionTraceL1Events>>;
  prepared?: ReadonlyMap<string, Prepared>;
};
const cells = new WeakMap<object, ReplayCell>();

export type ManifestBoundTransitionTraceWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  /** Every family step/yield and shared mint witness is published before Init. */
  referenceScripts: Readonly<Record<string, UTxO>>;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

type TransitionRuntime = Readonly<{
  config: ManifestBoundTransitionTraceWorkflowConfig;
  cell: ReplayCell;
}>;
type TransitionContext = FamilyAssemblyContext<
  "transitionTrace",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  false,
  9,
  TransitionRuntime
>;
const boundRuns = new WeakMap<TransitionContext, ReturnType<typeof bindRun>>();
const bindRun = (context: TransitionContext) => {
  const { binding } = context;
  const { config, cell } = context.runtime;
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  const references = Object.freeze({
    ...context.auxiliaryReferences,
    ...context.references.witnesses,
    ...Object.fromEntries(
      [
        "fraudProofTransitionTrace",
        ...TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES,
      ].map((name, index) => [name, context.references.steps[index]!]),
    ),
  });
  const witnesses: FaultProofWitnessReferenceScripts =
    context.references.witnesses;
  const prerequisite = context.fieldCarriagePrerequisites[0]!;
  const admit = (artifact: JournalJsonObject): Prepared => {
    if (typeof artifact.detectionId !== "string")
      throw new Error("Transition artifact has no detection identity");
    const fresh = cell.prepared?.get(artifact.detectionId);
    if (fresh === undefined)
      throw new Error(
        "Transition artifact has not been freshly derived from retained history",
      );
    requireTransitionTraceWorkflowArtifact(artifact, fresh.artifact);
    return fresh;
  };
  const refineAction = async ({
    action,
    artifact,
  }: {
    action: FraudProofWorkflowAction;
    artifact: JournalJsonObject;
  }): Promise<JournalJsonObject> => {
    if (action.input.stage === "step_08") {
      const prepared = admit(artifact);
      if (transitionTraceFinalIndex(prepared.proof) !== 6)
        throw new Error(
          "Timed transition mutation action selected another proof route",
        );
      return { requiresMutationLease: true };
    }
    if (action.input.stage !== "step_07") return {};
    const prepared = admit(artifact);
    const input = cursorFamilyActionInput({
      category: "transitionTrace",
      action,
    });
    const thread = await fetchUtxoByOutRef({
      label: "transition deposit mutation checkpoint",
      lucid: config.lucid,
      outRef: parseOutRef(
        cursorStringField(input, "threadOutRef"),
        "transition deposit checkpoint",
      ),
    });
    const requiresMutationLease = transitionDepositCheckpointRequiresQueueLease(
      {
        thread,
        address: binding.definition.computationThread.steps[6]!.address,
        unit:
          binding.definition.computationThread.policyId +
          binding.definition.categoryId +
          binding.definition.headerHash,
        prover: config.signer.paymentKeyHash,
        proofHash: transitionTraceProofChunks(prepared.proof).hash,
      },
    );
    return { requiresMutationLease };
  };
  const requirementForAction = async ({
    action,
    artifact,
  }: {
    action: FraudProofWorkflowAction;
    artifact: JournalJsonObject;
  }) => {
    const prepared = admit(artifact);
    const input = cursorFamilyActionInput({
      category: "transitionTrace",
      action,
    });
    const finalIndex = transitionTraceFinalIndex(prepared.proof);
    if (
      input.stage === "step_01" &&
      (finalIndex === 4 ||
        finalIndex === 5 ||
        transitionTraceTimedProofNeedsChunks(prepared.proof))
    )
      return createChunkedRawDatumPreimageRequirement({
        preimage: Buffer.from(
          transitionTraceProofChunks(prepared.proof).chunks.join(""),
          "hex",
        ),
      });
    if (input.stage !== "step_06" && input.stage !== "step_07") return null;
    const thread = await fetchUtxoByOutRef({
      label: "transition workflow checkpoint",
      lucid: config.lucid,
      outRef: parseOutRef(
        cursorStringField(input, "threadOutRef"),
        "transition workflow checkpoint",
      ),
    });
    if (thread.datum == null)
      throw new Error("Transition checkpoint omitted its datum");
    const datum = Data.from(
      thread.datum,
      SDK.TransitionTraceProofCommitmentDatum,
    );
    const state = datum.data;
    if (
      state === null ||
      datum.fraud_prover !== config.signer.paymentKeyHash ||
      state.proof_commitment.hash !==
        transitionTraceProofChunks(prepared.proof).hash
    )
      throw new Error(
        "Transition prerequisite checkpoint differs from admitted proof",
      );
    const outputs = transitionTraceYieldData({
      proof: prepared.proof,
      network: binding.network,
      depositPolicyId: prepared.depositPolicyId,
      depositOpening: prepared.depositOpening,
    }).find((item) => item.outputCbors !== undefined)?.outputCbors;
    const output = outputs?.[Number(state.output_index)];
    if (output === undefined) return null;
    if (state.kind === 0n && state.phase === 8n) {
      const datum = decodeMidgardTxOutput(Buffer.from(output, "hex")).datum;
      if (datum?.kind !== "inline") return null;
      const bytes = Buffer.from(datum.cbor).toString("hex");
      return structuredDataPublicationPlan(bytes).publicationDatums.length === 0
        ? null
        : createStructuredDataPreimageRequirement({ preimageHex: bytes });
    }
    return createChunkedRawDatumPreimageRequirement({
      preimage: Buffer.from(output, "hex"),
    });
  };
  const transactions: CursorFamilyTransactionPort<"transitionTrace"> = {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: "transitionTrace",
    prepare: async ({ evidence, classification }) => {
      const prepared = cell.prepared?.get(classification.selected.detectionId);
      if (
        cell.evidence !== evidence ||
        prepared === undefined ||
        classification.headerHash !== evidence.headerHash
      )
        throw new Error(
          "Transition classification differs from the freshly admitted replay",
        );
      return prepared.artifact;
    },
    capture: async ({ action, artifact }) => {
      const prepared = admit(artifact);
      const input = cursorFamilyActionInput({
        category: "transitionTrace",
        action,
      });
      if (input.stage === "remove")
        return captureCursorRemoval({
          category: "transitionTrace",
          lucid: config.lucid,
          blueprint: binding.blueprint,
          deploymentInfo: binding.deploymentInfo,
          network: binding.network,
          signer: config.signer,
          headerHash: binding.definition.headerHash,
          input,
          stateQueueMutationLeaseCoordinator:
            config.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      if ((await requirementForAction({ action, artifact })) !== null)
        await prerequisite.resolveAuthenticated({
          headerHash: binding.definition.headerHash,
          action,
          artifact,
        });
      const fields = await refineAction({ action, artifact });
      if (fields.requiresMutationLease !== input.requiresMutationLease)
        throw new Error(
          "Transition checkpoint queue mutation changed before capture",
        );
      const mutationLease =
        fields.requiresMutationLease === true
          ? await config.stateQueueMutationLeaseCoordinator.acquire()
          : undefined;
      try {
        await mutationLease?.renew();
        const transaction = await captureLocallyEvaluatedTransaction(
          async (boundary) => {
            const preSubmitBoundary: typeof boundary = async (built) => {
              await mutationLease?.renew();
              await boundary(built);
            };
            const common = {
              lucid: config.lucid,
              blueprint: binding.blueprint,
              deploymentInfo: binding.deploymentInfo,
              network: binding.network,
              signer: config.signer,
              witnessReferenceScripts: witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            };
            if (input.stage === "init") {
              await submitInit({
                ...common,
                fraudCategory: "transitionTrace",
                fraudulentBlockOutRef: cursorStringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                fraudulentHeaderHash: binding.definition.headerHash,
              });
              return;
            }
            const threadOutRef = cursorStringField(input, "threadOutRef");
            if (input.stage === "step_01") {
              await submitTransitionTraceRoute({
                ...common,
                threadOutRef,
                proof: prepared.proof,
              });
              return;
            }
            const expected = `step_${String(transitionTraceFinalIndex(prepared.proof) + 2).padStart(2, "0")}`;
            if (input.stage !== expected)
              throw new Error(
                "Transition workflow cursor selected a different final",
              );
            await submitTransitionTraceFinal({
              ...common,
              threadOutRef,
              proof: prepared.proof,
              additionalReferenceInputs: prepared.references,
              depositOpening: prepared.depositOpening,
            });
          },
        );
        return {
          transaction,
          ...(mutationLease === undefined ? {} : { mutationLease }),
        };
      } catch (error) {
        await mutationLease?.fail(
          `Terminal transition proof capture failed: ${String(error)}`,
        );
        throw error;
      }
    },
  };

  return {
    transactions,
    requirementForAction,
    references,
    witnesses,
    refineAction,
  };
};
const runFor = (context: TransitionContext) => {
  const existing = boundRuns.get(context);
  if (existing !== undefined) return existing;
  const run = bindRun(context);
  boundRuns.set(context, run);
  return run;
};
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
