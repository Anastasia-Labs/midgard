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
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
} from "../workflow/family-l1-observation.js";
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
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowAction,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import {
  createAuthenticatedRawDatumPreimagePrerequisitePort,
  createChunkedRawDatumPreimageRequirement,
  createStructuredDataPreimageRequirement,
  withRawDatumPreimagePrerequisite,
} from "../workflow/raw-datum-preimage-prerequisite.js";
import { structuredDataPublicationPlan } from "../workflow/structured-data-preimage.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import type { RetainedDaPayloadSource } from "./fetch.js";
import {
  captureTransitionTraceL1Events,
  requireTransitionTraceL1Events,
} from "./l1-events.js";
import { transitionTraceProofChunks } from "./proof-carriage.js";
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
]);
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
  ],
);

type Prepared = Readonly<{
  proof: SDK.TransitionFaultProof;
  artifact: JournalJsonObject;
  references: readonly UTxO[];
  depositPolicyId: string;
}>;
type ReplayCell = {
  evidence?: CanonicalBlockEvidence;
  corpus?: HistoricalNativeScriptCorpus;
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

export const createManifestBoundTransitionTraceWorkflow = async (
  config: ManifestBoundTransitionTraceWorkflowConfig,
) => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "transitionTrace",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: TRANSITION_TRACE_WORKFLOW_DATUM_SCHEMAS,
  });
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const names = TRANSITION_TRACE_WORKFLOW_REFERENCE_CONTRACT_NAMES;
  const references = Object.fromEntries(
    names.map((contractName) => {
      const utxo = config.referenceScripts[contractName];
      if (utxo === undefined)
        throw new Error(
          `Transition workflow omitted published ${contractName}`,
        );
      return [
        contractName,
        requireManifestBoundReferenceScriptUtxo({
          binding,
          contractName,
          utxo,
        }),
      ];
    }),
  );
  const witnesses: FaultProofWitnessReferenceScripts = {
    computationThreadMint: references.computationThreadMint!,
    fraudProofMint: references.fraudProofMint!,
    phasMembershipWithdraw: references.phasMembershipWithdraw!,
  };
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined)
    throw new Error("Transition workflow lacks raw L1 authority");
  const cell: ReplayCell = {};
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
    if (input.stage === "step_01" && (finalIndex === 4 || finalIndex === 5))
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
      additionalReferenceInputs: prepared.references,
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
  const prerequisite = createAuthenticatedRawDatumPreimagePrerequisitePort({
    category: "transitionTrace",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction,
    transactionConfirmed: (input) => l1.transactionConfirmed(input),
  });
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
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
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
            });
          },
        ),
      };
    },
  };
  const workflow = Object.freeze({
    binding,
    l1,
    transactions,
    references,
    witnesses,
    config,
    adapter: withRawDatumPreimagePrerequisite({
      category: "transitionTrace",
      prerequisite,
      base: createCursorFamilyWorkflowAdapter({
        spec: TRANSITION_TRACE_CURSOR_SPEC,
        l1,
        transactions,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
      }),
    }),
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
  cells.set(workflow, cell);
  return workflow;
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
    minimumConfirmationDepth:
      workflow.binding.releaseFinality.policy.confirmationDepth,
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
      proof: detection.proof,
      events: replay.referencesByEvent,
      finalReferences: [
        raw.hub,
        workflow.references[
          TRANSITION_TRACE_FINAL_REFERENCE_SCRIPT_ENTRIES[finalIndex]!
        ]!,
        workflow.references.computationThreadMint!,
        workflow.references.fraudProofMint!,
      ],
    });
    const detectionId = transitionTraceDetectionId(index, detection.kind);
    prepared.set(detectionId, {
      proof: bound.proof,
      references: bound.event === null ? [] : [bound.event.utxo],
      depositPolicyId: raw.depositPolicyId,
      artifact: createTransitionTraceWorkflowArtifact({
        evidence,
        corpus,
        proof: bound.proof,
        detectionId,
        l1Snapshot: replay.l1Snapshot,
        eventOutRef:
          bound.event === null
            ? null
            : `${bound.event.utxo.txHash}#${bound.event.utxo.outputIndex}`,
      }),
    });
  }
  cell.evidence = evidence;
  cell.corpus = corpus;
  cell.prepared = prepared;
  const replayer =
    createTransitionTraceCompleteCanonicalReplayFromRetainedHistory(
      corpus,
      l1Events,
    );
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
