import {
  FraudProofComputationThreadStepDatum,
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep08Datum,
  MissingNativeScriptUtxoStep02DatumSchema,
  MissingNativeScriptUtxoStep03DatumSchema,
  MissingNativeScriptUtxoStep04DatumSchema,
  MissingNativeScriptUtxoStep05DatumSchema,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  type CompleteCanonicalReplay,
  createMissingNativeScriptUtxoCompleteCanonicalReplay,
  requireCompleteCanonicalReplayDecision,
} from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import type { FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
} from "../workflow/family-definition.js";
import type { FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveHistoricalNativeScriptCorpus,
} from "../workflow/historical-native-script-corpus.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflow,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { admitMissingNativeScriptUtxoArtifact } from "./artifact.js";
import type { MissingNativeScriptUtxoContracts } from "./contracts.js";
import {
  type BoundConfig,
  type MissingNativeScriptUtxoWorkflowReferenceScripts,
  scriptFieldPlan,
  spendFieldPlan,
} from "./workflow.resolve-field.js";
import { transactionPort } from "./workflow.transaction-port.js";
import { MISSING_NATIVE_SCRIPT_UTXO_CURSOR_SPEC } from "./workflow-spec.js";

export type ManifestBoundMissingNativeScriptUtxoWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingNativeScriptUtxoWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingNativeScriptUtxoWorkflow = Readonly<{
  replayer: CompleteCanonicalReplay;
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptUtxo">;
  l1: FraudProofFamilyL1ObservationPort<"missingNativeScriptUtxo">;
  transactions: CursorFamilyTransactionPort<"missingNativeScriptUtxo">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
}>;

type HistoricalCorpusCell = {
  value?: HistoricalNativeScriptCorpus;
};

const historicalCorpusCells = new WeakMap<object, HistoricalCorpusCell>();

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
] as const;

const STEP_CONTRACT_NAMES = [
  "fraudProofMissingNativeScriptUtxo",
  "fraudProofMissingNativeScriptUtxoStep02",
  "fraudProofMissingNativeScriptUtxoStep03",
  "fraudProofMissingNativeScriptUtxoStep04",
  "fraudProofMissingNativeScriptUtxoStep05",
  "fraudProofMissingNativeScriptUtxoStep06",
  "fraudProofMissingNativeScriptUtxoStep07",
] as const;

type BoundContext = FamilyAssemblyContext<
  "missingNativeScriptUtxo",
  (typeof WITNESS_ROLES)[number],
  true,
  7
>;

const bindFamily = (context: BoundContext) => {
  const { binding, references } = context;
  const chain = binding.resolvedContracts.contracts.missingNativeScriptUtxo;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "missing-native-script-utxo manifest omitted required contracts",
    );
  }
  const contracts: MissingNativeScriptUtxoContracts = Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const corpusCell: HistoricalCorpusCell = {};
  const bound: BoundConfig = {
    binding,
    lucid: context.lucid,
    signer: context.signer,
    contracts,
    references,
    historicalCorpus: () => {
      const corpus = corpusCell.value;
      if (corpus === undefined) {
        throw new Error(
          "missing-native-script-utxo history was not derived from this workflow's public DA sources",
        );
      }
      return corpus;
    },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  };
  return { bound, corpusCell };
};

const boundFamilies = new WeakMap<
  BoundContext,
  ReturnType<typeof bindFamily>
>();

const boundFor = (context: BoundContext) => {
  const existing = boundFamilies.get(context);
  if (existing !== undefined) return existing;
  const created = bindFamily(context);
  boundFamilies.set(context, created);
  return created;
};

export const MISSING_NATIVE_SCRIPT_UTXO_FAMILY_DEFINITION = defineFamily<
  "missingNativeScriptUtxo",
  (typeof WITNESS_ROLES)[number],
  true,
  7
>({
  category: "missingNativeScriptUtxo",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    MissingNativeScriptUtxoStep02DatumSchema,
    MissingNativeScriptUtxoStep03DatumSchema,
    MissingNativeScriptUtxoStep04DatumSchema,
    MissingNativeScriptUtxoStep05DatumSchema,
    MissingNativeScriptTxStep07Datum,
    MissingNativeScriptTxStep08Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: (context) =>
    createMissingNativeScriptUtxoCompleteCanonicalReplay(() =>
      boundFor(context).bound.historicalCorpus(),
    ),
  adapter: {
    kind: "cursor",
    spec: MISSING_NATIVE_SCRIPT_UTXO_CURSOR_SPEC,
    stepContractNames: STEP_CONTRACT_NAMES,
    transactionPort: (context) => transactionPort(boundFor(context).bound),
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const { certificate, references } = context;
        const admitted = admitMissingNativeScriptUtxoArtifact(artifact);
        const planned =
          action.input.stage === "step_02"
            ? spendFieldPlan(admitted, context.signer.paymentKeyHash)
            : typeof action.input.stage === "string" &&
                ["step_05", "step_06", "step_07"].includes(action.input.stage)
              ? scriptFieldPlan(admitted, context.signer.paymentKeyHash)
              : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor: admitted.prepared.nativeTxCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: (_context, { action, artifact }) => {
    const admitted = admitMissingNativeScriptUtxoArtifact(artifact);
    return action.input.stage === "step_01"
      ? admitted.prepared.txInclusion.txMembershipProofCbor
      : action.input.stage === "step_03"
        ? admitted.prepared.membershipProofCbor
        : null;
  },
  extend: (context) => ({ historicalCorpusCell: boundFor(context).corpusCell }),
});

export const createManifestBoundMissingNativeScriptUtxoWorkflow = async (
  config: ManifestBoundMissingNativeScriptUtxoWorkflowConfig,
): Promise<ManifestBoundMissingNativeScriptUtxoWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    MISSING_NATIVE_SCRIPT_UTXO_FAMILY_DEFINITION,
    config,
  );
  const { binding } = deployment;
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: config.historicalNativeScriptCheckpointStore,
    historySource: config.historicalNativeScriptHistorySource,
  });
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    MISSING_NATIVE_SCRIPT_UTXO_FAMILY_DEFINITION,
    deployment,
  );
  const workflow = Object.freeze({
    ...assembled,
    transactions:
      assembled.transactions as CursorFamilyTransactionPort<"missingNativeScriptUtxo">,
    historicalNativeScriptCheckpointStore:
      config.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      config.historicalNativeScriptHistorySource,
  });
  historicalCorpusCells.set(
    workflow,
    (
      assembled as typeof assembled & {
        historicalCorpusCell: HistoricalCorpusCell;
      }
    ).historicalCorpusCell,
  );
  return workflow;
};

export const runOrResumeManifestBoundMissingNativeScriptUtxoWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingNativeScriptUtxoWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
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
      "missing-native-script-utxo workflow was not created by the manifest-bound constructor",
    );
  }
  if (
    cell.value !== undefined &&
    cell.value.corpusDigest !== corpus.corpusDigest
  ) {
    throw new Error(
      "missing-native-script-utxo authenticated history changed across resume",
    );
  }
  cell.value = corpus;
  const replayer = workflow.replayer;
  const decision = await replayer.replay(evidence);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    evidence,
    detections,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["missingNativeScriptUtxo"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
