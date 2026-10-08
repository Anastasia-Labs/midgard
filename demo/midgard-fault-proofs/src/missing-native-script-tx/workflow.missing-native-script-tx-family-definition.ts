import {
  FraudProofComputationThreadStepDatum,
  MissingNativeScriptTxStep02Datum,
  MissingNativeScriptTxStep03Datum,
  MissingNativeScriptTxStep04Datum,
  MissingNativeScriptTxStep05Datum,
  MissingNativeScriptTxStep06Datum,
  MissingNativeScriptTxStep07Datum,
  MissingNativeScriptTxStep08Datum,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
} from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptCorpus,
  type HistoricalNativeScriptHistorySource,
  requireHistoricalNativeScriptHistoryAuthority,
} from "../workflow/historical-native-script-corpus.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofRawL1Point } from "../workflow/raw-l1-snapshot.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { admitMissingNativeScriptTxArtifact } from "./artifact.js";
import type { MissingNativeScriptTxContracts } from "./contracts.js";
import {
  type HistoricalNativeScriptSourceRoster,
  requireHistoricalNativeScriptSourceRoster,
} from "./historical-script.js";
import {
  type BoundConfig,
  type MissingNativeScriptTxWorkflowReferenceScripts,
  outputFieldPlan,
  scriptFieldPlan,
  spendFieldPlan,
} from "./workflow.resolve-field.js";
import { transactionPort } from "./workflow.transaction-port.js";

export type ManifestBoundMissingNativeScriptTxWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: MissingNativeScriptTxWorkflowReferenceScripts;
  l1Source: FraudProofL1Source;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
  historicalNativeScriptL1Roster: HistoricalNativeScriptSourceRoster;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundMissingNativeScriptTxWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"missingNativeScriptTx">;
  l1: FraudProofFamilyL1ObservationPort<"missingNativeScriptTx">;
  transactions: CursorFamilyTransactionPort<"missingNativeScriptTx">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  historicalNativeScriptCheckpointStore: HistoricalNativeScriptCheckpointStore;
  historicalNativeScriptHistorySource: HistoricalNativeScriptHistorySource;
}>;

type HistoricalCorpusCell = {
  value?: HistoricalNativeScriptCorpus;
  throughPoint?: FraudProofRawL1Point;
};

export const historicalCorpusCells = new WeakMap<
  object,
  HistoricalCorpusCell
>();

type HistoricalRuntime = Pick<
  ManifestBoundMissingNativeScriptTxWorkflowConfig,
  | "historicalNativeScriptCheckpointStore"
  | "historicalNativeScriptHistorySource"
  | "historicalNativeScriptL1Roster"
> & { readonly corpusCell: HistoricalCorpusCell };

type AssemblyContext = FamilyAssemblyContext<
  "missingNativeScriptTx",
  keyof FaultProofWitnessReferenceScripts,
  true,
  8,
  HistoricalRuntime
>;

const boundConfigs = new WeakMap<AssemblyContext, BoundConfig>();

const boundFor = (context: AssemblyContext): BoundConfig => {
  const existing = boundConfigs.get(context);
  if (existing !== undefined) return existing;
  const { binding, certificate } = context;
  const { corpusCell } = context.runtime;
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint: binding.deploymentFingerprint,
    checkpointStore: context.runtime.historicalNativeScriptCheckpointStore,
    historySource: context.runtime.historicalNativeScriptHistorySource,
  });
  requireHistoricalNativeScriptSourceRoster(
    context.runtime.historicalNativeScriptL1Roster,
    binding.releaseFinality,
  );
  if (
    context.runtime.historicalNativeScriptHistorySource.providerRosterDigest !==
    context.runtime.historicalNativeScriptL1Roster.applicationOverlayDigest
  ) {
    throw new Error(
      "missing-native-script-tx history sources do not share one application overlay",
    );
  }
  const chain = binding.resolvedContracts.contracts.missingNativeScriptTx;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 8 ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "missing-native-script-tx manifest omitted required contracts",
    );
  }
  const contracts: MissingNativeScriptTxContracts = Object.freeze({
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
  const references = context.references;
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
          "missing-native-script-tx history was not derived from this workflow's public DA sources",
        );
      }
      return corpus;
    },
    historicalSourceRoster: context.runtime.historicalNativeScriptL1Roster,
    historicalThroughPoint: () => {
      const point = corpusCell.throughPoint;
      if (point === undefined) {
        throw new Error(
          "missing-native-script-tx L1 history boundary was not derived from the authenticated header observation",
        );
      }
      return point;
    },
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  };

  boundConfigs.set(context, bound);
  return bound;
};

export const MISSING_NATIVE_SCRIPT_TX_FAMILY_DEFINITION = defineFamily<
  "missingNativeScriptTx",
  keyof FaultProofWitnessReferenceScripts,
  true,
  8,
  HistoricalRuntime
>({
  category: "missingNativeScriptTx",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    MissingNativeScriptTxStep02Datum,
    MissingNativeScriptTxStep03Datum,
    MissingNativeScriptTxStep04Datum,
    MissingNativeScriptTxStep05Datum,
    MissingNativeScriptTxStep06Datum,
    MissingNativeScriptTxStep07Datum,
    MissingNativeScriptTxStep08Datum,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => MISSING_NATIVE_SCRIPT_TX_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofMissingNativeScriptTx",
      "fraudProofMissingNativeScriptTxStep02",
      "fraudProofMissingNativeScriptTxStep03",
      "fraudProofMissingNativeScriptTxStep04",
      "fraudProofMissingNativeScriptTxStep05",
      "fraudProofMissingNativeScriptTxStep06",
      "fraudProofMissingNativeScriptTxStep07",
      "fraudProofMissingNativeScriptTxStep08",
    ],
    transactionPort: (context) => {
      if (context.l1.observeBoundary === undefined)
        throw new Error(
          "missing-native-script-tx raw L1 boundary authority is unavailable",
        );
      return transactionPort(boundFor(context));
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { binding, certificate, references } = context;
        const bound = boundFor(context);

        const admitted = await admitMissingNativeScriptTxArtifact({
          value: artifact,
          historicalNativeScriptCorpus: bound.historicalCorpus(),
          historicalSourceRoster: bound.historicalSourceRoster,
          historicalThroughPoint: bound.historicalThroughPoint(),
          releaseFinality: binding.releaseFinality,
        });
        const planned =
          action.input.stage === "step_02"
            ? spendFieldPlan(admitted, context.signer.paymentKeyHash)
            : action.input.stage === "step_04"
              ? outputFieldPlan(admitted, context.signer.paymentKeyHash)
              : typeof action.input.stage === "string" &&
                  ["step_06", "step_07", "step_08"].includes(action.input.stage)
                ? scriptFieldPlan(admitted, context.signer.paymentKeyHash)
                : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor:
            action.input.stage === "step_04"
              ? admitted.evidence.producingTxInclusion.nativeTxCompactCbor
              : admitted.evidence.badTxInclusion.nativeTxCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  extend: (context) => ({
    historicalNativeScriptCheckpointStore:
      context.runtime.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      context.runtime.historicalNativeScriptHistorySource,
  }),
});

export const createManifestBoundMissingNativeScriptTxWorkflow = async (
  config: ManifestBoundMissingNativeScriptTxWorkflowConfig,
): Promise<ManifestBoundMissingNativeScriptTxWorkflow> => {
  const corpusCell: HistoricalCorpusCell = {};
  const runtime: HistoricalRuntime = { ...config, corpusCell };
  const workflow = await assembleManifestBoundFamilyWorkflow(
    MISSING_NATIVE_SCRIPT_TX_FAMILY_DEFINITION,
    config,
    runtime,
  );
  historicalCorpusCells.set(workflow, corpusCell);
  return workflow as unknown as ManifestBoundMissingNativeScriptTxWorkflow;
};
