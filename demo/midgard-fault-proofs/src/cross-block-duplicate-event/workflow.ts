import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type CompleteCanonicalReplayContext,
  CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY,
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
import { CROSS_BLOCK_DUPLICATE_EVENT_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import type {
  HistoricalNativeScriptCheckpointStore,
  HistoricalNativeScriptHistorySource,
} from "../workflow/historical-native-script-corpus.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import {
  admitCrossBlockDuplicateArtifact,
  prepareCrossBlockDuplicateArtifact,
} from "./artifact.js";
import type { CrossBlockDuplicateEventContracts } from "./contracts.js";
import {
  createCrossBlockSettlementAuthority,
  type CrossBlockSettlementAuthority,
  crossBlockSettlementRecords,
} from "./settlement-authority.js";
import { submitCrossBlockDuplicateEventStep01 } from "./submit-cross-block-duplicate-event-step-01.js";
import { submitCrossBlockDuplicateEventStep02 } from "./submit-cross-block-duplicate-event-step-02.js";
export type CrossBlockDuplicateEventWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: Pick<
    Required<FaultProofWitnessReferenceScripts>,
    "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw"
  >;
}>;
type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"crossBlockDuplicateEvent">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: CrossBlockDuplicateEventContracts;
  references: CrossBlockDuplicateEventWorkflowReferenceScripts;
  settlementAuthority: CrossBlockSettlementAuthority;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
export const createCrossBlockDuplicateEventTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"crossBlockDuplicateEvent"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "crossBlockDuplicateEvent",
  prepare: async ({ evidence, classification }) =>
    await prepareCrossBlockDuplicateArtifact({
      evidence,
      classification,
      context: await config.settlementAuthority.capture(evidence),
    }),
  capture: async ({ action, artifact }) => {
    const a = await admitCrossBlockDuplicateArtifact(artifact);
    if (a.current.headerHash !== config.binding.definition.headerHash)
      throw new Error("cross-block duplicate workflow header changed");
    const input = cursorFamilyActionInput({
      category: "crossBlockDuplicateEvent",
      action,
    });
    if (input.stage === "remove")
      return await captureCursorRemoval({
        category: "crossBlockDuplicateEvent",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: a.current.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    const refreshed = await config.settlementAuthority.capture(a.current);
    const live = crossBlockSettlementRecords(a.current, refreshed).find(
      (record) => record.headerHash === a.coordinate.settledHeaderHash,
    );
    if (
      live === undefined ||
      live.outRef !== a.settlementOutRef ||
      live.policyId !== a.settlementPolicyId ||
      live.datumCbor !== a.settlementDatumCbor ||
      live.payloadEnvelopeCbor !== a.settled.payloadEnvelopeCbor.toString("hex")
    )
      throw new Error(
        "cross-block duplicate settlement changed or is no longer live",
      );
    return {
      transaction: await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          if (input.stage === "init") {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "crossBlockDuplicateEvent",
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: a.current.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
            return;
          }
          const common = {
            lucid: config.lucid,
            contracts: config.contracts,
            signer: config.signer,
            threadOutRef: cursorStringField(input, "threadOutRef"),
            preSubmitBoundary,
            awaitConfirmation: false,
          };
          if (input.stage === "step_01") {
            await submitCrossBlockDuplicateEventStep01({
              ...common,
              network: config.binding.network,
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              committedEvent: a.prepared.challengedEvent,
              referenceScriptUtxo: config.references.steps[0],
            });
            return;
          }
          if (input.stage === "step_02") {
            await submitCrossBlockDuplicateEventStep02({
              ...common,
              settlementOutRef: live.outRef,
              settledHeaderHash: live.headerHash,
              settledEvent: a.prepared.settledEvent,
              referenceScriptUtxo: config.references.steps[1],
              witnessReferenceScripts: config.references.witnesses,
            });
            return;
          }
          throw new Error("cross-block duplicate workflow stage is invalid");
        },
      ),
    };
  },
});
export type ManifestBoundCrossBlockDuplicateEventWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: CrossBlockDuplicateEventWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  historySource: HistoricalNativeScriptHistorySource;
  checkpointStore: HistoricalNativeScriptCheckpointStore;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
export type ManifestBoundCrossBlockDuplicateEventWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"crossBlockDuplicateEvent">;
  l1: FraudProofFamilyL1ObservationPort<"crossBlockDuplicateEvent">;
  transactions: CursorFamilyTransactionPort<"crossBlockDuplicateEvent">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  settlementAuthority: CrossBlockSettlementAuthority;
}>;
export const createManifestBoundCrossBlockDuplicateEventWorkflow = async (
  config: ManifestBoundCrossBlockDuplicateEventWorkflowConfig,
): Promise<ManifestBoundCrossBlockDuplicateEventWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "crossBlockDuplicateEvent",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      SDK.FraudProofComputationThreadStepDatum,
      SDK.CrossBlockDuplicateEventStep02DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.crossBlockDuplicateEvent;
  if (
    chain === undefined ||
    binding.resolvedContracts.stateQueuePolicyId === undefined
  )
    throw new Error("cross-block duplicate manifest lacks required chain");
  const references: CrossBlockDuplicateEventWorkflowReferenceScripts = {
    steps: [
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofCrossBlockDuplicateEvent",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofCrossBlockDuplicateEventStep02",
        utxo: config.referenceScripts.steps[1],
      }),
    ],
    witnesses: {
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
      computationThreadMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
    },
  };
  const contracts: CrossBlockDuplicateEventContracts = {
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: binding.resolvedContracts.contracts.fraudProof,
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId: binding.resolvedContracts.stateQueuePolicyId,
  };
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const settlementAuthority = createCrossBlockSettlementAuthority({
    binding,
    source: config.source,
    historySource: config.historySource,
    checkpointStore: config.checkpointStore,
  });
  const transactions = createCrossBlockDuplicateEventTransactionPort({
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    settlementAuthority,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const adapter = createCursorFamilyWorkflowAdapter({
    spec: CROSS_BLOCK_DUPLICATE_EVENT_CURSOR_SPEC,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    settlementAuthority,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};
export const runOrResumeManifestBoundCrossBlockDuplicateEventWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundCrossBlockDuplicateEventWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}) => {
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  const replayContext: CompleteCanonicalReplayContext = {
    settlements: await workflow.settlementAuthority.capture({
      headerHash: workflow.binding.definition.headerHash,
    }),
  };
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: CROSS_BLOCK_DUPLICATE_EVENT_COMPLETE_CANONICAL_REPLAY,
    replayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["crossBlockDuplicateEvent"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
