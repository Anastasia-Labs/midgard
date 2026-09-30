import { type AuthenticatedStateQueueHeaderObservation } from "@al-ft/midgard-sdk";
import {
  DoubleSpendStep02Datum,
  DoubleSpendStep03Datum,
  DoubleSpendStep04Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import { createDoubleSpendConstrainedWorkflowAdapter } from "./double-spend-adapter.create-double-spend-constrained-workflow-adapter.js";
import {
  createDoubleSpendAuthenticatedL1TerminalVerifier,
  createDoubleSpendLocalKupmiosL1ObservationPort,
} from "./double-spend-adapter.create-double-spend-raw-l1-observation-port.js";
import {
  type DoubleSpendConstrainedWorkflowAdapterConfig,
  type DoubleSpendWorkflowReferenceScripts,
  type ManifestBoundDoubleSpendWorkflow,
  type ManifestBoundDoubleSpendWorkflowConfig,
} from "./double-spend-adapter.preflight-of.js";
import { observeFraudProofWorkflowHeader } from "./family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "./journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";

/**
 * Strict production construction. Every contract, network, category,
 * economics, and finality value comes from the same finalized manifest; the
 * caller supplies only live runtime capabilities and published UTxOs.
 */
export const createManifestBoundDoubleSpendWorkflow = async (
  config: ManifestBoundDoubleSpendWorkflowConfig,
): Promise<ManifestBoundDoubleSpendWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "doubleSpend",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      DoubleSpendStep02Datum,
      DoubleSpendStep03Datum,
      DoubleSpendStep04Datum,
    ],
  });
  const certificate = binding.fieldPreimageCertificate;
  if (certificate === null) {
    throw new Error(
      "double-spend deployment omitted the field-preimage certificate policy",
    );
  }
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const stepNames = [
    "fraudProofDoubleSpend",
    "fraudProofDoubleSpendStep02",
    "fraudProofDoubleSpendStep03",
    "fraudProofDoubleSpendStep04",
  ] as const;
  const stepReference = (index: 0 | 1 | 2 | 3): UTxO =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: stepNames[index],
      utxo: config.referenceScripts.steps[index],
    });
  const referenceScripts: DoubleSpendWorkflowReferenceScripts = {
    steps: [
      stepReference(0),
      stepReference(1),
      stepReference(2),
      stepReference(3),
    ],
    witnesses: {
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
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
      }),
    },
  };
  const certificateReferenceScript = requireManifestBoundReferenceScriptUtxo({
    binding,
    contractName: "fieldPreimageCertificateMint",
    utxo: config.fieldPreimageCertificateReferenceScript,
  });
  const l1 = createDoubleSpendLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const adapterConfig: DoubleSpendConstrainedWorkflowAdapterConfig = {
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    referenceScripts,
    fieldPreimageCertificate: {
      policyId: certificate.policyId,
      mintingScript: certificate.mintingScript,
      referenceScriptUtxo: certificateReferenceScript,
    },
    l1,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  };
  return {
    binding,
    adapterConfig,
    adapter: createDoubleSpendConstrainedWorkflowAdapter(adapterConfig),
    terminalVerifier: createDoubleSpendAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  };
};

/** Run/resume surface for supported constrained shapes; not production-ready. */
export const runOrResumeConstrainedDoubleSpendWorkflow = async ({
  deploymentFingerprint,
  observation,
  sources,
  journal,
  adapterConfig,
  releaseFinalityAuthority,
  maxSubmissionAttempts,
  maxActions,
}: {
  readonly deploymentFingerprint: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
  readonly adapterConfig: DoubleSpendConstrainedWorkflowAdapterConfig;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResult> => {
  const adapter = createDoubleSpendConstrainedWorkflowAdapter(adapterConfig);
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint,
    observation,
    sources,
    replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: ["doubleSpend"],
    }),
    journal,
    releaseFinalityAuthority,
    terminalVerifier: createDoubleSpendAuthenticatedL1TerminalVerifier(
      adapterConfig.l1,
    ),
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};

/** Production run/resume with the L1 header derived from admitted raw bytes. */
export const runOrResumeManifestBoundDoubleSpendWorkflow = async ({
  workflow,
  sources,
  journal,
  maxSubmissionAttempts,
  maxActions,
}: {
  readonly workflow: ManifestBoundDoubleSpendWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResult> => {
  const observeHeader = workflow.adapterConfig.l1.observeHeader;
  if (observeHeader === undefined) {
    throw new Error(
      "manifest-bound double-spend workflow omitted raw L1 header derivation",
    );
  }
  const observation = await observeFraudProofWorkflowHeader(
    {
      observeHeader,
      observeRetainedHeader: workflow.adapterConfig.l1.observeRetainedHeader,
    },
    { headerHash: workflow.binding.definition.headerHash },
  );
  return await runOrResumeConstrainedDoubleSpendWorkflow({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    journal,
    adapterConfig: workflow.adapterConfig,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};
