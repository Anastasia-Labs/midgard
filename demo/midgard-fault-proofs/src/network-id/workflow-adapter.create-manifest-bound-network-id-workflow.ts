import {
  FraudProofComputationThreadStepDatum,
  NetworkIdStep02Datum,
} from "@al-ft/midgard-sdk";

import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { NETWORK_ID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
} from "../workflow/deployment-manifest-binding.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import {
  createNetworkIdAuthenticatedL1TerminalVerifier,
  createNetworkIdLocalKupmiosL1ObservationPort,
  type NetworkIdWorkflowAdapterConfig,
} from "./workflow-adapter.create-network-id-raw-l1-observation-port.js";
import { createNetworkIdWorkflowAdapter } from "./workflow-adapter.create-network-id-workflow-adapter.js";
import {
  type ManifestBoundNetworkIdWorkflow,
  type ManifestBoundNetworkIdWorkflowConfig,
  sealManifestBoundNetworkIdRuntime,
} from "./workflow-adapter.seal-manifest-bound-network-id-runtime.js";

/** Manifest-closed production construction for Q35. */
export const createManifestBoundNetworkIdWorkflow = async (
  config: ManifestBoundNetworkIdWorkflowConfig,
): Promise<ManifestBoundNetworkIdWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "networkId",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      NetworkIdStep02Datum,
    ],
  });
  const resolved = binding.resolvedContracts;
  const networkIdContracts = resolved.contracts.networkId;
  if (networkIdContracts === undefined) {
    throw new Error("network-id deployment resolved a different family chain");
  }
  const certificate = binding.fieldPreimageCertificate;
  if (certificate === null) {
    throw new Error(
      "network-id deployment omitted the field-preimage certificate policy",
    );
  }
  const sealedRuntime = sealManifestBoundNetworkIdRuntime({
    binding,
    signer: config.signer,
    stepReferenceScripts: config.stepReferenceScripts,
    ...(config.forcedStepReferenceScript === undefined
      ? {}
      : { forcedStepReferenceScript: config.forcedStepReferenceScript }),
    ...(config.forcedScanReferenceScript === undefined
      ? {}
      : { forcedScanReferenceScript: config.forcedScanReferenceScript }),
    fieldPreimageCertificateReferenceScript:
      config.fieldPreimageCertificateReferenceScript,
    witnessReferenceScripts: config.witnessReferenceScripts,
    removal: config.removal,
  });
  const rawL1 = createNetworkIdLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const adapterConfig: NetworkIdWorkflowAdapterConfig = {
    lucid: config.lucid,
    blueprint: binding.blueprint,
    network: binding.network,
    contracts: {
      steps: networkIdContracts.steps,
      forcedStep: networkIdContracts.forcedStep,
      expectedNetworkId: binding.network === "Mainnet" ? 1n : 0n,
      computationThread: {
        policyId: resolved.contracts.computationThread.policyId,
        mintingScript: resolved.contracts.computationThread.mintingScript,
      },
      fraudProof: {
        policyId: resolved.contracts.fraudProof.policyId,
        mintingScript: resolved.contracts.fraudProof.mintingScript,
        spendingScriptAddress:
          resolved.contracts.fraudProof.spendingScriptAddress,
      },
      hubOraclePolicyId: resolved.hubOraclePolicyId,
      stateQueuePolicyId: binding.definition.stateQueue.policyId,
      fieldPreimageCertificatePolicyId: certificate.policyId,
      fieldPreimageCertificateMintingScript: certificate.mintingScript,
    },
    stateQueueAddress: binding.definition.stateQueue.address,
    category: resolved.category,
    catalogue: binding.catalogue,
    signer: config.signer,
    stepReferenceScripts: sealedRuntime.stepReferenceScripts,
    ...(sealedRuntime.forcedStepReferenceScript === undefined
      ? {}
      : {
          forcedStepReferenceScript: sealedRuntime.forcedStepReferenceScript,
        }),
    ...(sealedRuntime.forcedScanReferenceScript === undefined
      ? {}
      : {
          forcedScanReferenceScript: sealedRuntime.forcedScanReferenceScript,
        }),
    fieldPreimageCertificateReferenceScript:
      sealedRuntime.fieldPreimageCertificateReferenceScript,
    witnessReferenceScripts: sealedRuntime.witnessReferenceScripts,
    removal: {
      ...sealedRuntime.removal,
      deploymentInfo: binding.deploymentInfo,
      category: "networkId",
      requireReferenceScripts: true,
    },
    rawL1,
  };
  return {
    binding,
    adapterConfig,
    adapter: createNetworkIdWorkflowAdapter(adapterConfig),
    terminalVerifier: createNetworkIdAuthenticatedL1TerminalVerifier(rawL1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  };
};

/** Production run/resume with its header derived from admitted raw L1 bytes. */
export const runOrResumeManifestBoundNetworkIdWorkflow = async ({
  workflow,
  sources,
  journal,
  maxSubmissionAttempts,
  maxActions,
}: {
  readonly workflow: ManifestBoundNetworkIdWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
}): Promise<FraudProofWorkflowRunResult> => {
  const rawL1 = workflow.adapterConfig.rawL1;
  const observeHeader = rawL1?.observeHeader;
  if (observeHeader === undefined) {
    throw new Error(
      "manifest-bound network-id workflow omitted raw L1 header derivation",
    );
  }
  const observation = await observeFraudProofWorkflowHeader(
    { observeHeader, observeRetainedHeader: rawL1?.observeRetainedHeader },
    { headerHash: workflow.binding.definition.headerHash },
  );
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["networkId"],
    }),
    journal,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    terminalVerifier: workflow.terminalVerifier,
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};
