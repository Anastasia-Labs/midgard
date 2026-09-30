import {
  MissingSignatureForcedSignerDatum,
  MissingSignatureForcedStepDatum,
  MissingSignatureForcedWitnessDatum,
} from "@al-ft/midgard-sdk";
import {
  FraudProofComputationThreadStepDatum,
  MissingSignatureStep02Datum,
  MissingSignatureStep03Datum,
  MissingSignatureStep04Datum,
} from "@al-ft/midgard-sdk";

import type { MissingSignatureContracts } from "../missing-signature/contracts.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
} from "./family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "./family-l1-observation.js";
import { withFieldCarriagePrerequisite } from "./field-carriage-prerequisite.js";
import { type FraudProofWorkflowJournalStore } from "./journal.js";
import {
  type BoundMissingSignatureTransactionsConfig,
  type MissingSignatureBuilderSet,
  type MissingSignatureWorkflowReferenceScripts,
  productionBuilders,
} from "./missing-signature.admit-missing-signature-artifact.js";
import {
  createBoundTransactionPort,
  type ManifestBoundMissingSignatureWorkflow,
  type ManifestBoundMissingSignatureWorkflowConfig,
} from "./missing-signature.create-bound-transaction-port.js";
import { createMissingSignatureForcedFieldPrerequisite } from "./missing-signature.create-missing-signature-forced-field-prerequisite.js";
import {
  createMissingSignatureWorkflowAdapter,
  type MissingSignatureTransactionPort,
} from "./missing-signature-adapter.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";

export const createManifestBoundMissingSignatureWorkflow = async (
  config: ManifestBoundMissingSignatureWorkflowConfig,
): Promise<ManifestBoundMissingSignatureWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "missingSignature",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      MissingSignatureStep02Datum,
      MissingSignatureStep03Datum,
      MissingSignatureStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.missingSignature;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    binding.fieldPreimageCertificate === null
  ) {
    throw new Error(
      "missing-signature manifest binding omitted required contracts",
    );
  }
  const references: MissingSignatureWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignature",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignatureStep02",
        utxo: config.referenceScripts.steps[1],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignatureStep03",
        utxo: config.referenceScripts.steps[2],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMissingSignatureStep04",
        utxo: config.referenceScripts.steps[3],
      }),
    ] as const),
    ...(config.referenceScripts.forced === undefined
      ? {}
      : {
          forced: Object.freeze({
            bind: requireManifestBoundReferenceScriptUtxo({
              binding,
              contractName: "fraudProofMissingSignatureForcedStep",
              utxo: config.referenceScripts.forced.bind,
            }),
            signer: requireManifestBoundReferenceScriptUtxo({
              binding,
              contractName: "fraudProofMissingSignatureForcedSigner",
              utxo: config.referenceScripts.forced.signer,
            }),
            witness: requireManifestBoundReferenceScriptUtxo({
              binding,
              contractName: "fraudProofMissingSignatureForcedWitness",
              utxo: config.referenceScripts.forced.witness,
            }),
          }),
        }),
    witnesses: Object.freeze({
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
    }),
    ...(config.referenceScripts.fieldPreimageCertificateMint === undefined
      ? {}
      : {
          fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo(
            {
              binding,
              contractName: "fieldPreimageCertificateMint",
              utxo: config.referenceScripts.fieldPreimageCertificateMint,
            },
          ),
        }),
    ...(config.referenceScripts.fieldCertificates === undefined
      ? {}
      : { fieldCertificates: config.referenceScripts.fieldCertificates }),
  });
  const contracts: MissingSignatureContracts = Object.freeze({
    steps: chain.steps,
    forcedStep: chain.forcedStep,
    forcedSigner: chain.forcedSigner,
    forcedWitness: chain.forcedWitness,
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
    fieldPreimageCertificatePolicyId: binding.fieldPreimageCertificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: {
      ...binding.definition,
      computationThread: {
        ...binding.definition.computationThread,
        steps: [
          ...binding.definition.computationThread.steps,
          {
            role: "computation_thread_step_05",
            address: chain.forcedStep.spendingScriptAddress,
            datumSchema: MissingSignatureForcedStepDatum,
          },
          {
            role: "computation_thread_step_06",
            address: chain.forcedSigner.spendingScriptAddress,
            datumSchema: MissingSignatureForcedSignerDatum,
          },
          {
            role: "computation_thread_step_07",
            address: chain.forcedWitness.spendingScriptAddress,
            datumSchema: MissingSignatureForcedWitnessDatum,
          },
        ],
      },
    },
  });
  const transactions = createBoundTransactionPort({
    config: {
      lucid: config.lucid,
      blueprint: binding.blueprint,
      network: binding.network,
      signer: config.signer,
      headerHash: binding.definition.headerHash,
      contracts,
      category: binding.resolvedContracts.category,
      catalogue: binding.catalogue,
      referenceScripts: references,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: BigInt(
        binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
      deploymentInfo: binding.deploymentInfo,
    },
    builders: productionBuilders,
  });
  if (l1.publications === undefined)
    throw new Error("missing-signature raw L1 omitted publication observer");
  const certificate = binding.fieldPreimageCertificate;
  let adapter = createMissingSignatureWorkflowAdapter({
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const prerequisite = createMissingSignatureForcedFieldPrerequisite({
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    certificate,
    certificateReference: references.fieldPreimageCertificateMint,
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "missingSignature",
    base: adapter,
    prerequisite,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

export const runOrResumeManifestBoundMissingSignatureWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMissingSignatureWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: MISSING_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["missingSignature"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export const unsafeCreateMissingSignatureTransactionPortForTest = (input: {
  readonly config: BoundMissingSignatureTransactionsConfig;
  readonly builders: MissingSignatureBuilderSet;
}): MissingSignatureTransactionPort => createBoundTransactionPort(input);
