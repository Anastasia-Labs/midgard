import {
  FraudProofComputationThreadStepDatum,
  NonExistentInputStep02Datum,
  NonExistentInputStep03Datum,
  NonExistentInputStep04Datum,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import { neSubmitStep01 } from "../ne-submit-step-01.js";
import { neSubmitStep02 } from "../ne-submit-step-02.js";
import { neSubmitStep03 } from "../ne-submit-step-03.js";
import { neSubmitStep04 } from "../ne-submit-step-04.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import {
  type CompleteCanonicalReplayContext,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStore } from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  admitLedgerAbsenceArtifact,
  prepareLedgerAbsenceArtifact,
} from "./production-ledger-absence-artifact-v1.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./production-linear-family-adapter-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary-v1.js";

export type NonExistentInputWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
    readonly pexcludesWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"nonExistentInput">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NonExistentInputWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"nonExistentInput">["fieldPreimageCertificate"]
  >;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "non-existent-input workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "nonExistentInput" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("non-existent-input workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`non-existent-input workflow action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfig,
  planned: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("non-existent-input field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.certificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("non-existent-input field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const resolveChunks = async ({
  action,
  config,
  proofCbor,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly config: BoundConfig;
  readonly proofCbor: string;
}) => {
  const chunks = await resolveDirectFirstProofChunks({
    action,
    lucid: config.lucid,
    address: config.signer.address,
    proofCbor,
  });
  // Absence is the expected state for the direct-first attempt. Once a
  // publication route is journal-authorized, the outer prerequisite refuses
  // to expose the base step until the exact raw-L1-confirmed chunks exist.
  return chunks;
};

const captureRemoval = async (
  config: BoundConfig,
  input: Readonly<Record<string, unknown>>,
) => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransaction(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        fraudCategory: "nonExistentInput",
        fraudulentHeaderHash: config.binding.definition.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "non-existent-input removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease === undefined ? {} : { mutationLease }),
  });
};

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"nonExistentInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "nonExistentInput",
  prepare: async ({ evidence, replayContext, classification }) =>
    await prepareLedgerAbsenceArtifact({
      category: "nonExistentInput",
      evidence,
      replayContext,
      classification,
      owner: config.signer.paymentKeyHash,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitLedgerAbsenceArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (
      admitted.artifact.category !== "nonExistentInput" ||
      admitted.artifact.headerHash !== config.binding.definition.headerHash
    ) {
      throw new Error("non-existent-input artifact changed workflow identity");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "nonExistentInput",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.badTx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.txInclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveField(config, admitted.fieldPlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              inputsPreimage: admitted.inputPreimage.map((candidate) => ({
                txId: candidate.tx_id,
                index: candidate.output_index,
              })),
              nativeTxCompactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
              badInputIndex: BigInt(admitted.artifact.badInputIndex),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : {
                    certificateUtxo: carriage.certificate,
                    certificatePolicyId: config.certificate.policyId,
                  }),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.ledgerNonMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep03({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              ledgerNonMembershipProofCbor:
                admitted.artifact.ledgerNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[2],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.txsNonMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep04({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              txsNonMembershipProofCbor:
                admitted.artifact.txsNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[3],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `non-existent-input workflow cannot execute ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundNonExistentInputWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NonExistentInputWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContext;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundNonExistentInputWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"nonExistentInput">;
  l1: FraudProofFamilyL1ObservationPort<"nonExistentInput">;
  transactions: LinearFamilyTransactionPort<"nonExistentInput">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  replayContext?: CompleteCanonicalReplayContext;
}>;

export const createManifestBoundNonExistentInputWorkflow = async (
  config: ManifestBoundNonExistentInputWorkflowConfig,
): Promise<ManifestBoundNonExistentInputWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "nonExistentInput",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      NonExistentInputStep02Datum,
      NonExistentInputStep03Datum,
      NonExistentInputStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error(
      "non-existent-input manifest omitted field-preimage certificate policy",
    );
  }
  const certificate = binding.fieldPreimageCertificate;
  const stepNames = [
    "fraudProofNonExistentInput",
    "fraudProofNonExistentInputStep02",
    "fraudProofNonExistentInputStep03",
    "fraudProofNonExistentInputStep04",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
  const witness = <Name extends keyof FaultProofWitnessReferenceScripts>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name]!,
    });
  const references: NonExistentInputWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze({
      computationThreadMint: witness(
        "computationThreadMint",
        "computationThreadMint",
      ),
      fraudProofMint: witness("fraudProofMint", "fraudProofMint"),
      phasMembershipWithdraw: witness(
        "phasMembershipWithdraw",
        "phasMembershipWithdraw",
      ),
      chunkedVerifyWithdraw: witness(
        "chunkedVerifyWithdraw",
        "chunkedVerifyWithdraw",
      ),
      pexcludesWithdraw: witness("pexcludesWithdraw", "pexcludesWithdraw"),
    }),
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined || l1.publications === undefined) {
    throw new Error(
      "non-existent-input requires authenticated raw L1 and publication authorities",
    );
  }
  const bound: BoundConfig = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    referenceScripts: references,
    certificate,
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = createTransactionPort(bound);
  let adapter = createLinearFamilyWorkflowAdapter({
    category: "nonExistentInput",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: "nonExistentInput",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      if (actionInput(action).stage !== "step_02") return null;
      const admitted = admitLedgerAbsenceArtifact(
        artifact,
        config.signer.paymentKeyHash,
      );
      return {
        planned: admitted.fieldPlan,
        compactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
        certificate: {
          policyId: certificate.policyId,
          mintingScript: certificate.mintingScript,
          referenceScriptUtxo: references.fieldPreimageCertificateMint,
        },
      } satisfies FieldCarriageRequirement;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "nonExistentInput",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "nonExistentInput",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    maximumTransactionBytes: binding.cardanoProtocolParameters.maxTxSize,
    proofCborForAction: ({ action, artifact }) => {
      const stage = actionInput(action).stage;
      const admitted = admitLedgerAbsenceArtifact(
        artifact,
        config.signer.paymentKeyHash,
      );
      return stage === "step_01"
        ? admitted.artifact.badTx.txMembershipProofCbor
        : stage === "step_03"
          ? admitted.artifact.ledgerNonMembershipProofCbor
          : stage === "step_04"
            ? admitted.artifact.txsNonMembershipProofCbor
            : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProofChunkPrerequisite({
    category: "nonExistentInput",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
  });
};

export const runOrResumeManifestBoundNonExistentInputWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNonExistentInputWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
    ...(workflow.replayContext === undefined
      ? {}
      : { replayContext: workflow.replayContext }),
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["nonExistentInput"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
