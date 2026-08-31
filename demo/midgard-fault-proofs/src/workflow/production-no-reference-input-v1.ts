import {
  FraudProofComputationThreadStepDatum,
  NoReferenceInputStep02Datum,
  NoReferenceInputStep03Datum,
  NoReferenceInputStep04Datum,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlanV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import {
  requireDeploymentScriptHash,
  type ResolvedProverSigner,
} from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitNoReferenceInputStep01 } from "../submit-no-reference-input-step-01.js";
import { submitNoReferenceInputStep02 } from "../submit-no-reference-input-step-02.js";
import { submitNoReferenceInputStep03 } from "../submit-no-reference-input-step-03.js";
import { submitNoReferenceInputStep04 } from "../submit-no-reference-input-step-04.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  type CompleteCanonicalReplayContextV1,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStoreV1 } from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createProductionClaimRegistryPrerequisiteV1,
  type ProductionClaimRegistryPrerequisiteV1,
  type ProductionClaimRegistryPublicProofDeriverV1,
} from "./production-claim-registry-prerequisite-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  admitProductionLedgerAbsenceArtifactV1,
  prepareProductionLedgerAbsenceArtifactV1,
} from "./production-ledger-absence-artifact-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  resolveDirectFirstProofChunksV1,
  withProductionProofChunkPrerequisiteV1,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export type NoReferenceInputWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly claimRegistrySpend: UTxO;
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
    readonly pexcludesWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"noReferenceInput">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NoReferenceInputWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"noReferenceInput">["fieldPreimageCertificate"]
  >;
  replayContext?: CompleteCanonicalReplayContextV1;
  claimRegistry: ProductionClaimRegistryPrerequisiteV1<"noReferenceInput">;
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
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "no-reference-input workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "noReferenceInput" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("no-reference-input workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`no-reference-input workflow action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfigV1,
  planned: FaultProofFieldOpeningPlanV1,
) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("no-reference-input field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.binding.network,
    planned,
    certificatePolicyId: config.certificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("no-reference-input field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const resolveChunks = async ({
  action,
  config,
  proofCbor,
}: {
  readonly action: FraudProofWorkflowActionV1;
  readonly config: BoundConfigV1;
  readonly proofCbor: string;
}) => {
  const chunks = await resolveDirectFirstProofChunksV1({
    action,
    lucid: config.lucid,
    address: config.signer.address,
    proofCbor,
  });
  // Missing chunks select the exact direct attempt. The prerequisite wrapper
  // admits a publication route only after the direct body has failed for the
  // release-bound capacity reason and then holds the base step until its exact
  // raw-L1-confirmed chunks are present.
  return chunks;
};

const captureRemoval = async (
  config: BoundConfigV1,
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
  const transaction = await captureLocallyEvaluatedTransactionV1(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        fraudCategory: "noReferenceInput",
        fraudulentHeaderHash: config.binding.definition.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefsV1(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "no-reference-input removal changed authenticated inputs",
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
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"noReferenceInput"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "noReferenceInput",
  prepare: async ({ evidence, replayContext, classification }) =>
    await prepareProductionLedgerAbsenceArtifactV1({
      category: "noReferenceInput",
      evidence,
      replayContext,
      classification,
      owner: config.signer.paymentKeyHash,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionLedgerAbsenceArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (
      admitted.artifact.category !== "noReferenceInput" ||
      admitted.artifact.headerHash !== config.binding.definition.headerHash
    ) {
      throw new Error("no-reference-input artifact changed workflow identity");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      const claimRegistryMutation = await config.claimRegistry.resolveMutation({
        headerHash: admitted.artifact.headerHash,
        action,
        artifact,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "noReferenceInput",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              preparedClaimRegistryMutation: claimRegistryMutation,
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
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitNoReferenceInputStep01({
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
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitNoReferenceInputStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceInputsPreimage: admitted.inputPreimage.map(
                (candidate) => ({
                  txId: candidate.tx_id,
                  index: candidate.output_index,
                }),
              ),
              nativeTxCompactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
              badReferenceInputIndex: BigInt(admitted.artifact.badInputIndex),
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
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitNoReferenceInputStep03({
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
      const [chunks, claimRegistryMutation] = await Promise.all([
        resolveChunks({
          action,
          config,
          proofCbor: admitted.artifact.txsNonMembershipProofCbor,
        }),
        config.claimRegistry.resolveMutation({
          headerHash: admitted.artifact.headerHash,
          action,
          artifact,
        }),
      ]);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitNoReferenceInputStep04({
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
              claimRegistryMutation,
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
      `no-reference-input workflow cannot execute ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundNoReferenceInputWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NoReferenceInputWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  replayContext?: CompleteCanonicalReplayContextV1;
  claimRegistryProofs: ProductionClaimRegistryPublicProofDeriverV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundNoReferenceInputWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"noReferenceInput">;
  l1: FraudProofFamilyL1ObservationPortV1<"noReferenceInput">;
  transactions: ProductionLinearFamilyTransactionPortV1<"noReferenceInput">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
  replayContext?: CompleteCanonicalReplayContextV1;
}>;

export const createManifestBoundNoReferenceInputWorkflowV1 = async (
  config: ManifestBoundNoReferenceInputWorkflowConfigV1,
): Promise<ManifestBoundNoReferenceInputWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "noReferenceInput",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      NoReferenceInputStep02Datum,
      NoReferenceInputStep03Datum,
      NoReferenceInputStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error(
      "no-reference-input manifest omitted field-preimage certificate policy",
    );
  }
  const certificate = binding.fieldPreimageCertificate;
  const stepNames = [
    "fraudProofNoReferenceInput",
    "fraudProofNoReferenceInputStep02",
    "fraudProofNoReferenceInputStep03",
    "fraudProofNoReferenceInputStep04",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
  const witness = <Name extends keyof FaultProofWitnessReferenceScriptsV1>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name]!,
    });
  const references: NoReferenceInputWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze(steps),
    witnesses: Object.freeze({
      claimRegistrySpend: witness("claimRegistrySpend", "claimRegistrySpend"),
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
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined || l1.publications === undefined) {
    throw new Error(
      "no-reference-input requires authenticated raw L1 and publication authorities",
    );
  }
  const claimRegistry = createProductionClaimRegistryPrerequisiteV1({
    category: "noReferenceInput",
    categoryId: binding.resolvedContracts.category.categoryId,
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    computationThreadPolicyId:
      binding.resolvedContracts.contracts.computationThread.policyId,
    claimRegistryAddress: credentialToAddress(binding.network, {
      type: "Script",
      hash: requireDeploymentScriptHash(
        binding.deploymentInfo,
        "claimRegistrySpend",
      ),
    }),
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    rawL1: l1.rawL1,
    releaseFinality: binding.releaseFinality,
    publications: l1.publications,
    proofs: config.claimRegistryProofs,
    mutationForAction: ({ action }) => {
      const stage = actionInput(action).stage;
      return stage === "init"
        ? { kind: "open" }
        : stage === "step_04"
          ? { kind: "close" }
          : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  const bound: BoundConfigV1 = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    referenceScripts: references,
    certificate,
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
    claimRegistry,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = createTransactionPort(bound);
  let adapter = createProductionLinearFamilyWorkflowAdapterV1({
    category: "noReferenceInput",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "noReferenceInput",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      if (actionInput(action).stage !== "step_02") return null;
      const admitted = admitProductionLedgerAbsenceArtifactV1(
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
      } satisfies ProductionFieldCarriageRequirementV1;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "noReferenceInput",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "noReferenceInput",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    maximumTransactionBytes: binding.cardanoProtocolParameters.maxTxSize,
    proofCborForAction: ({ action, artifact }) => {
      const stage = actionInput(action).stage;
      const admitted = admitProductionLedgerAbsenceArtifactV1(
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
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "noReferenceInput",
    base: adapter,
    prerequisite: proofPrerequisite,
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "noReferenceInput",
    base: adapter,
    prerequisite: claimRegistry.proofChunks,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
    ...(config.replayContext === undefined
      ? {}
      : { replayContext: config.replayContext }),
  });
};

export const runOrResumeManifestBoundNoReferenceInputWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNoReferenceInputWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
    ...(workflow.replayContext === undefined
      ? {}
      : { replayContext: workflow.replayContext }),
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["noReferenceInput"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
