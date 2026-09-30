import { type LucidEvolution } from "@lucid-evolution/lucid";

import type { CanonicalDecodabilityContracts } from "../canonical-decodability/contracts.js";
import { submitCanonicalDecodabilityInit } from "../canonical-decodability/submit-canonical-decodability-init.js";
import { submitCanonicalDecodabilityStep01 } from "../canonical-decodability/submit-canonical-decodability-step-01.js";
import { submitCanonicalDecodabilityStep02 } from "../canonical-decodability/submit-canonical-decodability-step-02.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  admitCanonicalDecodabilityArtifact,
  type AssemblyContext,
  record,
  WITNESS_ROLES,
} from "./canonical-decodability.admit-canonical-decodability-artifact.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type LinearFamilyPrerequisiteInput,
  type LinearFamilyReferenceScripts,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  createRawCommittedFieldCarriagePlan,
  type FieldCarriagePrerequisitePort,
  type PreimageCarriageRequirement,
} from "./field-carriage-prerequisite.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export type CanonicalDecodabilityWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "canonicalDecodability",
    (typeof WITNESS_ROLES)[number],
    true
  >;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"canonicalDecodability">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: CanonicalDecodabilityContracts;
  category: FraudProofWorkflowDeploymentBinding<"canonicalDecodability">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"canonicalDecodability">["catalogue"];
  referenceScripts: CanonicalDecodabilityWorkflowReferenceScripts;
  fieldCarriage: FieldCarriagePrerequisitePort<"canonicalDecodability">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "canonical-decodability workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "canonicalDecodability" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("canonical-decodability workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  key: string,
): string => {
  const value = input[key];
  if (typeof value !== "string") {
    throw new Error(`canonical-decodability workflow action omitted ${key}`);
  }
  return value;
};

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"canonicalDecodability"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "canonicalDecodability",
  prepare: async () => {
    throw new Error(
      "canonical-decodability requires the authenticated raw committed-field evidence route",
    );
  },
  capture: async ({ action, artifact }) => {
    const admitted = await admitCanonicalDecodabilityArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "canonical-decodability artifact changed header identity",
      );
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitCanonicalDecodabilityInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const [proofChunks, field] = await Promise.all([
        resolvePublishedProofChunks({
          lucid: config.lucid,
          address: config.signer.address,
          proofCbor: admitted.artifact.txMembershipProofCbor,
        }),
        config.fieldCarriage.resolveAuthenticated({
          headerHash: config.headerHash,
          action,
          artifact,
        }),
      ]);
      if (proofChunks === undefined || field.requirement === null) {
        throw new Error(
          "canonical-decodability prerequisites disappeared before step-01",
        );
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitCanonicalDecodabilityStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.txInclusion,
              fieldIndex: admitted.artifact.selectedFieldIndex,
              committedPreimage: admitted.committedPreimage,
              ...(admitted.witnessSet === undefined
                ? {}
                : { witnessSet: admitted.witnessSet }),
              publishedProofChunks: proofChunks,
              publishedFieldCarriageUtxos: field.publications,
              ...(field.certificate === undefined
                ? {}
                : {
                    fieldCertificateUtxo: field.certificate,
                    fieldCertificatePolicyId:
                      config.contracts.fieldPreimageCertificatePolicyId,
                  }),
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
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitCanonicalDecodabilityStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
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
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "canonicalDecodability",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (built) => {
              if (
                !workflowTransactionInputOutRefs(built.signed).includes(
                  nextRemovalOutRef,
                ) ||
                !workflowTransactionReferenceInputOutRefs(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "canonical-decodability removal changed queue/proof identity",
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
    }
    throw new Error(
      `canonical-decodability workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundCanonicalDecodabilityWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "canonicalDecodability",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundCanonicalDecodabilityWorkflow =
  ManifestBoundLinearFamilyWorkflow<"canonicalDecodability", true>;

export const contracts = (
  context: AssemblyContext,
): CanonicalDecodabilityContracts => {
  const resolved = context.binding.resolvedContracts;
  const chain = resolved.contracts.canonicalDecodability;
  const stateQueuePolicyId = resolved.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error(
      "canonical-decodability manifest omitted its proof/certificate contracts",
    );
  }
  return Object.freeze({
    steps: chain.steps,
    computationThread: resolved.contracts.computationThread,
    fraudProof: {
      policyId: resolved.contracts.fraudProof.policyId,
      mintingScript: resolved.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        resolved.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: context.certificate.policyId,
  });
};

// Step-01 carries the selected committed field as raw bytes.
export const fieldCarriageRequirementForAction = async (
  context: AssemblyContext,
  { action, artifact }: LinearFamilyPrerequisiteInput,
): Promise<PreimageCarriageRequirement | null> => {
  if (action.input.stage !== "step_01") return null;
  const admitted = await admitCanonicalDecodabilityArtifact(artifact);
  return Object.freeze({
    planned: createRawCommittedFieldCarriagePlan({
      sourceKind: 0n,
      owner: context.signer.paymentKeyHash,
      nativeTxId: admitted.txInclusion.nativeTxId,
      fieldIndex: admitted.artifact.selectedFieldIndex,
      preimage: admitted.committedPreimage,
    }),
    compactCbor: admitted.txInclusion.nativeTxCompactCbor,
    ...(admitted.witnessSetCompactCbor === undefined
      ? {}
      : { witnessSetCompactCbor: admitted.witnessSetCompactCbor }),
    certificate: Object.freeze({
      policyId: context.certificate.policyId,
      mintingScript: context.certificate.mintingScript,
      referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
    }),
  });
};
