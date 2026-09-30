import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitReferenceInputNoIdxStep01 } from "../submit-reference-input-no-idx-step-01.js";
import { submitReferenceInputNoIdxStep02 } from "../submit-reference-input-no-idx-step-02.js";
import { submitReferenceInputNoIdxStep03 } from "../submit-reference-input-no-idx-step-03.js";
import { submitReferenceInputNoIdxStep04 } from "../submit-reference-input-no-idx-step-04.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type FieldPreimageCertificateBinding,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { exactJournalRecord } from "./native-index-artifact.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import {
  admitReferenceInputNoIdxArtifact,
  type AssemblyContext,
  prepareReferenceInputNoIdxArtifact,
  type ReferenceInputNoIdxWorkflowReferenceScripts,
  WITNESS_ROLES,
} from "./reference-input-no-idx.admit-reference-input-no-idx-artifact.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"referenceInputNoIdx">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: ReferenceInputNoIdxWorkflowReferenceScripts;
  certificate: FieldPreimageCertificateBinding;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

export const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = exactJournalRecord(
    action.input,
    Object.keys(action.input),
    "reference-input-no-idx workflow action",
  );
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "referenceInputNoIdx" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("reference-input-no-idx workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`reference-input-no-idx action omitted ${field}`);
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
    throw new Error("reference-input-no-idx publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.network,
    planned,
    certificatePolicyId: config.certificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("reference-input-no-idx certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
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
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "referenceInputNoIdx",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
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
              "reference-input-no-idx removal changed authenticated inputs",
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

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"referenceInputNoIdx"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "referenceInputNoIdx",
  prepare: async ({ evidence, classification }) =>
    await prepareReferenceInputNoIdxArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitReferenceInputNoIdxArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "reference-input-no-idx artifact changed manifest-bound header",
      );
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              fraudCategory: "referenceInputNoIdx",
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
    if (input.stage === "step_01" || input.stage === "step_03") {
      const stepIndex = input.stage === "step_01" ? 0 : 2;
      const inclusion =
        input.stage === "step_01"
          ? admitted.badInclusion
          : admitted.producingInclusion;
      const proofCbor =
        input.stage === "step_01"
          ? admitted.artifact.badTx.txMembershipProofCbor
          : admitted.artifact.producingTx.txMembershipProofCbor;
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const common = {
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: inclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (input.stage === "step_01") {
              await submitReferenceInputNoIdxStep01(common);
            } else {
              await submitReferenceInputNoIdxStep03(common);
            }
          },
        ),
      });
    }
    if (input.stage === "step_02" || input.stage === "step_04") {
      const stepIndex = input.stage === "step_02" ? 1 : 3;
      const plan =
        input.stage === "step_02"
          ? admitted.referenceInputFieldPlan
          : admitted.outputFieldPlan;
      const carriage = await resolveField(config, plan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const common = {
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (input.stage === "step_02") {
              await submitReferenceInputNoIdxStep02({
                ...common,
                referenceInputsPreimage: admitted.referenceInputs,
                nativeTxCompactCbor:
                  admitted.artifact.badTx.nativeTxCompactCbor,
              });
            } else {
              await submitReferenceInputNoIdxStep04({
                ...common,
                outputsPreimage: admitted.outputs,
                nativeTxCompactCbor:
                  admitted.artifact.producingTx.nativeTxCompactCbor,
                witnessReferenceScripts: config.referenceScripts.witnesses,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `reference-input-no-idx action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundReferenceInputNoIdxWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "referenceInputNoIdx",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundReferenceInputNoIdxWorkflow =
  ManifestBoundLinearFamilyWorkflow<"referenceInputNoIdx", true>;

export const fieldPreimageCertificate = (context: AssemblyContext) => ({
  policyId: context.certificate.policyId,
  mintingScript: context.certificate.mintingScript,
  referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
});
