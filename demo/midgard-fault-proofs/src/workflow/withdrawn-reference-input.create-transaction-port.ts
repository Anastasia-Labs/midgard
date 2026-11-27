import { type LucidEvolution } from "@lucid-evolution/lucid";

import { resolveFaultProofFieldPreimageCertificate } from "../field-opening.certify-fault-proof-field-carriage.js";
import type { FaultProofFieldOpeningPlan } from "../field-opening.plan-fault-proof-field-opening.js";
import { resolveFaultProofFieldCarriagePublications } from "../field-opening.publish-fault-proof-field-carriage.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.build-remove-fraudulent-block.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { WithdrawnReferenceInputContracts } from "../withdrawn-reference-input/contracts.js";
import { buildWithdrawnReferenceInputInit } from "../withdrawn-reference-input/submit-withdrawn-reference-input-init.js";
import { buildWithdrawnReferenceInputStep01 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-01.js";
import { buildWithdrawnReferenceInputStep02 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-02.js";
import { buildWithdrawnReferenceInputStep03 } from "../withdrawn-reference-input/submit-withdrawn-reference-input-step-03.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type FieldPreimageCertificateBinding,
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { exactArtifactRecord } from "./native-index-artifact.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import {
  type ProofMaterialsAccess,
  proofMaterialsAccess,
} from "./proof-materials-view.js";
import { admitRemovalTransaction } from "./removal-admission.js";
import { workflowStepTransaction } from "./step-transaction.js";
import {
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";
import {
  admitWithdrawnReferenceInputArtifact,
  prepareWithdrawnReferenceInputArtifact,
  WITNESS_ROLES,
} from "./withdrawn-reference-input.admit-withdrawn-reference-input-artifact.js";

export type WithdrawnReferenceInputWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "withdrawnReferenceInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;

type AssemblyContext = LinearFamilyAssemblyContext<
  "withdrawnReferenceInput",
  (typeof WITNESS_ROLES)[number],
  true
>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: WithdrawnReferenceInputContracts;
  category: FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"withdrawnReferenceInput">["catalogue"];
  referenceScripts: WithdrawnReferenceInputWorkflowReferenceScripts;
  certificate: FieldPreimageCertificateBinding;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

export const actionInput = (action: FraudProofWorkflowAction) => {
  const input = exactArtifactRecord(
    action.input,
    Object.keys(action.input),
    "withdrawn-reference-input action",
  );
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "withdrawnReferenceInput" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("withdrawn-reference-input action identity changed");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`withdrawn-reference-input action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfig,
  plan: FaultProofFieldOpeningPlan,
  materials: ProofMaterialsAccess,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    materials,
    lucid: config.lucid,
    publisherAddress: config.signer.proofMaterialsAddress,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("withdrawn-reference-input publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.network,
    planned: plan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("withdrawn-reference-input certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const captureRemoval = async (
  config: BoundConfig,
  input: Readonly<Record<string, unknown>>,
) => {
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  return await admitRemovalTransaction(
    await buildRemoveFraudulentBlock({
      fraudProofOutRef,
      lucid: config.lucid,
      blueprint: config.blueprint,
      deploymentInfo: config.deploymentInfo,
      network: config.network,
      signer: config.signer,
      fraudCategory: "withdrawnReferenceInput",
      fraudulentHeaderHash: config.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: config.fraudProverRewardLovelace,
    }),
    async (built) => {
      if (
        !workflowTransactionInputOutRefs(built.signed).includes(
          nextRemovalOutRef,
        ) ||
        !workflowTransactionReferenceInputOutRefs(built.signed).includes(
          fraudProofOutRef,
        )
      ) {
        throw new Error(
          "withdrawn-reference-input removal changed authenticated inputs",
        );
      }
    },
  );
};

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"withdrawnReferenceInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "withdrawnReferenceInput",
  prepare: async ({ evidence, classification }) =>
    await prepareWithdrawnReferenceInputArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact, holder }) => {
    const materials = proofMaterialsAccess({
      signer: config.signer,
      lucid: config.lucid,
      holder,
    });
    const admitted = await admitWithdrawnReferenceInputArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("withdrawn-reference-input artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: (
          await workflowStepTransaction(
            await buildWithdrawnReferenceInputInit({
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
            }),
            "withdrawn-reference-input buildWithdrawnReferenceInputInit",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_01") {
      return Object.freeze({
        transaction: (
          await workflowStepTransaction(
            await buildWithdrawnReferenceInputStep01({
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
              txInclusion: admitted.inclusion,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
            }),
            "withdrawn-reference-input buildWithdrawnReferenceInputStep01",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_02") {
      const field = await resolveField(
        config,
        admitted.referencePlan,
        materials,
      );
      return Object.freeze({
        transaction: (
          await workflowStepTransaction(
            await buildWithdrawnReferenceInputStep02({
              materials,
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceInputs: admitted.referenceInputs,
              nativeTxCompactCbor: admitted.artifact.tx.nativeTxCompactCbor,
              badReferenceInputIndex: BigInt(
                admitted.artifact.badReferenceInputIndex,
              ),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              publishedCarriageUtxos: field.publications,
              ...(field.certificate === undefined
                ? {}
                : { certificateUtxo: field.certificate }),
              publishMissingCarriage: false,
            }),
            "withdrawn-reference-input buildWithdrawnReferenceInputStep02",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: (
          await workflowStepTransaction(
            await buildWithdrawnReferenceInputStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              withdrawalMembership: admitted.withdrawalMembership,
              referenceScriptUtxo: config.referenceScripts.steps[2],
              witnessReferenceScripts: config.referenceScripts.witnesses,
            }),
            "withdrawn-reference-input buildWithdrawnReferenceInputStep03",
          )
        ).transaction,
      });
    }
    if (input.stage === "remove") return await captureRemoval(config, input);
    throw new Error(
      `unsupported withdrawn-reference-input stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundWithdrawnReferenceInputWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "withdrawnReferenceInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundWithdrawnReferenceInputWorkflow =
  ManifestBoundLinearFamilyWorkflow<"withdrawnReferenceInput", true>;

export const contracts = (
  context: AssemblyContext,
): WithdrawnReferenceInputContracts => {
  const { binding, certificate } = context;
  const chain = binding.resolvedContracts.contracts.withdrawnReferenceInput;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    stateQueuePolicyId === undefined
  ) {
    throw new Error("withdrawn-reference-input deployment chain is incomplete");
  }
  return Object.freeze({
    steps: [chain.steps[0]!, chain.steps[1]!, chain.steps[2]!] as const,
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
};
