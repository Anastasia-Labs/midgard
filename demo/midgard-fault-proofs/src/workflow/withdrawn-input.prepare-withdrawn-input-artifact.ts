import { WithdrawalSourceMembershipProof } from "@al-ft/midgard-sdk/core";
import { committedWithdrawalKeyBytes } from "@al-ft/midgard-sdk/fraud-proof";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { resolveFaultProofFieldPreimageCertificate } from "../field-opening.certify-fault-proof-field-carriage.js";
import type { FaultProofFieldOpeningPlan } from "../field-opening.plan-fault-proof-field-opening.js";
import { resolveFaultProofFieldCarriagePublications } from "../field-opening.publish-fault-proof-field-carriage.js";
import { prepareWithdrawnInputFromCanonicalEvidence } from "../prepare-withdrawn-input.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.build-remove-fraudulent-block.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { WithdrawnInputContracts } from "../withdrawn-input/contracts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  CHUNKED_VERIFY_WITNESS_ROLES,
  type FieldPreimageCertificateBinding,
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
} from "./family-definition.family-adapter-arm.js";
import { normalizeWorkflowJson } from "./identity.js";
import { exactArtifactRecord } from "./native-index-artifact.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import type { ProofMaterialsAccess } from "./proof-materials-view.js";
import { admitRemovalTransaction } from "./removal-admission.js";
import {
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";
import {
  admitWithdrawnInputArtifact,
  selectedIdentity,
  WITHDRAWN_INPUT_ARTIFACT,
  type WithdrawnInputArtifact,
} from "./withdrawn-input.admit-withdrawn-input-artifact.js";

export const prepareWithdrawnInputArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<WithdrawnInputArtifact> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("withdrawn-input classification changed header");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareWithdrawnInputFromCanonicalEvidence({
    evidence,
    badTxId: selected.txId,
    badInputIndex: selected.badInputIndex,
  });
  const selectedKey = committedWithdrawalKeyBytes(prepared.withdrawalId);
  const selectedWithdrawal =
    evidence.reconstruction.withdrawals[selected.withdrawalIndex];
  if (
    selectedWithdrawal === undefined ||
    committedWithdrawalKeyBytes(selectedWithdrawal.key) !==
      selected.withdrawalKey ||
    selectedKey !== selected.withdrawalKey ||
    prepared.badTxInclusion.nativeTxId !== selected.txId
  ) {
    throw new Error("withdrawn-input selected public evidence changed");
  }
  const artifact = normalizeWorkflowJson({
    schemaVersion: WITHDRAWN_INPUT_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    tx: {
      nativeTxId: prepared.badTxInclusion.nativeTxId,
      nativeTxCompactCbor: prepared.badTxInclusion.nativeTxCompactCbor,
      l2TransactionSourceCbor: prepared.badTxInclusion.l2TransactionSourceCbor,
      transactionsPhasRoot: prepared.badTxInclusion.transactionsPhasRoot,
      txMembershipProofCbor: prepared.badTxInclusion.txMembershipProofCbor,
    },
    spendInputs: prepared.spendInputs.map((input) => ({
      tx_id: input.tx_id,
      output_index: input.output_index.toString(),
    })),
    badInputIndex: prepared.badInputIndex,
    withdrawalIndex: selected.withdrawalIndex,
    withdrawalMembershipCbor: Data.to(
      prepared.withdrawalMembership,
      WithdrawalSourceMembershipProof,
    ),
  }) as WithdrawnInputArtifact;
  await admitWithdrawnInputArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = CHUNKED_VERIFY_WITNESS_ROLES;

export type WithdrawnInputWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "withdrawnInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "withdrawnInput",
  (typeof WITNESS_ROLES)[number],
  true
>;

export type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"withdrawnInput">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: WithdrawnInputContracts;
  category: FraudProofWorkflowDeploymentBinding<"withdrawnInput">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"withdrawnInput">["catalogue"];
  referenceScripts: WithdrawnInputWorkflowReferenceScripts;
  certificate: FieldPreimageCertificateBinding;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

export const actionInput = (action: FraudProofWorkflowAction) => {
  const input = exactArtifactRecord(
    action.input,
    Object.keys(action.input),
    "withdrawn-input action",
  );
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "withdrawnInput" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("withdrawn-input action identity changed");
  }
  return input;
};

export const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`withdrawn-input action omitted ${field}`);
  }
  return value;
};

export const captureRemoval = async (
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
      fraudCategory: "withdrawnInput",
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
        throw new Error("withdrawn-input removal changed authenticated inputs");
      }
    },
  );
};

export const resolveField = async (
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
    throw new Error("withdrawn-input field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.network,
    planned: plan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("withdrawn-input field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};
