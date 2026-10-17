import {
  committedWithdrawalKeyBytes,
  WithdrawalSourceMembershipProof,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { prepareWithdrawnInputFromCanonicalEvidence } from "../prepare-withdrawn-input.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { WithdrawnInputContracts } from "../withdrawn-input/contracts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type FieldPreimageCertificateBinding,
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
} from "./family-definition.js";
import { normalizeJournalJson } from "./journal.js";
import { exactJournalRecord } from "./native-index-artifact.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
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
  const artifact = normalizeJournalJson({
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

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

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
  const input = exactJournalRecord(
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
        fraudCategory: "withdrawnInput",
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
              "withdrawn-input removal changed authenticated inputs",
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

export const resolveField = async (
  config: BoundConfig,
  plan: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
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
