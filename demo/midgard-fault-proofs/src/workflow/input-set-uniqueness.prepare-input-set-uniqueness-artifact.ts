import { InputSetUniquenessVerdictSubjectSchema } from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { InputSetUniquenessContracts } from "../input-set-uniqueness/contracts.js";
import {
  detectInputSetUniquenessForcedReplay,
  INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID,
} from "../input-set-uniqueness/replay.js";
import { scanInputSetUniqueness } from "../input-set-uniqueness/scan.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItem,
} from "../prepare-double-spend.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
} from "./family-definition.js";
import {
  claimIdentity,
  claimJson,
  INPUT_SET_UNIQUENESS_ARTIFACT,
  INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
  inputItems,
  type InputSetUniquenessArtifact,
  type InputSetUniquenessForcedArtifact,
  InputSetUniquenessForcedSourceSchema,
} from "./input-set-uniqueness.admit-accepted-input-set-uniqueness-artifact.js";
import {
  admitInputSetUniquenessArtifact,
  admitInputSetUniquenessForcedArtifact,
  selectedIdentity,
} from "./input-set-uniqueness.admit-input-set-uniqueness-forced-artifact.js";
import { normalizeJournalJson } from "./journal.js";
import { exactJournalRecord } from "./native-index-artifact.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const prepareInputSetUniquenessArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<InputSetUniquenessArtifact | InputSetUniquenessForcedArtifact> => {
  if (classification.headerHash !== evidence.headerHash) {
    throw new Error("input-set-uniqueness classification changed header");
  }
  if (
    classification.category === "inputSetUniqueness" &&
    classification.selected.violationId ===
      INPUT_SET_UNIQUENESS_WRONGFUL_REJECTION_VIOLATION_ID
  ) {
    const detection = detectInputSetUniquenessForcedReplay(evidence).find(
      (candidate) =>
        candidate.detectionId === classification.selected.detectionId &&
        candidate.position === classification.selected.position,
    );
    if (detection === undefined) {
      throw new Error(
        "input-set-uniqueness forced classification disappeared on replay",
      );
    }
    const transaction =
      evidence.reconstruction.forcedTransactions[detection.forcedIndex];
    if (transaction === undefined) {
      throw new Error("input-set-uniqueness forced leaf disappeared");
    }
    const membership = await buildForcedTransactionLeafMembershipProof({
      reconstruction: evidence.reconstruction,
      eventKey: {
        ForcedTransactionEventKey: { tx_order_id: transaction.key },
      },
    });
    const artifact = normalizeJournalJson({
      schemaVersion: INPUT_SET_UNIQUENESS_FORCED_ARTIFACT,
      headerHash: evidence.headerHash,
      detectionId: detection.detectionId,
      position: detection.forcedIndex,
      forcedIndex: detection.forcedIndex,
      transactionId: detection.transactionId,
      subjectCbor: Data.to(
        detection.bound.subject as never,
        InputSetUniquenessVerdictSubjectSchema as never,
      ),
      nativeTxCompactCbor: transaction.value.submitted_source.compact_cbor,
      spendInputItemCbors: detection.spendInputItemCbors,
      referenceInputItemCbors: detection.referenceInputItemCbors,
      forcedSourceCbor: Data.to(
        { header: evidence.header, membership } as never,
        InputSetUniquenessForcedSourceSchema as never,
      ),
    }) as InputSetUniquenessForcedArtifact;
    admitInputSetUniquenessForcedArtifact(artifact);
    return Object.freeze(artifact);
  }
  const selected = selectedIdentity(classification);
  const decoded = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const tx = decoded.find((candidate) => candidate.nodeTxId === selected.txId);
  if (tx === undefined) {
    throw new Error("input-set-uniqueness selected transaction disappeared");
  }
  const spends = inputItems(tx.nativeTx, "spend");
  const references = inputItems(tx.nativeTx, "reference");
  const claim = scanInputSetUniqueness({
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
  }).find((candidate) =>
    classification.selected.detectionId.endsWith(claimIdentity(candidate)),
  );
  if (claim === undefined) {
    throw new Error("input-set-uniqueness selected claim disappeared");
  }
  const trie = await buildTrieView(decoded.map(transactionSourceTrieItem));
  const artifact = normalizeJournalJson({
    schemaVersion: INPUT_SET_UNIQUENESS_ARTIFACT,
    headerHash: evidence.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    tx: {
      nativeTxId: tx.nodeTxId,
      nativeTxCompactCbor: tx.nativeCompactCbor,
      l2TransactionSourceCbor: tx.l2TransactionSourceCbor,
      transactionsPhasRoot: trie.root,
      txMembershipProofCbor: requireProof(
        trie,
        transactionSourceTrieItem(tx).key,
        "input-set-uniqueness transaction",
      ),
    },
    spendInputItemCbors: spends,
    referenceInputItemCbors: references,
    claim: claimJson(claim),
  }) as InputSetUniquenessArtifact;
  admitInputSetUniquenessArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "inputSetUniqueness",
  (typeof WITNESS_ROLES)[number],
  true
>;

export type InputSetUniquenessWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "inputSetUniqueness",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: InputSetUniquenessContracts;
  category: FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["catalogue"];
  referenceScripts: InputSetUniquenessWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"inputSetUniqueness">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const record = (value: unknown, label: string) =>
  exactJournalRecord(
    value,
    typeof value === "object" && value !== null ? Object.keys(value) : [],
    label,
  );

export const actionInput = (action: FraudProofWorkflowAction) => {
  const input = record(action.input, "input-set-uniqueness action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "inputSetUniqueness" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("input-set-uniqueness action identity changed");
  }
  return input;
};

export const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`input-set-uniqueness action omitted ${field}`);
  }
  return value;
};

export const resolveField = async (
  config: BoundConfig,
  plan: FaultProofFieldOpeningPlan | null,
) => {
  if (plan === null)
    return Object.freeze({ publications: [], certificate: undefined });
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("input-set-uniqueness field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.network,
    planned: plan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("input-set-uniqueness field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
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
        fraudCategory: "inputSetUniqueness",
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
              "input-set-uniqueness removal changed authenticated inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease ? { mutationLease } : {}),
  });
};
