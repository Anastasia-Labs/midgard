import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import {
  encodeMidgardAddressWitnessCanonical,
  INVALID_SIGNATURE_VIOLATION_ID,
  invalidSignatureAddressWitnessesCommitment,
  invalidSignatureWitnessSetCommitment,
  MIDGARD_FIELD_INDEX,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import { prepareInvalidSignatureFromCanonicalEvidence } from "../evidence/prepare-from-evidence.js";
import { planFaultProofFieldOpening } from "../field-opening.js";
import {
  admitInvalidSignatureForcedArtifact,
  INVALID_SIGNATURE_FORCED_ARTIFACT,
} from "../invalid-signature/artifact.js";
import { type StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import { type LinearFamilyReferenceScripts } from "./family-definition.js";
import {
  type AdmittedArtifact,
  EVEN_HEX,
  exact,
  hex,
  HEX_28,
  HEX_32,
  INVALID_SIGNATURE_ARTIFACT,
  type InvalidSignatureArtifact,
  NATURAL,
  parseAddressWitnesses,
  parseWitnessSet,
  proofSteps,
  record,
  safeNatural,
} from "./invalid-signature.parse-address-witnesses.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";

export const admitInvalidSignatureArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("invalid-signature carriage owner is malformed");
  }
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
      "witnessSet",
      "addressWitnesses",
      "badWitnessIndex",
    ],
    "invalid-signature artifact",
  );
  if (
    parsed.schemaVersion !== INVALID_SIGNATURE_ARTIFACT ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("invalid-signature artifact identity changed");
  }
  const witnessSet = parseWitnessSet(parsed.witnessSet);
  const addressWitnesses = parseAddressWitnesses(parsed.addressWitnesses);
  const artifact = Object.freeze({
    schemaVersion: INVALID_SIGNATURE_ARTIFACT,
    headerHash: hex(parsed.headerHash, HEX_28, "artifact header hash"),
    detectionId: parsed.detectionId,
    position: safeNatural(parsed.position, "artifact position"),
    nativeTxId: hex(parsed.nativeTxId, HEX_32, "artifact transaction id"),
    nativeTxCompactCbor: hex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      "artifact compact transaction",
    ),
    l2TransactionSourceCbor: hex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      "artifact transaction source",
    ),
    transactionsPhasRoot: hex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "artifact transactions PHAS root",
    ),
    txMembershipProofCbor: hex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "artifact membership proof",
    ),
    witnessSet,
    addressWitnesses,
    badWitnessIndex: safeNatural(
      parsed.badWitnessIndex,
      "artifact bad witness index",
    ),
  }) satisfies InvalidSignatureArtifact;
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompact(
        Buffer.from(artifact.nativeTxCompactCbor, "hex"),
      ),
    ),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProofCbor: artifact.txMembershipProofCbor,
  });
  let openedRoot: Buffer | null;
  try {
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    throw new Error("invalid-signature membership proof cannot be replayed");
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error(
      "invalid-signature membership proof does not open its PHAS root",
    );
  }
  if (
    invalidSignatureWitnessSetCommitment(witnessSet) !==
      inclusion.nativeTx.witness_set_hash ||
    invalidSignatureAddressWitnessesCommitment(addressWitnesses) !==
      witnessSet.addr_tx_wits_hash
  ) {
    throw new Error(
      "invalid-signature witness material does not open the committed transaction",
    );
  }
  const badWitness = addressWitnesses[artifact.badWitnessIndex];
  if (
    badWitness === undefined ||
    verifyAddressWitness({ txId: artifact.nativeTxId, witness: badWitness }) ||
    artifact.detectionId !==
      `${INVALID_SIGNATURE_VIOLATION_ID}:${artifact.position.toString()}:${artifact.badWitnessIndex.toString()}:${artifact.nativeTxId}:${badWitness.verification_key}`
  ) {
    throw new Error(
      "invalid-signature artifact does not re-derive its selected violation",
    );
  }
  const fieldPlan = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: artifact.nativeTxId,
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    itemCbors: addressWitnesses.map(encodeMidgardAddressWitnessCanonical),
    owner: carriageOwner,
    publish: false,
    witnessSet,
    anchorWitnessSetHash: inclusion.nativeTx.witness_set_hash,
    label: "invalid-signature artifact address witnesses",
  });
  return Object.freeze({
    artifact,
    inclusion,
    witnessSet,
    addressWitnesses,
    fieldPlan,
  });
};

export const admitWorkflowArtifact = async (
  artifact: JournalJsonObject,
  owner: string,
) => {
  if (artifact.schemaVersion !== INVALID_SIGNATURE_FORCED_ARTIFACT)
    return { ...admitInvalidSignatureArtifact(artifact, owner), forced: null };
  const prepared = await admitInvalidSignatureForcedArtifact(artifact);
  const { evidence } = prepared;
  return {
    forced: prepared,
    artifact: {
      headerHash: prepared.headerHash,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      badWitnessIndex: evidence.witnessIndex,
      txMembershipProofCbor: "",
    },
    inclusion: null,
    witnessSet: evidence.witnessSet,
    addressWitnesses: evidence.addressWitnesses,
    fieldPlan: planFaultProofFieldOpening({
      anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
      fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
      anchorTxId: evidence.subject.transaction_id,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      itemCbors: evidence.addressWitnesses.map(
        encodeMidgardAddressWitnessCanonical,
      ),
      owner,
      witnessSet: evidence.witnessSet,
      anchorWitnessSetHash: evidence.witnessSetHash,
      label: "invalid-signature forced artifact",
    }),
  };
};

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  if (
    classification.category !== "invalidSignature" ||
    classification.selected.violationId !== INVALID_SIGNATURE_VIOLATION_ID ||
    fields.length !== 5 ||
    fields[0] !== INVALID_SIGNATURE_VIOLATION_ID ||
    !NATURAL.test(fields[1] ?? "") ||
    !NATURAL.test(fields[2] ?? "") ||
    !HEX_32.test(fields[3] ?? "") ||
    !HEX_32.test(fields[4] ?? "") ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("invalid-signature classification identity is malformed");
  }
  return Object.freeze({
    transactionIndex: Number(fields[1]),
    witnessIndex: Number(fields[2]),
    txId: fields[3]!,
    verificationKey: fields[4]!,
  });
};

export const prepareInvalidSignatureArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareInvalidSignatureFromCanonicalEvidence
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<InvalidSignatureArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "invalid-signature classification differs from canonical evidence",
    );
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareInvalidSignatureFromCanonicalEvidence({
    evidence,
    txId: selected.txId,
  });
  if (
    prepared.tx.badAddrTxWitIndex !== selected.witnessIndex ||
    prepared.tx.badAddrTxWitVerificationKey !== selected.verificationKey
  ) {
    throw new Error(
      "invalid-signature prepared evidence changed the selected witness",
    );
  }
  const artifact = normalizeJournalJson({
    schemaVersion: INVALID_SIGNATURE_ARTIFACT,
    headerHash: prepared.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.transactionIndex,
    nativeTxId: prepared.tx.nodeTxId,
    nativeTxCompactCbor: prepared.tx.nativeTxCompactCbor,
    l2TransactionSourceCbor: prepared.tx.txInclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: prepared.transactionsPhasRoot,
    txMembershipProofCbor: prepared.tx.txInclusion.txMembershipProofCbor,
    witnessSet: prepared.tx.badTxWitnessSetCompact,
    addressWitnesses: prepared.tx.addrTxWitsPreimage,
    badWitnessIndex: prepared.tx.badAddrTxWitIndex,
  }) as InvalidSignatureArtifact;
  admitInvalidSignatureArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

export type InvalidSignatureWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "invalidSignature",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"invalidSignature">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: InvalidSignatureWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"invalidSignature">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

export const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "invalid-signature workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "invalidSignature" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("invalid-signature workflow action changed identity");
  }
  return input;
};
