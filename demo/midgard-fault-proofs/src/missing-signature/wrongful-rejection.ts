/** Authenticated RequiredSignerUnsigned contradiction and durable evidence. */
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  decodeAddressWitnessPreimage,
  forcedVerdictSubject,
  type MidgardAddressWitness,
  missingSignatureVkeyHash,
  type VerdictSubject,
  verdictSubjectIsCanonical,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { deriveRejectedTransactionFaultEvidenceMaterial } from "../evidence/rejected-transaction.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";

export const MISSING_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID =
  "missing-signature-wrongful-rejection" as const;
export type MissingSignatureWrongfulRejectionEvidence = Readonly<{
  subject: VerdictSubject;
  signerIndex: bigint;
  requiredSignerHashes: readonly string[];
  addrTxWits: readonly MidgardAddressWitness[];
}>;

/** Missing coordinate or a genuinely signed required key contradicts this reason. */
export const missingSignatureWrongfulRejectionWitnessIndex = (
  evidence: MissingSignatureWrongfulRejectionEvidence,
): bigint | null => {
  const { signerIndex, requiredSignerHashes, addrTxWits, subject } = evidence;
  const reason = subject.rejection_reason;
  if (
    !verdictSubjectIsCanonical(subject) ||
    subject.direction !== 1n ||
    subject.source_kind !== 1n ||
    reason === null ||
    typeof reason !== "object" ||
    !("RequiredSignerUnsigned" in reason) ||
    reason.RequiredSignerUnsigned.signer_index !== signerIndex
  )
    return null;
  if (signerIndex < 0n || signerIndex >= BigInt(requiredSignerHashes.length))
    return -1n;
  const hash = requiredSignerHashes[Number(signerIndex)];
  const index = addrTxWits.findIndex((witness) => {
    if (missingSignatureVkeyHash(witness.verification_key) !== hash)
      return false;
    return verifyAddressWitness({ txId: subject.transaction_id, witness });
  });
  return index < 0 ? null : BigInt(index);
};
export const missingSignatureWrongfulRejectionCloses = (
  evidence: MissingSignatureWrongfulRejectionEvidence,
): boolean => missingSignatureWrongfulRejectionWitnessIndex(evidence) !== null;

export const detectMissingSignatureForcedTransaction = (
  forced: Pick<
    CanonicalBlockEvidence["reconstruction"]["forcedTransactions"][number],
    "key" | "value" | "fullTransactionCbor"
  >,
  headerHash: string,
  forcedIndex: number,
) => {
  if (forced.value.verdict === "ForcedTxValid") return [];
  const reason = forced.value.verdict.ForcedTxInvalid.reason;
  if (typeof reason !== "object" || !("RequiredSignerUnsigned" in reason))
    return [];
  const material = deriveRejectedTransactionFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.transactionId.toString("hex") !== forced.value.tx_id ||
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.source.field_preimage_lengths_cbor
  )
    throw new Error(
      "missingSignature: retained forced transaction differs from its authenticated leaf",
    );
  const decoded = material.canonical;
  const evidence: MissingSignatureWrongfulRejectionEvidence = {
    subject: forcedVerdictSubject({
      transactionId: forced.value.tx_id,
      sourceKey: forced.key,
      rejectionReason: reason,
    }),
    signerIndex: reason.RequiredSignerUnsigned.signer_index,
    requiredSignerHashes: decodeMidgardNativeByteListPreimage(
      decoded.body.requiredSignersPreimageCbor,
    ).map((hash) => Buffer.from(hash).toString("hex")),
    addrTxWits: decodeAddressWitnessPreimage(
      decoded.witnessSet.addrTxWitsPreimageCbor,
    ),
  };
  const witnessIndex = missingSignatureWrongfulRejectionWitnessIndex(evidence);
  return witnessIndex === null
    ? []
    : [
        {
          detectionId: `${MISSING_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID}:${forcedIndex}:${forced.value.tx_id}`,
          headerHash,
          violationId: MISSING_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID,
          position: BigInt(forcedIndex),
          forcedIndex,
          transactionId: forced.value.tx_id,
          evidence,
          witnessIndex,
        },
      ];
};
export const detectMissingSignatureWrongfulRejections = ({
  block,
}: {
  readonly block: CanonicalBlockEvidence;
}) =>
  block.reconstruction.forcedTransactions.flatMap((forced, index) =>
    detectMissingSignatureForcedTransaction(forced, block.headerHash, index),
  );

export const prepareMissingSignatureWrongfulRejection = async ({
  block,
}: {
  readonly block: CanonicalBlockEvidence;
}) => {
  const detection = detectMissingSignatureWrongfulRejections({ block })[0];
  if (detection === undefined)
    throw new Error("missingSignature: no authenticated wrongful rejection");
  const forced =
    block.reconstruction.forcedTransactions[detection.forcedIndex]!;
  const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
    forced.fullTransactionCbor,
  );
  const witnessSet = deriveMidgardNativeTxWitnessSetCompact(decoded.witnessSet);
  return {
    ...detection,
    nativeTxCompactCbor: forced.value.source.compact_cbor,
    witnessSetCompact: {
      addr_tx_wits_hash: witnessSet.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: witnessSet.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: witnessSet.redeemerTxWitsHash.toString("hex"),
    },
    verifiedWitnessSetHash: deriveRejectedTransactionFaultEvidenceMaterial(
      forced.fullTransactionCbor,
    ).compact.transactionWitnessSetHash.toString("hex"),
    forcedSource: {
      header: block.header,
      membership: await buildForcedTransactionLeafMembershipProof({
        reconstruction: block.reconstruction,
        eventKey: { ForcedTransactionEventKey: { tx_order_id: forced.key } },
      }),
      direction: 1n,
    },
  };
};
export type PreparedMissingSignatureWrongfulRejection = Awaited<
  ReturnType<typeof prepareMissingSignatureWrongfulRejection>
>;
