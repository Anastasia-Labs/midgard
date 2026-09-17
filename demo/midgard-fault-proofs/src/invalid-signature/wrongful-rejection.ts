import { deriveMidgardNativeTxWitnessSetCompact } from "@al-ft/midgard-core";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core/codec/forced";
import {
  decodeAddressWitnessPreimage,
  forcedVerdictSubject,
  invalidSignatureTerminalContradiction,
  type MidgardAddressWitness,
  type NativeTxWitnessSetCompact,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";

export const INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID =
  "invalid-signature-wrongful-rejection" as const;
export type InvalidSignatureWrongfulRejectionEvidence = Readonly<{
  subject: VerdictSubject;
  witnessIndex: bigint;
  witnessSetHash: string;
  witnessSet: NativeTxWitnessSetCompact;
  addressWitnesses: readonly MidgardAddressWitness[];
  nativeTxCompactCbor: string;
}>;
export const invalidSignatureWrongfulRejectionCloses = (
  evidence: InvalidSignatureWrongfulRejectionEvidence,
): boolean => invalidSignatureTerminalContradiction(evidence);

export const invalidSignatureEvidenceFromForcedSource = (
  forced: Pick<
    CanonicalBlockEvidence["reconstruction"]["forcedTransactions"][number],
    "key" | "value" | "fullTransactionCbor"
  >,
): InvalidSignatureWrongfulRejectionEvidence | null => {
  if (forced === undefined || forced.value.verdict === "ForcedTxValid")
    return null;
  const reason = forced.value.verdict.ForcedTxInvalid.reason;
  if (
    typeof reason !== "object" ||
    !("AddressWitnessSignatureInvalid" in reason)
  )
    return null;
  const decoded = decodeMidgardForcedTxFullFromCanonicalCbor(
    forced.fullTransactionCbor,
  );
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    forced.fullTransactionCbor,
  );
  if (
    material.transactionId.toString("hex") !== forced.value.tx_id ||
    material.proofSource.compactCbor.toString("hex") !==
      forced.value.submitted_source.compact_cbor ||
    material.proofSource.witnessSetCompactCbor.toString("hex") !==
      forced.value.submitted_source.witness_set_compact_cbor ||
    material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.submitted_source.field_preimage_lengths_cbor
  )
    throw new Error(
      "invalidSignature: forced preimage differs from authenticated leaf",
    );
  const compact = deriveMidgardNativeTxWitnessSetCompact(decoded.witnessSet);
  return Object.freeze({
    subject: forcedVerdictSubject({
      transactionId: forced.value.tx_id,
      sourceKey: forced.key,
      rejectionReason: reason,
    }),
    witnessIndex: reason.AddressWitnessSignatureInvalid.witness_index,
    witnessSetHash: material.compact.transactionWitnessSetHash.toString("hex"),
    witnessSet: {
      addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
      script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
      redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
    },
    addressWitnesses: Object.freeze(
      decodeAddressWitnessPreimage(decoded.witnessSet.addrTxWitsPreimageCbor),
    ),
    nativeTxCompactCbor: forced.value.submitted_source.compact_cbor,
  });
};
export const detectInvalidSignatureWrongfulRejections = ({
  block,
}: Readonly<{ block: CanonicalBlockEvidence }>) =>
  Object.freeze(
    block.reconstruction.forcedTransactions.flatMap((_, forcedIndex) => {
      const evidence = invalidSignatureEvidenceFromForcedSource(
        block.reconstruction.forcedTransactions[forcedIndex]!,
      );
      return evidence !== null &&
        invalidSignatureWrongfulRejectionCloses(evidence)
        ? [
            Object.freeze({
              detectionId: `${INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID}:${forcedIndex.toString()}:${evidence.subject.transaction_id}`,
              headerHash: block.headerHash,
              violationId: INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID,
              position: BigInt(forcedIndex),
              forcedIndex,
              transactionId: evidence.subject.transaction_id,
              evidence,
            }),
          ]
        : [];
    }),
  );
export const prepareInvalidSignatureWrongfulRejection = async ({
  block,
}: Readonly<{ block: CanonicalBlockEvidence }>) => {
  const detection = detectInvalidSignatureWrongfulRejections({ block })[0];
  if (detection === undefined)
    throw new Error("invalidSignature: no authenticated wrongful rejection");
  const forced =
    block.reconstruction.forcedTransactions[detection.forcedIndex]!;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: forced.key } },
  });
  return Object.freeze({
    headerHash: block.headerHash,
    evidence: detection.evidence,
    forcedSource: Object.freeze({
      header: block.header,
      membership,
      direction: 1n,
    }),
  });
};
export type PreparedInvalidSignatureWrongfulRejection = Awaited<
  ReturnType<typeof prepareInvalidSignatureWrongfulRejection>
>;
