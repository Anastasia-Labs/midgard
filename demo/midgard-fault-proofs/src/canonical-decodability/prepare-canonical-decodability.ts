/**
 * Pure, fail-closed canonical-decodability evidence preparation.
 *
 * This is the watcher-facing nucleus: it authenticates the accused compact and
 * field bytes before emitting either the wire claim or the next-step state.
 */
import {
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
} from "@al-ft/midgard-core";
import {
  type CanonicalDecodabilityEvidence,
  canonicalDecodabilityEvidenceFromCommittedField,
  type CanonicalDecodabilityStep02State,
  canonicalDecodabilityStep02StateFromEvidence,
  type CommittedFieldClaim,
  type FieldCarriage,
  isMidgardWitnessSetField,
  MIDGARD_COMMITTED_FIELD_COUNT,
  MIDGARD_FIELD_INDEX,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import { canonicalDecodabilitySubmitError } from "./submit-common.js";

export type PreparedCanonicalDecodability = {
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: string;
  readonly committedFieldHash: string;
  readonly evidence: CanonicalDecodabilityEvidence;
  readonly claim: CommittedFieldClaim;
  readonly step02State: CanonicalDecodabilityStep02State;
};

const requireHash32 = (value: string, label: string): string => {
  const normalized = value.toLowerCase();
  if (!/^[0-9a-f]{64}$/u.test(normalized)) {
    throw canonicalDecodabilitySubmitError(
      `${label} must be a 32-byte hexadecimal hash.`,
    );
  }
  return normalized;
};

const requireHex = (value: string, label: string): string => {
  const normalized = value.toLowerCase();
  if (!/^(?:[0-9a-f]{2})+$/u.test(normalized)) {
    throw canonicalDecodabilitySubmitError(
      `${label} must be non-empty even-length hexadecimal.`,
    );
  }
  return normalized;
};

const witnessSetHash = (witnessSet: NativeTxWitnessSetCompact): string =>
  computeHash32(
    encodeMidgardNativeTxWitnessSetCompact({
      addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
      scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
      redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
    }),
  ).toString("hex");

/** Positional twin of `native_tx_field_access_v1.field_commitment_at`. */
const committedFieldHashV1 = ({
  fieldIndex,
  compact,
  witnessSet,
}: {
  readonly fieldIndex: number;
  readonly compact: ReturnType<typeof decodeMidgardNativeTxCompact>;
  readonly witnessSet?: NativeTxWitnessSetCompact;
}): string => {
  if (isMidgardWitnessSetField(fieldIndex)) {
    if (witnessSet === undefined) {
      throw canonicalDecodabilitySubmitError(
        `field ${fieldIndex.toString()} is a witness-set field, but no compact witness set was supplied.`,
      );
    }
    const computedWitnessSetHash = witnessSetHash(witnessSet);
    const committedWitnessSetHash = Buffer.from(
      compact.transactionWitnessSetHash,
    ).toString("hex");
    if (computedWitnessSetHash !== committedWitnessSetHash) {
      throw canonicalDecodabilitySubmitError(
        `compact witness set hashes to ${computedWitnessSetHash}, not the committed witness_set_hash ${committedWitnessSetHash}.`,
      );
    }
    const byIndex: Readonly<Record<number, string>> = {
      [MIDGARD_FIELD_INDEX.scriptWitnesses]: witnessSet.script_tx_wits_hash,
      [MIDGARD_FIELD_INDEX.addressWitnesses]: witnessSet.addr_tx_wits_hash,
      [MIDGARD_FIELD_INDEX.redeemers]: witnessSet.redeemer_tx_wits_hash,
    };
    const commitment = byIndex[fieldIndex];
    if (commitment === undefined) {
      throw canonicalDecodabilitySubmitError(
        `field ${fieldIndex.toString()} has no witness-set commitment.`,
      );
    }
    return requireHash32(
      commitment,
      `field ${fieldIndex.toString()} commitment`,
    );
  }
  if (witnessSet !== undefined) {
    throw canonicalDecodabilitySubmitError(
      `field ${fieldIndex.toString()} is a body field and must not carry a compact witness set.`,
    );
  }
  const body = compact.transactionBody;
  const commitments = [
    body.spendInputsHash,
    body.referenceInputsHash,
    body.outputsHash,
    body.requiredObserversHash,
    body.requiredSignersHash,
    body.mintHash,
  ];
  const commitment = commitments[fieldIndex];
  if (commitment === undefined) {
    throw canonicalDecodabilitySubmitError(
      `field ${fieldIndex.toString()} has no body commitment.`,
    );
  }
  return Buffer.from(commitment).toString("hex");
};

export const prepareCanonicalDecodability = ({
  badTxId,
  nativeTxCompactCbor,
  fieldIndex,
  committedPreimage,
  witnessSet,
  carriage,
}: {
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  /**
   * Exact §8 carriage resolved against the step transaction's complete
   * reference-input set. Omit only for an inline-sized preimage.
   */
  readonly carriage?: FieldCarriage;
}): PreparedCanonicalDecodability => {
  const normalizedBadTxId = requireHash32(badTxId, "bad transaction id");
  if (
    !Number.isInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_COMMITTED_FIELD_COUNT
  ) {
    throw canonicalDecodabilitySubmitError(
      `field index ${fieldIndex.toString()} is outside 0..${(MIDGARD_COMMITTED_FIELD_COUNT - 1).toString()}.`,
    );
  }
  const normalizedCompactCbor = requireHex(
    nativeTxCompactCbor,
    "native compact transaction CBOR",
  );
  let compact: ReturnType<typeof decodeMidgardNativeTxCompact>;
  try {
    compact = decodeMidgardNativeTxCompact(
      Buffer.from(normalizedCompactCbor, "hex"),
    );
  } catch (cause) {
    throw canonicalDecodabilitySubmitError(
      `native compact transaction CBOR does not decode: ${String(cause)}`,
    );
  }
  if (
    encodeMidgardNativeTxCompact(compact).toString("hex") !==
    normalizedCompactCbor
  ) {
    throw canonicalDecodabilitySubmitError(
      "native compact transaction CBOR is not canonical.",
    );
  }
  const derivedTxId = computeMidgardNativeTxId(compact).toString("hex");
  if (derivedTxId !== normalizedBadTxId) {
    throw canonicalDecodabilitySubmitError(
      `native compact transaction re-derives to ${derivedTxId}, not the committed key ${normalizedBadTxId}.`,
    );
  }

  const committedFieldHash = committedFieldHashV1({
    fieldIndex,
    compact,
    ...(witnessSet === undefined ? {} : { witnessSet }),
  });
  const preimage = Buffer.from(committedPreimage);
  if (
    carriage === undefined &&
    preimage.length > MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES
  ) {
    throw canonicalDecodabilitySubmitError(
      `committed preimage is ${preimage.length.toString()} bytes, above the ${MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES.toString()}-byte inline frontier; publish a RawUtxo/Certified carriage before submission.`,
    );
  }
  const carriedHash = computeHash32(preimage).toString("hex");
  if (carriedHash !== committedFieldHash) {
    throw canonicalDecodabilitySubmitError(
      `carried field bytes hash to ${carriedHash}, not the positional commitment ${committedFieldHash} at field ${fieldIndex.toString()}.`,
    );
  }

  const evidence = canonicalDecodabilityEvidenceFromCommittedField({
    badTxId: normalizedBadTxId,
    fieldIndex,
    committedPreimage: preimage,
  });
  if (!evidence.isViolation) {
    throw canonicalDecodabilitySubmitError(
      `field ${fieldIndex.toString()} has verdict 0 (grammatical); a valid block cannot be challenged.`,
    );
  }
  const resolvedCarriage =
    carriage ?? ({ Inline: { preimage: preimage.toString("hex") } } as const);
  const claim: CommittedFieldClaim = isMidgardWitnessSetField(fieldIndex)
    ? {
        WitnessFieldClaim: {
          field_index: BigInt(fieldIndex),
          witness_set: witnessSet!,
          carriage: resolvedCarriage,
        },
      }
    : {
        BodyFieldClaim: {
          field_index: BigInt(fieldIndex),
          carriage: resolvedCarriage,
        },
      };
  return Object.freeze({
    badTxId: normalizedBadTxId,
    nativeTxCompactCbor: normalizedCompactCbor,
    fieldIndex,
    committedPreimage: preimage.toString("hex"),
    committedFieldHash,
    evidence,
    claim,
    step02State: canonicalDecodabilityStep02StateFromEvidence(evidence),
  });
};
