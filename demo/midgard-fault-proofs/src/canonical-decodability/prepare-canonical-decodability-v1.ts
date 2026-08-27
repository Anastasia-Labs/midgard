/**
 * Pure, fail-closed canonical-decodability evidence preparation.
 *
 * This is the watcher-facing nucleus: it authenticates the accused compact and
 * field bytes before emitting either the wire claim or the next-step state.
 */
import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
} from "@al-ft/midgard-core";
import {
  canonicalDecodabilityEvidenceFromCommittedFieldV1,
  type CanonicalDecodabilityEvidenceV1,
  type CanonicalDecodabilityStep02State,
  canonicalDecodabilityStep02StateFromEvidenceV1,
  type CommittedFieldClaimV1,
  isMidgardWitnessSetFieldV1,
  MIDGARD_COMMITTED_FIELD_COUNT_V1,
  MIDGARD_FIELD_INDEX_V1,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import { canonicalDecodabilitySubmitError } from "./submit-common-v1.js";

export type PreparedCanonicalDecodabilityV1 = {
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: string;
  readonly committedFieldHash: string;
  readonly evidence: CanonicalDecodabilityEvidenceV1;
  readonly claim: CommittedFieldClaimV1;
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

const witnessSetHashV1 = (witnessSet: NativeTxWitnessSetCompact): string =>
  computeHash32(
    encodeMidgardNativeTxWitnessSetCompactV1({
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
  readonly compact: ReturnType<typeof decodeMidgardNativeTxCompactV1>;
  readonly witnessSet?: NativeTxWitnessSetCompact;
}): string => {
  if (isMidgardWitnessSetFieldV1(fieldIndex)) {
    if (witnessSet === undefined) {
      throw canonicalDecodabilitySubmitError(
        `field ${fieldIndex.toString()} is a witness-set field, but no compact witness set was supplied.`,
      );
    }
    const computedWitnessSetHash = witnessSetHashV1(witnessSet);
    const committedWitnessSetHash = Buffer.from(
      compact.transactionWitnessSetHash,
    ).toString("hex");
    if (computedWitnessSetHash !== committedWitnessSetHash) {
      throw canonicalDecodabilitySubmitError(
        `compact witness set hashes to ${computedWitnessSetHash}, not the committed witness_set_hash ${committedWitnessSetHash}.`,
      );
    }
    const byIndex: Readonly<Record<number, string>> = {
      [MIDGARD_FIELD_INDEX_V1.scriptWitnesses]: witnessSet.script_tx_wits_hash,
      [MIDGARD_FIELD_INDEX_V1.addressWitnesses]: witnessSet.addr_tx_wits_hash,
      [MIDGARD_FIELD_INDEX_V1.redeemers]: witnessSet.redeemer_tx_wits_hash,
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

export const prepareCanonicalDecodabilityV1 = ({
  badTxId,
  nativeTxCompactCbor,
  fieldIndex,
  committedPreimage,
  witnessSet,
}: {
  readonly badTxId: string;
  readonly nativeTxCompactCbor: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
  readonly witnessSet?: NativeTxWitnessSetCompact;
}): PreparedCanonicalDecodabilityV1 => {
  const normalizedBadTxId = requireHash32(badTxId, "bad transaction id");
  if (
    !Number.isInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_COMMITTED_FIELD_COUNT_V1
  ) {
    throw canonicalDecodabilitySubmitError(
      `field index ${fieldIndex.toString()} is outside 0..${(MIDGARD_COMMITTED_FIELD_COUNT_V1 - 1).toString()}.`,
    );
  }
  const normalizedCompactCbor = requireHex(
    nativeTxCompactCbor,
    "native compact transaction CBOR",
  );
  let compact: ReturnType<typeof decodeMidgardNativeTxCompactV1>;
  try {
    compact = decodeMidgardNativeTxCompactV1(
      Buffer.from(normalizedCompactCbor, "hex"),
    );
  } catch (cause) {
    throw canonicalDecodabilitySubmitError(
      `native compact transaction CBOR does not decode: ${String(cause)}`,
    );
  }
  if (
    encodeMidgardNativeTxCompactV1(compact).toString("hex") !==
    normalizedCompactCbor
  ) {
    throw canonicalDecodabilitySubmitError(
      "native compact transaction CBOR is not canonical.",
    );
  }
  const derivedTxId = computeMidgardNativeTxIdV1(compact).toString("hex");
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
  if (preimage.length > MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1) {
    throw canonicalDecodabilitySubmitError(
      `committed preimage is ${preimage.length.toString()} bytes, above the ${MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1.toString()}-byte inline frontier; publish a RawUtxo/Certified carriage before submission.`,
    );
  }
  const carriedHash = computeHash32(preimage).toString("hex");
  if (carriedHash !== committedFieldHash) {
    throw canonicalDecodabilitySubmitError(
      `carried field bytes hash to ${carriedHash}, not the positional commitment ${committedFieldHash} at field ${fieldIndex.toString()}.`,
    );
  }

  const evidence = canonicalDecodabilityEvidenceFromCommittedFieldV1({
    badTxId: normalizedBadTxId,
    fieldIndex,
    committedPreimage: preimage,
  });
  if (!evidence.isViolation) {
    throw canonicalDecodabilitySubmitError(
      `field ${fieldIndex.toString()} has verdict 0 (grammatical); a valid block cannot be challenged.`,
    );
  }
  const carriage = { Inline: { preimage: preimage.toString("hex") } } as const;
  const claim: CommittedFieldClaimV1 = isMidgardWitnessSetFieldV1(fieldIndex)
    ? {
        WitnessFieldClaim: {
          field_index: BigInt(fieldIndex),
          witness_set: witnessSet!,
          carriage,
        },
      }
    : {
        BodyFieldClaim: {
          field_index: BigInt(fieldIndex),
          carriage,
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
    step02State: canonicalDecodabilityStep02StateFromEvidenceV1(evidence),
  });
};
