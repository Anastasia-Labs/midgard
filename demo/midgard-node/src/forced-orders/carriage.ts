/**
 * A forced order's §8 carriage, read from its own creating transaction
 * (plan §12.2 to §12.4). Pure: no store, no network.
 *
 * The order's payload commits nine field hashes. Each non-empty one
 * consumes one entry of the mint redeemer's carriage vector, in field
 * order, and the vector must be exhausted exactly (§8.11). An entry is
 * tier 1 (the preimage inline in the redeemer), tier 2 (one raw-bytes datum
 * at a reference input) or tier 3 (chunks at several reference inputs).
 * Reference-input indices address the ledger's sorted set, which is how the
 * follower stores a transaction's reference inputs.
 *
 * Every field read here is hashed whole against its commitment, at every
 * tier: a source can cost an order its ingestion, never buy it one. A tier
 * 3 certificate is never read; the whole-preimage hash makes it redundant
 * for the node (§12.3).
 */
import {
  buildMidgardWholeFieldView,
  encodeMidgardFieldArrayHeader,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  type MidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  midgardTxFieldCommitmentsFromSource,
  reconstructMidgardTransaction,
} from "@al-ft/midgard-core/consensus-validation";
import {
  type OutRef,
  outRefKey,
  type RedeemerSummary,
  type TxSummary,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export type TxOrderPayload = SDK.TxOrderUTxOV1["datum"]["event"]["tx"];

/** The nine-field proof source a payload commits to. */
export const payloadSource = (payload: TxOrderPayload) => ({
  compactCbor: Buffer.from(payload.submitted_source.compact_cbor, "hex"),
  witnessSetCompactCbor: Buffer.from(
    payload.submitted_source.witness_set_compact_cbor,
    "hex",
  ),
  fieldPreimageLengthsCbor: Buffer.from(
    payload.submitted_source.field_preimage_lengths_cbor,
    "hex",
  ),
});

/** The nine committed field hashes, positionally from the payload (§4). */
export const payloadCommitments = (
  payload: TxOrderPayload,
): readonly Buffer[] =>
  midgardTxFieldCommitmentsFromSource(payloadSource(payload), "forced");

/**
 * The redeemer the tx-order policy ran in `tx`: the mint redeemer whose
 * index is the policy's position among the minted policies in ascending
 * order (the ledger's pointer), never "the first mint redeemer". Null when
 * the tx mints nothing under the policy or carries no redeemer for it.
 */
export const txOrderMintRedeemer = (
  tx: Pick<TxSummary, "mint" | "redeemers">,
  policyId: string,
): RedeemerSummary | null => {
  const index = [...tx.mint.keys()].sort().indexOf(policyId);
  if (index < 0) return null;
  return (
    tx.redeemers.find((r) => r.purpose === "mint" && r.index === index) ?? null
  );
};

const exactIndex = (value: bigint): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error(
      `reference-input index ${value.toString()} is out of range`,
    );
  return Number(value);
};

/** The carriage vector of a tx-order mint redeemer (the SDK's schema). */
export const carriageVector = (
  redeemerCbor: Buffer,
): readonly MidgardFieldCarriage[] =>
  (
    Data.from(
      redeemerCbor.toString("hex"),
      SDK.TxOrderMintRedeemer,
    ) as SDK.TxOrderMintRedeemer
  ).material_carriage.map((entry): MidgardFieldCarriage => {
    if ("Inline" in entry)
      return {
        carriage: "Inline",
        preimage: Buffer.from(entry.Inline.preimage, "hex"),
      };
    if ("RawUtxo" in entry)
      return {
        carriage: "RawUtxo",
        refInputIndex: exactIndex(entry.RawUtxo.ref_input_index),
      };
    return {
      carriage: "Certified",
      certRefInputIndex: exactIndex(entry.Certified.cert_ref_input_index),
      chunkRefInputIndices:
        entry.Certified.chunk_ref_input_indices.map(exactIndex),
    };
  });

const referenceInputAt = (
  referenceInputs: readonly OutRef[],
  index: number,
): OutRef => {
  const outRef = referenceInputs[index];
  if (outRef === undefined)
    throw new Error(
      `carriage names reference input ${index.toString()} of ${referenceInputs.length.toString()}`,
    );
  return outRef;
};

/**
 * The outrefs whose datums the carriage reads: each tier 2 input and each
 * tier 3 chunk, in vector order. Never a certificate. Throws when an index
 * names no reference input.
 */
export const carriageOutRefs = (
  carriage: readonly MidgardFieldCarriage[],
  referenceInputs: readonly OutRef[],
): OutRef[] => {
  const seen = new Map<string, OutRef>();
  for (const entry of carriage) {
    const indices =
      entry.carriage === "Inline"
        ? []
        : entry.carriage === "RawUtxo"
          ? [entry.refInputIndex]
          : entry.chunkRefInputIndices;
    for (const index of indices) {
      const outRef = referenceInputAt(referenceInputs, index);
      seen.set(outRefKey(outRef), outRef);
    }
  }
  return [...seen.values()];
};

/** A carriage output's inline datum (Plutus data CBOR), or null. */
export type DatumOf = (outRef: OutRef) => Buffer | null;

const rawBytes = (
  referenceInputs: readonly OutRef[],
  index: number,
  datumOf: DatumOf,
): Buffer => {
  const outRef = referenceInputAt(referenceInputs, index);
  const datum = datumOf(outRef);
  if (datum === null)
    throw new Error(
      `carriage output ${outRef.txHash.toString("hex")}#${outRef.index.toString()} has no inline datum`,
    );
  return SDK.fieldPreimagePublicationBytes(datum.toString("hex"));
};

/**
 * The nine field preimages the carriage supplies, each hashed whole against
 * its commitment (`buildMidgardWholeFieldView`: hash, aggregate bound,
 * grammar). Empty fields take the empty preimage and no entry. Throws on a
 * short or long vector, an unresolvable index, a datum that is not raw
 * bytes, or a preimage that does not match its commitment.
 */
export const carriageFieldPreimages = (input: {
  readonly payload: TxOrderPayload;
  readonly carriage: readonly MidgardFieldCarriage[];
  readonly referenceInputs: readonly OutRef[];
  readonly datumOf: DatumOf;
}): Buffer[] => {
  const empty = encodeMidgardFieldArrayHeader(0);
  const unconsumed = [...input.carriage];
  const preimages = payloadCommitments(input.payload).map(
    (commitment, fieldIndex) => {
      if (commitment.equals(MIDGARD_EMPTY_FIELD_COMMITMENT)) return empty;
      const entry = unconsumed.shift();
      if (entry === undefined)
        throw new Error(
          `forced order carries material in field ${fieldIndex.toString()} with no §8 carriage for it (${input.carriage.length.toString()} supplied)`,
        );
      const preimage =
        entry.carriage === "Inline"
          ? entry.preimage
          : entry.carriage === "RawUtxo"
            ? rawBytes(
                input.referenceInputs,
                entry.refInputIndex,
                input.datumOf,
              )
            : Buffer.concat(
                entry.chunkRefInputIndices.map((index) =>
                  rawBytes(input.referenceInputs, index, input.datumOf),
                ),
              );
      buildMidgardWholeFieldView({
        fieldIndex,
        preimage,
        expectedCommitment: commitment,
      });
      return Buffer.from(preimage);
    },
  );
  if (unconsumed.length > 0)
    throw new Error(
      `forced order supplied ${unconsumed.length.toString()} §8 carriage entries more than its commitments name`,
    );
  return preimages;
};

/**
 * Reconstructs the canonical native transaction an order committed to from
 * its nine field preimages. `reconstructMidgardTransaction` re-checks each
 * preimage against the compact structures and re-derives the id and the
 * transaction commitment from the same bytes.
 */
export const reconstructTxOrderMaterial = (input: {
  readonly payload: TxOrderPayload;
  readonly fieldPreimages: readonly Uint8Array[];
}): Buffer =>
  reconstructMidgardTransaction({
    sourceKind: "forced",
    transactionId: Buffer.from(input.payload.tx_id, "hex"),
    transactionCommitment: Buffer.from(
      input.payload.transaction_commitment,
      "hex",
    ),
    source: payloadSource(input.payload),
    fieldPreimages: input.fieldPreimages,
  });

/** Nine preimages as stored: each a u32 big-endian length, then its bytes. */
export const encodeFieldPreimages = (
  preimages: readonly Uint8Array[],
): Buffer =>
  Buffer.concat(
    preimages.flatMap((preimage) => {
      const length = Buffer.alloc(4);
      length.writeUInt32BE(preimage.length);
      return [length, Buffer.from(preimage)];
    }),
  );

export const decodeFieldPreimages = (bytes: Buffer): Buffer[] => {
  const preimages: Buffer[] = [];
  let at = 0;
  while (at < bytes.length) {
    if (at + 4 > bytes.length) throw new Error("truncated field preimages");
    const length = bytes.readUInt32BE(at);
    at += 4;
    if (at + length > bytes.length)
      throw new Error("truncated field preimages");
    preimages.push(bytes.subarray(at, at + length));
    at += length;
  }
  return preimages;
};
