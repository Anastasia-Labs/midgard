import { computeHash32 } from "./hash.js";
import {
  decodeItemHeaderAt,
  decodeMidgardFieldArrayHeader,
  midgardFieldCommitment,
  midgardFieldStride,
} from "./native-tx-field-access.decode-midgard-field-array-header-at.js";
import {
  failGrammar,
  MIDGARD_WALK_DERIVED_STRIDE,
  wholeReader,
} from "./native-tx-field-access.encode-midgard-definite-bytes.js";

/**
 * The off-chain twin of the field grammar certificate's datum computation
 * (`onchain/aiken/lib/midgard/field-grammar-certification-v1.ak`).
 *
 * A field grammar certificate lets a step read one item of a variable-width
 * field without walking the whole field, so the step's cost no longer grows
 * with the field's item count. Every value it records is a function of the
 * field's bytes: the item count, and the wrapper offset of every
 * `MIDGARD_FIELD_GRAMMAR_WAYPOINT_INTERVAL`-th item. This module computes the
 * same values from the same bytes, so a builder knows the datum each
 * transaction must produce before it submits one.
 */

/** ASCII "MIDGARD_FIELD_GRAMMAR_CERT", the certificate token's asset name. */
export const MIDGARD_FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME: Buffer = Buffer.from(
  "MIDGARD_FIELD_GRAMMAR_CERT",
  "ascii",
);

/** Items between two waypoints; a protocol constant of the wire format. */
export const MIDGARD_FIELD_GRAMMAR_WAYPOINT_INTERVAL = 16;

/** Each waypoint is an unsigned big-endian offset of this many bytes. */
export const MIDGARD_FIELD_GRAMMAR_WAYPOINT_BYTES = 2;

const GRAMMAR_CHECKPOINT_DOMAIN = Buffer.from(
  "MidgardFieldGrammarCheckpointV1",
  "ascii",
);

const U24_MAX = 0xff_ff_ff;

/** A variable-width field's walked grammar. */
export type MidgardFieldGrammar = Readonly<{
  fieldIndex: number;
  fieldHash: Buffer;
  totalLength: number;
  itemCount: number;
  /** Wrapper offset of every item, then the total length. */
  offsets: readonly number[];
}>;

/**
 * Walks a variable-width field's §5.1 envelope from its header to its last
 * byte, refusing exactly what the certificate validator refuses: a field of
 * fixed stride, a non-canonical header or wrapper, and a walk that does not
 * end on the last byte at the declared count.
 */
export const walkMidgardFieldGrammar = (
  fieldIndex: number,
  preimage: Uint8Array,
): MidgardFieldGrammar => {
  if (midgardFieldStride(fieldIndex) !== MIDGARD_WALK_DERIVED_STRIDE) {
    return failGrammar(
      "field grammar certificates cover the variable-width fields 2, 5, 6 and 8",
      `field=${fieldIndex.toString()}`,
    );
  }
  const read = wholeReader(preimage);
  const { nextOffset, count } = decodeMidgardFieldArrayHeader(preimage);
  const offsets: number[] = [];
  let cursor = nextOffset;
  for (let index = 0; index < count; index += 1) {
    offsets.push(cursor);
    const { payloadOffset, length } = decodeItemHeaderAt(read, cursor);
    cursor = payloadOffset + length;
    if (index + 1 < count && cursor >= preimage.length) {
      return failGrammar(
        "§5.1 field preimage ends before its declared count",
        `item=${index.toString()},count=${count.toString()}`,
      );
    }
  }
  if (cursor !== preimage.length) {
    return failGrammar(
      "§5.1 field preimage does not end on its last item",
      `walked=${cursor.toString()},length=${preimage.length.toString()}`,
    );
  }
  offsets.push(cursor);
  return {
    fieldIndex,
    fieldHash: midgardFieldCommitment(preimage),
    totalLength: preimage.length,
    itemCount: count,
    offsets,
  };
};

const requireItemIndex = (grammar: MidgardFieldGrammar, next: number) => {
  if (!Number.isSafeInteger(next) || next < 0 || next > grammar.itemCount) {
    throw new Error(
      `field grammar position ${String(next)} is outside 0..${grammar.itemCount.toString()}`,
    );
  }
  return next;
};

/** The waypoints a certificate holds once `next` items are certified. */
export const midgardFieldGrammarWaypoints = (
  grammar: MidgardFieldGrammar,
  next: number,
): Buffer => {
  const reached = requireItemIndex(grammar, next);
  const entries = Math.ceil(reached / MIDGARD_FIELD_GRAMMAR_WAYPOINT_INTERVAL);
  const waypoints = Buffer.alloc(
    entries * MIDGARD_FIELD_GRAMMAR_WAYPOINT_BYTES,
  );
  for (let entry = 0; entry < entries; entry += 1) {
    waypoints.writeUIntBE(
      grammar.offsets[entry * MIDGARD_FIELD_GRAMMAR_WAYPOINT_INTERVAL]!,
      entry * MIDGARD_FIELD_GRAMMAR_WAYPOINT_BYTES,
      MIDGARD_FIELD_GRAMMAR_WAYPOINT_BYTES,
    );
  }
  return waypoints;
};

const u24 = (value: number): Buffer => {
  if (!Number.isSafeInteger(value) || value < 0 || value > U24_MAX)
    throw new Error("field grammar checkpoint value exceeds 24 bits");
  const encoded = Buffer.alloc(4);
  encoded[0] = 0x43;
  encoded.writeUIntBE(value, 1, 3);
  return encoded;
};

const hash32 = (value: Uint8Array, label: string): Buffer => {
  if (value.length !== 32) throw new Error(`${label} must be 32 bytes`);
  return Buffer.from(value);
};

/** The 87-byte grammar checkpoint after `next` items of `txId`'s field. */
export const encodeMidgardFieldGrammarCheckpoint = (
  txId: Uint8Array,
  grammar: MidgardFieldGrammar,
  next: number,
): Buffer => {
  const reached = requireItemIndex(grammar, next);
  return Buffer.concat([
    Buffer.from([0x87, 0x58, 0x20]),
    hash32(txId, "field grammar tx id"),
    Buffer.from([0x41, grammar.fieldIndex, 0x58, 0x20]),
    grammar.fieldHash,
    u24(grammar.totalLength),
    u24(grammar.itemCount),
    u24(reached),
    u24(grammar.offsets[reached]!),
  ]);
};

/** The digest an `InProgress` certificate holds for that checkpoint. */
export const midgardFieldGrammarCheckpointHash = (checkpoint: Uint8Array) =>
  computeHash32(Buffer.concat([GRAMMAR_CHECKPOINT_DOMAIN, checkpoint]));

/** A certificate datum's values; `Complete` exactly when every item is. */
export type MidgardFieldGrammarCertificate = Readonly<
  {
    owner: Buffer;
    txId: Buffer;
    fieldIndex: number;
    fieldHash: Buffer;
    totalLength: number;
    waypoints: Buffer;
  } & (
    | { kind: "InProgress"; grammarCheckpointHash: Buffer }
    | { kind: "Complete"; itemCount: number }
  )
>;

/** The certificate after `next` of the field's items are certified. */
export const deriveMidgardFieldGrammarCertificate = ({
  owner,
  txId,
  grammar,
  next,
}: {
  readonly owner: Uint8Array;
  readonly txId: Uint8Array;
  readonly grammar: MidgardFieldGrammar;
  readonly next: number;
}): MidgardFieldGrammarCertificate => {
  if (owner.length !== 28)
    throw new Error("certificate owner must be 28 bytes");
  const shared = {
    owner: Buffer.from(owner),
    txId: hash32(txId, "field grammar tx id"),
    fieldIndex: grammar.fieldIndex,
    fieldHash: grammar.fieldHash,
    totalLength: grammar.totalLength,
    waypoints: midgardFieldGrammarWaypoints(grammar, next),
  };
  return next === grammar.itemCount
    ? { ...shared, kind: "Complete", itemCount: grammar.itemCount }
    : {
        ...shared,
        kind: "InProgress",
        grammarCheckpointHash: midgardFieldGrammarCheckpointHash(
          encodeMidgardFieldGrammarCheckpoint(txId, grammar, next),
        ),
      };
};
