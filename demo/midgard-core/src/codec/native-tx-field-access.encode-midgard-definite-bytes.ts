import {
  MidgardTxCodecError,
  type MidgardTxCodecErrorCode,
  MidgardTxCodecErrorCodes,
} from "./errors.js";

// ---------------------------------------------------------------------------
// Constants — one declaration per Aiken constant, same values, same spec cites
// ---------------------------------------------------------------------------

/** §2.5. The nine committed fields. Field identity is positional. */
export const MIDGARD_FIELD_COUNT = 9;

/** §5.4. Retained from the counted era by owner ruling. */
export const MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES = 32_768;

/** §5.4. Field 0's own bound equals the aggregate bound. */
export const MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES = 32_768;

/** §5.4. The operative spend maximum, a Cardano shape bound. */
export const MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT = 296;

/** §5.1 caps the item-count header at the `99 NNNN` form. */
export const MIDGARD_MAX_FIELD_ITEM_COUNT = 65_535;

/**
 * §8.3 tier-2 bound and tier-3 chunk size — the value the spec's §8.3 erratum
 * E1 re-pinned it to, applied.
 *
 * **E1 falsified the provisional 15,900 and this constant now carries the
 * repair.** A real signed key-address publication of a 15,900-byte chunk
 * measures 16,648 bytes against a 16,384-byte `maxTxSize`, so at 15,900 *every*
 * tier-3 plan's first chunk was unpublishable and the whole window
 * `(15,148, 32,768]` had no admissible carriage. 15,148 is the reserve-clearing
 * publication frontier: a 15,148-byte chunk publishes as exactly 15,872 bytes,
 * which is `maxTxSize` minus the 512-byte reliability reserve. It is therefore
 * the same number as {@link MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES_V1} in
 * `./native-tx-carriage.js`, which derives it from the cost model rather than
 * writing it down; the two are asserted equal there rather than trusted to
 * agree, and that assertion is what keeps `K` a measured bound instead of a
 * literal.
 *
 * `ceil(32,768 / 15,148) = 3`, so {@link MIDGARD_MAX_TIER3_CHUNK_COUNT} is
 * unmoved by the re-pin. The Aiken twin `chunk_bytes_k` in
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak` moves in the same
 * commit, because it is the *split*, not merely a bound: every fixture, golden,
 * corner and cost row taken at a 15,900-byte boundary re-derives with it.
 */
export const MIDGARD_CHUNK_BYTES_K = 15_148;

/**
 * §8.3 tier-1 bound. **PROVISIONAL**, and still unmeasured: `maxTxSize`
 * (16,384) minus a round 2,048-byte allowance for step machinery, pending
 * #557's M2. §8.3 erratum E1 does not falsify it but does narrow it — a
 * 14,336-byte preimage occupies 14,786 bytes once encoded as Plutus Data, so
 * 450 of the 2,048-byte allowance is spent before any step machinery exists.
 */
export const MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES = 14_336;

/** §8.3, derived: `ceil(32,768 / K)`. */
export const MIDGARD_MAX_TIER3_CHUNK_COUNT = 3;

/** §5.3. Each fixed-width item carries a two-byte `58 LL` wrapper. */
export const MIDGARD_FIXED_ITEM_WRAPPER_BYTES = 2;

/** §5.3 fields 0/1: `82 ‖ 58 20 tx_id ‖ 19 index_be16`. */
export const MIDGARD_SPEND_INPUT_ITEM_BYTES = 38;

export const MIDGARD_SPEND_INPUT_STRIDE = 40;

/** §5.3 fields 3/4: a raw 28-byte hash per item. */
export const MIDGARD_HASH28_ITEM_BYTES = 28;

export const MIDGARD_HASH28_STRIDE = 30;

/** §5.3 field 7: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)`. */
export const MIDGARD_ADDRESS_WITNESS_ITEM_BYTES = 101;

export const MIDGARD_ADDRESS_WITNESS_STRIDE = 103;

/** Fields 2/5/6/8 are variable-width: top-level access walks the envelope. */
export const MIDGARD_WALK_DERIVED_STRIDE = 0;

/**
 * §8.8. Constructor order is frozen consensus wire format; off-chain builders
 * emit exactly these Constr tags. The array index *is* the tag.
 */
export const MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS = [
  "Inline",
  "RawUtxo",
  "Certified",
] as const;

/** §8.8. Frozen alongside the carriage tags. */
export const MIDGARD_FIELD_VIEW_CONSTRUCTORS = [
  "Whole",
  "Chunked",
  "ProvisionalWhole",
] as const;

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

const fail = (
  code: MidgardTxCodecErrorCode,
  message: string,
  detail?: string,
): never => {
  throw new MidgardTxCodecError(code, message, detail);
};

export const failGrammar = (message: string, detail?: string): never =>
  fail(MidgardTxCodecErrorCodes.CborDecode, message, detail);

export const failField = (message: string, detail?: string): never =>
  fail(MidgardTxCodecErrorCodes.InvalidFieldType, message, detail);

export const failHash = (message: string, detail?: string): never =>
  fail(MidgardTxCodecErrorCodes.HashMismatch, message, detail);

export const exactCount = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    return failField(
      `${label} must be a non-negative safe integer`,
      `${value}`,
    );
  }
  return value;
};

/** §2.5's `0..8` bound, enforced rather than assumed of the caller. */
export const exactMidgardFieldIndex = (fieldIndex: number): number => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_FIELD_COUNT
  ) {
    return failField(
      "native-V1 field index must be 0..8",
      `field_index=${fieldIndex}`,
    );
  }
  return fieldIndex;
};

// ---------------------------------------------------------------------------
// §7.3 abort-never-clamp primitives
// ---------------------------------------------------------------------------

/**
 * The only slice in this module. `Uint8Array.subarray` clamps out-of-range
 * arguments, and two clamped out-of-range reads are byte-equal — which would
 * fabricate equality evidence out of a perfectly valid block. The bound is
 * checked first and the read fails closed.
 */
export const sliceExact = (
  bytes: Uint8Array,
  offset: number,
  length: number,
): Buffer => {
  if (!Number.isSafeInteger(offset) || offset < 0) {
    return failGrammar("slice offset must be non-negative", `offset=${offset}`);
  }
  if (!Number.isSafeInteger(length) || length < 0) {
    return failGrammar("slice length must be non-negative", `length=${length}`);
  }
  if (offset + length > bytes.length) {
    return failGrammar(
      "slice leaves the authenticated bytes",
      `offset=${offset},length=${length},available=${bytes.length}`,
    );
  }
  return Buffer.from(bytes.subarray(offset, offset + length));
};

/** A `(offset, length) -> bytes` reader over one contiguous buffer. */
export const wholeReader =
  (bytes: Uint8Array) =>
  (offset: number, length: number): Buffer =>
    sliceExact(bytes, offset, length);

// ---------------------------------------------------------------------------
// §5.1 envelope grammar
// ---------------------------------------------------------------------------

/**
 * §5.1 `definite_array_header(N)`: `80+N` for N ≤ 23, `98 NN` for N ≤ 255,
 * `99 NNNN` for N ≤ 65,535 — minimal width, capped at the two-byte form.
 */
export const encodeMidgardFieldArrayHeader = (count: number): Buffer => {
  const exact = exactCount(count, "field item count");
  if (exact <= 23) {
    return Buffer.from([0x80 + exact]);
  }
  if (exact <= 0xff) {
    return Buffer.from([0x98, exact]);
  }
  if (exact > MIDGARD_MAX_FIELD_ITEM_COUNT) {
    return failField(
      "field item count exceeds the §5.1 `99 NNNN` bound",
      `count=${exact}`,
    );
  }
  return Buffer.from([0x99, (exact >> 8) & 0xff, exact & 0xff]);
};

/**
 * §5.1 `definite_bytes_header(L) ‖ payload`.
 *
 * **The `5a` branch is deliberately kept, and deliberately unreachable.** §5.1's
 * grammar stops at `59 LLLL`, so the four-byte head this emits for a payload
 * above 65,535 bytes is one that {@link decodeMidgardFieldPreimage} and the
 * door's item reader both refuse. The asymmetry is not this module's invention:
 * it is `codec.encode_definite_bytes`, a general CBOR encoder serving the whole
 * native-tx codec rather than §5.1 alone, transcribed exactly.
 *
 * Narrowing it here alone would manufacture the divergence this twin exists to
 * prevent — a payload TypeScript refuses to encode and Aiken encodes happily —
 * and narrowing the Aiken side is a consensus change to a shared encoder.
 * Reachability is closed instead where §5.4 already lives: every entry point
 * that admits a preimage bounds it at 32,768 bytes
 * ({@link buildMidgardWholeFieldView}, {@link buildMidgardChunkedFieldView},
 * {@link selectMidgardFieldCarriageTier},
 * {@link splitMidgardFieldPreimageIntoChunks}), and one item wide enough to
 * reach `5a` needs a field twice that bound. A caller that assembles a preimage
 * with {@link encodeMidgardFieldPreimage} and never presents it to a door has
 * left the format already, and gets bytes no door will read back.
 */
export const encodeMidgardDefiniteBytes = (payload: Uint8Array): Buffer => {
  const length = payload.length;
  if (length <= 23) {
    return Buffer.concat([Buffer.from([0x40 + length]), payload]);
  }
  if (length <= 0xff) {
    return Buffer.concat([Buffer.from([0x58, length]), payload]);
  }
  if (length <= 0xffff) {
    return Buffer.concat([
      Buffer.from([0x59, (length >> 8) & 0xff, length & 0xff]),
      payload,
    ]);
  }
  const header = Buffer.alloc(5);
  header[0] = 0x5a;
  header.writeUInt32BE(length, 1);
  return Buffer.concat([header, payload]);
};

/**
 * §5.1. A definite array header followed by one definite byte-string-wrapped
 * item per element. All nine fields share it; an empty field is exactly `80`.
 */
export const encodeMidgardFieldPreimage = (
  items: readonly Uint8Array[],
): Buffer =>
  Buffer.concat([
    encodeMidgardFieldArrayHeader(items.length),
    ...items.map(encodeMidgardDefiniteBytes),
  ]);

export type MidgardFieldArrayHeader = {
  /** Offset one past the header. */
  readonly nextOffset: number;
  readonly count: number;
};

export const byteAt = (bytes: Uint8Array, offset: number): number => {
  if (!Number.isSafeInteger(offset) || offset < 0 || offset >= bytes.length) {
    return failGrammar(
      "field preimage read is out of range",
      `offset=${offset},length=${bytes.length}`,
    );
  }
  return bytes[offset];
};
