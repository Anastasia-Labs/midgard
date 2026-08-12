/**
 * The TypeScript twin of `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`
 * — the shared surface of `docs/spec/midgard-tx.md` §4, §5.1, §7 and §8.
 *
 * The spec (§1, "Encoders") requires both implementation twins to emit
 * byte-identical output for every value in its domain. This module is the
 * off-chain half of that obligation for everything the nine fields *share*:
 *
 *   * the §5.1 enveloped preimage grammar and its fail-closed decoder;
 *   * the §4 flat `blake2b_256` field commitment;
 *   * the §5.3 stride table and §5.4/§8.3 byte bounds;
 *   * the §8.8 frozen carriage / field-view sum types, with the §7 access
 *     invariants (authenticate-once, abort-never-clamp, count consistency,
 *     straddle-aware lazy chunk verify) enforced rather than documented; and
 *   * the §8.4/§8.6 deterministic chunk split and certificate derivation.
 *
 * The nine **per-field item encodings** of §5.3 are deliberately *not* here:
 * they live with their own producers, and their cross-language vectors fan out
 * over disjoint files. What is here is what all nine agree on, so that a field
 * whose items this module never inspects still commits, walks and slices
 * identically on both sides.
 *
 * Three deviations from a line-by-line Aiken transcription, each deliberate:
 *
 *   * **Lazy chunk verify is memoised.** On-chain `read_chunked_range`
 *     re-hashes the chunk a read lands in on every read, because a Plutus
 *     script has nowhere to record that it already did. The guarantee §8.6
 *     actually makes is *laziness* — a chunk nobody reads is never checked
 *     against its certified digest — so this module verifies each chunk the
 *     first time a read reaches it and remembers. Same acceptance set, same
 *     untouched-chunk guarantee, without re-hashing 15.9 KB per off-chain read.
 *   * **Failures throw `MidgardTxCodecError`** where Aiken aborts. Every
 *     `expect` in the Aiken module has a throwing counterpart here; none is
 *     softened into a clamp or a default (§7.3).
 *   * **Tier 3 authenticates against the field hash directly.** The on-chain
 *     door never re-hashes a certified preimage, and does not need to: the
 *     certificate minting policy checked the chunks against the field
 *     commitment before the token could exist, so requiring that policy's
 *     `(tx_id, field_index)` token at the named reference input *is* the §4
 *     check, carried into the door by the `Value` (§8.6). Off-chain there is
 *     no policy and no `Value` to interrogate. The §8.6 asset-name derivation
 *     is a public function of public inputs, so it says which manifest a
 *     carriage means and nothing about whether its bytes are the field's — the
 *     door therefore discharges the policy's obligation itself, hashing the
 *     concatenated chunks against `expectedCommitment`. The result is the same
 *     acceptance set as on-chain, reached without a minting policy. See
 *     {@link buildMidgardChunkedFieldViewV1} for what it costs and why §4
 *     leaves no cheaper option.
 */

import {
  MidgardTxCodecError,
  type MidgardTxCodecErrorCode,
  MidgardTxCodecErrorCodes,
} from "./errors.js";
import { computeHash32 } from "./hash.js";

// ---------------------------------------------------------------------------
// Constants — one declaration per Aiken constant, same values, same spec cites
// ---------------------------------------------------------------------------

/** §2.5. The nine committed fields. Field identity is positional. */
export const MIDGARD_FIELD_COUNT_V1 = 9;

/** §5.4. Retained from the counted era by owner ruling. */
export const MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1 = 32_768;

/** §5.4. Field 0's own bound equals the aggregate bound. */
export const MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES_V1 = 32_768;

/** §5.4. The operative spend maximum, a Cardano shape bound. */
export const MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT_V1 = 296;

/** §5.1 caps the item-count header at the `99 NNNN` form. */
export const MIDGARD_MAX_FIELD_ITEM_COUNT_V1 = 65_535;

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
 * `./native-tx-carriage-v1.js`, which derives it from the cost model rather than
 * writing it down; the two are asserted equal there rather than trusted to
 * agree, and that assertion is what keeps `K` a measured bound instead of a
 * literal.
 *
 * `ceil(32,768 / 15,148) = 3`, so {@link MIDGARD_MAX_TIER3_CHUNK_COUNT_V1} is
 * unmoved by the re-pin. The Aiken twin `chunk_bytes_k` in
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak` moves in the same
 * commit, because it is the *split*, not merely a bound: every fixture, golden,
 * corner and cost row taken at a 15,900-byte boundary re-derives with it.
 */
export const MIDGARD_CHUNK_BYTES_K_V1 = 15_148;

/**
 * §8.3 tier-1 bound. **PROVISIONAL**, and still unmeasured: `maxTxSize`
 * (16,384) minus a round 2,048-byte allowance for step machinery, pending
 * #557's M2. §8.3 erratum E1 does not falsify it but does narrow it — a
 * 14,336-byte preimage occupies 14,786 bytes once encoded as Plutus Data, so
 * 450 of the 2,048-byte allowance is spent before any step machinery exists.
 */
export const MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1 = 14_336;

/** §8.3, derived: `ceil(32,768 / K)`. */
export const MIDGARD_MAX_TIER3_CHUNK_COUNT_V1 = 3;

/** §5.3. Each fixed-width item carries a two-byte `58 LL` wrapper. */
export const MIDGARD_FIXED_ITEM_WRAPPER_BYTES_V1 = 2;

/** §5.3 fields 0/1: `82 ‖ 58 20 tx_id ‖ 19 index_be16`. */
export const MIDGARD_SPEND_INPUT_ITEM_BYTES_V1 = 38;
export const MIDGARD_SPEND_INPUT_STRIDE_V1 = 40;

/** §5.3 fields 3/4: a raw 28-byte hash per item. */
export const MIDGARD_HASH28_ITEM_BYTES_V1 = 28;
export const MIDGARD_HASH28_STRIDE_V1 = 30;

/** §5.3 field 7: `82 ‖ 58 20 vkey(32) ‖ 58 40 signature(64)`. */
export const MIDGARD_ADDRESS_WITNESS_ITEM_BYTES_V1 = 101;
export const MIDGARD_ADDRESS_WITNESS_STRIDE_V1 = 103;

/** Fields 2/5/6/8 are variable-width: top-level access walks the envelope. */
export const MIDGARD_WALK_DERIVED_STRIDE_V1 = 0;

/**
 * §8.8. Constructor order is frozen consensus wire format; off-chain builders
 * emit exactly these Constr tags. The array index *is* the tag.
 */
export const MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1 = [
  "Inline",
  "RawUtxo",
  "Certified",
] as const;

/** §8.8. Frozen alongside the carriage tags. */
export const MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1 = ["Whole", "Chunked"] as const;

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

const failGrammar = (message: string, detail?: string): never =>
  fail(MidgardTxCodecErrorCodes.CborDecode, message, detail);

const failField = (message: string, detail?: string): never =>
  fail(MidgardTxCodecErrorCodes.InvalidFieldType, message, detail);

const failHash = (message: string, detail?: string): never =>
  fail(MidgardTxCodecErrorCodes.HashMismatch, message, detail);

const exactCount = (value: number, label: string): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    return failField(`${label} must be a non-negative safe integer`, `${value}`);
  }
  return value;
};

/** §2.5's `0..8` bound, enforced rather than assumed of the caller. */
export const exactMidgardFieldIndexV1 = (fieldIndex: number): number => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_FIELD_COUNT_V1
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
const sliceExact = (
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
const wholeReader =
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
export const encodeMidgardFieldArrayHeaderV1 = (count: number): Buffer => {
  const exact = exactCount(count, "field item count");
  if (exact <= 23) {
    return Buffer.from([0x80 + exact]);
  }
  if (exact <= 0xff) {
    return Buffer.from([0x98, exact]);
  }
  if (exact > MIDGARD_MAX_FIELD_ITEM_COUNT_V1) {
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
 * above 65,535 bytes is one that {@link decodeMidgardFieldPreimageV1} and the
 * door's item reader both refuse. The asymmetry is not this module's invention:
 * it is `codec.encode_definite_bytes`, a general CBOR encoder serving the whole
 * native-tx codec rather than §5.1 alone, transcribed exactly.
 *
 * Narrowing it here alone would manufacture the divergence this twin exists to
 * prevent — a payload TypeScript refuses to encode and Aiken encodes happily —
 * and narrowing the Aiken side is a consensus change to a shared encoder.
 * Reachability is closed instead where §5.4 already lives: every entry point
 * that admits a preimage bounds it at 32,768 bytes
 * ({@link buildMidgardWholeFieldViewV1}, {@link buildMidgardChunkedFieldViewV1},
 * {@link selectMidgardFieldCarriageTierV1},
 * {@link splitMidgardFieldPreimageIntoChunksV1}), and one item wide enough to
 * reach `5a` needs a field twice that bound. A caller that assembles a preimage
 * with {@link encodeMidgardFieldPreimageV1} and never presents it to a door has
 * left the format already, and gets bytes no door will read back.
 */
export const encodeMidgardDefiniteBytesV1 = (payload: Uint8Array): Buffer => {
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
export const encodeMidgardFieldPreimageV1 = (
  items: readonly Uint8Array[],
): Buffer =>
  Buffer.concat([
    encodeMidgardFieldArrayHeaderV1(items.length),
    ...items.map(encodeMidgardDefiniteBytesV1),
  ]);

export type MidgardFieldArrayHeaderV1 = {
  /** Offset one past the header. */
  readonly nextOffset: number;
  readonly count: number;
};

const byteAt = (bytes: Uint8Array, offset: number): number => {
  if (!Number.isSafeInteger(offset) || offset < 0 || offset >= bytes.length) {
    return failGrammar(
      "field preimage read is out of range",
      `offset=${offset},length=${bytes.length}`,
    );
  }
  return bytes[offset];
};

/**
 * The **one** §5.1 `definite_array_header(N)` decoder, at any offset.
 *
 * §5.1's acceptance set is narrower than CBOR's: minimal width only, capped at
 * the `99 NNNN` form. The four-byte `9a` head is well-formed CBOR and rejects
 * here, exactly as it does in `decode_field_array_header_at`. Every §5.1
 * envelope decoded in this package goes through this function, so the grammar
 * has one verdict rather than two.
 */
export const decodeMidgardFieldArrayHeaderV1At = (
  bytes: Uint8Array,
  offset: number,
): MidgardFieldArrayHeaderV1 => {
  const tag = byteAt(bytes, offset);
  if (tag >= 0x80 && tag <= 0x97) {
    return { nextOffset: offset + 1, count: tag - 0x80 };
  }
  if (tag === 0x98) {
    const count = byteAt(bytes, offset + 1);
    if (count < 24) {
      return failGrammar(
        "non-minimal §5.1 array header",
        `tag=98,count=${count}`,
      );
    }
    return { nextOffset: offset + 2, count };
  }
  if (tag !== 0x99) {
    return failGrammar(
      "not a §5.1 definite array header",
      `tag=${tag.toString(16)}`,
    );
  }
  const count = byteAt(bytes, offset + 1) * 256 + byteAt(bytes, offset + 2);
  if (count <= 0xff) {
    return failGrammar("non-minimal §5.1 array header", `tag=99,count=${count}`);
  }
  return { nextOffset: offset + 3, count };
};

export const decodeMidgardFieldArrayHeaderV1 = (
  preimage: Uint8Array,
): MidgardFieldArrayHeaderV1 => decodeMidgardFieldArrayHeaderV1At(preimage, 0);

/**
 * §5.1's `definite_bytes_header(L)` at `offset`, minimal width, capped at
 * `59 LLLL`. The twin of the door's `item_header_at`.
 */
const decodeItemHeaderAt = (
  read: (offset: number, length: number) => Buffer,
  offset: number,
): { readonly payloadOffset: number; readonly length: number } => {
  const tag = read(offset, 1)[0];
  if (tag >= 0x40 && tag <= 0x57) {
    return { payloadOffset: offset + 1, length: tag - 0x40 };
  }
  if (tag === 0x58) {
    const length = read(offset + 1, 1)[0];
    if (length < 24) {
      return failGrammar(
        "non-minimal §5.1 item wrapper",
        `tag=58,length=${length}`,
      );
    }
    return { payloadOffset: offset + 2, length };
  }
  if (tag !== 0x59) {
    return failGrammar(
      "not a §5.1 definite byte-string header",
      `tag=${tag.toString(16)}`,
    );
  }
  const head = read(offset + 1, 2);
  const length = head[0] * 256 + head[1];
  if (length <= 0xff) {
    return failGrammar(
      "non-minimal §5.1 item wrapper",
      `tag=59,length=${length}`,
    );
  }
  return { payloadOffset: offset + 3, length };
};

/** The §5.1 header width a given item count occupies. */
export const midgardFieldHeaderLengthForCountV1 = (count: number): number => {
  const exact = exactCount(count, "field item count");
  if (exact <= 23) {
    return 1;
  }
  if (exact <= 0xff) {
    return 2;
  }
  if (exact > MIDGARD_MAX_FIELD_ITEM_COUNT_V1) {
    return failField(
      "field item count exceeds the §5.1 `99 NNNN` bound",
      `count=${exact}`,
    );
  }
  return 3;
};

/**
 * The fail-closed §5.1 decoder: wrapper/length mismatch, non-minimal header,
 * an item count disagreeing with the walked content, and trailing bytes after
 * item `N-1` all reject.
 */
export const decodeMidgardFieldPreimageV1 = (
  preimage: Uint8Array,
): readonly Buffer[] => {
  const read = wholeReader(preimage);
  const { nextOffset, count } = decodeMidgardFieldArrayHeaderV1(preimage);
  const items: Buffer[] = [];
  let cursor = nextOffset;
  for (let index = 0; index < count; index += 1) {
    const { payloadOffset, length } = decodeItemHeaderAt(read, cursor);
    items.push(read(payloadOffset, length));
    cursor = payloadOffset + length;
  }
  if (cursor !== preimage.length) {
    return failGrammar(
      "§5.1 field preimage has trailing bytes",
      `walked=${cursor},length=${preimage.length}`,
    );
  }
  return items;
};

// ---------------------------------------------------------------------------
// §4 flat commitment
// ---------------------------------------------------------------------------

/**
 * §4 — plain hashing. No domain tag, no version prefix, no field index in the
 * hash input; a watcher needs the raw bytes and `blake2b_256`, nothing else.
 */
export const midgardFieldCommitmentV1 = (preimage: Uint8Array): Buffer =>
  computeHash32(preimage);

/** The producer twin of {@link midgardFieldCommitmentV1}: envelope, then hash. */
export const midgardFieldCommitmentFromItemsV1 = (
  items: readonly Uint8Array[],
): Buffer => midgardFieldCommitmentV1(encodeMidgardFieldPreimageV1(items));

/**
 * `blake2b_256(#"80")` — what every empty field commits to.
 *
 * Under §4's plain hashing the commitment carries no field index, so all nine
 * empty fields share this value. The Aiken twin pins the same 32 bytes as
 * `native_tx_field_access_v1.empty_field_commitment`; the cross-language
 * golden vector proves the two against each other.
 */
export const MIDGARD_EMPTY_FIELD_COMMITMENT_V1: Buffer =
  midgardFieldCommitmentFromItemsV1([]);

/** §5.3 stride table. `0` marks the variable-width fields. */
export const midgardFieldStrideV1 = (fieldIndex: number): number => {
  const exact = exactMidgardFieldIndexV1(fieldIndex);
  if (exact === 0 || exact === 1) {
    return MIDGARD_SPEND_INPUT_STRIDE_V1;
  }
  if (exact === 3 || exact === 4) {
    return MIDGARD_HASH28_STRIDE_V1;
  }
  if (exact === 7) {
    return MIDGARD_ADDRESS_WITNESS_STRIDE_V1;
  }
  return MIDGARD_WALK_DERIVED_STRIDE_V1;
};

// ---------------------------------------------------------------------------
// §8.4/§8.6 chunking and certificates
// ---------------------------------------------------------------------------

/**
 * §8.4's deterministic split rule: chunk `j` is bytes `[j·K, (j+1)·K)` with a
 * ragged last chunk, minimum-necessary chunks by construction.
 */
export const midgardExpectedChunkCountV1 = (totalLength: number): number => {
  const exact = exactCount(totalLength, "preimage total length");
  if (exact === 0) {
    return failField("tier-3 total length must be positive", "total_length=0");
  }
  return Math.ceil(exact / MIDGARD_CHUNK_BYTES_K_V1);
};

/**
 * §8.4. Determinism is what makes independent publishers byte-compatible:
 * identical chunks, identical digest vectors, interchangeable certificates.
 */
export const splitMidgardFieldPreimageIntoChunksV1 = (
  preimage: Uint8Array,
): readonly Buffer[] => {
  if (preimage.length > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1) {
    return failField(
      "field preimage exceeds the §5.4 aggregate bound",
      `length=${preimage.length}`,
    );
  }
  const chunks: Buffer[] = [];
  for (
    let start = 0;
    start < preimage.length;
    start += MIDGARD_CHUNK_BYTES_K_V1
  ) {
    chunks.push(
      Buffer.from(
        preimage.subarray(
          start,
          Math.min(start + MIDGARD_CHUNK_BYTES_K_V1, preimage.length),
        ),
      ),
    );
  }
  return chunks;
};

/** §8.6. The tier-3 digest manifest the certificate validator mints. */
export type MidgardFieldPreimageCertificateV1 = {
  /** Min-Ada reclaim authority, set by the minter. 28-byte vkey hash. */
  readonly owner: Buffer;
  /** The L2 transaction's id (32 bytes). */
  readonly txId: Buffer;
  /** 0..8. */
  readonly fieldIndex: number;
  /** Preimage byte length — ragged-last plus offset math. */
  readonly totalLength: number;
  /** `blake2b_256` per chunk, in order; length = `ceil(totalLength / K)`. */
  readonly chunkDigests: readonly Buffer[];
};

const exactBytes = (
  value: Uint8Array,
  length: number,
  label: string,
): Buffer => {
  if (value.length !== length) {
    return failField(`${label} must be ${length} bytes`, `got=${value.length}`);
  }
  return Buffer.from(value);
};

/**
 * §8.6's deterministic certificate asset name, derived from
 * `(tx_id, field_index)` so indexers can find a certificate and so a consumer
 * can name the exact token it requires without trusting the redeemer:
 *
 * ```
 * asset_name = blake2b_256(field_index_byte ‖ tx_id)
 * ```
 *
 * The single-byte prefix is domain separation, not a length header: with
 * `field_index` bounded to 0..8 and `tx_id` fixed at 32 bytes the 33-byte
 * preimage is unambiguous. Both bounds are enforced, not assumed.
 */
export const midgardFieldPreimageCertificateAssetNameV1 = ({
  txId,
  fieldIndex,
}: {
  readonly txId: Uint8Array;
  readonly fieldIndex: number;
}): Buffer =>
  midgardFieldCommitmentV1(
    Buffer.concat([
      Buffer.from([exactMidgardFieldIndexV1(fieldIndex)]),
      exactBytes(txId, 32, "certificate tx_id"),
    ]),
  );

/**
 * Builds the §8.6 certificate an off-chain publisher mints for a tier-3
 * preimage. `preimage_len > K` is the §8.4 tier boundary and is enforced here
 * for the same reason the door enforces it on the way in: the ladder is a
 * partition, so a preimage that fits tier 1 or tier 2 has exactly one
 * admissible carriage.
 */
export const deriveMidgardFieldPreimageCertificateV1 = ({
  owner,
  txId,
  fieldIndex,
  preimage,
}: {
  readonly owner: Uint8Array;
  readonly txId: Uint8Array;
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
}): MidgardFieldPreimageCertificateV1 => {
  if (preimage.length <= MIDGARD_CHUNK_BYTES_K_V1) {
    return failField(
      "tier-3 certification requires preimage_len > K (§8.4)",
      `length=${preimage.length},k=${MIDGARD_CHUNK_BYTES_K_V1}`,
    );
  }
  const chunks = splitMidgardFieldPreimageIntoChunksV1(preimage);
  return {
    owner: exactBytes(owner, 28, "certificate owner"),
    txId: exactBytes(txId, 32, "certificate tx_id"),
    fieldIndex: exactMidgardFieldIndexV1(fieldIndex),
    totalLength: preimage.length,
    chunkDigests: chunks.map(midgardFieldCommitmentV1),
  };
};

// ---------------------------------------------------------------------------
// §8.8 wire types
// ---------------------------------------------------------------------------

/**
 * §8.1–§8.4. How a field's preimage bytes reach the consuming transaction.
 * Constructor order is frozen: `Inline` is Constr 0, `RawUtxo` 1,
 * `Certified` 2.
 */
export type MidgardFieldCarriageV1 =
  | {
      /** Tier 1 — the step's own redeemer carries the preimage. */
      readonly carriage: "Inline";
      readonly preimage: Buffer;
    }
  | {
      /**
       * Tier 2 — one nothing-but-bytes inline datum at the prover's key
       * address, named by its positional reference-input index.
       */
      readonly carriage: "RawUtxo";
      readonly refInputIndex: number;
    }
  | {
      /**
       * Tier 3 — deterministic fixed-K chunks plus one certified
       * digest-manifest. `chunkRefInputIndices` is all-chunks-positional:
       * element `k` is the reference-input index of chunk `k`.
       */
      readonly carriage: "Certified";
      readonly certRefInputIndex: number;
      readonly chunkRefInputIndices: readonly number[];
    };

/**
 * An authenticated field, ready to slice. Carriage tier is an encoding detail
 * that consumer logic never branches on — the accessors read both variants.
 */
export type MidgardFieldViewV1 =
  | {
      /** Tiers 1–2: the whole preimage is present and hash-checked. */
      readonly view: "Whole";
      readonly bytes: Buffer;
      readonly count: number;
      readonly stride: number;
    }
  | {
      /**
       * Tier 3: chunks are present but unhashed until touched; `chunkDigests`
       * and the item count come from the mint-verified certificate.
       */
      readonly view: "Chunked";
      readonly chunks: readonly Buffer[];
      readonly chunkDigests: readonly Buffer[];
      readonly count: number;
      readonly stride: number;
    };

/**
 * §8, simplest-fitting-first. The ladder is a partition (§8.4), so a preimage
 * has exactly one admissible carriage and a builder never chooses.
 */
export const selectMidgardFieldCarriageTierV1 = (
  preimageLength: number,
): (typeof MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1)[number] => {
  const exact = exactCount(preimageLength, "preimage length");
  if (exact > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1) {
    return failField(
      "field preimage exceeds the §5.4 aggregate bound",
      `length=${exact}`,
    );
  }
  if (exact <= MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1) {
    return "Inline";
  }
  if (exact <= MIDGARD_CHUNK_BYTES_K_V1) {
    return "RawUtxo";
  }
  return "Certified";
};

// ---------------------------------------------------------------------------
// Lazy chunk verification
// ---------------------------------------------------------------------------

/**
 * Which chunks of a given view have already been verified. Off-chain the
 * laziness that matters is "a chunk nobody reads is never hashed"; re-hashing
 * on every read is an on-chain artifact of having nowhere to record the fact.
 */
const verifiedChunkIndexes = new WeakMap<MidgardFieldViewV1, Set<number>>();

const verifiedChunksOf = (view: MidgardFieldViewV1): Set<number> => {
  const existing = verifiedChunkIndexes.get(view);
  if (existing !== undefined) {
    return existing;
  }
  const created = new Set<number>();
  verifiedChunkIndexes.set(view, created);
  return created;
};

/**
 * Returns chunk `chunkIndex` after proving it against the certificate's digest.
 * `view` is the memo key: pass `undefined` when no view exists yet (the header
 * read at tier-3 construction), which verifies without remembering.
 */
const verifyChunk = (
  view: MidgardFieldViewV1 | undefined,
  chunks: readonly Buffer[],
  chunkDigests: readonly Buffer[],
  chunkIndex: number,
): Buffer => {
  const chunk = chunks[chunkIndex];
  const digest = chunkDigests[chunkIndex];
  if (chunk === undefined || digest === undefined) {
    return failGrammar(
      "chunked read leaves the certified chunk vector",
      `chunk_index=${chunkIndex},chunks=${chunks.length}`,
    );
  }
  const verified = view === undefined ? undefined : verifiedChunksOf(view);
  if (verified?.has(chunkIndex) === true) {
    return chunk;
  }
  if (!midgardFieldCommitmentV1(chunk).equals(digest)) {
    return failHash(
      "chunk does not match its certified digest",
      `chunk_index=${chunkIndex}`,
    );
  }
  verified?.add(chunkIndex);
  return chunk;
};

const readChunkedRange = (
  view: MidgardFieldViewV1 | undefined,
  chunks: readonly Buffer[],
  chunkDigests: readonly Buffer[],
  offset: number,
  length: number,
): Buffer => {
  if (!Number.isSafeInteger(offset) || offset < 0) {
    return failGrammar("chunked read offset must be non-negative", `${offset}`);
  }
  if (!Number.isSafeInteger(length) || length < 0) {
    return failGrammar("chunked read length must be non-negative", `${length}`);
  }
  const pieces: Buffer[] = [];
  let cursor = offset;
  let remaining = length;
  while (remaining > 0) {
    const chunkIndex = Math.floor(cursor / MIDGARD_CHUNK_BYTES_K_V1);
    const within = cursor - chunkIndex * MIDGARD_CHUNK_BYTES_K_V1;
    const chunk = verifyChunk(view, chunks, chunkDigests, chunkIndex);
    const available = chunk.length - within;
    if (available <= 0) {
      return failGrammar(
        "chunked read leaves the certified bytes",
        `offset=${cursor}`,
      );
    }
    const take = Math.min(remaining, available);
    pieces.push(sliceExact(chunk, within, take));
    cursor += take;
    remaining -= take;
  }
  return Buffer.concat(pieces);
};

// ---------------------------------------------------------------------------
// View construction
// ---------------------------------------------------------------------------

const walkToEnd = (
  preimage: Uint8Array,
  offset: number,
  count: number,
): number => {
  const read = wholeReader(preimage);
  let cursor = offset;
  for (let remaining = count; remaining > 0; remaining -= 1) {
    const { payloadOffset, length } = decodeItemHeaderAt(read, cursor);
    cursor = payloadOffset + length;
  }
  return cursor;
};

/**
 * Tiers 1–2 (§8.1/§8.2): the whole preimage is in hand, so it is hashed once
 * against the positionally-extracted commitment and then structurally
 * validated before any accessor may touch it.
 *
 * Construction proves what is O(1) or already O(N)-unavoidable: the header is
 * minimal, and the declared count agrees with the byte length — by arithmetic
 * for the fixed-stride fields (§7.4), by a full walk for the variable-width
 * ones, which is the only way to know where their items end. Per-item wrapper
 * canonicality for a fixed-stride field is proved at each read instead, so a
 * field with 296 items does not pay 296 wrapper decodes to answer one dispute.
 */
export const buildMidgardWholeFieldViewV1 = ({
  fieldIndex,
  preimage,
  expectedCommitment,
}: {
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
  readonly expectedCommitment: Uint8Array;
}): MidgardFieldViewV1 => {
  const stride = midgardFieldStrideV1(fieldIndex);
  const bytes = Buffer.from(preimage);
  const actual = midgardFieldCommitmentV1(bytes);
  if (!actual.equals(Buffer.from(expectedCommitment))) {
    return failHash(
      "field preimage does not match the committed field hash",
      `field_index=${fieldIndex},actual=${actual.toString("hex")}`,
    );
  }
  const totalLength = bytes.length;
  if (totalLength > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1) {
    return failField(
      "field preimage exceeds the §5.4 aggregate bound",
      `length=${totalLength}`,
    );
  }
  const { nextOffset: headerLength, count } =
    decodeMidgardFieldArrayHeaderV1(bytes);
  if (stride > MIDGARD_WALK_DERIVED_STRIDE_V1) {
    if (headerLength + stride * count !== totalLength) {
      return failGrammar(
        "§7.4 count consistency failed for a fixed-stride field",
        `field_index=${fieldIndex},count=${count},length=${totalLength}`,
      );
    }
  } else if (walkToEnd(bytes, headerLength, count) !== totalLength) {
    return failGrammar(
      "§5.1 walked content does not account for exactly the declared items",
      `field_index=${fieldIndex},count=${count},length=${totalLength}`,
    );
  }
  return { view: "Whole", bytes, count, stride };
};

const countFromTotalLength = (stride: number, totalLength: number): number => {
  for (const headerLength of [1, 2, 3]) {
    const candidate = Math.floor((totalLength - headerLength) / stride);
    const inRange =
      headerLength === 1
        ? candidate >= 0 && candidate <= 23
        : headerLength === 2
          ? candidate >= 24 && candidate <= 0xff
          : candidate > 0xff && candidate <= MIDGARD_MAX_FIELD_ITEM_COUNT_V1;
    if (inRange && headerLength + stride * candidate === totalLength) {
      return candidate;
    }
  }
  return failGrammar(
    "no §5.1 header width reconciles the certified total length with the stride",
    `stride=${stride},total_length=${totalLength}`,
  );
};

/**
 * Tier 3 (§8.4): the carriage is proved against `expectedCommitment` once, and
 * chunks then stay unverified against their certified digests until an accessor
 * touches them.
 *
 * **Why the preimage is hashed here.** On-chain the certificate is
 * mint-verified — the policy checked the chunks against the field hash, so
 * `totalLength` and the digest vector arrive as authenticated data and
 * `certified_view` never re-hashes anything. Off-chain a
 * `MidgardFieldPreimageCertificateV1` is just a record the caller handed over;
 * no policy stands behind it, and the §8.6 asset name a door can check is
 * derivable by anyone from `(txId, fieldIndex)`. Authentication therefore has
 * to happen here or nowhere, and §4 commits the *whole* preimage under one flat
 * `blake2b_256` with no Merkle structure to open a single chunk against — so
 * authenticating any byte of a tier-3 field means hashing all of them.
 *
 * That is the trade this function makes: one `blake2b_256` over at most 32,768
 * bytes (§5.4) at construction, in exchange for a view whose bytes are the
 * field's. It is the same bound `buildMidgardWholeFieldViewV1` already pays for
 * tiers 1–2, and §7.1 authenticate-once means a caller pays it per field, not
 * per read. What it does *not* buy back is chunk-level laziness at the door: a
 * caller that reads one item still hashes the whole preimage, because §4 gives
 * no way to authenticate less. Laziness below the door is untouched — the
 * per-chunk digest check still happens only on the chunks a read reaches, which
 * keeps the acceptance set identical to the on-chain door's.
 *
 * Everything `totalLength` is used for is authenticated as a consequence: the
 * chunk lengths are checked against §8.4's split before the hash, so a
 * commitment match means the concatenation is exactly `totalLength` committed
 * bytes.
 *
 * **Why no walk here.** Tier 3 exists precisely because the field is larger
 * than one chunk, and §8.4's guarantee is that reaching one item costs one
 * chunk hash (two when the item straddles). Walking a variable-width field to
 * its end at construction would spend `N` chunk hashes on-chain, and running
 * the walk only here would accept preimages the on-chain door accepts too —
 * a divergence in the wrong direction. So the §5.1 count-consistency check
 * `buildMidgardWholeFieldViewV1` runs is not available here, and this function
 * does not pretend otherwise: a fixed-stride field keeps the full §7.4
 * arithmetic check against `totalLength`, and a variable-width field's header
 * count is committed but never reconciled with its content, which is why
 * {@link midgardFieldItemCountV1} refuses to answer for one.
 */
export const buildMidgardChunkedFieldViewV1 = ({
  fieldIndex,
  txId,
  certificate,
  chunks,
  expectedCommitment,
}: {
  readonly fieldIndex: number;
  readonly txId: Uint8Array;
  readonly certificate: MidgardFieldPreimageCertificateV1;
  readonly chunks: readonly Uint8Array[];
  /**
   * §4's committed field hash, extracted positionally from a compact structure
   * by the caller — the same obligation {@link buildMidgardWholeFieldViewV1}
   * carries, and required for the same reason: a view is an *authenticated*
   * field, so there is no admissible way to build one without it.
   */
  readonly expectedCommitment: Uint8Array;
}): MidgardFieldViewV1 => {
  const exactField = exactMidgardFieldIndexV1(fieldIndex);
  const stride = midgardFieldStrideV1(exactField);
  if (!certificate.txId.equals(Buffer.from(txId))) {
    return failField(
      "certificate tx_id does not match the authenticated transaction",
      `certificate=${certificate.txId.toString("hex")}`,
    );
  }
  if (certificate.fieldIndex !== exactField) {
    return failField(
      "certificate field_index does not match the requested field",
      `certificate=${certificate.fieldIndex},requested=${exactField}`,
    );
  }
  const totalLength = exactCount(certificate.totalLength, "certificate total");
  // §8.4 defines tier 3 as the `preimage_len > K` case. Enforcing that lower
  // bound is what makes the ladder a partition rather than a preference:
  // without it a single-chunk "certificate" authenticates a preimage of any
  // size, and every structural check tiers 1-2 run at view construction can be
  // side-stepped by re-carrying the same bytes here.
  if (totalLength <= MIDGARD_CHUNK_BYTES_K_V1) {
    return failField(
      "tier-3 certificate must declare total_length > K (§8.4)",
      `total_length=${totalLength}`,
    );
  }
  if (totalLength > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1) {
    return failField(
      "certificate total_length exceeds the §5.4 aggregate bound",
      `total_length=${totalLength}`,
    );
  }
  const chunkCount = certificate.chunkDigests.length;
  if (chunkCount !== midgardExpectedChunkCountV1(totalLength)) {
    return failField(
      "certificate digest vector does not match the §8.4 chunk count",
      `digests=${chunkCount},total_length=${totalLength}`,
    );
  }
  if (chunkCount > MIDGARD_MAX_TIER3_CHUNK_COUNT_V1) {
    return failField(
      "certificate exceeds the §8.3 maximum tier-3 chunk count",
      `digests=${chunkCount}`,
    );
  }
  if (chunks.length !== chunkCount) {
    return failField(
      "carriage supplies a different chunk count than the certificate",
      `chunks=${chunks.length},digests=${chunkCount}`,
    );
  }
  const exactChunks = chunks.map((chunk, index) => {
    const expected =
      index === chunkCount - 1
        ? totalLength - index * MIDGARD_CHUNK_BYTES_K_V1
        : MIDGARD_CHUNK_BYTES_K_V1;
    if (chunk.length !== expected) {
      return failField(
        "chunk length disagrees with the §8.4 deterministic split",
        `chunk_index=${index},length=${chunk.length},expected=${expected}`,
      );
    }
    return Buffer.from(chunk);
  });
  // §4/§7.1. The chunk lengths above already pin the concatenation to exactly
  // `totalLength` bytes, so this is the field's preimage or it is nothing.
  // On-chain this check belongs to the certificate minting policy and reaches
  // the door as a token; off-chain there is no policy, so it is made here.
  const actual = midgardFieldCommitmentV1(Buffer.concat(exactChunks));
  if (!actual.equals(Buffer.from(expectedCommitment))) {
    return failHash(
      "field preimage does not match the committed field hash",
      `field_index=${exactField},carriage=Certified,actual=${actual.toString("hex")}`,
    );
  }
  const digests = certificate.chunkDigests.map((digest, index) =>
    exactBytes(digest, 32, `certificate chunk digest ${index}`),
  );
  if (stride > MIDGARD_WALK_DERIVED_STRIDE_V1) {
    // §7.4 count consistency against the mint-verified `totalLength`; no chunk
    // hash is spent to learn the count (§8.6).
    return {
      view: "Chunked",
      chunks: exactChunks,
      chunkDigests: digests,
      count: countFromTotalLength(stride, totalLength),
      stride,
    };
  }
  // A variable-width field has no arithmetic count, so the header is read out
  // of chunk 0 — and chunk 0 is verified at that moment, so the number is at
  // least the one the committed bytes carry. Above the tier boundary
  // `totalLength` always leaves three bytes to read.
  const header = readChunkedRange(undefined, exactChunks, digests, 0, 3);
  const { nextOffset: headerLength, count } =
    decodeMidgardFieldArrayHeaderV1(header);
  // The one count check that is O(1) here: an enveloped item is at least one
  // byte (`40`), so `count` items cannot fit in fewer than `count` bytes. This
  // bounds the read guard below; it does not authenticate the count.
  if (headerLength + count > totalLength) {
    return failGrammar(
      "tier-3 header count cannot fit inside the certified length",
      `count=${count},total_length=${totalLength}`,
    );
  }
  return {
    view: "Chunked",
    chunks: exactChunks,
    chunkDigests: digests,
    count,
    stride,
  };
};

/** What a resolved reference input contributes to carriage resolution. */
export type ResolvedCarriageReferenceInputV1 = {
  /** §8.5 raw carriage: the nothing-but-bytes inline datum payload. */
  readonly inlineDatumBytes?: Uint8Array;
  /** §8.6 manifest: the decoded certificate datum. */
  readonly certificate?: MidgardFieldPreimageCertificateV1;
  /**
   * The certificate token's asset name as observed at that UTxO, checked
   * against the §8.6 derivation when supplied.
   *
   * This is an *identity* check, not an authentication: the derivation is a
   * public function of `(txId, fieldIndex)`, so anyone can produce a matching
   * name for a manifest over any bytes at all. It catches a carriage pointing
   * at the wrong manifest; what proves the bytes are the field's is the §4
   * commitment check in {@link buildMidgardChunkedFieldViewV1}, which does not
   * depend on this field being supplied.
   */
  readonly certificateAssetName?: Uint8Array;
};

const referenceInputAt = (
  referenceInputs: readonly ResolvedCarriageReferenceInputV1[],
  index: number,
): ResolvedCarriageReferenceInputV1 => {
  if (!Number.isSafeInteger(index) || index < 0) {
    return failField("reference-input index must be non-negative", `${index}`);
  }
  const input = referenceInputs[index];
  if (input === undefined) {
    return failField(
      "carriage names a reference input that is not present",
      `ref_input_index=${index}`,
    );
  }
  return input;
};

const rawCarriageBytes = (
  referenceInputs: readonly ResolvedCarriageReferenceInputV1[],
  index: number,
): Uint8Array => {
  const input = referenceInputAt(referenceInputs, index);
  if (input.inlineDatumBytes === undefined) {
    return failField(
      "raw carriage reference input carries no nothing-but-bytes inline datum",
      `ref_input_index=${index}`,
    );
  }
  return input.inlineDatumBytes;
};

/**
 * The single off-chain field-access door, the twin of
 * `authenticated_field_view`.
 *
 * Authenticates field `fieldIndex` against whichever carriage tier `carriage`
 * names and returns a view the slice-only accessors can read. §7.1
 * authenticate-once is the caller's to exploit: build the view once per field
 * and read it many times; an untouched field is never authenticated.
 *
 * `expectedCommitment` is the caller's responsibility to extract positionally
 * from a committed compact structure (§4). Unlike the on-chain door this
 * function cannot enforce that — off-chain there is no script context to read
 * the committed structure out of — so callers that face untrusted input MUST
 * take the hash from the compact structure in view and never from a redeemer.
 *
 * It is checked on **every** tier. On-chain tier 3 delegates the §4 check to
 * the certificate minting policy and consults a token instead; off-chain there
 * is no policy, so the tier-3 branch re-establishes the commitment itself
 * rather than trusting a manifest a caller assembled. Carriage stays an
 * encoding detail: the same forged bytes are refused whichever tier presents
 * them.
 */
export const authenticatedMidgardFieldViewV1 = ({
  fieldIndex,
  txId,
  expectedCommitment,
  carriage,
  referenceInputs = [],
}: {
  readonly fieldIndex: number;
  readonly txId: Uint8Array;
  readonly expectedCommitment: Uint8Array;
  readonly carriage: MidgardFieldCarriageV1;
  readonly referenceInputs?: readonly ResolvedCarriageReferenceInputV1[];
}): MidgardFieldViewV1 => {
  const exactField = exactMidgardFieldIndexV1(fieldIndex);
  switch (carriage.carriage) {
    case "Inline":
      return buildMidgardWholeFieldViewV1({
        fieldIndex: exactField,
        preimage: carriage.preimage,
        expectedCommitment,
      });
    case "RawUtxo":
      return buildMidgardWholeFieldViewV1({
        fieldIndex: exactField,
        preimage: rawCarriageBytes(referenceInputs, carriage.refInputIndex),
        expectedCommitment,
      });
    case "Certified": {
      const certInput = referenceInputAt(
        referenceInputs,
        carriage.certRefInputIndex,
      );
      if (certInput.certificate === undefined) {
        return failField(
          "tier-3 carriage names a reference input that carries no certificate",
          `ref_input_index=${carriage.certRefInputIndex}`,
        );
      }
      const expectedAssetName = midgardFieldPreimageCertificateAssetNameV1({
        txId,
        fieldIndex: exactField,
      });
      if (
        certInput.certificateAssetName !== undefined &&
        !expectedAssetName.equals(Buffer.from(certInput.certificateAssetName))
      ) {
        return failField(
          "certificate token is not the §8.6 (tx_id, field_index) name",
          `expected=${expectedAssetName.toString("hex")}`,
        );
      }
      return buildMidgardChunkedFieldViewV1({
        fieldIndex: exactField,
        txId,
        certificate: certInput.certificate,
        expectedCommitment,
        chunks: carriage.chunkRefInputIndices.map((index) =>
          rawCarriageBytes(referenceInputs, index),
        ),
      });
    }
  }
};

// ---------------------------------------------------------------------------
// Accessors — slice-only, straddle-aware
// ---------------------------------------------------------------------------

/**
 * Reveal-derived item count (§5.2). Every answer this returns is
 * authenticated: under tiers 1–2 the header is read out of a hash-checked
 * preimage that also passed §5.1's full-content check, and under tier 3 a
 * fixed-stride field's count is §7.4 arithmetic over a `totalLength` the
 * commitment check has pinned to the committed bytes.
 *
 * **It throws for a variable-width field under tier 3.** The header itself is
 * committed bytes like any other, but nothing reconciles the number it declares
 * with the items that follow — that is §5.1's walk, and tier 3 does not run it.
 * Rather than return a number nobody checked — which a count-consuming rule
 * would take for a fact — the door declines to answer. Reads are unaffected:
 * {@link midgardFieldItemAtV1} still serves such a view, and the envelope walk
 * behind it fails closed the moment it leaves the committed bytes.
 */
export const midgardFieldItemCountV1 = (view: MidgardFieldViewV1): number => {
  if (view.view === "Whole") {
    return view.count;
  }
  if (view.stride <= MIDGARD_WALK_DERIVED_STRIDE_V1) {
    return failField(
      "a variable-width field carried under tier 3 has no authenticated item count (§7.4)",
      "carriage=Certified",
    );
  }
  return view.count;
};

/**
 * The count as the view holds it, with no authentication claim attached.
 * Private, and used only as the range guard on a read — never handed to a
 * caller as the item count.
 */
const declaredItemCount = (view: MidgardFieldViewV1): number => view.count;

export const midgardFieldTotalLengthV1 = (view: MidgardFieldViewV1): number =>
  view.view === "Whole"
    ? view.bytes.length
    : view.chunks.reduce((total, chunk) => total + chunk.length, 0);

/**
 * Reads `length` bytes at `offset`, stitching across chunk boundaries and
 * verifying every chunk it touches (§8.8 straddle awareness, lazy verify).
 */
export const midgardFieldReadRangeV1 = (
  view: MidgardFieldViewV1,
  offset: number,
  length: number,
): Buffer =>
  view.view === "Whole"
    ? sliceExact(view.bytes, offset, length)
    : readChunkedRange(view, view.chunks, view.chunkDigests, offset, length);

const viewReader =
  (view: MidgardFieldViewV1) =>
  (offset: number, length: number): Buffer =>
    midgardFieldReadRangeV1(view, offset, length);

export type MidgardFieldItemExtentV1 = {
  readonly offset: number;
  readonly length: number;
};

/**
 * The `(offset, length)` of item `index`'s payload within the preimage.
 * Exposed because a resumable walk records positions, never bytes (§7.6).
 *
 * **Both branches read the item's own wrapper.** The fixed-stride arithmetic
 * says *where* item `index` begins; only the two `definite_bytes_header` bytes
 * there say that the item is spelled the one way §5.1 admits. Skipping them
 * would let `81 ‖ 00 00 ‖ <28 B>` and `81 ‖ ff ff ‖ <28 B>` open beside the
 * canonical `81 ‖ 58 1c ‖ <28 B>` and hand back the same payload — three
 * admissible byte forms for one logical field, which §6.1 forbids and which
 * would leave a non-canonically committed preimage unfaultable.
 */
export const midgardFieldItemExtentV1 = (
  view: MidgardFieldViewV1,
  index: number,
): MidgardFieldItemExtentV1 => {
  const count = declaredItemCount(view);
  if (!Number.isSafeInteger(index) || index < 0 || index >= count) {
    return failField(
      "field item index is out of range (§7.3 abort, never clamp)",
      `index=${index},count=${count}`,
    );
  }
  const read = viewReader(view);
  const headerLength = midgardFieldHeaderLengthForCountV1(count);
  if (view.stride > MIDGARD_WALK_DERIVED_STRIDE_V1) {
    const itemOffset = headerLength + view.stride * index;
    const { payloadOffset, length } = decodeItemHeaderAt(read, itemOffset);
    // Every fixed stride in §5.3 wraps 24..255 payload bytes, so the canonical
    // wrapper is exactly the two-byte `58 LL` form and the stride pins `LL`.
    if (
      payloadOffset !== itemOffset + MIDGARD_FIXED_ITEM_WRAPPER_BYTES_V1 ||
      length !== view.stride - MIDGARD_FIXED_ITEM_WRAPPER_BYTES_V1
    ) {
      return failGrammar(
        "fixed-stride item wrapper is not the canonical §5.1 spelling",
        `index=${index},stride=${view.stride},length=${length}`,
      );
    }
    return { offset: payloadOffset, length };
  }
  let cursor = headerLength;
  for (let remaining = index; ; remaining -= 1) {
    const { payloadOffset, length } = decodeItemHeaderAt(read, cursor);
    if (remaining <= 0) {
      return { offset: payloadOffset, length };
    }
    cursor = payloadOffset + length;
  }
};

/**
 * The canonical item encoding `enc_i` at `index`, with its byte-string wrapper
 * stripped. Fails unless `0 ≤ index < count` **and** the item's full byte
 * range lies inside the preimage (§7.3).
 */
export const midgardFieldItemAtV1 = (
  view: MidgardFieldViewV1,
  index: number,
): Buffer => {
  const { offset, length } = midgardFieldItemExtentV1(view, index);
  return midgardFieldReadRangeV1(view, offset, length);
};
