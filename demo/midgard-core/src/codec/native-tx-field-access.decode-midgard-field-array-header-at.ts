import { computeHash32 } from "./hash.js";
import {
  byteAt,
  encodeMidgardFieldPreimage,
  exactCount,
  exactMidgardFieldIndex,
  failField,
  failGrammar,
  MIDGARD_ADDRESS_WITNESS_STRIDE,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_HASH28_STRIDE,
  MIDGARD_MAX_FIELD_ITEM_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_SPEND_INPUT_STRIDE,
  MIDGARD_WALK_DERIVED_STRIDE,
  type MidgardFieldArrayHeader,
  wholeReader,
} from "./native-tx-field-access.encode-midgard-definite-bytes.js";

/**
 * The **one** §5.1 `definite_array_header(N)` decoder, at any offset.
 *
 * §5.1's acceptance set is narrower than CBOR's: minimal width only, capped at
 * the `99 NNNN` form. The four-byte `9a` head is well-formed CBOR and rejects
 * here, exactly as it does in `decode_field_array_header_at`. Every §5.1
 * envelope decoded in this package goes through this function, so the grammar
 * has one verdict rather than two.
 */
export const decodeMidgardFieldArrayHeaderAt = (
  bytes: Uint8Array,
  offset: number,
): MidgardFieldArrayHeader => {
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
    return failGrammar(
      "non-minimal §5.1 array header",
      `tag=99,count=${count}`,
    );
  }
  return { nextOffset: offset + 3, count };
};

export const decodeMidgardFieldArrayHeader = (
  preimage: Uint8Array,
): MidgardFieldArrayHeader => decodeMidgardFieldArrayHeaderAt(preimage, 0);

/**
 * §5.1's `definite_bytes_header(L)` at `offset`, minimal width, capped at
 * `59 LLLL`. The twin of the door's `item_header_at`.
 */
export const decodeItemHeaderAt = (
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
export const midgardFieldHeaderLengthForCount = (count: number): number => {
  const exact = exactCount(count, "field item count");
  if (exact <= 23) {
    return 1;
  }
  if (exact <= 0xff) {
    return 2;
  }
  if (exact > MIDGARD_MAX_FIELD_ITEM_COUNT) {
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
export const decodeMidgardFieldPreimage = (
  preimage: Uint8Array,
): readonly Buffer[] => {
  const read = wholeReader(preimage);
  const { nextOffset, count } = decodeMidgardFieldArrayHeader(preimage);
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
export const midgardFieldCommitment = (preimage: Uint8Array): Buffer =>
  computeHash32(preimage);

/** The producer twin of {@link midgardFieldCommitment}: envelope, then hash. */
export const midgardFieldCommitmentFromItems = (
  items: readonly Uint8Array[],
): Buffer => midgardFieldCommitment(encodeMidgardFieldPreimage(items));

/**
 * `blake2b_256(#"80")` — what every empty field commits to.
 *
 * Under §4's plain hashing the commitment carries no field index, so all nine
 * empty fields share this value. The Aiken twin pins the same 32 bytes as
 * `native_tx_field_access_v1.empty_field_commitment`; the cross-language
 * golden vector proves the two against each other.
 */
export const MIDGARD_EMPTY_FIELD_COMMITMENT: Buffer =
  midgardFieldCommitmentFromItems([]);

/** §5.3 stride table. `0` marks the variable-width fields. */
export const midgardFieldStride = (fieldIndex: number): number => {
  const exact = exactMidgardFieldIndex(fieldIndex);
  if (exact === 0 || exact === 1) {
    return MIDGARD_SPEND_INPUT_STRIDE;
  }
  if (exact === 3 || exact === 4) {
    return MIDGARD_HASH28_STRIDE;
  }
  if (exact === 7) {
    return MIDGARD_ADDRESS_WITNESS_STRIDE;
  }
  return MIDGARD_WALK_DERIVED_STRIDE;
};

// ---------------------------------------------------------------------------
// §8.4/§8.6 chunking and certificates
// ---------------------------------------------------------------------------

/**
 * §8.4's deterministic split rule: chunk `j` is bytes `[j·K, (j+1)·K)` with a
 * ragged last chunk, minimum-necessary chunks by construction.
 */
export const midgardExpectedChunkCount = (totalLength: number): number => {
  const exact = exactCount(totalLength, "preimage total length");
  if (exact === 0) {
    return failField("tier-3 total length must be positive", "total_length=0");
  }
  return Math.ceil(exact / MIDGARD_CHUNK_BYTES_K);
};

/**
 * §8.4. Determinism is what makes independent publishers byte-compatible:
 * identical chunks, identical digest vectors, interchangeable certificates.
 */
export const splitMidgardFieldPreimageIntoChunks = (
  preimage: Uint8Array,
): readonly Buffer[] => {
  if (preimage.length > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES) {
    return failField(
      "field preimage exceeds the §5.4 aggregate bound",
      `length=${preimage.length}`,
    );
  }
  const chunks: Buffer[] = [];
  for (let start = 0; start < preimage.length; start += MIDGARD_CHUNK_BYTES_K) {
    chunks.push(
      Buffer.from(
        preimage.subarray(
          start,
          Math.min(start + MIDGARD_CHUNK_BYTES_K, preimage.length),
        ),
      ),
    );
  }
  return chunks;
};

/** §8.6. The tier-3 digest manifest the certificate validator mints. */
export type MidgardFieldPreimageCertificate = {
  /** Min-Ada reclaim authority, set by the minter. 28-byte vkey hash. */
  readonly owner: Buffer;
  /** The L2 transaction's id (32 bytes). */
  readonly txId: Buffer;
  /** 0..8. */
  readonly fieldIndex: number;
  /**
   * The §4 flat field commitment of the whole preimage — mint-welded to
   * `chunkDigests` (#606, owner ruling 2026-08-16): the policy checks
   * `hash(concat(chunks))` against this exact value, so a consumer comparing
   * it to a commitment it authenticated itself is talking about these bytes.
   */
  readonly fieldHash: Buffer;
  /** Preimage byte length — ragged-last plus offset math. */
  readonly totalLength: number;
  /** `blake2b_256` per chunk, in order; length = `ceil(totalLength / K)`. */
  readonly chunkDigests: readonly Buffer[];
};

export const exactBytes = (
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
 * §8.6's certificate asset name — one constant for every certificate of the
 * policy (#606, owner ruling 2026-08-16; supersedes the retired
 * `blake2b_256(field_index_byte ‖ tx_id)` derivation). The name is branding,
 * not identity: everything the derived name encoded is in the mint-verified
 * datum, which since #606 also carries the welded `fieldHash`. The mint still
 * pins the constant, so a token of the policy is always this name over a
 * datum the mint proved. Discovery is by enumerating the single certificate
 * address and filtering by datum; same-name tokens are disambiguated by
 * datum, never by token alone.
 *
 * ASCII "MIDGARD_FIELD_PREIMAGE_CERT", 27 bytes — the twin of
 * `field_preimage_certificate_asset_name` in
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`, pinned by the
 * cross-language golden channel.
 */
export const MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME: Buffer =
  Buffer.from("MIDGARD_FIELD_PREIMAGE_CERT", "ascii");
