import {
  decodeItemHeaderAt,
  decodeMidgardFieldArrayHeader,
  exactBytes,
  midgardFieldCommitment,
  type MidgardFieldPreimageCertificate,
  midgardFieldStride,
  splitMidgardFieldPreimageIntoChunks,
} from "./native-tx-field-access.decode-midgard-field-array-header-at.js";
import {
  exactCount,
  exactMidgardFieldIndex,
  failField,
  failGrammar,
  failHash,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS,
  MIDGARD_MAX_FIELD_ITEM_COUNT,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_WALK_DERIVED_STRIDE,
  sliceExact,
  wholeReader,
} from "./native-tx-field-access.encode-midgard-definite-bytes.js";

/**
 * Builds the §8.6 certificate an off-chain publisher mints for a tier-3
 * preimage. `preimage_len > K` is the §8.4 tier boundary and is enforced here
 * for the same reason the door enforces it on the way in: the ladder is a
 * partition, so a preimage that fits tier 1 or tier 2 has exactly one
 * admissible carriage.
 */
export const deriveMidgardFieldPreimageCertificate = ({
  owner,
  txId,
  fieldIndex,
  preimage,
}: {
  readonly owner: Uint8Array;
  readonly txId: Uint8Array;
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
}): MidgardFieldPreimageCertificate => {
  if (preimage.length <= MIDGARD_CHUNK_BYTES_K) {
    return failField(
      "tier-3 certification requires preimage_len > K (§8.4)",
      `length=${preimage.length},k=${MIDGARD_CHUNK_BYTES_K}`,
    );
  }
  const chunks = splitMidgardFieldPreimageIntoChunks(preimage);
  return {
    owner: exactBytes(owner, 28, "certificate owner"),
    txId: exactBytes(txId, 32, "certificate tx_id"),
    fieldIndex: exactMidgardFieldIndex(fieldIndex),
    // The mint weld (#606): the §4 commitment of the bytes themselves, the
    // same value the policy checks `hash(concat(chunks))` against.
    fieldHash: midgardFieldCommitment(preimage),
    totalLength: preimage.length,
    chunkDigests: chunks.map(midgardFieldCommitment),
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
export type MidgardFieldCarriage =
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
export type MidgardFieldView =
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
export const selectMidgardFieldCarriageTier = (
  preimageLength: number,
): (typeof MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS)[number] => {
  const exact = exactCount(preimageLength, "preimage length");
  if (exact > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES) {
    return failField(
      "field preimage exceeds the §5.4 aggregate bound",
      `length=${exact}`,
    );
  }
  if (exact <= MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES) {
    return "Inline";
  }
  if (exact <= MIDGARD_CHUNK_BYTES_K) {
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
const verifiedChunkIndexes = new WeakMap<MidgardFieldView, Set<number>>();

const verifiedChunksOf = (view: MidgardFieldView): Set<number> => {
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
  view: MidgardFieldView | undefined,
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
  if (!midgardFieldCommitment(chunk).equals(digest)) {
    return failHash(
      "chunk does not match its certified digest",
      `chunk_index=${chunkIndex}`,
    );
  }
  verified?.add(chunkIndex);
  return chunk;
};

export const readChunkedRange = (
  view: MidgardFieldView | undefined,
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
    const chunkIndex = Math.floor(cursor / MIDGARD_CHUNK_BYTES_K);
    const within = cursor - chunkIndex * MIDGARD_CHUNK_BYTES_K;
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
export const buildMidgardWholeFieldView = ({
  fieldIndex,
  preimage,
  expectedCommitment,
}: {
  readonly fieldIndex: number;
  readonly preimage: Uint8Array;
  readonly expectedCommitment: Uint8Array;
}): MidgardFieldView => {
  const stride = midgardFieldStride(fieldIndex);
  const bytes = Buffer.from(preimage);
  const actual = midgardFieldCommitment(bytes);
  if (!actual.equals(Buffer.from(expectedCommitment))) {
    return failHash(
      "field preimage does not match the committed field hash",
      `field_index=${fieldIndex},actual=${actual.toString("hex")}`,
    );
  }
  const totalLength = bytes.length;
  if (totalLength > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES) {
    return failField(
      "field preimage exceeds the §5.4 aggregate bound",
      `length=${totalLength}`,
    );
  }
  const { nextOffset: headerLength, count } =
    decodeMidgardFieldArrayHeader(bytes);
  if (stride > MIDGARD_WALK_DERIVED_STRIDE) {
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

export const countFromTotalLength = (
  stride: number,
  totalLength: number,
): number => {
  for (const headerLength of [1, 2, 3]) {
    const candidate = Math.floor((totalLength - headerLength) / stride);
    const inRange =
      headerLength === 1
        ? candidate >= 0 && candidate <= 23
        : headerLength === 2
          ? candidate >= 24 && candidate <= 0xff
          : candidate > 0xff && candidate <= MIDGARD_MAX_FIELD_ITEM_COUNT;
    if (inRange && headerLength + stride * candidate === totalLength) {
      return candidate;
    }
  }
  return failGrammar(
    "no §5.1 header width reconciles the certified total length with the stride",
    `stride=${stride},total_length=${totalLength}`,
  );
};
