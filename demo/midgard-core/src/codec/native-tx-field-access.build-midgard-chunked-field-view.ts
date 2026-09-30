import {
  countFromTotalLength,
  type MidgardFieldView,
  readChunkedRange,
} from "./native-tx-field-access.build-midgard-whole-field-view.js";
import {
  decodeMidgardFieldArrayHeader,
  exactBytes,
  midgardExpectedChunkCount,
  midgardFieldCommitment,
  type MidgardFieldPreimageCertificate,
  midgardFieldStride,
} from "./native-tx-field-access.decode-midgard-field-array-header-at.js";
import {
  exactCount,
  exactMidgardFieldIndex,
  failField,
  failGrammar,
  failHash,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_WALK_DERIVED_STRIDE,
} from "./native-tx-field-access.encode-midgard-definite-bytes.js";

/**
 * Tier 3 (§8.4): the carriage is proved against `expectedCommitment` once, and
 * chunks then stay unverified against their certified digests until an accessor
 * touches them.
 *
 * **Why the preimage is hashed here.** On-chain the certificate is
 * mint-verified — the policy checked the chunks against the field hash, so
 * `totalLength` and the digest vector arrive as authenticated data and
 * `certified_view` never re-hashes anything. Off-chain a
 * `MidgardFieldPreimageCertificate` is just a record the caller handed over;
 * no policy stands behind it, and the §8.6 constant asset name a door can
 * check names the policy, never the content (#606). Authentication therefore
 * has to happen here or nowhere, and §4 commits the *whole* preimage under one
 * flat `blake2b_256` with no Merkle structure to open a single chunk against —
 * so authenticating any byte of a tier-3 field means hashing all of them.
 *
 * That is the trade this function makes: one `blake2b_256` over at most 32,768
 * bytes (§5.4) at construction, in exchange for a view whose bytes are the
 * field's. It is the same bound `buildMidgardWholeFieldView` already pays for
 * tiers 1–2, and §7.1 authenticate-once means a caller pays it per field, not
 * per read. What it does *not* buy back is chunk-level laziness at the door: a
 * caller that reads one item still hashes the whole preimage, because §4 gives
 * no way to authenticate less. Laziness below the door is untouched — the
 * per-chunk digest check still happens only on the chunks a read reaches. That
 * retained check is one of the two tier-3 divergences this module's header
 * enumerates, not a parity: it matches Aiken's *lazy* `authenticated_field_view`,
 * which also verifies a touched chunk against the certificate's digest, but
 * `authenticated_whole_field_view` — the entry point the tx-order mint opens —
 * collapses to a `Whole` view and discards the digest vector, so nothing there
 * ever re-checks a chunk. Against that door this module is therefore stricter
 * here, and the header's "Why neither divergence has a witness" paragraph is why
 * the extra strictness cannot refuse anything the whole-door accepts.
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
 * `buildMidgardWholeFieldView` runs is not available here, and this function
 * does not pretend otherwise: a fixed-stride field keeps the full §7.4
 * arithmetic check against `totalLength`, and a variable-width field's header
 * count is committed but never reconciled with its content, which is why
 * {@link midgardFieldItemCount} refuses to answer for one.
 */
export const buildMidgardChunkedFieldView = ({
  fieldIndex,
  txId,
  certificate,
  chunks,
  expectedCommitment,
}: {
  readonly fieldIndex: number;
  readonly txId: Uint8Array;
  readonly certificate: MidgardFieldPreimageCertificate;
  readonly chunks: readonly Uint8Array[];
  /**
   * §4's committed field hash, extracted positionally from a compact structure
   * by the caller — the same obligation {@link buildMidgardWholeFieldView}
   * carries, and required for the same reason: a view is an *authenticated*
   * field, so there is no admissible way to build one without it.
   */
  readonly expectedCommitment: Uint8Array;
}): MidgardFieldView => {
  const exactField = exactMidgardFieldIndex(fieldIndex);
  const stride = midgardFieldStride(exactField);
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
  // The load-bearing E2 equality (#606), mirrored from the on-chain door: the
  // certificate's mint-welded `field_hash` must be the commitment the caller
  // authenticated. Off-chain the reconstruction below re-establishes the
  // commitment anyway, but a manifest welded to some other hash is a manifest
  // the on-chain door would refuse, and this twin must refuse it in step.
  if (!certificate.fieldHash.equals(Buffer.from(expectedCommitment))) {
    return failField(
      "certificate field_hash does not match the anchored commitment (#606)",
      `certificate=${certificate.fieldHash.toString("hex")}`,
    );
  }
  const totalLength = exactCount(certificate.totalLength, "certificate total");
  // §8.4 defines tier 3 as the `preimage_len > K` case. Enforcing that lower
  // bound is what makes the ladder a partition rather than a preference:
  // without it a single-chunk "certificate" authenticates a preimage of any
  // size, and every structural check tiers 1-2 run at view construction can be
  // side-stepped by re-carrying the same bytes here.
  if (totalLength <= MIDGARD_CHUNK_BYTES_K) {
    return failField(
      "tier-3 certificate must declare total_length > K (§8.4)",
      `total_length=${totalLength}`,
    );
  }
  if (totalLength > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES) {
    return failField(
      "certificate total_length exceeds the §5.4 aggregate bound",
      `total_length=${totalLength}`,
    );
  }
  const chunkCount = certificate.chunkDigests.length;
  if (chunkCount !== midgardExpectedChunkCount(totalLength)) {
    return failField(
      "certificate digest vector does not match the §8.4 chunk count",
      `digests=${chunkCount},total_length=${totalLength}`,
    );
  }
  if (chunkCount > MIDGARD_MAX_TIER3_CHUNK_COUNT) {
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
        ? totalLength - index * MIDGARD_CHUNK_BYTES_K
        : MIDGARD_CHUNK_BYTES_K;
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
  const actual = midgardFieldCommitment(Buffer.concat(exactChunks));
  if (!actual.equals(Buffer.from(expectedCommitment))) {
    return failHash(
      "field preimage does not match the committed field hash",
      `field_index=${exactField},carriage=Certified,actual=${actual.toString("hex")}`,
    );
  }
  const digests = certificate.chunkDigests.map((digest, index) =>
    exactBytes(digest, 32, `certificate chunk digest ${index}`),
  );
  if (stride > MIDGARD_WALK_DERIVED_STRIDE) {
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
    decodeMidgardFieldArrayHeader(header);
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
export type ResolvedCarriageReferenceInput = {
  /** §8.5 raw carriage: the nothing-but-bytes inline datum payload. */
  readonly inlineDatumBytes?: Uint8Array;
  /** §8.6 manifest: the decoded certificate datum. */
  readonly certificate?: MidgardFieldPreimageCertificate;
  /**
   * The certificate token's asset name as observed at that UTxO, checked
   * against the §8.6 constant when supplied (#606: every certificate token
   * carries the one constant name; identity lives in the datum).
   *
   * This is an *identity* check, not an authentication: it catches a carriage
   * pointing at a token that is not the certificate policy's at all. What
   * proves the bytes are the field's is the §4 commitment check in
   * {@link buildMidgardChunkedFieldView} together with the welded
   * `fieldHash` equality, neither of which depends on this field being
   * supplied.
   */
  readonly certificateAssetName?: Uint8Array;
};

export const referenceInputAt = (
  referenceInputs: readonly ResolvedCarriageReferenceInput[],
  index: number,
): ResolvedCarriageReferenceInput => {
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

export const rawCarriageBytes = (
  referenceInputs: readonly ResolvedCarriageReferenceInput[],
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
