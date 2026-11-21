import {
  buildMidgardChunkedFieldView,
  rawCarriageBytes,
  referenceInputAt,
  type ResolvedCarriageReferenceInput,
} from "./native-tx-field-access.build-midgard-chunked-field-view.js";
import {
  buildMidgardWholeFieldView,
  type MidgardFieldCarriage,
  type MidgardFieldView,
  readChunkedRange,
} from "./native-tx-field-access.build-midgard-whole-field-view.js";
import {
  decodeItemHeaderAt,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  midgardFieldHeaderLengthForCount,
} from "./native-tx-field-access.decode-midgard-field-array-header-at.js";
import {
  exactMidgardFieldIndex,
  failField,
  failGrammar,
  MIDGARD_FIXED_ITEM_WRAPPER_BYTES,
  MIDGARD_WALK_DERIVED_STRIDE,
  sliceExact,
} from "./native-tx-field-access.encode-midgard-definite-bytes.js";

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
export const authenticatedMidgardFieldView = ({
  fieldIndex,
  txId,
  expectedCommitment,
  carriage,
  referenceInputs = [],
}: {
  readonly fieldIndex: number;
  readonly txId: Uint8Array;
  readonly expectedCommitment: Uint8Array;
  readonly carriage: MidgardFieldCarriage;
  readonly referenceInputs?: readonly ResolvedCarriageReferenceInput[];
}): MidgardFieldView => {
  const exactField = exactMidgardFieldIndex(fieldIndex);
  switch (carriage.carriage) {
    case "Inline":
      return buildMidgardWholeFieldView({
        fieldIndex: exactField,
        preimage: carriage.preimage,
        expectedCommitment,
      });
    case "RawUtxo":
      return buildMidgardWholeFieldView({
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
      if (
        certInput.certificateAssetName !== undefined &&
        !MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME.equals(
          Buffer.from(certInput.certificateAssetName),
        )
      ) {
        return failField(
          "certificate token is not the §8.6 constant name (#606)",
          `expected=${MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME.toString("hex")}`,
        );
      }
      return buildMidgardChunkedFieldView({
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
 * {@link midgardFieldItemAt} still serves such a view, and the envelope walk
 * behind it fails closed the moment it leaves the committed bytes.
 */
export const midgardFieldItemCount = (view: MidgardFieldView): number => {
  if (view.view === "Whole") {
    return view.count;
  }
  if (view.stride <= MIDGARD_WALK_DERIVED_STRIDE) {
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
const declaredItemCount = (view: MidgardFieldView): number => view.count;

export const midgardFieldTotalLength = (view: MidgardFieldView): number =>
  view.view === "Whole"
    ? view.bytes.length
    : view.chunks.reduce((total, chunk) => total + chunk.length, 0);

/**
 * Reads `length` bytes at `offset`, stitching across chunk boundaries and
 * verifying every chunk it touches (§8.8 straddle awareness, lazy verify).
 */
export const midgardFieldReadRange = (
  view: MidgardFieldView,
  offset: number,
  length: number,
): Buffer =>
  view.view === "Whole"
    ? sliceExact(view.bytes, offset, length)
    : readChunkedRange(view, view.chunks, view.chunkDigests, offset, length);

const viewReader =
  (view: MidgardFieldView) =>
  (offset: number, length: number): Buffer =>
    midgardFieldReadRange(view, offset, length);

export type MidgardFieldItemExtent = {
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
export const midgardFieldItemExtent = (
  view: MidgardFieldView,
  index: number,
): MidgardFieldItemExtent => {
  const count = declaredItemCount(view);
  if (!Number.isSafeInteger(index) || index < 0 || index >= count) {
    return failField(
      "field item index is out of range (§7.3 abort, never clamp)",
      `index=${index},count=${count}`,
    );
  }
  const read = viewReader(view);
  const headerLength = midgardFieldHeaderLengthForCount(count);
  if (view.stride > MIDGARD_WALK_DERIVED_STRIDE) {
    const itemOffset = headerLength + view.stride * index;
    const { payloadOffset, length } = decodeItemHeaderAt(read, itemOffset);
    // Every fixed stride in §5.3 wraps 24..255 payload bytes, so the canonical
    // wrapper is exactly the two-byte `58 LL` form and the stride pins `LL`.
    if (
      payloadOffset !== itemOffset + MIDGARD_FIXED_ITEM_WRAPPER_BYTES ||
      length !== view.stride - MIDGARD_FIXED_ITEM_WRAPPER_BYTES
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
export const midgardFieldItemAt = (
  view: MidgardFieldView,
  index: number,
): Buffer => {
  const { offset, length } = midgardFieldItemExtent(view, index);
  return midgardFieldReadRange(view, offset, length);
};
