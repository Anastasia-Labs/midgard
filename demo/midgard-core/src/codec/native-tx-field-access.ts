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
 *     concatenated chunks against `expectedCommitment`. See
 *     {@link buildMidgardChunkedFieldView} for what it costs and why §4
 *     leaves no cheaper option.
 *
 * **The two acceptance sets are not identical at tier 3, and diverge in both
 * directions.** An earlier revision of this header claimed they were; they are
 * not, and the claim was doing no work that stating the real relationship does not
 * do better. Aiken has *two* tier-3 entry points and this module corresponds to
 * neither exactly:
 *
 *   * Against Aiken's lazy `authenticated_field_view`, this module is **stricter**:
 *     that door authenticates a `Certified` field by requiring the §8.6 token and
 *     leaves the chunk bytes unhashed until an accessor touches one, while this one
 *     hashes the concatenation at construction (third deviation above).
 *   * Against Aiken's `authenticated_whole_field_view` — the entry point the
 *     tx-order mint opens — this module is **looser in one place and stricter in
 *     another**. Looser: that door concatenates the chunks and hands them to
 *     `whole_view`, which runs the full §5.1 `walk_to_end` count-consistency check
 *     for the variable-width fields; this module deliberately does not (see
 *     {@link buildMidgardChunkedFieldView}, "Why no walk here") and substitutes
 *     the O(1) `header_len + count ≤ total_length` bound. Stricter: that door
 *     collapses to a `Whole` view and *discards* the certificate's digest vector,
 *     so nothing afterwards ever checks a chunk against its certified digest,
 *     whereas this module keeps the vector and verifies each chunk the first time a
 *     read reaches it.
 *
 * **Why neither divergence has a witness in the paths that exist.** The stricter
 * direction cannot refuse anything the whole-door accepts: a chunk that is part of
 * a concatenation which already matched §4's `expectedCommitment` also matches its
 * own certified digest, whenever the certificate is the one §8.6's policy minted
 * for those chunks — and a certificate for any other chunk set is refused at the
 * cert-name and digest-vector checks before a byte is read. The looser direction
 * needs a tier-3 preimage that matches the committed field hash yet whose §5.1
 * header count does not reconcile with its content; both sides reach a tier-3 view
 * only through such a certificate, so they are looking at the *same* bytes, and the
 * question is only whether those bytes are structurally well-formed. Where the
 * on-chain whole-door is the authority it refuses first — a tx-order mint over such
 * a payload never produces an order NFT, so ingestion never receives it — and where
 * this module is used to predict an on-chain step, a looser prediction yields a
 * step the chain rejects rather than a state it accepts. The residual is therefore
 * that this module is not a substitute for the on-chain walk on adversarial input;
 * it is honest about that rather than claiming a parity it does not have.
 */

import "./errors.js";
import "./hash.js";
import "./native-tx-field-access.encode-midgard-definite-bytes.js";
import "./native-tx-field-access.decode-midgard-field-array-header-at.js";
import "./native-tx-field-access.build-midgard-whole-field-view.js";
import "./native-tx-field-access.build-midgard-chunked-field-view.js";
import "./native-tx-field-access.authenticated-midgard-field-view.js";
export {
  authenticatedMidgardFieldView,
  midgardFieldItemAt,
  midgardFieldItemCount,
  type MidgardFieldItemExtent,
  midgardFieldItemExtent,
  midgardFieldReadRange,
  midgardFieldTotalLength,
} from "./native-tx-field-access.authenticated-midgard-field-view.js";
export {
  buildMidgardChunkedFieldView,
  type ResolvedCarriageReferenceInput,
} from "./native-tx-field-access.build-midgard-chunked-field-view.js";
export {
  buildMidgardWholeFieldView,
  deriveMidgardFieldPreimageCertificate,
  type MidgardFieldCarriage,
  type MidgardFieldView,
  selectMidgardFieldCarriageTier,
} from "./native-tx-field-access.build-midgard-whole-field-view.js";
export {
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldArrayHeaderAt,
  decodeMidgardFieldPreimage,
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  midgardExpectedChunkCount,
  midgardFieldCommitment,
  midgardFieldCommitmentFromItems,
  midgardFieldHeaderLengthForCount,
  type MidgardFieldPreimageCertificate,
  midgardFieldStride,
  splitMidgardFieldPreimageIntoChunks,
} from "./native-tx-field-access.decode-midgard-field-array-header-at.js";
export {
  encodeMidgardDefiniteBytes,
  encodeMidgardFieldArrayHeader,
  encodeMidgardFieldPreimage,
  exactMidgardFieldIndex,
  MIDGARD_ADDRESS_WITNESS_ITEM_BYTES,
  MIDGARD_ADDRESS_WITNESS_STRIDE,
  MIDGARD_CHUNK_BYTES_K,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS,
  MIDGARD_FIELD_COUNT,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS,
  MIDGARD_FIXED_ITEM_WRAPPER_BYTES,
  MIDGARD_HASH28_ITEM_BYTES,
  MIDGARD_HASH28_STRIDE,
  MIDGARD_MAX_FIELD_ITEM_COUNT,
  MIDGARD_MAX_SPEND_INPUTS_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES,
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_MAXIMUM_CARDANO_SPEND_REDEEMER_COUNT,
  MIDGARD_SPEND_INPUT_ITEM_BYTES,
  MIDGARD_SPEND_INPUT_STRIDE,
  MIDGARD_WALK_DERIVED_STRIDE,
  type MidgardFieldArrayHeader,
} from "./native-tx-field-access.encode-midgard-definite-bytes.js";
