/**
 * The TypeScript twins of the **nine per-field item encodings** of
 * `docs/spec/midgard-tx.md` §5.3, and of the per-field §5.1 preimage producers
 * built over them.
 *
 * `native-tx-field-access.ts` owns what all nine fields *share* — the §5.1
 * envelope, the §4 flat commitment, the §5.3 stride table, the §8 carriage
 * ladder. This module owns what makes each field itself: the bytes of `enc_i`.
 * Together they are the off-chain half of §1's "Encoders" obligation, and the
 * cross-language golden vectors in
 * `tests/fixtures/native-tx-field-items-v1.generated.json` pin them against the
 * Aiken producers in
 * `onchain/aiken/lib/midgard/fraud-proofs/native-tx/{preimages,components}.ak`.
 *
 * Two of the nine item encoders already existed as canonical encoders and are
 * **reused** rather than re-spelled — §5.3 says their interiors are "unchanged
 * from the current canonical encoders", so a second spelling here would be a
 * divergence waiting to happen:
 *
 *   * field 2 — {@link encodeMidgardTxOutput} (§5.5);
 *   * field 6 — {@link encodeMidgardVersionedScript} (§5.3's tag table).
 *
 * Reused, not re-exported: both keep their own module as their single export
 * site, for the barrel reason spelled out at the import block below.
 *
 * The rest are new here because the flat reversion changed them: fields 0/1
 * gained the fixed 3-byte output index, fields 3/4 gained an asserted 28-byte
 * width, field 5 moved from a raw map to enveloped per-policy items, and fields
 * 7/8 moved from raw concatenation into the §5.1 envelope.
 *
 * **This module is a producer, not an access idiom.** Reading a *committed*
 * field goes through `authenticatedMidgardFieldViewV1`; what is here builds the
 * bytes that get committed.
 */

import "./cbor.js";
import "./errors.js";
import "./native-tx-field-access.js";
import "./output.js";
import "./versioned-script.js";
import "./native-tx-field-items.encode-midgard-mint-policy-item.js";
import "./native-tx-field-items.midgard-field-items.js";
export {
  compareMidgardCanonicalKeyBytes,
  encodeMidgardFixedOutputIndex,
  encodeMidgardHash28Item,
  encodeMidgardMintPolicyItem,
  encodeMidgardSpendInputItem,
  MIDGARD_MAX_OUTPUT_INDEX,
  type MidgardMintAsset,
  type MidgardMintPolicyItem,
  type MidgardTxInput,
  sortMidgardMintItems,
} from "./native-tx-field-items.encode-midgard-mint-policy-item.js";
export {
  encodeMidgardAddressWitnessItem,
  encodeMidgardFieldItems,
  encodeMidgardFieldPreimageForField,
  encodeMidgardRedeemerWitnessItem,
  MIDGARD_FIELD_NAMES,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  type MidgardAddressWitness,
  type MidgardExecutionUnits,
  midgardFieldCommitmentForField,
  type MidgardFieldItems,
  type MidgardRedeemerPurpose,
  type MidgardRedeemerWitness,
} from "./native-tx-field-items.midgard-field-items.js";
