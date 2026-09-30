/**
 * The TypeScript twin of `onchain/aiken/lib/midgard/native-tx-carriage-v1.ak`
 * — the **publication half** of the `docs/spec/midgard-tx.md` §8 field-preimage
 * carriage ladder.
 *
 * `./native-tx-field-access.js` is the consumer half: it takes carriage that
 * already exists and turns it into an authenticated view. This module is what
 * decides which carriage should exist in the first place, and it owns exactly
 * three things:
 *
 *   * **The plan (§8.1–§8.4).** Given a preimage and the `(tx_id, field_index)`
 *     it belongs to, which tier carries it, what has to be published, and what
 *     certificate — if any — has to be minted. Tier selection is a total
 *     function of the preimage's length, so it is never a builder's choice.
 *   * **The reference-input layout.** Tier-3 carriage names its manifest and
 *     its chunks by *position*, and a plan that produces indices separately
 *     from the reference-input list they index into is a plan with two places
 *     to get an off-by-one. {@link layOutMidgardFieldCarriage} emits both
 *     from one traversal, so the carriage a step's redeemer carries and the
 *     reference inputs the step resolves cannot disagree.
 *   * **Healing (§8.7).** Content addressing is the whole mechanism: because
 *     the §8.4 split is a pure function of the preimage bytes, an unrelated
 *     party who obtains the same preimage produces byte-identical publications
 *     and an interchangeable certificate. {@link healMidgardFieldCarriage}
 *     is that re-derivation, and
 *     {@link midgardFieldCarriagePlansAreInterchangeable} is the predicate a
 *     caller checks it by rather than trusting it.
 *
 * **What a tier is, and is not, visible to.** The tier is branched on *inside
 * this module* — three times, and each one is a place where the three tiers
 * really are three different objects: {@link planMidgardFieldCarriage}
 * (nothing, one UTxO or `n` UTxOs plus a certificate to publish),
 * {@link layOutMidgardFieldCarriage} (a different carriage constructor and a
 * different reference-input layout) and
 * {@link midgardFieldCarriagePlansAreInterchangeable} (only tier 3 has a
 * certificate to compare). What the claim is really about is the boundary, not
 * the count: **no caller of this module branches on the tier**, because
 * {@link layOutMidgardFieldCarriage} hands back a
 * {@link MidgardFieldCarriage} and the reference inputs it indexes, and
 * `authenticatedMidgardFieldViewV1` turns any of the three into the same
 * {@link MidgardFieldViewV1}. That is §8's simplest-fitting-first mandate
 * expressed as a type: there is no tier-shaped argument anywhere downstream of
 * this module. The one place tier survives into consumer-visible *behaviour*
 * is `midgardFieldItemCountV1`, which declines to answer for a variable-width
 * field under tier 3 because §7.4's arithmetic does not apply there and no
 * other check reconciles the declared count — a documented refusal, never a
 * different answer.
 *
 * **Nothing here authenticates anything.** Raw carriage is unauthenticated data
 * (§8.5) and a plan is a statement about bytes the caller already holds. The
 * `(tx_id, field_index)` a plan is built against is used to derive the
 * certificate and its asset name; it is *not* evidence that the preimage is
 * that field's. What proves that is the §4 commitment check the door runs on
 * the way back in, and — on-chain — the minting policy that would not have let
 * the certificate exist otherwise.
 */

import "../consensus-profile.js";
import "./errors.js";
import "./native-tx-field-access.js";
import "./native-tx-carriage.plan-midgard-field-carriage.js";
import "./native-tx-carriage.lay-out-midgard-field-carriage.js";
import "./native-tx-carriage.midgard-field-carriage-publishability.js";
export {
  layOutMidgardFieldCarriage,
  MIDGARD_CARRIAGE_PUBLICATION_FIXED_FRAMING_BYTES,
  MIDGARD_CARRIAGE_RELIABILITY_RESERVE_BYTES,
  midgardCarriageDataByteStringBytes,
  midgardCarriagePublicationBytes,
  midgardCarriagePublicationFramingBytes,
  type MidgardFieldCarriageLayout,
  midgardFieldCarriagePlansAreInterchangeable,
} from "./native-tx-carriage.lay-out-midgard-field-carriage.js";
export {
  MIDGARD_EXACT_PUBLISHABLE_CARRIAGE_BYTES,
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardFieldCarriageBounds,
  midgardFieldCarriagePublishability,
  type MidgardUnpublishableChunk,
} from "./native-tx-carriage.midgard-field-carriage-publishability.js";
export {
  healMidgardFieldCarriage,
  type MidgardFieldCarriagePlan,
  type MidgardFieldCarriageTier,
  type MidgardFieldPublication,
  planMidgardFieldCarriage,
} from "./native-tx-carriage.plan-midgard-field-carriage.js";
