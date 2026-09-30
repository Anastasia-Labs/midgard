import "./complete-item-carriage-tiers-emulator.outputs-for-field-two-preimage-bytes.js";

import { MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";

/**
 * §8.4's partition at the two field-2 preimage sizes #600 measured — the old
 * C21 frontier and the exact maximum admissible output — carried here by two
 * items apiece rather than by one, for the reason
 * {@link outputsForFieldTwoPreimageBytes} states.
 */
export const TIER2_PREIMAGE_BYTES = 14_778;

export const TIER3_PREIMAGE_BYTES = 16_388;

/**
 * The single output at the applied publication cap, and the field-2 envelope it
 * produces: `81 ‖ 59 LLLL ‖ item` is four bytes of framing, so
 * 14,396 -> 14,400.
 *
 * **This is where the publication-maximum case lives now** (owner ruling,
 * 2026-08-14). It used to sit in
 * `complete-item-proof-fit-emulator.test.ts`, whose harness is tier-1 only.
 * That suite selected its complete-item step by `(phase, kind)` alone, so it
 * silently measured field 0's few-dozen-byte preimage instead of field 2's; the
 * row appeared to run at the cap while never carrying anything near it. Once the
 * selector was corrected the row could not run there at all, because 14,400
 * bytes is tier-2 `RawUtxo` and that harness refuses anything but tier-1
 * `Inline`. A >tier-1 publication belongs in this suite.
 *
 * **#580 NOTE — the 64-byte overhang.** `maxSinglePublicationCompleteItemBytes`
 * is 14,396, but §8.4's tier-1 ceiling of
 * `MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES` = 14,336 admits an item of at
 * most **14,332** bytes once the 4-byte field-2 envelope is counted. The applied
 * publication cap therefore sits **64 bytes above** the largest item whose field
 * can be carried inline, so items in (14,332, 14,396] are publishable but not
 * inline-carriable. That is a real gap, not an artefact of this retarget, and
 * **#580 has now measured and dispositioned it**: the band selects tier 2
 * (`RawUtxo`) rather than failing, so the gap is a carriage-tier transition and
 * not a hole — see §8.3 of `docs/spec/midgard-tx.md`. The assertion below is
 * unchanged and stays as an anti-conflation guard, so a future change that
 * silently equates the publication cap with the inline ceiling goes red.
 * Recorded in prose at §8.10 of `docs/spec/midgard-tx.md` too.
 */
export const PUBLICATION_MAXIMUM_ITEM_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes;

export const PUBLICATION_MAXIMUM_PREIMAGE_BYTES = 14_400;

/**
 * The four bytes a single-item field-2 §5.1 envelope costs: `81 ‖ 59 LLLL`.
 * Named so the derivation below states the arithmetic instead of restating its
 * result — 14,332 is not an independent measurement, it is the tier-1 ceiling
 * minus this envelope, and a change to that ceiling must move it.
 */
const SINGLE_ITEM_FIELD_ENVELOPE_BYTES = 4;

export const TIER1_ADMISSIBLE_ITEM_BYTES =
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES - SINGLE_ITEM_FIELD_ENVELOPE_BYTES;

export const PUBLICATION_MAXIMUM_TIER1_OVERHANG_BYTES = 64;
