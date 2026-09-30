/**
 * `canonical-decodability` fault-proof family — `docs/spec/midgard-tx.md` §12.7.
 *
 * **Rule.** Under §4 an operator commits `blake2b_256(preimage_i)` over
 * arbitrary bytes; §5.1 says what those bytes may be, and nothing in §4 makes
 * them be it.
 *
 * **Violation `canonical-decodability`.** A committed preimage that is not a
 * §5.1 envelope. Every §8.8 view door ends in the §7.4 count-consistency check
 * and that check aborts, so such a field aborts every consumer — including the
 * `CanonicalDecode` phase whose job is to render a verdict about it. No step is
 * producible by anyone and the dispute stalls rather than rejecting, which is an
 * operator escape hatch. §12.7 closes it by direct fault; the doors' abort
 * semantics are unchanged, because aborting is still the correct answer to a
 * *prover* supplying the wrong bytes (§7.3).
 *
 * The evidence is decided without decoding the field. The prover carries the
 * committed bytes, the step hashes them against the positionally-extracted
 * commitment (so wrong bytes simply fail), and the verdict below is a **total**
 * function of what comes back.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/rule.ak`. The
 * cross-language vectors are generated from it by
 * `scripts/generate-canonical-decodability-v1-goldens.mjs` into
 * `tests/fixtures/canonical-decodability-v1.generated.json` and
 * `onchain/aiken/lib/midgard/fraud-proofs/canonical-decodability/rule-golden.test.ak`,
 * and are recomputed on both sides.
 */

import "@al-ft/midgard-core/lucid-data";
import "@lucid-evolution/lucid";
import "../common.js";
import "../native-tx-field-access.js";
import "./native.js";
import "./canonical-decodability.walk-midgard-envelope-items.js";
import "./canonical-decodability.canonical-decodability-evidence-from-committed-field.js";
export {
  BodyFieldClaimSchema,
  BodyFieldClaimV1,
  canonicalDecodabilityEvidenceFromCommittedField,
  CanonicalDecodabilityStep01Args,
  CanonicalDecodabilityStep01ArgsSchema,
  CanonicalDecodabilityStep01Datum,
  CanonicalDecodabilityStep01DatumSchema,
  CanonicalDecodabilityStep01SpendRedeemer,
  CanonicalDecodabilityStep01SpendRedeemerSchema,
  CanonicalDecodabilityStep02Args,
  CanonicalDecodabilityStep02ArgsSchema,
  CanonicalDecodabilityStep02Datum,
  CanonicalDecodabilityStep02DatumSchema,
  CanonicalDecodabilityStep02SpendRedeemer,
  CanonicalDecodabilityStep02SpendRedeemerSchema,
  CanonicalDecodabilityStep02State,
  canonicalDecodabilityStep02StateFromEvidence,
  CanonicalDecodabilityStep02StateSchema,
  CanonicalDecodabilityStepCancel,
  CanonicalDecodabilityStepCancelSchema,
  CanonicalDecodabilityTxInclusionArgs,
  CanonicalDecodabilityTxInclusionArgsSchema,
  CommittedFieldClaim,
  CommittedFieldClaimSchema,
  WitnessFieldClaimSchema,
  WitnessFieldClaimV1,
} from "./canonical-decodability.canonical-decodability-evidence-from-committed-field.js";
export {
  CANONICAL_DECODABILITY_VIOLATION_ID,
  type CanonicalDecodabilityEvidence,
  isCanonicalDecodabilityViolation,
  MIDGARD_COMMITTED_FIELD_COUNT,
  MIDGARD_ENVELOPE_VERDICT_CODE_COUNT,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER,
  MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
  MIDGARD_ENVELOPE_VERDICT_NAMES,
  MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ARRAY_HEADER,
  MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ITEM_HEADER,
  MIDGARD_ENVELOPE_VERDICT_NOT_AN_ARRAY_HEADER,
  MIDGARD_ENVELOPE_VERDICT_NOT_AN_ITEM_HEADER,
  MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES,
  MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ARRAY_HEADER,
  MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_HEADER,
  MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_PAYLOAD,
  midgardEnvelopeVerdict,
  miscountedMidgardFieldPreimage,
} from "./canonical-decodability.walk-midgard-envelope-items.js";

/**
 * §2.5's split point: fields 0–5 are the body's, 6–8 the witness set's.
 *
 * Re-exported from `./field-opening.js` rather than restated, so the boundary
 * has one definition — that one is derived off the §2.5 field table and pinned
 * against `field-opening-v1.ak`'s own `first_witness_set_field_index` by
 * `tests/field-opening.test.ts`. Two independent `= 6` literals would agree
 * until the day they did not.
 */
export { MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX } from "./field-opening.js";
