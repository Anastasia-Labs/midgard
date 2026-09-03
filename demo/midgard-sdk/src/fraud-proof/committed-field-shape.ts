/**
 * `committed-field-shape` fault-proof family — `docs/spec/midgard-tx.md` §12.8.
 *
 * **Rule.** §12.7 closed the stall for a committed preimage that is not a §5.1
 * envelope, and named two it could not close. Both are committed preimages that
 * *are* §5.1 envelopes and that slot `i`'s own rules still refuse, so §12.7's
 * bytes-only verdict is 0 and the §8.8 door aborts anyway:
 *
 * - **§7.4 fixed-stride arithmetic** at fields 0, 1, 3, 4 and 7 — an envelope
 *   whose items are not the slot's stride; and
 * - **§5.4's per-field byte bound** — an envelope above
 *   `MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES`.
 *
 * Both leave the identical stall: no step is producible by anyone, the dispute
 * stalls rather than rejecting, and nothing is slashed. §12.8 closes them as a
 * **sibling** fault kind stated over `(fieldIndex, preimage)` rather than by
 * widening §12.7, because a stride and a byte bound are functions of the slot
 * and §12.7's whole boundary against §12.3 is that it applies no per-field rule.
 *
 * **The boundary is in the code, not only in the prose.**
 * {@link midgardCommittedFieldShapeVerdict} renders
 * {@link MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE} — a **non-convicting**
 * code — for every byte string §12.7 convicts, and
 * {@link isCommittedFieldShapeViolation} refuses it. The two fault kinds
 * partition the committed byte strings a door refuses; no committed field is
 * faultable under both.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule.ak`. The
 * cross-language vectors are generated from it by
 * `scripts/generate-committed-field-shape-v1-goldens.mjs` into
 * `tests/fixtures/committed-field-shape-v1.generated.json` and
 * `onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule-golden.test.ak`,
 * and are recomputed on both sides.
 */
import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_WALK_DERIVED_STRIDE,
  midgardFieldStride,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "../common.js";
import {
  CommittedFieldClaimSchema,
  MIDGARD_COMMITTED_FIELD_COUNT,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  midgardEnvelopeVerdict,
} from "./canonical-decodability.js";
import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  NativeTxInclusionCarriageSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const COMMITTED_FIELD_SHAPE_VIOLATION_ID =
  "committed-field-shape" as const;

// ## Verdict codes (byte-for-byte twin of `committed-field-shape/rule.ak`)

/**
 * Slot `fieldIndex`'s door opens these bytes: the envelope is inside §5.4's byte
 * bound and, at a fixed-stride slot, its items are the slot's stride.
 */
export const MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE = 0;

/**
 * The bytes are not a §5.1 envelope — **§12.7's fault, not this one's**. Where
 * the two fault kinds are held apart: rendered rather than convicted, and
 * refused by {@link isCommittedFieldShapeViolation}.
 */
export const MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE = 1;

/**
 * §5.4. A §5.1 envelope above `MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES`.
 * Checked before the stride, because that is the order the on-chain
 * `whole_view` refuses in and one committed field must earn one accusation
 * (§12.1).
 */
export const MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND = 2;

/**
 * §7.4. A §5.1 envelope at one of the five fixed-stride slots whose
 * `headerLen + stride·N` is not its `totalLength`.
 */
export const MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE = 3;

/**
 * One past the largest verdict code. Every value
 * {@link midgardCommittedFieldShapeVerdict} returns is in
 * `0 ..< MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT`, which is what lets a
 * step-02 refuse a state carrying a code no verdict produces.
 */
export const MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT = 4;

/** Every verdict code paired with the name the Aiken twin gives it. */
export const MIDGARD_FIELD_SHAPE_VERDICT_NAMES = Object.freeze([
  "admissible",
  "not_an_envelope",
  "field_byte_bound",
  "wrong_stride",
] as const);

/**
 * §5.3's five fixed-stride slots, derived from the shared stride table rather
 * than transcribed. A hard-coded list here would be a second reading of §5.3.
 */
export const MIDGARD_FIXED_STRIDE_FIELD_INDICES = Object.freeze(
  Array.from({ length: MIDGARD_COMMITTED_FIELD_COUNT }, (_, i) => i).filter(
    (fieldIndex) =>
      midgardFieldStride(fieldIndex) !== MIDGARD_WALK_DERIVED_STRIDE,
  ),
);

// ## The verdict — total over arbitrary bytes at each of §2.5's nine slots

/**
 * §5.1's `definite_array_header(N)` at offset 0, as an **option** rather than as
 * a throw. Returns `{ headerLen, count }`, or `undefined` when the leading bytes
 * are not a minimal §5.1 array header.
 *
 * Narrower than a grammar: it reads the three header widths and stops. Whether
 * the *items* satisfy §5.1 is {@link midgardEnvelopeVerdict}'s answer and is
 * asked of it, so there is one §5.1 decision procedure per language and not two.
 */
export const midgardMinimalArrayHeader = (
  preimage: Uint8Array,
): { readonly headerLen: number; readonly count: number } | undefined => {
  const total = preimage.length;
  if (total === 0) {
    return undefined;
  }
  const tag = preimage[0]!;
  if (tag >= 0x80 && tag <= 0x97) {
    return { headerLen: 1, count: tag - 0x80 };
  }
  if (tag === 0x98) {
    if (total < 2) {
      return undefined;
    }
    const count = preimage[1]!;
    return count < 24 ? undefined : { headerLen: 2, count };
  }
  if (tag === 0x99) {
    if (total < 3) {
      return undefined;
    }
    const count = preimage[1]! * 256 + preimage[2]!;
    return count <= 0xff ? undefined : { headerLen: 3, count };
  }
  return undefined;
};

/**
 * Slot `fieldIndex`'s §7.4/§5.4 shape rules, as a **verdict** over any byte
 * string at all.
 *
 * Total over the bytes and never throws for them: every index below is preceded
 * by the bound that makes it safe. The **field index** is a different matter and
 * the asymmetry is deliberate — `midgardFieldStride` throws outside §2.5's
 * nine slots, and that throw is this function's index bound. The preimage is the
 * operator's and a throw on it would be the stall under adjudication; the index
 * is the prover's, and §7.3 says refusing is the right answer to a prover
 * supplying something outside the format.
 *
 * The order of the three questions is the on-chain `whole_view`'s order, which
 * is a §12.1 obligation rather than a style choice: an oversize envelope at a
 * fixed-stride slot whose stride also fails is convicted as the byte-bound
 * violation alone.
 */
export const midgardCommittedFieldShapeVerdict = (
  fieldIndex: number,
  preimage: Uint8Array,
): number => {
  const stride = midgardFieldStride(fieldIndex);
  const header = midgardMinimalArrayHeader(preimage);
  if (header === undefined) {
    return MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE;
  }
  if (
    midgardEnvelopeVerdict(preimage) !== MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL
  ) {
    return MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE;
  }
  const totalLength = preimage.length;
  if (totalLength > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES) {
    return MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND;
  }
  if (stride === MIDGARD_WALK_DERIVED_STRIDE) {
    return MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE;
  }
  return header.headerLen + stride * header.count === totalLength
    ? MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE
    : MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE;
};

/**
 * The adjudicated violation predicate, over the two values step 01 pins into the
 * computation thread.
 *
 * The `not_an_envelope` refusal is the disjointness boundary, enforced rather
 * than described. The two convicting codes are named explicitly instead of
 * testing `verdict !== admissible`, so a code added later is non-convicting
 * until someone says otherwise. The bounds are §12.1's one-spelling rule applied
 * to a state that crosses a transaction boundary; they are refusals rather than
 * clamps (§7.3) — this returns `false`, and the on-chain twin aborts.
 */
export const isCommittedFieldShapeViolation = ({
  fieldIndex,
  verdict,
}: {
  readonly fieldIndex: number;
  readonly verdict: number;
}): boolean =>
  Number.isInteger(fieldIndex) &&
  fieldIndex >= 0 &&
  fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT &&
  Number.isInteger(verdict) &&
  verdict >= 0 &&
  verdict < MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT &&
  (verdict === MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND ||
    verdict === MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE);

// ## Producer side — the §5.1 envelope, built at a chosen length

/**
 * A §5.1 envelope of exactly `totalLength` bytes carrying one item, byte-for-byte
 * twin of `rule.ak`'s `sized_field_envelope_v1`.
 *
 * The shapes this family adjudicates are lengths, not contents, so the fixture
 * a vector wants is "an envelope that is grammatical and exactly this long".
 * The item is `59 LLLL`-headed, so `totalLength` must leave a payload above 255
 * bytes; below that the minimal item header is narrower and the length would not
 * be the one asked for.
 */
export const sizedMidgardFieldEnvelope = (
  totalLength: number,
  fill: number,
): Buffer => {
  const payloadLength = totalLength - 4;
  if (!Number.isInteger(payloadLength) || payloadLength <= 0xff) {
    throw new Error(
      `sized field envelope needs a payload above 255 bytes; ${String(totalLength)} leaves ${String(payloadLength)}`,
    );
  }
  return encodeMidgardFieldPreimage([Buffer.alloc(payloadLength, fill)]);
};

// ## Evidence

/**
 * Canonical evidence record for one committed field: exactly the triple step 01
 * pins into the computation thread, plus the authenticated bytes it was derived
 * from and the slot rule they left.
 */
export type CommittedFieldShapeEvidence = {
  readonly violationId: typeof COMMITTED_FIELD_SHAPE_VIOLATION_ID;
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly fieldStride: number;
  readonly committedPreimage: string;
  readonly committedPreimageByteCount: number;
  readonly verdict: number;
  readonly verdictName: string;
  readonly isViolation: boolean;
};

/**
 * Builds the evidence record for one committed field.
 *
 * The caller must have authenticated `committedPreimage` against the field's §4
 * commitment, positionally extracted from the compact structures the block's
 * `transactions_root` committed — on chain that check is the door's, and it is
 * what makes these bytes evidence rather than a claim. This function performs no
 * I/O; it throws only for a `fieldIndex` outside §2.5's nine, which is the same
 * refusal the on-chain twin makes.
 */
export const committedFieldShapeEvidenceFromCommittedField = ({
  badTxId,
  fieldIndex,
  committedPreimage,
}: {
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
}): CommittedFieldShapeEvidence => {
  const verdict = midgardCommittedFieldShapeVerdict(
    fieldIndex,
    committedPreimage,
  );
  return Object.freeze({
    violationId: COMMITTED_FIELD_SHAPE_VIOLATION_ID,
    badTxId: badTxId.toLowerCase(),
    fieldIndex,
    fieldStride: midgardFieldStride(fieldIndex),
    committedPreimage: Buffer.from(committedPreimage).toString("hex"),
    committedPreimageByteCount: committedPreimage.length,
    verdict,
    verdictName: MIDGARD_FIELD_SHAPE_VERDICT_NAMES[verdict] ?? "unknown",
    isViolation: isCommittedFieldShapeViolation({ fieldIndex, verdict }),
  });
};

// ## On-chain schemas (positional agreement with the Aiken step modules)

/**
 * **The claim is §12.7's `CommittedFieldClaimV1`, used rather than
 * re-declared.**
 *
 * The accusation the two sibling fault kinds make is the same accusation —
 * which of §2.5's nine slots, and how that slot's committed bytes reach the step
 * — and §6.1's one-spelling rule applies to a wire type as much as to a scalar.
 * The Aiken `committed_field_shape/step_01.Args` names the same type, so a
 * second schema here would be a second spelling of one wire form; it is imported
 * from `./canonical-decodability.js` above and consumers take it from there.
 * What is *not* shared is the verdict code space and the state it travels in.
 */
export const CommittedFieldShapeStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type CommittedFieldShapeStep01Datum = Data.Static<
  typeof CommittedFieldShapeStep01DatumSchema
>;
export const CommittedFieldShapeStep01Datum =
  CommittedFieldShapeStep01DatumSchema as unknown as CommittedFieldShapeStep01Datum;

/** Mirrors `midgard/fraud_proofs/committed_field_shape/step_01.Args`. */
export const CommittedFieldShapeStep01ArgsSchema = Data.Object({
  inclusion: NativeTxInclusionCarriageSchema,
  claim: CommittedFieldClaimSchema,
});
export type CommittedFieldShapeStep01Args = Data.Static<
  typeof CommittedFieldShapeStep01ArgsSchema
>;
export const CommittedFieldShapeStep01Args =
  CommittedFieldShapeStep01ArgsSchema as unknown as CommittedFieldShapeStep01Args;

export const CommittedFieldShapeStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CommittedFieldShapeStep01ArgsSchema);
export type CommittedFieldShapeStep01SpendRedeemer = Data.Static<
  typeof CommittedFieldShapeStep01SpendRedeemerSchema
>;
export const CommittedFieldShapeStep01SpendRedeemer =
  CommittedFieldShapeStep01SpendRedeemerSchema as unknown as CommittedFieldShapeStep01SpendRedeemer;

/**
 * Mirrors `midgard/fraud_proofs/committed_field_shape/step_02.State`.
 *
 * Structurally identical to §12.7's step-02 state and deliberately a separate
 * schema: the two verdict code spaces are different, so one type would let a
 * §12.7 code satisfy this family's bounds check and mean something else while
 * doing it.
 */
export const CommittedFieldShapeStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  field_index: Data.Integer(),
  verdict: Data.Integer(),
});
export type CommittedFieldShapeStep02State = Data.Static<
  typeof CommittedFieldShapeStep02StateSchema
>;
export const CommittedFieldShapeStep02State =
  CommittedFieldShapeStep02StateSchema as unknown as CommittedFieldShapeStep02State;

export const CommittedFieldShapeStep02DatumSchema = faultProofStepDatumSchema(
  CommittedFieldShapeStep02StateSchema,
);
export type CommittedFieldShapeStep02Datum = Data.Static<
  typeof CommittedFieldShapeStep02DatumSchema
>;
export const CommittedFieldShapeStep02Datum =
  CommittedFieldShapeStep02DatumSchema as unknown as CommittedFieldShapeStep02Datum;

export const CommittedFieldShapeStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type CommittedFieldShapeStep02Args = Data.Static<
  typeof CommittedFieldShapeStep02ArgsSchema
>;
export const CommittedFieldShapeStep02Args =
  CommittedFieldShapeStep02ArgsSchema as unknown as CommittedFieldShapeStep02Args;

export const CommittedFieldShapeStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CommittedFieldShapeStep02ArgsSchema);
export type CommittedFieldShapeStep02SpendRedeemer = Data.Static<
  typeof CommittedFieldShapeStep02SpendRedeemerSchema
>;
export const CommittedFieldShapeStep02SpendRedeemer =
  CommittedFieldShapeStep02SpendRedeemerSchema as unknown as CommittedFieldShapeStep02SpendRedeemer;

export const CommittedFieldShapeTxInclusionArgsSchema =
  NativeTxInclusionArgsSchema;
export type CommittedFieldShapeTxInclusionArgs = NativeTxInclusionArgs;
export const CommittedFieldShapeTxInclusionArgs = NativeTxInclusionArgs;

export const CommittedFieldShapeStepCancelSchema = FaultProofStepCancelSchema;
export type CommittedFieldShapeStepCancel = FaultProofStepCancel;
export const CommittedFieldShapeStepCancel = FaultProofStepCancel;

/**
 * Builds the step-02 state exactly as the on-chain step-01 validator derives it,
 * so an off-chain builder and the L1 verifier cannot drift.
 */
export const committedFieldShapeStep02StateFromEvidence = (
  evidence: CommittedFieldShapeEvidence,
): CommittedFieldShapeStep02State => ({
  bad_tx_id: evidence.badTxId,
  field_index: BigInt(evidence.fieldIndex),
  verdict: BigInt(evidence.verdict),
});
