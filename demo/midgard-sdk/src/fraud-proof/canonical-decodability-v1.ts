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
import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";
import { FieldCarriageV1Schema } from "@/native-tx-field-access-v1.js";

import {
  FaultProofStepCancel,
  FaultProofStepCancelSchema,
  faultProofStepDatumSchema,
  faultProofStepRedeemerSchema,
  NativeTxInclusionArgs,
  NativeTxInclusionArgsSchema,
  NativeTxInclusionCarriageSchema,
  NativeTxWitnessSetCompactSchema,
} from "./native.js";

/** Catalogue violation identifier adjudicated by this family. */
export const CANONICAL_DECODABILITY_VIOLATION_ID_V1 =
  "canonical-decodability" as const;

// ## §5.1 verdict codes (byte-for-byte twin of `rule.ak`)

/** The bytes are a §5.1 envelope. The only verdict that is not a violation. */
export const MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL_V1 = 0;

/** Zero bytes; §5.1's shortest admissible preimage is the one-byte `80`. */
export const MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER_V1 = 1;

/**
 * The leading byte is not a §5.1 `definite_array_header`. §5.1's acceptance set
 * is narrower than CBOR's: the four-byte `9a` head is well-formed CBOR and
 * outside the grammar.
 */
export const MIDGARD_ENVELOPE_VERDICT_NOT_AN_ARRAY_HEADER_V1 = 2;

/** A `98`/`99` array header whose count a narrower form spells (§6.1). */
export const MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ARRAY_HEADER_V1 = 3;

/** A `98`/`99` array header whose own width leaves the preimage. */
export const MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ARRAY_HEADER_V1 = 4;

/** Items remain to be read and no byte remains to start one. */
export const MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER_V1 = 5;

/** An item's leading byte is not a §5.1 `definite_bytes_header`. */
export const MIDGARD_ENVELOPE_VERDICT_NOT_AN_ITEM_HEADER_V1 = 6;

/** A `58`/`59` item header whose length a narrower form spells. */
export const MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ITEM_HEADER_V1 = 7;

/** A `58`/`59` item header whose own width leaves the preimage. */
export const MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_HEADER_V1 = 8;

/** An item whose declared payload leaves the preimage. */
export const MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_PAYLOAD_V1 = 9;

/** All declared items were read and bytes remain; §5.1 admits no trailing content. */
export const MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES_V1 = 10;

/**
 * One past the largest verdict code. Every value {@link midgardEnvelopeVerdictV1}
 * returns is in `0 ..< MIDGARD_ENVELOPE_VERDICT_CODE_COUNT_V1`, which is what
 * lets a step-02 refuse a state carrying a code no walk produces.
 */
export const MIDGARD_ENVELOPE_VERDICT_CODE_COUNT_V1 = 11;

/** §2.5's nine committed fields — the range a fault address may name. */
export const MIDGARD_COMMITTED_FIELD_COUNT_V1 = 9;

/** §2.5's split point: fields 0–5 are the body's, 6–8 the witness set's. */
export const MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1 = 6;

/** Every verdict code paired with the name `rule.ak` gives it. */
export const MIDGARD_ENVELOPE_VERDICT_NAMES_V1 = Object.freeze([
  "grammatical",
  "missing_array_header",
  "not_an_array_header",
  "non_minimal_array_header",
  "truncated_array_header",
  "missing_item_header",
  "not_an_item_header",
  "non_minimal_item_header",
  "truncated_item_header",
  "truncated_item_payload",
  "trailing_bytes",
] as const);

// ## The verdict — a total function over arbitrary bytes

/**
 * §5.1 grammaticality as a **verdict**, over any byte string at all.
 *
 * Total by construction and never throws: every index below is preceded by the
 * bound that makes it safe, and the bound's failure is a return value. A verdict
 * that could throw would be the door again, and the door is what this family
 * exists to route around.
 *
 * It is the door's grammar restated as a decision rather than reused as one —
 * `decodeMidgardFieldPreimageV1` and its Aiken counterparts decide the same
 * predicate by throwing/aborting. Agreement between the two is a conformance
 * obligation discharged by vectors in both directions, not an artefact of shared
 * code.
 */
export const midgardEnvelopeVerdictV1 = (preimage: Uint8Array): number => {
  const total = preimage.length;
  if (total === 0) {
    return MIDGARD_ENVELOPE_VERDICT_MISSING_ARRAY_HEADER_V1;
  }
  const tag = preimage[0]!;
  if (tag >= 0x80 && tag <= 0x97) {
    return walkMidgardEnvelopeItems(preimage, total, 1, tag - 0x80);
  }
  if (tag === 0x98) {
    if (total < 2) {
      return MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ARRAY_HEADER_V1;
    }
    const count = preimage[1]!;
    return count < 24
      ? MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ARRAY_HEADER_V1
      : walkMidgardEnvelopeItems(preimage, total, 2, count);
  }
  if (tag === 0x99) {
    if (total < 3) {
      return MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ARRAY_HEADER_V1;
    }
    const count = preimage[1]! * 256 + preimage[2]!;
    return count <= 0xff
      ? MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ARRAY_HEADER_V1
      : walkMidgardEnvelopeItems(preimage, total, 3, count);
  }
  return MIDGARD_ENVELOPE_VERDICT_NOT_AN_ARRAY_HEADER_V1;
};

/**
 * Reads `remaining` enveloped items from `offset` and decides where they end.
 *
 * Iterative where the Aiken twin recurses, and that is the one shape difference
 * between them: Aiken's recursion is a tail call the compiler turns into a loop,
 * while JavaScript has no such guarantee and a 16,382-item field is inside
 * §5.4's byte bound. A verdict that threw `RangeError` on the largest committed
 * field the format admits would be the abort this family exists to remove,
 * reintroduced in the twin.
 */
const walkMidgardEnvelopeItems = (
  preimage: Uint8Array,
  total: number,
  startOffset: number,
  startRemaining: number,
): number => {
  let offset = startOffset;
  let remaining = startRemaining;
  for (;;) {
    if (remaining <= 0) {
      return offset === total
        ? MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL_V1
        : MIDGARD_ENVELOPE_VERDICT_TRAILING_BYTES_V1;
    }
    if (offset >= total) {
      return MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER_V1;
    }
    const head = preimage[offset]!;
    let payloadOffset: number;
    let length: number;
    if (head >= 0x40 && head <= 0x57) {
      payloadOffset = offset + 1;
      length = head - 0x40;
    } else if (head === 0x58) {
      if (offset + 2 > total) {
        return MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_HEADER_V1;
      }
      length = preimage[offset + 1]!;
      if (length < 24) {
        return MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ITEM_HEADER_V1;
      }
      payloadOffset = offset + 2;
    } else if (head === 0x59) {
      if (offset + 3 > total) {
        return MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_HEADER_V1;
      }
      length = preimage[offset + 1]! * 256 + preimage[offset + 2]!;
      if (length <= 0xff) {
        return MIDGARD_ENVELOPE_VERDICT_NON_MINIMAL_ITEM_HEADER_V1;
      }
      payloadOffset = offset + 3;
    } else {
      return MIDGARD_ENVELOPE_VERDICT_NOT_AN_ITEM_HEADER_V1;
    }
    if (payloadOffset + length > total) {
      return MIDGARD_ENVELOPE_VERDICT_TRUNCATED_ITEM_PAYLOAD_V1;
    }
    offset = payloadOffset + length;
    remaining -= 1;
  }
};

/**
 * The adjudicated violation predicate, over the two values step 01 pins into the
 * computation thread.
 *
 * The bounds are §12.1's one-spelling rule applied to a state that crosses a
 * transaction boundary: a state naming a tenth field or a twelfth code is one
 * step 01 could not have written, and admitting it would let one fault finalize
 * under many spellings. They are refusals rather than clamps (§7.3) — this
 * returns `false`, and the on-chain twin aborts.
 */
export const isCanonicalDecodabilityViolationV1 = ({
  fieldIndex,
  verdict,
}: {
  readonly fieldIndex: number;
  readonly verdict: number;
}): boolean =>
  Number.isInteger(fieldIndex) &&
  fieldIndex >= 0 &&
  fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT_V1 &&
  Number.isInteger(verdict) &&
  verdict >= 0 &&
  verdict < MIDGARD_ENVELOPE_VERDICT_CODE_COUNT_V1 &&
  verdict !== MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL_V1;

// ## Producer side — the §5.1 envelope, built to be broken

/**
 * `encodeMidgardFieldPreimageV1`'s deliberately-wrong twin: an envelope whose
 * array header declares `declaredCount` while the body carries `items`.
 *
 * It exists because the honest producer cannot express the fixtures this family
 * adjudicates — it writes the count it measures, so no argument to it produces a
 * miscounted envelope, which is exactly the shape §5.1 refuses and a malicious
 * operator commits. Byte-for-byte twin of `rule.ak`'s
 * `miscounted_field_preimage_v1`.
 */
export const miscountedMidgardFieldPreimageV1 = (
  declaredCount: number,
  items: readonly Uint8Array[],
): Buffer =>
  Buffer.concat([
    midgardFieldArrayHeaderV1(declaredCount),
    ...items.map((item) =>
      Buffer.concat([midgardDefiniteBytesHeaderV1(item.length), item]),
    ),
  ]);

/**
 * §5.1's widest header is `99 NNNN` / `59 LLLL`, so both helpers below are
 * unbounded only in the sense that matters — they will write a count or a
 * length its content does not carry — and not beyond the two-byte form §5.1
 * caps at. Outside it `rule.ak`'s twin aborts in `from_int_big_endian(_, 2)`;
 * these throw rather than truncate to the low sixteen bits, so a producer
 * cannot emit bytes the on-chain builder would have refused to.
 */
const MIDGARD_FIELD_HEADER_MAX_V1 = 0xffff;

const assertMidgardFieldHeaderRangeV1 = (label: string, value: number): void => {
  if (
    !Number.isInteger(value) ||
    value < 0 ||
    value > MIDGARD_FIELD_HEADER_MAX_V1
  ) {
    throw new Error(
      `${label} must be an integer in 0..${MIDGARD_FIELD_HEADER_MAX_V1}`,
    );
  }
};

/** §5.1's `definite_array_header(N)` in minimal width. */
const midgardFieldArrayHeaderV1 = (count: number): Buffer => {
  assertMidgardFieldHeaderRangeV1("declared item count", count);
  if (count <= 23) {
    return Buffer.from([0x80 + count]);
  }
  if (count <= 255) {
    return Buffer.from([0x98, count]);
  }
  return Buffer.from([0x99, (count >> 8) & 0xff, count & 0xff]);
};

/** §5.1's `definite_bytes_header(L)` in minimal width. */
const midgardDefiniteBytesHeaderV1 = (length: number): Buffer => {
  assertMidgardFieldHeaderRangeV1("item length", length);
  if (length <= 23) {
    return Buffer.from([0x40 + length]);
  }
  if (length <= 255) {
    return Buffer.from([0x58, length]);
  }
  return Buffer.from([0x59, (length >> 8) & 0xff, length & 0xff]);
};

// ## Evidence

/**
 * Canonical evidence record for one committed field: exactly the triple step 01
 * pins into the computation thread, plus the authenticated bytes it was derived
 * from.
 */
export type CanonicalDecodabilityEvidenceV1 = {
  readonly violationId: typeof CANONICAL_DECODABILITY_VIOLATION_ID_V1;
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: string;
  readonly committedPreimageByteCount: number;
  readonly verdict: number;
  readonly verdictName: string;
  readonly isViolation: boolean;
};

/**
 * Builds the evidence record for one committed field.
 *
 * The caller must have authenticated `committedPreimage` against the field's
 * §4 commitment, positionally extracted from the compact structures the block's
 * `transactions_root` committed — on chain that check is the door's, and it is
 * what makes these bytes evidence rather than a claim. This function performs no
 * I/O and never throws.
 */
export const canonicalDecodabilityEvidenceFromCommittedFieldV1 = ({
  badTxId,
  fieldIndex,
  committedPreimage,
}: {
  readonly badTxId: string;
  readonly fieldIndex: number;
  readonly committedPreimage: Uint8Array;
}): CanonicalDecodabilityEvidenceV1 => {
  const verdict = midgardEnvelopeVerdictV1(committedPreimage);
  return Object.freeze({
    violationId: CANONICAL_DECODABILITY_VIOLATION_ID_V1,
    badTxId: badTxId.toLowerCase(),
    fieldIndex,
    committedPreimage: Buffer.from(committedPreimage).toString("hex"),
    committedPreimageByteCount: committedPreimage.length,
    verdict,
    verdictName: MIDGARD_ENVELOPE_VERDICT_NAMES_V1[verdict] ?? "unknown",
    isViolation: isCanonicalDecodabilityViolationV1({ fieldIndex, verdict }),
  });
};

// ## On-chain schemas (positional agreement with the Aiken step modules)

/**
 * §2.5 fields 0–5. No witness set is carried, because the door consults none
 * for a body field — a claim cannot carry one the door would ignore.
 */
export const BodyFieldClaimV1Schema = Data.Object({
  field_index: Data.Integer(),
  carriage: FieldCarriageV1Schema,
});
export type BodyFieldClaimV1 = Data.Static<typeof BodyFieldClaimV1Schema>;
export const BodyFieldClaimV1 =
  BodyFieldClaimV1Schema as unknown as BodyFieldClaimV1;

/**
 * §2.5 fields 6–8. `witness_set` is unauthenticated on arrival and is checked
 * against the block-committed `witness_set_hash` inside the door.
 */
export const WitnessFieldClaimV1Schema = Data.Object({
  field_index: Data.Integer(),
  witness_set: NativeTxWitnessSetCompactSchema,
  carriage: FieldCarriageV1Schema,
});
export type WitnessFieldClaimV1 = Data.Static<typeof WitnessFieldClaimV1Schema>;
export const WitnessFieldClaimV1 =
  WitnessFieldClaimV1Schema as unknown as WitnessFieldClaimV1;

/**
 * `midgard/fraud_proofs/canonical_decodability/rule.CommittedFieldClaimV1`.
 *
 * **Constructor order is wire format.** `Data.Enum` pins the Constr index
 * positionally: `BodyFieldClaim` 0, `WitnessFieldClaim` 1, exactly as the Aiken
 * sum declares them.
 */
export const CommittedFieldClaimV1Schema = Data.Enum([
  Data.Object({ BodyFieldClaim: BodyFieldClaimV1Schema }),
  Data.Object({ WitnessFieldClaim: WitnessFieldClaimV1Schema }),
]);
export type CommittedFieldClaimV1 = Data.Static<
  typeof CommittedFieldClaimV1Schema
>;
export const CommittedFieldClaimV1 =
  CommittedFieldClaimV1Schema as unknown as CommittedFieldClaimV1;

export const CanonicalDecodabilityStep01DatumSchema = faultProofStepDatumSchema(
  Data.Any(),
);
export type CanonicalDecodabilityStep01Datum = Data.Static<
  typeof CanonicalDecodabilityStep01DatumSchema
>;
export const CanonicalDecodabilityStep01Datum =
  CanonicalDecodabilityStep01DatumSchema as unknown as CanonicalDecodabilityStep01Datum;

/** Mirrors `midgard/fraud_proofs/canonical_decodability/step_01.Args`. */
export const CanonicalDecodabilityStep01ArgsSchema = Data.Object({
  inclusion: NativeTxInclusionCarriageSchema,
  claim: CommittedFieldClaimV1Schema,
});
export type CanonicalDecodabilityStep01Args = Data.Static<
  typeof CanonicalDecodabilityStep01ArgsSchema
>;
export const CanonicalDecodabilityStep01Args =
  CanonicalDecodabilityStep01ArgsSchema as unknown as CanonicalDecodabilityStep01Args;

export const CanonicalDecodabilityStep01SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CanonicalDecodabilityStep01ArgsSchema);
export type CanonicalDecodabilityStep01SpendRedeemer = Data.Static<
  typeof CanonicalDecodabilityStep01SpendRedeemerSchema
>;
export const CanonicalDecodabilityStep01SpendRedeemer =
  CanonicalDecodabilityStep01SpendRedeemerSchema as unknown as CanonicalDecodabilityStep01SpendRedeemer;

/**
 * Mirrors `midgard/fraud_proofs/canonical_decodability/step_02.State`.
 *
 * `(bad_tx_id, field_index)` is §12.1's fault address; `verdict` is the proof.
 * No preimage bytes travel — the bytes were authenticated in the transaction
 * that read them.
 */
export const CanonicalDecodabilityStep02StateSchema = Data.Object({
  bad_tx_id: H32Schema,
  field_index: Data.Integer(),
  verdict: Data.Integer(),
});
export type CanonicalDecodabilityStep02State = Data.Static<
  typeof CanonicalDecodabilityStep02StateSchema
>;
export const CanonicalDecodabilityStep02State =
  CanonicalDecodabilityStep02StateSchema as unknown as CanonicalDecodabilityStep02State;

export const CanonicalDecodabilityStep02DatumSchema = faultProofStepDatumSchema(
  CanonicalDecodabilityStep02StateSchema,
);
export type CanonicalDecodabilityStep02Datum = Data.Static<
  typeof CanonicalDecodabilityStep02DatumSchema
>;
export const CanonicalDecodabilityStep02Datum =
  CanonicalDecodabilityStep02DatumSchema as unknown as CanonicalDecodabilityStep02Datum;

export const CanonicalDecodabilityStep02ArgsSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  fraud_proof_mint_redeemer_index: Data.Integer(),
});
export type CanonicalDecodabilityStep02Args = Data.Static<
  typeof CanonicalDecodabilityStep02ArgsSchema
>;
export const CanonicalDecodabilityStep02Args =
  CanonicalDecodabilityStep02ArgsSchema as unknown as CanonicalDecodabilityStep02Args;

export const CanonicalDecodabilityStep02SpendRedeemerSchema =
  faultProofStepRedeemerSchema(CanonicalDecodabilityStep02ArgsSchema);
export type CanonicalDecodabilityStep02SpendRedeemer = Data.Static<
  typeof CanonicalDecodabilityStep02SpendRedeemerSchema
>;
export const CanonicalDecodabilityStep02SpendRedeemer =
  CanonicalDecodabilityStep02SpendRedeemerSchema as unknown as CanonicalDecodabilityStep02SpendRedeemer;

export const CanonicalDecodabilityTxInclusionArgsSchema =
  NativeTxInclusionArgsSchema;
export type CanonicalDecodabilityTxInclusionArgs = NativeTxInclusionArgs;
export const CanonicalDecodabilityTxInclusionArgs = NativeTxInclusionArgs;

export const CanonicalDecodabilityStepCancelSchema = FaultProofStepCancelSchema;
export type CanonicalDecodabilityStepCancel = FaultProofStepCancel;
export const CanonicalDecodabilityStepCancel = FaultProofStepCancel;

/**
 * Builds the step-02 state exactly as the on-chain step-01 validator derives it,
 * so an off-chain builder and the L1 verifier cannot drift.
 */
export const canonicalDecodabilityStep02StateFromEvidenceV1 = (
  evidence: CanonicalDecodabilityEvidenceV1,
): CanonicalDecodabilityStep02State => ({
  bad_tx_id: evidence.badTxId,
  field_index: BigInt(evidence.fieldIndex),
  verdict: BigInt(evidence.verdict),
});
