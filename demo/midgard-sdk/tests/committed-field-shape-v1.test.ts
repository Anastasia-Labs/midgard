import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_WALK_DERIVED_STRIDE,
  midgardFieldCommitment,
  midgardFieldStride,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  CommittedFieldClaim,
  MIDGARD_COMMITTED_FIELD_COUNT,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  midgardEnvelopeVerdict,
} from "../src/fraud-proof/canonical-decodability-v1.js";
import {
  COMMITTED_FIELD_SHAPE_VIOLATION_ID,
  committedFieldShapeEvidenceFromCommittedField,
  CommittedFieldShapeStep02State,
  committedFieldShapeStep02StateFromEvidence,
  CommittedFieldShapeStep02StateSchema,
  isCommittedFieldShapeViolation,
  MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
  MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT,
  MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
  MIDGARD_FIELD_SHAPE_VERDICT_NAMES,
  MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
  MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
  MIDGARD_FIXED_STRIDE_FIELD_INDICES,
  midgardCommittedFieldShapeVerdict,
  midgardMinimalArrayHeader,
  sizedMidgardFieldEnvelope,
} from "../src/fraud-proof/committed-field-shape-v1.js";

/**
 * The TypeScript half of the §12.8 golden channel.
 *
 * The generator emits the fixture from `dist/`; this suite recomputes every
 * vector from `src/`, so a twin edit that has not been rebuilt into a fixture
 * fails here rather than shipping. The Aiken half —
 * `onchain/aiken/lib/midgard/fraud-proofs/committed-field-shape/rule-golden.test.ak`
 * — recomputes the same verdicts under the fork runner. Neither half is
 * authoritative on its own: the fixture is what they are both held to.
 */

type Construction =
  | { readonly kind: "literal"; readonly hex: string }
  | { readonly kind: "envelope"; readonly items: readonly string[] }
  | {
      readonly kind: "sized";
      readonly totalLength: number;
      readonly fill: number;
    };

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/committed-field-shape-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as {
  readonly schema: string;
  readonly version: number;
  readonly specDocument: string;
  readonly generator: string;
  readonly verdictCodeCount: number;
  readonly verdictNames: readonly string[];
  readonly fixedStrideFieldIndices: readonly number[];
  readonly fieldByteBound: number;
  readonly vectors: readonly {
    readonly label: string;
    readonly note: string;
    readonly fieldIndex: number;
    readonly fieldStride: number;
    readonly construction: Construction;
    readonly byteCount: number;
    readonly preimageCommitment: string;
    readonly envelopeVerdict: number;
    readonly envelopeVerdictName: string;
    readonly verdict: number;
    readonly verdictName: string;
    readonly convicts: boolean;
  }[];
  readonly wireVectors: readonly {
    readonly label: string;
    readonly aikenType: string;
    readonly value: unknown;
    readonly cborHex: string;
  }[];
};

/** The same three constructions the generator and the Aiken golden build. */
const buildPreimage = (construction: Construction): Buffer => {
  if (construction.kind === "literal") {
    return Buffer.from(construction.hex, "hex");
  }
  if (construction.kind === "envelope") {
    return encodeMidgardFieldPreimage(
      construction.items.map((item) => Buffer.from(item, "hex")),
    );
  }
  return sizedMidgardFieldEnvelope(construction.totalLength, construction.fill);
};

describe("§12.8 committed-field-shape golden channel", () => {
  it("pins the channel's identity", () => {
    expect(golden.schema).toBe("midgard-committed-field-shape-golden");
    expect(golden.version).toBe(1);
    expect(golden.specDocument).toBe("docs/spec/midgard-tx.md");
    expect(golden.verdictCodeCount).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT,
    );
    expect(golden.verdictNames).toEqual([...MIDGARD_FIELD_SHAPE_VERDICT_NAMES]);
    expect(golden.fieldByteBound).toBe(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
    );
  });

  it("recomputes every vector from src, bytes and both verdicts", () => {
    expect(golden.vectors.length).toBeGreaterThan(0);
    for (const vector of golden.vectors) {
      const preimage = buildPreimage(vector.construction);
      expect(preimage.length).toBe(vector.byteCount);
      // §4's own hash: the two sides built the same bytes, not merely the same
      // description of them.
      expect(midgardFieldCommitment(preimage).toString("hex")).toBe(
        vector.preimageCommitment,
      );
      expect(midgardFieldStride(vector.fieldIndex)).toBe(vector.fieldStride);
      expect(
        midgardCommittedFieldShapeVerdict(vector.fieldIndex, preimage),
      ).toBe(vector.verdict);
      expect(midgardEnvelopeVerdict(preimage)).toBe(vector.envelopeVerdict);
      expect(MIDGARD_FIELD_SHAPE_VERDICT_NAMES[vector.verdict]).toBe(
        vector.verdictName,
      );
    }
  });

  it("reaches every verdict code, and no code outside the space", () => {
    const reached = new Set(golden.vectors.map((vector) => vector.verdict));
    expect(reached.size).toBe(MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT);
    for (const code of reached) {
      expect(code).toBeGreaterThanOrEqual(0);
      expect(code).toBeLessThan(MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT);
    }
  });

  it("covers every fixed-stride slot §5.3 declares", () => {
    expect(golden.fixedStrideFieldIndices).toEqual([
      ...MIDGARD_FIXED_STRIDE_FIELD_INDICES,
    ]);
    const slots = new Set(golden.vectors.map((vector) => vector.fieldIndex));
    for (const fieldIndex of MIDGARD_FIXED_STRIDE_FIELD_INDICES) {
      expect(slots.has(fieldIndex)).toBe(true);
    }
  });

  it("keeps the partition against §12.7 over the whole vector set", () => {
    for (const vector of golden.vectors) {
      const preimage = buildPreimage(vector.construction);
      const verdict = midgardCommittedFieldShapeVerdict(
        vector.fieldIndex,
        preimage,
      );
      const convicts = isCommittedFieldShapeViolation({
        fieldIndex: vector.fieldIndex,
        verdict,
      });
      expect(convicts).toBe(vector.convicts);
      const envelope = midgardEnvelopeVerdict(preimage);
      // Convicting here implies §12.7 does not convict…
      if (convicts) {
        expect(envelope).toBe(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL);
      }
      // …and §12.7 convicting implies this family renders the deferring code,
      // which its adjudication refuses.
      if (envelope !== MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL) {
        expect(verdict).toBe(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE);
        expect(convicts).toBe(false);
      }
    }
  });

  it("re-encodes every wire vector to the same bytes", () => {
    expect(golden.wireVectors.length).toBeGreaterThan(0);
    for (const vector of golden.wireVectors) {
      expect(vector.aikenType).toBe("State");
      const decoded = Data.from(vector.cborHex, CommittedFieldShapeStep02State);
      expect(Data.to(decoded, CommittedFieldShapeStep02State)).toBe(
        vector.cborHex,
      );
    }
  });

  it("uses §12.7's claim wire form rather than a second spelling", () => {
    // The Aiken `committed_field_shape/step_01.Args` names §12.7's
    // `CommittedFieldClaim`, so the off-chain builder must emit exactly those
    // bytes for the same accusation. If this family ever grew its own claim
    // schema, the constructor tags below would be the first thing to drift.
    const claim = {
      BodyFieldClaim: {
        field_index: 0n,
        carriage: { Inline: { preimage: "8044deadbeef" } },
      },
    };
    const encoded = Data.to(claim, CommittedFieldClaim);
    expect(encoded.startsWith("d8799f")).toBe(true);
    expect(Data.from(encoded, CommittedFieldClaim)).toEqual(claim);
  });
});

describe("§12.8 shape verdict", () => {
  it("reads §5.3's stride table as five fixed slots and four walked ones", () => {
    expect([...MIDGARD_FIXED_STRIDE_FIELD_INDICES]).toEqual([0, 1, 3, 4, 7]);
    for (
      let fieldIndex = 0;
      fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT;
      fieldIndex += 1
    ) {
      const fixed =
        midgardFieldStride(fieldIndex) !== MIDGARD_WALK_DERIVED_STRIDE;
      expect(MIDGARD_FIXED_STRIDE_FIELD_INDICES.includes(fieldIndex)).toBe(
        fixed,
      );
    }
  });

  it("gives the same bytes different answers at different slots", () => {
    // This is what "a function of (field_index, preimage)" means, and it is the
    // property §12.7's bytes-only verdict cannot express.
    const preimage = encodeMidgardFieldPreimage([
      Buffer.from([0xde, 0xad, 0xbe, 0xef]),
    ]);
    for (
      let fieldIndex = 0;
      fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT;
      fieldIndex += 1
    ) {
      expect(midgardCommittedFieldShapeVerdict(fieldIndex, preimage)).toBe(
        MIDGARD_FIXED_STRIDE_FIELD_INDICES.includes(fieldIndex)
          ? MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE
          : MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
      );
    }
  });

  it("defers rather than convicts when the stride holds and the items are not §5.1's", () => {
    // The one direction of the door agreement §12.8 deliberately does not
    // claim, pinned here as it is pinned on the Aiken side: `whole_view`
    // settles §5.1 by arithmetic at a fixed-stride slot and never walks the
    // items, so `81` followed by forty bytes that are not an item head opens
    // the door. This family renders the deferring code and convicts nothing —
    // fail-safe by construction, because the disagreement can only ever leave a
    // field to §12.7, never take one from it.
    const preimage = Buffer.concat([
      Buffer.from([0x81]),
      Buffer.alloc(midgardFieldStride(0), 0xff),
    ]);
    expect(preimage.length).toBe(1 + midgardFieldStride(0));
    expect(midgardEnvelopeVerdict(preimage)).not.toBe(
      MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
    );
    const verdict = midgardCommittedFieldShapeVerdict(0, preimage);
    expect(verdict).toBe(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE);
    expect(isCommittedFieldShapeViolation({ fieldIndex: 0, verdict })).toBe(
      false,
    );
  });

  it("pins §7.4's arithmetic as an equality, refused in both directions", () => {
    const stride = midgardFieldStride(0);
    const honest = encodeMidgardFieldPreimage([Buffer.alloc(stride - 2, 0x00)]);
    expect(honest.length).toBe(1 + stride);
    expect(midgardCommittedFieldShapeVerdict(0, honest)).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
    );
    for (const delta of [-1, 1]) {
      const off = encodeMidgardFieldPreimage([
        Buffer.alloc(stride - 2 + delta, 0x00),
      ]);
      expect(off.length).toBe(1 + stride + delta);
      expect(midgardCommittedFieldShapeVerdict(0, off)).toBe(
        MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
      );
    }
  });

  it("holds the arithmetic across every §5.1 header width", () => {
    const stride = midgardFieldStride(0);
    for (const count of [0, 1, 23, 24, 255, 256]) {
      const items = Array.from({ length: count }, () =>
        Buffer.alloc(stride - 2, 0x00),
      );
      expect(
        midgardCommittedFieldShapeVerdict(0, encodeMidgardFieldPreimage(items)),
      ).toBe(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE);
    }
  });

  it("pins §5.4's bound as a bound, not a clamp", () => {
    const bound = MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES;
    const at = sizedMidgardFieldEnvelope(bound, 0x00);
    const above = sizedMidgardFieldEnvelope(bound + 1, 0x00);
    expect(at.length).toBe(bound);
    expect(above.length).toBe(bound + 1);
    expect(midgardCommittedFieldShapeVerdict(2, at)).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
    );
    expect(midgardCommittedFieldShapeVerdict(2, above)).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
    );
  });

  it("checks the byte bound before the stride, so one field earns one accusation", () => {
    const above = sizedMidgardFieldEnvelope(
      MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES + 1,
      0x00,
    );
    // The stride really does fail for these bytes, so the assertion below is an
    // ordering pin and not an accident of the fixture.
    const header = midgardMinimalArrayHeader(above);
    expect(header).toBeDefined();
    expect(header!.headerLen + midgardFieldStride(0) * header!.count).not.toBe(
      above.length,
    );
    expect(midgardCommittedFieldShapeVerdict(0, above)).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
    );
  });

  it("never throws on any prefix of any vector, at any slot", () => {
    // Totality over the *bytes* is the property the family rests on: a verdict
    // that could throw on operator-committed bytes would be the door again.
    for (const vector of golden.vectors) {
      const preimage = buildPreimage(vector.construction);
      // The long vectors are exercised at their own length only; every short
      // one is exercised at every prefix, which is where a header truncated
      // mid-width lives.
      const lengths =
        preimage.length > 64
          ? [0, 1, 2, 3, 4, preimage.length]
          : Array.from({ length: preimage.length + 1 }, (_, i) => i);
      for (const length of lengths) {
        for (
          let fieldIndex = 0;
          fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT;
          fieldIndex += 1
        ) {
          const verdict = midgardCommittedFieldShapeVerdict(
            fieldIndex,
            preimage.subarray(0, length),
          );
          expect(Number.isInteger(verdict)).toBe(true);
          expect(verdict).toBeGreaterThanOrEqual(0);
          expect(verdict).toBeLessThan(MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT);
        }
      }
    }
  });

  it("refuses a slot outside §2.5's nine, at both ends", () => {
    // The index is the *prover's* argument, so §7.3 says refusing is right —
    // the Aiken twin aborts in `field_stride` for the same reason.
    const preimage = Buffer.from([0x80]);
    expect(() => midgardCommittedFieldShapeVerdict(-1, preimage)).toThrow();
    expect(() =>
      midgardCommittedFieldShapeVerdict(
        MIDGARD_COMMITTED_FIELD_COUNT,
        preimage,
      ),
    ).toThrow();
  });

  it("refuses a sized envelope whose payload §5.1 would spell more narrowly", () => {
    expect(() => sizedMidgardFieldEnvelope(4, 0x00)).toThrow();
    expect(() => sizedMidgardFieldEnvelope(259, 0x00)).toThrow();
    expect(sizedMidgardFieldEnvelope(260, 0x00).length).toBe(260);
  });
});

describe("§12.8 adjudication predicate", () => {
  it("convicts the two shape codes and neither non-convicting one", () => {
    for (
      let fieldIndex = 0;
      fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT;
      fieldIndex += 1
    ) {
      for (const verdict of [
        MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
        MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
      ]) {
        expect(isCommittedFieldShapeViolation({ fieldIndex, verdict })).toBe(
          false,
        );
      }
      for (const verdict of [
        MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
        MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
      ]) {
        expect(isCommittedFieldShapeViolation({ fieldIndex, verdict })).toBe(
          true,
        );
      }
    }
  });

  it("refuses a state no step 01 could have written", () => {
    const verdict = MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE;
    expect(isCommittedFieldShapeViolation({ fieldIndex: -1, verdict })).toBe(
      false,
    );
    expect(
      isCommittedFieldShapeViolation({
        fieldIndex: MIDGARD_COMMITTED_FIELD_COUNT,
        verdict,
      }),
    ).toBe(false);
    expect(
      isCommittedFieldShapeViolation({
        fieldIndex: 0,
        verdict: MIDGARD_FIELD_SHAPE_VERDICT_CODE_COUNT,
      }),
    ).toBe(false);
    expect(isCommittedFieldShapeViolation({ fieldIndex: 0, verdict: -1 })).toBe(
      false,
    );
  });
});

describe("§12.8 evidence", () => {
  const badTxId = "22".repeat(32);

  it("builds the step-02 state the on-chain step 01 derives", () => {
    const preimage = encodeMidgardFieldPreimage([
      Buffer.from([0xde, 0xad, 0xbe, 0xef]),
    ]);
    const evidence = committedFieldShapeEvidenceFromCommittedField({
      badTxId,
      fieldIndex: 0,
      committedPreimage: preimage,
    });
    expect(evidence.violationId).toBe(COMMITTED_FIELD_SHAPE_VIOLATION_ID);
    expect(evidence.verdict).toBe(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE);
    expect(evidence.verdictName).toBe("wrong_stride");
    expect(evidence.fieldStride).toBe(midgardFieldStride(0));
    expect(evidence.isViolation).toBe(true);
    expect(evidence.committedPreimageByteCount).toBe(preimage.length);

    const state = committedFieldShapeStep02StateFromEvidence(evidence);
    expect(state).toEqual({
      bad_tx_id: badTxId,
      field_index: 0n,
      verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE),
    });
    // The state is a datum, so it has to survive the encoder a builder uses.
    expect(
      Data.from(
        Data.to(state, CommittedFieldShapeStep02State),
        CommittedFieldShapeStep02State,
      ),
    ).toEqual(state);
    expect(CommittedFieldShapeStep02StateSchema).toBeDefined();
  });

  it("never convicts an honest field", () => {
    const evidence = committedFieldShapeEvidenceFromCommittedField({
      badTxId,
      fieldIndex: 0,
      committedPreimage: encodeMidgardFieldPreimage([
        Buffer.alloc(midgardFieldStride(0) - 2, 0x00),
      ]),
    });
    expect(evidence.verdict).toBe(MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE);
    expect(evidence.verdictName).toBe("admissible");
    expect(evidence.isViolation).toBe(false);
  });

  it("never convicts a field §12.7 owns", () => {
    const evidence = committedFieldShapeEvidenceFromCommittedField({
      badTxId,
      fieldIndex: 0,
      committedPreimage: Buffer.from([0x80, 0x41]),
    });
    expect(evidence.verdict).toBe(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE);
    expect(evidence.verdictName).toBe("not_an_envelope");
    expect(evidence.isViolation).toBe(false);
  });
});
