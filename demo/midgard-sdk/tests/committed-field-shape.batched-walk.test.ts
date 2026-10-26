import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
  MIDGARD_WALK_DERIVED_STRIDE,
  midgardFieldStride,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  MIDGARD_COMMITTED_FIELD_COUNT,
  MIDGARD_ENVELOPE_BATCH_ITEMS,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  midgardEnvelopeVerdict,
  miscountedMidgardFieldPreimage,
} from "../src/fraud-proof/canonical-decodability.js";
import { advanceCommittedFieldShapeWalk } from "../src/fraud-proof/committed-field-shape.advance-committed-field-shape-walk.js";
import {
  CommittedFieldShapeWalkState,
  MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
  MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
  MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
  MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
  midgardCommittedFieldShapeVerdict,
  midgardMinimalArrayHeader,
  sizedMidgardFieldEnvelope,
} from "../src/fraud-proof/committed-field-shape.js";

/**
 * §12.8 step 01 as a batched walk: §12.7's item-head walk one batch per
 * transaction, and the slot's shape verdict from the batch that ends it. Held
 * to the golden vectors and to the verdict as the family rendered it in one
 * evaluation, transcribed below as the oracle.
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
  readonly vectors: readonly {
    readonly fieldIndex: number;
    readonly construction: Construction;
    readonly verdict: number;
  }[];
};

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

/**
 * The shape verdict as the family rendered it over a whole field in one
 * evaluation: header first, then §12.7's verdict, the byte bound and the
 * stride.
 */
const oneEvaluationVerdict = (
  fieldIndex: number,
  preimage: Uint8Array,
): number => {
  const stride = midgardFieldStride(fieldIndex);
  const header = midgardMinimalArrayHeader(preimage);
  if (header === undefined) return MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE;
  if (
    midgardEnvelopeVerdict(preimage) !== MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL
  ) {
    return MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE;
  }
  if (preimage.length > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES) {
    return MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND;
  }
  if (stride === MIDGARD_WALK_DERIVED_STRIDE) {
    return MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE;
  }
  return header.headerLen + stride * header.count === preimage.length
    ? MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE
    : MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE;
};

describe("§12.8 batched walk", () => {
  const badTxId = "33".repeat(32);
  const spendInputs = 0;
  const outputs = 2;
  const batch = MIDGARD_ENVELOPE_BATCH_ITEMS;

  /** Every transition one honest prover takes, first step to verdict. */
  const walk = (fieldIndex: number, committedPreimage: Uint8Array) => {
    const transitions = [];
    let previous: CommittedFieldShapeWalkState | null = null;
    for (;;) {
      const next = advanceCommittedFieldShapeWalk({
        badTxId,
        fieldIndex,
        committedPreimage,
        previous,
      });
      transitions.push(next);
      if (next.kind === "verdict") return transitions;
      previous = next.state;
    }
  };

  const verdictOf = (fieldIndex: number, committedPreimage: Uint8Array) => {
    const last = walk(fieldIndex, committedPreimage).at(-1)!;
    return last.kind === "verdict" ? Number(last.state.verdict) : -1;
  };

  /** `declared` under a minimal head, then `present` one-byte items. */
  const oneByteItems = (declared: number, present: number) =>
    miscountedMidgardFieldPreimage(
      declared,
      Array.from({ length: present }, () => Buffer.alloc(0)),
    );

  it("ends at every golden vector's verdict", () => {
    for (const vector of golden.vectors) {
      expect(
        verdictOf(vector.fieldIndex, buildPreimage(vector.construction)),
      ).toBe(vector.verdict);
    }
  });

  it("agrees with the one-evaluation verdict across batch boundaries, at every slot", () => {
    // Fields of up to a few batches whose items are byte strings of every
    // width that meets a slot's stride or changes a head's width, mixed or
    // uniform, then each one with a byte overwritten or cut short: the walked
    // verdict is the one-evaluation verdict wherever the batch cuts fall.
    let seed = 0xc0ffee;
    const next = (bound: number) => {
      seed = (seed * 1_103_515_245 + 12_345) % 2 ** 31;
      return seed % bound;
    };
    const byteString = (length: number) =>
      Buffer.concat([
        length < 24
          ? Buffer.from([0x40 + length])
          : length < 256
            ? Buffer.from([0x58, length])
            : Buffer.from([0x59, length >> 8, length & 0xff]),
        Buffer.alloc(length, 0xab),
      ]);
    const payloads = [0, 1, 22, 23, 24, 28, 38, 101, 255, 256];
    const seen = new Set<number>();
    let decidedAcrossBatches = 0;
    for (let round = 0; round < 32; round += 1) {
      const count = next(2) === 0 ? 1 + next(819) : batch - 2 + next(1_300);
      const payload = payloads[next(payloads.length)]!;
      const mixed = next(3) === 0;
      const items = Array.from({ length: count }, () =>
        byteString(
          mixed && next(4) === 0 ? payloads[next(payloads.length)]! : payload,
        ),
      );
      const honest = encodeMidgardFieldPreimage(items);
      const variants = [honest];
      for (let flip = 0; flip < 3; flip += 1) {
        const mutated = Buffer.from(honest);
        mutated[next(mutated.length)] = next(256);
        variants.push(mutated);
      }
      variants.push(honest.subarray(0, next(honest.length)));
      for (const preimage of variants) {
        for (
          let fieldIndex = 0;
          fieldIndex < MIDGARD_COMMITTED_FIELD_COUNT;
          fieldIndex += 1
        ) {
          const oracle = oneEvaluationVerdict(fieldIndex, preimage);
          seen.add(oracle);
          const transitions = walk(fieldIndex, preimage);
          const last = transitions.at(-1)!;
          expect(last.kind).toBe("verdict");
          expect(Number((last.state as { verdict: bigint }).verdict)).toBe(
            oracle,
          );
          if (
            transitions.length > 1 &&
            oracle !== MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE
          ) {
            decidedAcrossBatches += 1;
          }
          expect(midgardCommittedFieldShapeVerdict(fieldIndex, preimage)).toBe(
            oracle,
          );
        }
      }
    }
    expect([...seen].sort()).toEqual([
      MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
      MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
      MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
      MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
    ]);
    expect(decidedAcrossBatches).toBeGreaterThan(0);
  });

  it("takes one step per batch of items and convicts at a fixed-stride slot", () => {
    expect(walk(spendInputs, oneByteItems(batch - 1, batch - 1))).toHaveLength(
      1,
    );
    expect(walk(spendInputs, oneByteItems(batch, batch))).toHaveLength(1);
    const overOne = walk(spendInputs, oneByteItems(batch + 1, batch + 1));
    expect(overOne).toHaveLength(2);
    expect(overOne[0]).toEqual({
      kind: "continue",
      state: {
        bad_tx_id: badTxId,
        field_index: BigInt(spendInputs),
        remaining: 1n,
        offset: BigInt(3 + batch),
      },
    });
    for (const count of [batch - 1, batch, batch + 1]) {
      expect(verdictOf(spendInputs, oneByteItems(count, count))).toBe(
        MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE,
      );
      expect(verdictOf(outputs, oneByteItems(count, count))).toBe(
        MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
      );
    }
  });

  it("ends the walk at once when items are owed at the last byte", () => {
    // A batch that reads its full share onto the last byte, and runs of eight
    // heads that do, with items still owed: no envelope, in one step, at the
    // slot where a grammatical reading would convict.
    for (const preimage of [
      oneByteItems(batch + 1, batch),
      Buffer.from("894040404040404040", "hex"),
      oneByteItems(17, 16),
      oneByteItems(255, 8),
    ]) {
      const transitions = walk(spendInputs, preimage);
      expect(transitions).toHaveLength(1);
      expect(transitions[0]).toMatchObject({
        kind: "verdict",
        state: { verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE) },
      });
    }
  });

  it("walks the largest legal field in bounded steps", () => {
    // 32,765 empty items under `99 7ffd` fill §5.4's 32,768-byte bound: the
    // highest item count any committed field reaches.
    const largest = oneByteItems(32_765, 32_765);
    expect(largest.length).toBe(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES);
    const steps = Math.ceil(32_765 / batch);
    const convicting = walk(spendInputs, largest);
    expect(convicting).toHaveLength(steps);
    expect(convicting.at(-1)).toMatchObject({
      kind: "verdict",
      state: { verdict: BigInt(MIDGARD_FIELD_SHAPE_VERDICT_WRONG_STRIDE) },
    });
    expect(verdictOf(outputs, largest)).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_ADMISSIBLE,
    );
    // One item more than the bound admits, at a walked slot.
    expect(verdictOf(outputs, oneByteItems(32_766, 32_766))).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_FIELD_BYTE_BOUND,
    );
    // One item more declared than present.
    expect(verdictOf(spendInputs, oneByteItems(32_766, 32_765))).toBe(
      MIDGARD_FIELD_SHAPE_VERDICT_NOT_AN_ENVELOPE,
    );
  });

  it("refuses to resume another transaction's or field's walk", () => {
    const preimage = oneByteItems(batch + 1, batch + 1);
    const first = walk(spendInputs, preimage)[0]!;
    expect(first.kind).toBe("continue");
    const state = first.state as CommittedFieldShapeWalkState;
    expect(() =>
      advanceCommittedFieldShapeWalk({
        badTxId: "44".repeat(32),
        fieldIndex: spendInputs,
        committedPreimage: preimage,
        previous: state,
      }),
    ).toThrow();
    expect(() =>
      advanceCommittedFieldShapeWalk({
        badTxId,
        fieldIndex: spendInputs + 1,
        committedPreimage: preimage,
        previous: state,
      }),
    ).toThrow();
  });

  it("encodes the walk state as the validator's four-field constructor", () => {
    const state: CommittedFieldShapeWalkState = {
      bad_tx_id: badTxId,
      field_index: 0n,
      remaining: 31_765n,
      offset: 1_003n,
    };
    const cbor = Data.to(state, CommittedFieldShapeWalkState);
    expect(cbor.startsWith("d8799f5820" + badTxId + "00")).toBe(true);
    expect(Data.from(cbor, CommittedFieldShapeWalkState)).toEqual(state);
  });
});
