import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  decodeMidgardFieldPreimage,
  encodeMidgardFieldPreimage,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceCanonicalDecodabilityWalk,
  CanonicalDecodabilityWalkState,
  MIDGARD_ENVELOPE_BATCH_ITEMS,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
  MIDGARD_ENVELOPE_VERDICT_NOT_AN_ITEM_HEADER,
  miscountedMidgardFieldPreimage,
} from "../src/fraud-proof/canonical-decodability.js";

/**
 * §12.7 step 01 as a batched walk: one batch of item heads per transaction,
 * held to the one-evaluation verdict and to the fail-closed decoder. The
 * golden vectors are the same fixture `canonical-decodability.test.ts` checks.
 */

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/canonical-decodability-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as {
  readonly vectors: readonly {
    readonly preimage: string;
    readonly verdict: number;
  }[];
};

const bytes = (hexValue: string) => Buffer.from(hexValue, "hex");

describe("§12.7 batched walk", () => {
  const badTxId = "33".repeat(32);
  const fieldIndex = 2;

  /** Every transition one honest prover takes, first step to verdict. */
  const walk = (committedPreimage: Uint8Array) => {
    const transitions = [];
    let previous: CanonicalDecodabilityWalkState | null = null;
    for (;;) {
      const next = advanceCanonicalDecodabilityWalk({
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

  /** `declared` under a minimal head, then `present` one-byte items. */
  const oneByteItems = (declared: number, present: number) =>
    miscountedMidgardFieldPreimage(
      declared,
      Array.from({ length: present }, () => Buffer.alloc(0)),
    );

  const verdictOf = (committedPreimage: Uint8Array) => {
    const last = walk(committedPreimage).at(-1)!;
    return last.kind === "verdict" ? Number(last.state.verdict) : -1;
  };

  it("ends at every golden vector's verdict", () => {
    for (const vector of golden.vectors) {
      expect(verdictOf(bytes(vector.preimage))).toBe(vector.verdict);
    }
  });

  it("agrees with the fail-closed decoder across batch boundaries", () => {
    // Fields of a few batches with every item width, then each one with one
    // byte overwritten: the walk calls a field grammatical exactly when the
    // throwing decoder accepts it, wherever the batch cuts fall.
    let seed = 0x5eed;
    const next = (bound: number) => {
      seed = (seed * 1_103_515_245 + 12_345) % 2 ** 31;
      return seed % bound;
    };
    const widths = [0, 1, 23, 24, 255, 256];
    for (let round = 0; round < 24; round += 1) {
      const count = MIDGARD_ENVELOPE_BATCH_ITEMS + next(2_000);
      const items = Array.from({ length: count }, () =>
        Buffer.alloc(widths[next(widths.length)]!),
      );
      const honest = encodeMidgardFieldPreimage(items);
      const variants = [honest];
      for (let flip = 0; flip < 6; flip += 1) {
        const mutated = Buffer.from(honest);
        mutated[next(mutated.length)] = next(256);
        variants.push(mutated);
      }
      variants.push(honest.subarray(0, next(honest.length)));
      for (const preimage of variants) {
        let decodes = true;
        try {
          decodeMidgardFieldPreimage(preimage);
        } catch {
          decodes = false;
        }
        expect(
          verdictOf(preimage) === MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
        ).toBe(decodes);
      }
    }
  });

  it("takes one step per batch of items", () => {
    const batch = MIDGARD_ENVELOPE_BATCH_ITEMS;
    expect(walk(oneByteItems(batch - 1, batch - 1))).toHaveLength(1);
    expect(walk(oneByteItems(batch, batch))).toHaveLength(1);
    const overOne = walk(oneByteItems(batch + 1, batch + 1));
    expect(overOne).toHaveLength(2);
    expect(overOne[0]).toEqual({
      kind: "continue",
      state: {
        bad_tx_id: badTxId,
        field_index: BigInt(fieldIndex),
        remaining: 1n,
        offset: BigInt(3 + batch),
      },
    });
    // A batch that reads its full share onto the last byte with items still
    // owed ends the walk there; no further step could find a header.
    const owed = walk(oneByteItems(batch + 1, batch));
    expect(owed).toHaveLength(1);
    expect(owed[0]).toMatchObject({
      kind: "verdict",
      state: { verdict: BigInt(MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER) },
    });
    for (const transitions of [
      walk(oneByteItems(batch - 1, batch - 1)),
      walk(oneByteItems(batch, batch)),
      overOne,
    ]) {
      expect(transitions.at(-1)).toMatchObject({
        kind: "verdict",
        state: { verdict: BigInt(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL) },
      });
    }
  });

  it("walks the largest legal field in bounded steps", () => {
    // 32,765 empty items under `99 7ffd` fill §5.4's 32,768-byte bound: the
    // highest item count any committed field reaches.
    const honest = oneByteItems(32_765, 32_765);
    expect(honest.length).toBe(MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES);
    const honestWalk = walk(honest);
    expect(honestWalk).toHaveLength(
      Math.ceil(32_765 / MIDGARD_ENVELOPE_BATCH_ITEMS),
    );
    expect(honestWalk.at(-1)).toMatchObject({
      kind: "verdict",
      state: { verdict: BigInt(MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL) },
    });
    // One item more declared than present: the walk finds no header for it.
    const overclaimed = oneByteItems(32_766, 32_765);
    expect(verdictOf(overclaimed)).toBe(
      MIDGARD_ENVELOPE_VERDICT_MISSING_ITEM_HEADER,
    );
  });

  it("stops at a bad item wherever its batch falls", () => {
    const count = 2 * MIDGARD_ENVELOPE_BATCH_ITEMS + 5;
    for (const position of [
      0,
      MIDGARD_ENVELOPE_BATCH_ITEMS - 1,
      MIDGARD_ENVELOPE_BATCH_ITEMS,
      count - 1,
    ]) {
      const items = Array.from({ length: count }, () => Buffer.alloc(0));
      const preimage = encodeMidgardFieldPreimage(items);
      preimage[3 + position] = 0x60;
      const transitions = walk(preimage);
      expect(transitions).toHaveLength(
        Math.floor(position / MIDGARD_ENVELOPE_BATCH_ITEMS) + 1,
      );
      expect(verdictOf(preimage)).toBe(
        MIDGARD_ENVELOPE_VERDICT_NOT_AN_ITEM_HEADER,
      );
    }
  });

  it("refuses to resume another transaction's or field's walk", () => {
    const preimage = oneByteItems(
      MIDGARD_ENVELOPE_BATCH_ITEMS + 1,
      MIDGARD_ENVELOPE_BATCH_ITEMS + 1,
    );
    const first = walk(preimage)[0]!;
    expect(first.kind).toBe("continue");
    const state = first.state as CanonicalDecodabilityWalkState;
    expect(() =>
      advanceCanonicalDecodabilityWalk({
        badTxId: "44".repeat(32),
        fieldIndex,
        committedPreimage: preimage,
        previous: state,
      }),
    ).toThrow();
    expect(() =>
      advanceCanonicalDecodabilityWalk({
        badTxId,
        fieldIndex: fieldIndex + 1,
        committedPreimage: preimage,
        previous: state,
      }),
    ).toThrow();
  });

  it("encodes the walk state as the validator's four-field constructor", () => {
    const state: CanonicalDecodabilityWalkState = {
      bad_tx_id: badTxId,
      field_index: 2n,
      remaining: 31_765n,
      offset: 1_003n,
    };
    const cbor = Data.to(state, CanonicalDecodabilityWalkState);
    expect(cbor.startsWith("d8799f5820" + badTxId + "02")).toBe(true);
    expect(Data.from(cbor, CanonicalDecodabilityWalkState)).toEqual(state);
  });
});
