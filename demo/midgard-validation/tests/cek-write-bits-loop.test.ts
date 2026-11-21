import { emptyMidgardCekDataListSummary } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { midgardCekLoopHopFits } from "../src/cek-loop-hop-budget.js";
import { midgardCekWriteBitsHopCost } from "../src/cek-write-bits-loop.cost.js";
import {
  type MidgardCekWriteBitsLoop,
  midgardCekWriteBitsLoopOf,
  type MidgardCekWriteBitsPlannedHop,
  planMidgardCekWriteBitsHops,
} from "../src/cek-write-bits-loop.hops.js";
import {
  advanceMidgardCekWriteBitsFailureHop,
  advanceMidgardCekWriteBitsHop,
  midgardCekWriteBits,
  type MidgardCekWriteBitsHop,
  midgardCekWriteBitsItemSummary,
  type MidgardCekWriteBitsState,
  type MidgardCekWriteBitsWalk,
} from "../src/cek-write-bits-loop.js";
import {
  compactedArguments,
  compactedBytes,
  evaluatorWriteBits,
  evaluatorWritesTo,
  pattern,
  spread,
} from "./helpers/cek-write-bits-runs.js";

/**
 * The writeBits loop's hops, as `cek_write_bits_loop_v1` runs them: an
 * honest plan ends with the PV11 evaluator's result and charge, a failure's
 * walk ends on the hop that opens an index outside the input, and every
 * forged hop is refused.
 */

const success = (
  input: Buffer,
  indices: readonly bigint[],
  set: boolean,
): MidgardCekWriteBitsLoop => {
  const out = midgardCekWriteBits(input, indices, set);
  if (out === null) throw new Error("an index is outside the input");
  return midgardCekWriteBitsLoopOf({
    failure: false,
    arguments: compactedArguments(input, indices, set),
    result: compactedBytes(out),
    input,
    indices,
  });
};

const failure = (
  input: Buffer,
  indices: readonly bigint[],
  set = true,
): MidgardCekWriteBitsLoop =>
  midgardCekWriteBitsLoopOf({
    failure: true,
    arguments: compactedArguments(input, indices, set),
    result: null,
    input,
    indices,
  });

const facts = (loop: MidgardCekWriteBitsLoop) => ({
  arguments: loop.arguments,
  result: loop.result!,
});

/** Runs `hop` as a success's hop from where `planned` starts. */
const advance = (
  loop: MidgardCekWriteBitsLoop,
  planned: MidgardCekWriteBitsPlannedHop,
  hop: MidgardCekWriteBitsHop = planned.hop,
  end: number = planned.end,
) =>
  advanceMidgardCekWriteBitsHop(
    facts(loop),
    hop,
    planned.from === null
      ? null
      : {
          cursor: planned.from.cursor,
          state: planned.from.state as MidgardCekWriteBitsState,
        },
    end,
  );

const walk = (
  loop: MidgardCekWriteBitsLoop,
  planned: MidgardCekWriteBitsPlannedHop,
  hop: MidgardCekWriteBitsHop = planned.hop,
  end: number = planned.end,
) =>
  advanceMidgardCekWriteBitsFailureHop(
    loop.arguments,
    hop,
    planned.from === null
      ? null
      : {
          cursor: planned.from.cursor,
          walk: planned.from.state as MidgardCekWriteBitsWalk,
        },
    end,
  );

describe("an honest writeBits loop", () => {
  it.each([
    [1, 1, 1, true],
    [64, 65, 8, true],
    [512, 200, 32, false],
    [4_096, 1_000, 50, true],
    [4_096, 2_000, 1, false],
  ] as const)(
    "over %i bytes and %i indices, %i a hop, ends with the PV11 evaluator's result and charge",
    (length, count, limit, set) => {
      const input = pattern(length, count);
      const indices = spread(count, length, limit);
      const loop = success(input, indices, set);
      const hops = planMidgardCekWriteBitsHops(loop, { limit });
      expect(hops).toHaveLength(Math.ceil(count / limit));
      expect(hops.slice(0, -1).every(({ charge }) => charge === null)).toBe(
        true,
      );
      const evaluator = evaluatorWriteBits(input, indices, set);
      expect(evaluator.ok && hops.at(-1)!.charge).toEqual(
        evaluator.ok && evaluator.charge,
      );
      expect(
        evaluatorWritesTo(
          input,
          indices,
          set,
          midgardCekWriteBits(input, indices, set)!,
        ),
      ).toBe(true);
    },
  );

  it("planned to the hop budget opens as many indices a hop as the budget fits", () => {
    const input = pattern(4_096, 7);
    const indices = spread(1_000, 4_096, 7);
    const hops = planMidgardCekWriteBitsHops(success(input, indices, true));
    expect(hops.length).toBeGreaterThan(1);
    expect(hops.every(({ cost }) => midgardCekLoopHopFits(cost))).toBe(true);
    // Each hop but the last would not fit with the next hop's first index.
    hops.slice(0, -1).forEach(({ shape }, at) => {
      const next = midgardCekWriteBitsItemSummary(hops[at + 1]!.hop.items[0]!);
      expect(
        midgardCekLoopHopFits(
          midgardCekWriteBitsHopCost({
            ...shape,
            items: shape.items + 1,
            itemBytes: shape.itemBytes + Number(next.cborLength),
          }),
        ),
      ).toBe(false);
    });
    expect(hops.at(-1)!.end).toBe(1_000);
  });

  it("with a repeated and a last-bit index sets and clears as the PV11 evaluator does", () => {
    const input = pattern(16);
    const indices = [0n, 127n, 0n, 64n, 127n, 7n, 8n];
    for (const set of [true, false]) {
      const loop = success(input, indices, set);
      const hops = planMidgardCekWriteBitsHops(loop, { limit: 3 });
      const evaluator = evaluatorWriteBits(input, indices, set);
      expect(evaluator.ok && hops.at(-1)!.charge).toEqual(
        evaluator.ok && evaluator.charge,
      );
      expect(
        evaluatorWritesTo(
          input,
          indices,
          set,
          midgardCekWriteBits(input, indices, set)!,
        ),
      ).toBe(true);
    }
  });
});

describe("a writeBits failure's walk", () => {
  it.each([
    ["past the last bit, in the third hop", 64, 20, 17, 512n, 2],
    ["negative, in the first hop", 64, 20, 2, -1n, 0],
    ["past the last bit, last in the list", 4_096, 300, 299, 32_768n, 49],
    ["far past the input, first in the list", 1, 9, 0, 2n ** 70n, 0],
  ] as const)(
    "with an index %s ends on the hop that opens it, as the PV11 evaluator fails",
    (_, length, count, at, bad, endsAt) => {
      const input = pattern(length);
      const indices = spread(count, length);
      indices[at] = bad;
      expect(evaluatorWriteBits(input, indices, true).ok).toBe(false);
      const hops = planMidgardCekWriteBitsHops(failure(input, indices), {
        limit: 6,
      });
      expect(hops).toHaveLength(endsAt + 1);
      expect(hops.at(-1)!.next).toBeNull();
      expect(hops.at(-1)!.charge).toBeNull();
      expect(hops.at(-1)!.hop.items.at(-1)).toBe(bad);
    },
  );

  it("over a list with every index in range is refused at its end", () => {
    const input = pattern(64);
    const loop = failure(input, spread(20, 64));
    expect(() => planMidgardCekWriteBitsHops(loop, { limit: 6 })).toThrow(
      "no index outside the input",
    );
    const honest = planMidgardCekWriteBitsHops(
      success(input, spread(20, 64), true),
      {
        limit: 6,
      },
    );
    const last = honest.at(-1)!;
    expect(
      walk(loop, {
        ...last,
        from: {
          cursor: last.from!.cursor,
          state: { length: 64, root: last.from!.state.root },
        },
      }),
    ).toEqual({ kind: "rejected", reason: "no index outside the input" });
  });
});

describe("a forged writeBits hop", () => {
  const input = pattern(64);
  const indices = spread(20, 64, 3);
  const loop = success(input, indices, true);
  const hops = planMidgardCekWriteBitsHops(loop, { limit: 6 });
  const [first, second] = hops;
  const last = hops.at(-1)!;

  it("is accepted unforged at every hop", () => {
    expect(hops.map((planned) => advance(loop, planned).kind)).toEqual([
      "next",
      "next",
      "next",
      "done",
    ]);
  });

  it.each([
    [
      "reorders the indices it opens",
      () =>
        advance(loop, second!, {
          ...second!.hop,
          items: [...second!.hop.items].reverse(),
        }),
    ],
    [
      "skips an index",
      () =>
        advance(
          loop,
          second!,
          { ...second!.hop, items: second!.hop.items.slice(1) },
          second!.end - 1,
        ),
    ],
    ["replays the previous hop", () => advance(loop, second!, first!.hop)],
    [
      "names a tail that is not the rest",
      () => advance(loop, second!, { ...second!.hop, tail: hops[2]!.hop.tail }),
    ],
    [
      "ends the list early",
      () =>
        advance(loop, second!, {
          ...second!.hop,
          tail: emptyMidgardCekDataListSummary(),
        }),
    ],
    [
      "ends past the indices it opens",
      () => advance(loop, second!, second!.hop, second!.end + 1),
    ],
    [
      "opens no index",
      () =>
        advance(
          loop,
          second!,
          { ...second!.hop, items: [], tail: first!.hop.tail },
          second!.from!.cursor,
        ),
    ],
  ])("that %s is refused", (_, forged) => {
    expect(forged()).toEqual({ kind: "rejected", reason: "opened" });
  });

  it.each([
    ["reveals no start", { start: null }, "start"],
    [
      "reveals another input",
      { start: { ...first!.hop.start!, input: pattern(64, 1) } },
      "input",
    ],
    [
      "reveals another index count",
      { start: { ...first!.hop.start!, itemsCount: 21n } },
      "indices",
    ],
    [
      "reveals another list",
      {
        start: {
          ...first!.hop.start!,
          itemsRoot: Buffer.from(second!.hop.tail.root),
        },
      },
      "indices",
    ],
  ] as const)("whose first hop %s is refused", (_, change, reason) => {
    expect(advance(loop, first!, { ...first!.hop, ...change })).toEqual({
      kind: "rejected",
      reason,
    });
  });

  it("whose last hop gives another result is refused", () => {
    const other = success(input, indices, false);
    expect(advance({ ...loop, result: other.result }, last)).toEqual({
      kind: "rejected",
      reason: "result",
    });
  });

  it("whose arguments are not writeBits' are refused", () => {
    const [bytes, list, flag] = loop.arguments;
    const rows = [
      [[list!, list!, flag!], "input type"],
      [[bytes!, bytes!, flag!], "indices type"],
      [[bytes!, list!, bytes!], "value"],
      [[bytes!, list!], "arguments"],
      [[bytes!, list!, flag!, flag!], "arguments"],
    ] as const;
    for (const [arguments_, reason] of rows)
      expect(
        advanceMidgardCekWriteBitsHop(
          { arguments: arguments_, result: loop.result! },
          first!.hop,
          null,
          first!.end,
        ),
      ).toEqual({ kind: "rejected", reason });
  });

  it("of a success that opens an index outside the input is refused", () => {
    const bad = [...indices];
    bad[8] = 512n;
    const forged = midgardCekWriteBitsLoopOf({
      ...loop,
      arguments: compactedArguments(input, bad, true),
      indices: bad,
    });
    expect(() => planMidgardCekWriteBitsHops(forged, { limit: 6 })).toThrow(
      "writeBits hop is rejected: index",
    );
  });

  it("of a failure is refused on the same forgeries", () => {
    const bad = [...indices];
    bad[17] = 512n;
    const walked = failure(input, bad);
    const plan = planMidgardCekWriteBitsHops(walked, { limit: 6 });
    expect(plan.map((planned) => walk(walked, planned).kind)).toEqual([
      "next",
      "next",
      "failed",
    ]);
    const [start, middle] = plan;
    const forgeries = [
      walk(walked, middle!, {
        ...middle!.hop,
        items: [...middle!.hop.items].reverse(),
      }),
      walk(walked, middle!, start!.hop),
      walk(walked, middle!, middle!.hop, middle!.end + 1),
      walk(walked, middle!, {
        ...middle!.hop,
        tail: emptyMidgardCekDataListSummary(),
      }),
    ];
    for (const forged of forgeries)
      expect(forged).toEqual({ kind: "rejected", reason: "opened" });
    expect(walk(walked, start!, { ...start!.hop, start: null })).toEqual({
      kind: "rejected",
      reason: "start",
    });
    expect(
      walk(walked, start!, {
        ...start!.hop,
        start: { ...start!.hop.start!, input: pattern(128) },
      }),
    ).toEqual({ kind: "rejected", reason: "input" });
    expect(
      walk(walked, start!, {
        ...start!.hop,
        start: { ...start!.hop.start!, itemsCount: 19n },
      }),
    ).toEqual({ kind: "rejected", reason: "indices" });
    for (const arguments_ of [
      walked.arguments.slice(0, 2),
      [...walked.arguments, walked.arguments[2]!],
    ])
      expect(
        advanceMidgardCekWriteBitsFailureHop(
          arguments_,
          start!.hop,
          null,
          start!.end,
        ),
      ).toEqual({ kind: "rejected", reason: "arguments" });
  });
});
