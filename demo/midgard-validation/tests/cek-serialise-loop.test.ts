import {
  buildMidgardCekDataTraverseTrace,
  type MidgardCekStagedOutRef,
} from "@al-ft/midgard-core";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { describe, expect, it } from "vitest";

import { vectors } from "../../midgard-core/tests/fixtures/cek-data-canonical-corpus-v1.vectors.mjs";
import {
  MIDGARD_CEK_SERIALISE_START,
  type MidgardCekSerialiseLoop,
  type MidgardCekSerialisePlannedHop,
  type MidgardCekSerialisePoint,
  planMidgardCekSerialiseHop,
  planMidgardCekSerialiseHops,
} from "../src/cek-serialise-loop.hops.js";
import {
  advanceMidgardCekSerialiseHop,
  type MidgardCekSerialiseFacts,
  midgardCekSerialiseFactsOf,
} from "../src/cek-serialise-loop.js";
import { midgardCekSerialiseChain } from "../src/cek-serialise-walk.data.js";
import { runMidgardCekSerialise } from "../src/cek-serialise-walk.js";
import {
  bytes,
  constr,
  int,
  list,
  values,
} from "./fixtures/cek-serialise-loop-v1.vectors.mjs";

/**
 * The serialise loop's off-chain twin: its reader gives every canonical Data
 * value the summary the canonical traverse gives it and refuses every other
 * span; an honest prover's hops end the loop for any hop split; and every
 * deviation from the honest hops is rejected.
 */

const summaryOf = (source: Buffer) => {
  const summary = buildMidgardCekDataTraverseTrace({ sourceStart: 0, source })
    .terminal.result;
  if (summary === null || summary === undefined)
    throw new Error("not canonical Data");
  return summary;
};

/** The staging output of run `index`, as the Aiken fixture names it. */
const refOf = (index: number): MidgardCekStagedOutRef => ({
  transactionId: computeHash32(Buffer.from([10 + index])),
  outputIndex: 10 + index,
});

const runsOf = (chunks: readonly Buffer[]): Buffer[][] => {
  const runs: Buffer[][] = [];
  for (let index = 0; index < chunks.length; index += 3)
    runs.push(chunks.slice(index, index + 3));
  return runs;
};

/** The loop of serialiseData over `argument`, its result staged as `staged`. */
const loopOf = (
  argument: Buffer,
  staged: Buffer = argument,
): MidgardCekSerialiseLoop => {
  const { facts, chunks } = midgardCekSerialiseFactsOf(
    summaryOf(argument),
    staged,
  );
  const runs = runsOf(chunks);
  return { facts, runs, refs: runs.map((_, index) => refOf(index)) };
};

const hexLoop = (hex: string) => loopOf(Buffer.from(hex, "hex"));

/** Plans and runs every hop at most `limit` steps long; the hops it ran. */
const drive = (
  loop: MidgardCekSerialiseLoop,
  limit?: number,
): MidgardCekSerialisePlannedHop[] => {
  const hops: MidgardCekSerialisePlannedHop[] = [];
  let point: MidgardCekSerialisePoint | null = MIDGARD_CEK_SERIALISE_START;
  while (point !== null) {
    const planned = planMidgardCekSerialiseHop(loop, point, { limit });
    hops.push(planned);
    point = planned.point;
  }
  return hops;
};

const advance = (
  loop: { readonly facts: MidgardCekSerialiseFacts },
  planned: MidgardCekSerialisePlannedHop,
  edit: Partial<MidgardCekSerialisePlannedHop> = {},
) => {
  const hop = { ...planned, ...edit };
  return advanceMidgardCekSerialiseHop(
    loop.facts,
    hop.hop,
    hop.referenceInputs,
    hop.from,
    hop.end,
  );
};

describe("the serialise reader over the canonical Data corpus", () => {
  it.each(vectors.map((vector) => [vector.name, vector] as const))(
    "%s",
    (_, vector) => {
      const source = Buffer.from(vector.hex, "hex");
      const progress = runMidgardCekSerialise(
        source,
        0,
        { kind: "root" },
        null,
        source.length,
        4 * source.length + 8,
      );
      if (vector.verdict === "accepted")
        expect(progress).toEqual({
          kind: "finished",
          summary: summaryOf(source),
        });
      else expect(progress.kind).toBe("refused");
    },
  );
});

describe("an honest prover's hops end the loop", () => {
  it.each(
    values
      .filter((value) => !value.maximum)
      .map((value) => [value.name, value.hex] as const),
  )("%s, in budget-sized hops", (_, hex) => {
    const loop = hexLoop(hex);
    const hops = planMidgardCekSerialiseHops(loop);
    expect(hops.at(-1)!.next).toBeNull();
    for (const planned of hops)
      expect(advance(loop, planned)).toEqual(
        planned.next === null
          ? { kind: "done" }
          : { kind: "next", state: planned.next },
      );
  });

  it.each([1, 2, 3, 7, 64])("small_mixed, in hops of %i steps", (limit) => {
    const loop = hexLoop(values[0]!.hex);
    const hops = drive(loop, limit);
    for (const planned of hops.slice(0, -1))
      expect(planned.end - (planned.from?.cursor ?? 0)).toBe(limit);
    for (const planned of hops)
      expect(advance(loop, planned).kind).toBe(
        planned.next === null ? "done" : "next",
      );
  });

  it("a leaf across runs, one leaf chunk a hop", () => {
    const loop = hexLoop(bytes(20_000));
    const hops = drive(loop, 1);
    expect(hops.length).toBeGreaterThan(5);
    expect(hops.some((planned) => planned.next?.carry !== null)).toBe(true);
    for (const planned of hops)
      expect(advance(loop, planned).kind).toBe(
        planned.next === null ? "done" : "next",
      );
  });

  it("resumes from any hop's end with the same plan", () => {
    const loop = hexLoop(values[3]!.hex);
    const whole = drive(loop, 5);
    const resumed = drive(loop, 5);
    expect(resumed.map((planned) => planned.end)).toEqual(
      whole.map((planned) => planned.end),
    );
    const middle = whole[Math.floor(whole.length / 2)]!;
    expect(
      planMidgardCekSerialiseHop(loop, middle.point!, { limit: 5 }).end,
    ).toBe(whole[Math.floor(whole.length / 2) + 1]!.end);
  });
});

describe("the loop rejects every deviation from the honest hops", () => {
  const mixed = Buffer.from(values[0]!.hex, "hex");
  const leaf = Buffer.from(bytes(20_000), "hex");

  /** One hop over all of R's runs, from the start, up to `end` steps. */
  const whole = (loop: MidgardCekSerialiseLoop, end?: number) => {
    const { result } = loop.facts;
    if (result.kind !== "semanticConstant") throw new Error("no result");
    return advanceMidgardCekSerialiseHop(
      loop.facts,
      {
        refs: loop.refs,
        order: loop.refs.map((_, index) => index),
        restore: [],
        base: Buffer.alloc(0),
      },
      loop.refs.map((outRef, index) => ({
        outRef,
        chunks: loop.runs[index]!,
      })),
      null,
      end ?? Number(16n * result.witness.memory),
    );
  };

  type Witness = Extract<
    MidgardCekSerialiseFacts["result"],
    { kind: "semanticConstant" }
  >["witness"];

  /** The loop of `mixed` with its argument's or result's witness edited. */
  const edited = (
    part: "argument" | "result",
    edit: (witness: Witness) => Witness,
  ): MidgardCekSerialiseLoop => {
    const loop = loopOf(mixed);
    const value = (
      part === "argument" ? loop.facts.arguments[0] : loop.facts.result
    )!;
    if (value.kind !== "semanticConstant") throw new Error("not semantic");
    const changed = { ...value, witness: edit(value.witness) };
    return {
      ...loop,
      facts:
        part === "argument"
          ? { ...loop.facts, arguments: [changed] }
          : { ...loop.facts, result: changed },
    };
  };

  it("accepts the honest argument and result", () => {
    expect(whole(edited("argument", (witness) => witness))).toEqual({
      kind: "done",
    });
    expect(whole(edited("result", (witness) => witness))).toEqual({
      kind: "done",
    });
  });

  it.each([
    [
      "an argument of another type",
      "argument",
      (witness: Witness) => ({
        ...witness,
        typeCbor: Buffer.from("9f00ff", "hex"),
      }),
      "summary",
    ],
    [
      "an argument with another memory",
      "argument",
      (witness: Witness) => ({ ...witness, memory: witness.memory + 1n }),
      "summary",
    ],
    [
      "a result of another type",
      "result",
      (witness: Witness) => ({
        ...witness,
        typeCbor: Buffer.from("9f02ff", "hex"),
      }),
      "result",
    ],
    [
      "a result with another memory",
      "result",
      (witness: Witness) => ({ ...witness, memory: witness.memory + 1n }),
      "result",
    ],
    [
      "a result with another CBOR length",
      "result",
      (witness: Witness) => ({
        ...witness,
        payload: {
          ...witness.payload,
          cborLength: witness.payload.cborLength + 1n,
        },
      }),
      "result",
    ],
  ] as const)("refuses %s", (_, part, edit, reason) => {
    expect(whole(edited(part, edit))).toEqual({ kind: "rejected", reason });
  });

  it("ends the loop in one hop over the canonical bytes", () => {
    expect(whole(loopOf(mixed))).toEqual({ kind: "done" });
  });

  it.each([
    ["R with a trailing byte", Buffer.concat([mixed, Buffer.from([0])])],
    ["a truncated tail", mixed.subarray(0, mixed.length - 1)],
    ["two values", Buffer.concat([mixed, mixed])],
  ])("refuses %s", (_, staged) => {
    expect(whole(loopOf(mixed, staged))).toEqual({
      kind: "rejected",
      reason: "unreadable",
    });
  });

  it("refuses the bytes of another value", () => {
    const other = Buffer.from(list([int(1), int(2)]), "hex");
    expect(whole(loopOf(mixed, other))).toEqual({
      kind: "rejected",
      reason: "summary",
    });
    expect(() => planMidgardCekSerialiseHops(loopOf(mixed, other))).toThrow(
      /summary/u,
    );
  });

  it("refuses the bytes of another value of the same memory", () => {
    const zero = Buffer.from(int(0), "hex");
    const one = Buffer.from(int(1), "hex");
    expect(summaryOf(zero).memory).toBe(summaryOf(one).memory);
    expect(whole(loopOf(zero, one))).toEqual({
      kind: "rejected",
      reason: "summary",
    });
  });

  it.each([
    ["a non-minimal head", "9f1817ff"],
    ["a definite long byte string", `5841${"00".repeat(65)}`],
    ["an indefinite short byte string", `5f4100ff`],
    ["an indefinite empty list", "9fff"],
    ["a definite field list", "d8798100"],
    ["an indefinite map", "bf0000ff"],
    ["a short bignum", `c248${"01".repeat(8)}`],
    ["a large tag form for a small index", "d866820080"],
    [
      "a byte string with a short inner chunk",
      `5f5820${"00".repeat(32)}5840${"00".repeat(64)}ff`,
    ],
  ])("refuses %s", (_, staged) => {
    expect(
      whole(loopOf(Buffer.from("80", "hex"), Buffer.from(staged, "hex"))),
    ).toEqual({ kind: "rejected", reason: "unreadable" });
  });

  it("refuses runs read in another order", () => {
    const loop = loopOf(leaf);
    const planned = drive(loop).find(({ hop }) => hop.order.length > 1);
    expect(planned).toBeDefined();
    expect(advance(loop, planned!)).not.toMatchObject({ kind: "rejected" });
    const swapped = [...planned!.hop.order].reverse();
    expect(
      advance(loop, planned!, { hop: { ...planned!.hop, order: swapped } }),
    ).toMatchObject({ kind: "rejected" });
  });

  it("refuses a run read twice and a skipped run", () => {
    const loop = hexLoop(bytes(20_000));
    const hops = drive(loop, 1);
    const index = hops.findIndex(
      (planned) => planned.hop.order.length > 1 && planned.from !== null,
    );
    const planned = hops[index]!;
    const [carried, ...fresh] = planned.hop.order;
    for (const order of [
      [carried!, carried!, ...fresh],
      [carried!],
      [carried!, ...fresh, ...fresh],
    ])
      expect(
        advance(loop, planned, { hop: { ...planned.hop, order } }),
      ).toMatchObject({ kind: "rejected" });
  });

  it("refuses a tail read from another run than the carried one", () => {
    const loop = hexLoop(bytes(20_000));
    expect(loop.runs).toHaveLength(2);
    const [other, carried] = loop.refs;
    const planned = drive(loop, 1).find(
      ({ from }) => from?.state.carry?.run.outputIndex === carried!.outputIndex,
    );
    expect(planned).toBeDefined();
    expect(advance(loop, planned!)).not.toMatchObject({ kind: "rejected" });
    // The hop's first run, where it reads its tail, becomes run 0.
    const [, ...refs] = planned!.hop.refs;
    expect(
      advance(loop, planned!, {
        hop: { ...planned!.hop, refs: [other!, ...refs] },
        referenceInputs: [
          ...planned!.referenceInputs,
          { outRef: other!, chunks: loop.runs[0]! },
        ],
      }),
    ).toEqual({ kind: "rejected", reason: "carry" });
  });

  it("refuses a hop that names a run its reading does not reach", () => {
    const loop = hexLoop(bytes(20_000));
    expect(loop.runs).toHaveLength(2);
    const [first] = drive(loop, 1);
    const next = advance(loop, first!);
    // The honest first hop stops inside run 0.
    expect(next).toMatchObject({
      kind: "next",
      state: { carry: { run: loop.refs[0] } },
    });
    expect(
      advance(loop, first!, {
        hop: {
          ...first!.hop,
          refs: [...loop.refs],
          order: [0, 1],
        },
        referenceInputs: loop.refs.map((outRef, index) => ({
          outRef,
          chunks: loop.runs[index]!,
        })),
      }),
    ).toEqual({ kind: "rejected", reason: "tail" });
  });

  it("refuses a mis-split staging: a short chunk before the last", () => {
    const loop = hexLoop(bytes(20_000));
    const [planned] = planMidgardCekSerialiseHops(loop);
    const shortened = planned!.referenceInputs.map((input, index) =>
      index === 0
        ? {
            ...input,
            chunks: [input.chunks![0]!.subarray(1), ...input.chunks!.slice(1)],
          }
        : input,
    );
    expect(advance(loop, planned!, { referenceInputs: shortened })).toEqual({
      kind: "rejected",
      reason: "frontier",
    });
  });

  it("refuses a replayed hop and a hop that ends at its cursor", () => {
    const loop = hexLoop(values[3]!.hex);
    const hops = drive(loop, 4);
    const second = hops[1]!;
    const replay = advance(loop, second, {
      from: { cursor: second.end, state: second.next! },
      end: second.end + 4,
    });
    expect(replay).toMatchObject({ kind: "rejected" });
    expect(advance(loop, second, { end: second.from!.cursor })).toEqual({
      kind: "rejected",
      reason: "cursor",
    });
    expect(advance(loop, hops[0]!, { end: 0 })).toEqual({
      kind: "rejected",
      reason: "cursor",
    });
  });

  it("refuses a hop past the step bound", () => {
    const loop = hexLoop(constr(0, []));
    const [planned] = planMidgardCekSerialiseHops(loop);
    expect(advance(loop, planned!, { end: 1_000 })).toEqual({
      kind: "rejected",
      reason: "end",
    });
  });

  it("refuses a hand-on stack the chain does not hold", () => {
    const loop = hexLoop(values[2]!.hex);
    const hops = drive(loop, 140);
    const popping = hops.find((planned) => planned.hop.restore.length > 0)!;
    expect(
      advance(loop, popping, {
        hop: { ...popping.hop, restore: popping.hop.restore.slice(1) },
      }),
    ).toEqual({ kind: "rejected", reason: "stack root" });
    expect(
      advance(loop, popping, {
        hop: { ...popping.hop, base: Buffer.alloc(32, 1) },
      }),
    ).toEqual({ kind: "rejected", reason: "stack root" });
  });

  it("refuses a restore too short for the hop's pops", () => {
    const loop = hexLoop(values[2]!.hex);
    const hops = drive(loop, 140);
    const popping = hops.find((planned) => planned.hop.restore.length > 1)!;
    const [top, ...rest] = popping.hop.restore;
    expect(
      advance(loop, popping, {
        hop: {
          ...popping.hop,
          restore: [top!],
          base: midgardCekSerialiseChain(rest, popping.hop.base),
        },
      }),
    ).toEqual({ kind: "rejected", reason: "restore more of the stack" });
  });
});
