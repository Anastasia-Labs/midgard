import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekDataTraverse,
  buildMidgardCekDataTraverseTrace,
  decodeMidgardDatum,
  driveMidgardCekDataTraverse,
  isCanonicalMidgardCekData,
  isCanonicalMidgardDataStream,
  isUnreadableMidgardCekDataTraverse,
  MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
  type MidgardDataStreamFrame,
  midgardDataStreamStackRoot,
  runMidgardDataStream,
} from "../src/index.js";
import { vectors } from "./fixtures/cek-data-canonical-corpus-v1.vectors.mjs";

const STACK_ROOT_GOLDEN =
  "04366e7a83b284987e4d689293ab1ae13e11a503371f30218af912b12e65c9c8";

const nodeAdmitsDatum = (source: Buffer): boolean => {
  try {
    decodeMidgardDatum(source);
    return true;
  } catch {
    return false;
  }
};

/**
 * The resumable reader driven the way `data-stream-driver.ak` drives it: the
 * span sits at offset 1 of a carrier, each call reads two `chunk`-byte chunks
 * from the chunk holding the cursor, runs `budget` nodes and reveals only the
 * top open container, so every resume goes through the stored hash chain.
 */
const streamResumed = (source: Buffer, chunk = 68, budget = 1): boolean => {
  const carrier = Buffer.concat([
    Buffer.from([0x5a]),
    source,
    Buffer.from([0xa5]),
  ]);
  const start = 1;
  const end = start + source.length;
  let cursor = start;
  let stack: readonly MidgardDataStreamFrame[] = [];
  for (;;) {
    const base =
      Math.floor(Math.min(cursor, carrier.length - 1) / chunk) * chunk;
    const windowEnd = Math.min(base + 2 * chunk, carrier.length);
    const full = windowEnd >= end;
    const revealed = stack.slice(0, 1);
    const hidden = stack.slice(1);
    const tailRoot = midgardDataStreamStackRoot(
      hidden,
      MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
    );
    const outcome = runMidgardDataStream({
      bytes: carrier.subarray(base, full ? end : windowEnd),
      full,
      rootStart: start - base,
      offset: cursor - base,
      frames: revealed,
      tailRoot,
      budget,
    });
    if (outcome.kind === "canonical") return true;
    if (outcome.kind === "nonCanonical") return false;
    if (outcome.nodes === 0) throw new Error("resumed read made no progress");
    // A resume rebuilds the stack the chain commits to.
    expect(midgardDataStreamStackRoot(outcome.frames, tailRoot)).toEqual(
      midgardDataStreamStackRoot(
        [...outcome.frames, ...hidden],
        MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
      ),
    );
    cursor = base + outcome.offset;
    stack = [...outcome.frames, ...hidden];
  }
};

// The same corpus the generated Aiken module drives through
// `cek_data_traverse_v1` (cek-data-canonical-corpus-v1-golden.test.ak).
describe("canonical Data corpus V1 through the TypeScript traverse twin", () => {
  it.each(vectors.map((vector) => [vector.name, vector] as const))(
    "%s",
    (_name, vector) => {
      const source = Buffer.from(vector.hex, "hex");
      for (const sourceStart of [0, 1]) {
        const outcome = driveMidgardCekDataTraverse({ sourceStart, source });
        expect(outcome.verdict).toBe(vector.verdict);
        // Every advancing step of the honest drive is a step the public
        // transition admits, and none of them is also Unreadable.
        for (const step of outcome.steps) {
          expect(advanceMidgardCekDataTraverse(step)).toEqual(step.next);
          expect(isUnreadableMidgardCekDataTraverse(step)).toBe(false);
        }
        if (outcome.verdict === "accepted") {
          const trace = buildMidgardCekDataTraverseTrace({
            sourceStart,
            source,
          });
          expect(outcome.summary).toEqual(trace.terminal.result);
        } else {
          if (vector.verdict !== "unreadable") throw new Error("unreachable");
          expect(outcome.stage).toBe(vector.stage);
          expect(isUnreadableMidgardCekDataTraverse(outcome.refusal)).toBe(
            true,
          );
          expect(advanceMidgardCekDataTraverse(outcome.refusal)).toBeNull();
          expect(() =>
            buildMidgardCekDataTraverseTrace({ sourceStart, source }),
          ).toThrow();
        }
      }
      expect(isCanonicalMidgardCekData(source)).toBe(
        vector.verdict === "accepted",
      );
      // The resumable reader the datum canonicity proof replays on chain.
      expect(isCanonicalMidgardDataStream(source)).toBe(
        vector.verdict === "accepted",
      );
      expect(streamResumed(source)).toBe(vector.verdict === "accepted");
      // Node admission keeps its own single-pass gate in P1 (DC ruling F3);
      // it must take exactly the corpus's accepted spans.
      expect(nodeAdmitsDatum(source)).toBe(vector.verdict === "accepted");
    },
  );

  it("refuses an Unreadable claim whose window is not the derived one", () => {
    const refused = vectors.find(
      (vector) => vector.verdict === "unreadable" && vector.stage === 0,
    )!;
    const outcome = driveMidgardCekDataTraverse({
      source: Buffer.from(refused.hex, "hex"),
    });
    if (outcome.verdict !== "unreadable") throw new Error("expected refusal");
    const { control, sourceBytes, action } = outcome.refusal;
    expect(
      isUnreadableMidgardCekDataTraverse({
        control,
        sourceBytes: Buffer.concat([sourceBytes!, Buffer.from([0])]),
        action,
      }),
    ).toBe(false);
    expect(
      isUnreadableMidgardCekDataTraverse({
        control,
        sourceBytes,
        action: { kind: "headScalar" },
      }),
    ).toBe(false);
  });

  // Node admission keeps its single-pass gate (well-formedness plus the exact
  // `serialiseData` re-encoding), which is far cheaper than the hashed
  // traverse; this locks its verdict to the traverse's, so a datum the node
  // admits is one the datum canonicity proof reads as canonical and back.
  it("node datum admission agrees with the traverse on the corpus and seeded mutations", () => {
    const seeds = vectors
      .map((vector) => Buffer.from(vector.hex, "hex"))
      .filter((source) => source.length < 400);
    let state = 0x5eed;
    const random = (bound: number): number => {
      state = (Math.imul(state, 1103515245) + 12345) & 0x7fffffff;
      return state % bound;
    };
    const cases: Buffer[] = [...seeds];
    for (let index = 0; index < 4_000; index += 1) {
      let source = Buffer.from(seeds[random(seeds.length)]!);
      for (let edit = 0, edits = 1 + random(3); edit < edits; edit += 1) {
        const at = random(source.length + 1);
        switch (random(4)) {
          case 0:
            if (source.length > 0)
              source[Math.min(at, source.length - 1)] = random(256);
            break;
          case 1:
            source = Buffer.concat([
              source.subarray(0, at),
              Buffer.from([random(256)]),
              source.subarray(at),
            ]);
            break;
          case 2:
            source = Buffer.concat([
              source.subarray(0, at),
              source.subarray(at + 1),
            ]);
            break;
          default:
            source = Buffer.concat([
              source.subarray(0, at),
              seeds[random(seeds.length)]!,
              source.subarray(at),
            ]);
        }
      }
      if (source.length > 0) cases.push(source);
    }
    const disagreements = cases
      .filter((source) => {
        const verdict = isCanonicalMidgardCekData(source);
        return (
          nodeAdmitsDatum(source) !== verdict ||
          isCanonicalMidgardDataStream(source) !== verdict ||
          streamResumed(source) !== verdict
        );
      })
      .map((source) => source.toString("hex"));
    expect(disagreements).toEqual([]);
    expect(cases.filter(nodeAdmitsDatum).length).toBeGreaterThan(200);
  }, 120_000);

  // The same constant is pinned against `stack_root_v1` in
  // canonical-plutus-data-stream-v1.test.ak, so both readers hash every
  // frame kind, a bignum map count and a multi-byte chunk count alike.
  it("hashes the open-container stack as the on-chain reader does", () => {
    const frames: MidgardDataStreamFrame[] = [
      { kind: "list", started: true },
      { kind: "list", started: false },
      { kind: "map", remaining: 0n },
      { kind: "map", remaining: (1n << 65n) - 2n },
      { kind: "map", remaining: 300n },
      { kind: "chunks", seen: 0, leading: true },
      { kind: "chunks", seen: 255, leading: false },
    ];
    expect(
      midgardDataStreamStackRoot(
        frames,
        MIDGARD_DATA_STREAM_EMPTY_STACK_ROOT,
      ).toString("hex"),
    ).toBe(STACK_ROOT_GOLDEN);
  });
});
