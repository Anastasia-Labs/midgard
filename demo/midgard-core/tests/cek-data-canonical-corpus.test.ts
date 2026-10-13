import { describe, expect, it } from "vitest";

import {
  advanceMidgardCekDataTraverse,
  buildMidgardCekDataTraverseTrace,
  decodeMidgardDatum,
  driveMidgardCekDataTraverse,
  isCanonicalMidgardCekData,
  isUnreadableMidgardCekDataTraverse,
} from "../src/index.js";
import { vectors } from "./fixtures/cek-data-canonical-corpus-v1.vectors.mjs";

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
    const admitted = (source: Buffer): boolean => {
      try {
        decodeMidgardDatum(source);
        return true;
      } catch {
        return false;
      }
    };
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
      .filter(
        (source) => admitted(source) !== isCanonicalMidgardCekData(source),
      )
      .map((source) => source.toString("hex"));
    expect(disagreements).toEqual([]);
    expect(cases.filter(admitted).length).toBeGreaterThan(200);
  }, 120_000);
});
