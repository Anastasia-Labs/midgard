import { compareMidgardCekStagedOutRefs } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import type { MidgardCekSerialiseLoop } from "../src/cek-serialise-loop.hops.js";
import { bytes, list } from "./fixtures/cek-serialise-loop-v1.vectors.mjs";
import {
  advance,
  drive,
  hexLoop,
  refOf,
} from "./helpers/cek-serialise-loop.js";

/**
 * The serialise loop over R whose runs repeat: a driver stages runs alike
 * once, on one output, so several runs a hop reads name the same output. A
 * transaction reads an output once, and the loop refuses a hop that names
 * one twice, so the honest hops name each output once and every run it
 * reads by that output.
 */

/** `loop` with each run staged on the output of the first run alike. */
const sharedOutputs = (
  loop: MidgardCekSerialiseLoop,
): MidgardCekSerialiseLoop => {
  const keys = loop.runs.map((run) => Buffer.concat(run).toString("hex"));
  return {
    ...loop,
    refs: keys.map((key) => refOf(keys.indexOf(key))),
  };
};

describe("the serialise loop over runs staged on shared outputs", () => {
  // 800 copies of one 65-byte leaf: R repeats every 65 bytes, and a run of
  // three 4,095-byte chunks is 189 leaves long, so every whole run after the
  // first is alike.
  const loop = sharedOutputs(hexLoop(list(Array(800).fill(bytes(63)))));

  it("has runs alike, staged once", () => {
    const outputs = new Set(loop.refs.map((ref) => ref.outputIndex));
    expect(outputs.size).toBeLessThan(loop.refs.length);
  });

  it.each([
    ["budget-sized", undefined],
    ["400-step", 400],
  ] as const)(
    "plans %s hops that each name an output once and that the loop accepts to its end",
    (_, limit) => {
      const hops = drive(loop, limit);
      for (const planned of hops) {
        expect(advance(loop, planned)).toEqual(
          planned.next === null
            ? { kind: "done" }
            : { kind: "next", state: planned.next },
        );
        const { refs, order } = planned.hop;
        for (let index = 1; index < refs.length; index += 1)
          expect(
            compareMidgardCekStagedOutRefs(refs[index - 1]!, refs[index]!),
          ).toBeLessThan(0);
        expect(order.every((at) => at >= 0 && at < refs.length)).toBe(true);
      }
      // Some hop reads two runs alike, by one output.
      expect(hops.some(({ hop }) => hop.order.length > hop.refs.length)).toBe(
        true,
      );
    },
  );
});
