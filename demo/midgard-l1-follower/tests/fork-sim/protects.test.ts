import { describe, expect, it } from "vitest";

import { outRefKey } from "../../src/codec.js";
import { decodeBlock } from "../../src/decode/block.js";
import type { FollowerProjection } from "../../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  type SimOutput,
  simTxHash,
} from "../../src/testing/index.js";
import { SIM_K } from "../support/fork-sim.js";

/** A datum only this test's traffic puts on its outputs. */
const MARKER = Buffer.from("d87a9f0affff", "hex");

const isMarked = (output: SimOutput): boolean =>
  output.datum !== undefined && output.datum.equals(MARKER);

/**
 * A projection whose traffic creates one marked output per block at the
 * tracked address and never spends one, so any spend of a marked output
 * comes from the filler.
 */
const marking = (
  protects: boolean,
  created: Set<string>,
): FollowerProjection => ({
  name: "marking",
  traffic: ({ chain }) => {
    const tx = {
      inputs: [chain.outsideInput()],
      outputs: [
        {
          address: chain.universe.trackedAddress,
          lovelace: 2_000_000n,
          datum: MARKER,
        },
      ],
      nonce: chain.nonce(),
    };
    created.add(outRefKey({ txHash: simTxHash(tx), index: 0 }));
    return [tx];
  },
  ...(protects ? { protects: isMarked } : {}),
});

/** How many inputs or collaterals, over every block served, spend a marked output. */
const markedSpends = (protects: boolean): number => {
  let spends = 0;
  for (const { scenario } of forkCorpus(SIM_K)) {
    const created = new Set<string>();
    const { steps } = buildForkSteps(scenario, [marking(protects, created)]);
    for (const { event } of steps) {
      if (event.kind !== "roll_forward") continue;
      for (const tx of decodeBlock(event.block).txs)
        for (const outRef of [...tx.inputs, ...tx.collaterals])
          if (created.has(outRefKey(outRef))) spends += 1;
    }
  }
  return spends;
};

describe("a projection's protected outputs in the simulator", () => {
  it("the filler spends unprotected projection outputs (the check can fail)", () => {
    expect(markedSpends(false)).toBeGreaterThan(0);
  });

  it("the filler never spends or consumes as collateral a protected output", () => {
    expect(markedSpends(true)).toBe(0);
  });
});
