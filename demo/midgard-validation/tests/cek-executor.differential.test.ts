import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  MIDGARD_CEK_DIFFERENTIAL_CORPUS,
  type MidgardCekDifferentialOutcome,
  runMidgardCekDifferentialCase,
} from "./helpers/cek-differential-corpus.js";

/**
 * The structural executor builds each step's result nodes from its argument
 * roots. The reference outcomes in the fixture were recorded from the
 * executor under the current cost and halting rules (startup cost, word
 * memory sizing, the Unit result rule). For every program in the corpus the
 * executor must still produce, in both modes, the same step count, stop
 * reason and terminal state, and in proof mode the same pre-state,
 * post-state, step encoding and witness at every step (folded into the
 * digest). A run that throws must throw in both.
 */

type RecordedCase = {
  readonly proof: MidgardCekDifferentialOutcome;
  readonly verdict: MidgardCekDifferentialOutcome;
};

const recorded = JSON.parse(
  readFileSync(
    new URL("./fixtures/cek-executor-differential-v1.json", import.meta.url),
    "utf8",
  ),
) as Readonly<Record<string, RecordedCase>>;

describe("structural executor differential", () => {
  it("covers exactly the recorded corpus", () => {
    expect(MIDGARD_CEK_DIFFERENTIAL_CORPUS.map((entry) => entry.label)).toEqual(
      Object.keys(recorded),
    );
  });

  for (const entry of MIDGARD_CEK_DIFFERENTIAL_CORPUS) {
    it(entry.label, { timeout: 120_000 }, () => {
      const prepared = entry.prepare();
      expect({
        proof: runMidgardCekDifferentialCase(prepared, "proof"),
        verdict: runMidgardCekDifferentialCase(prepared, "verdict"),
      }).toEqual(recorded[entry.label]);
    });
  }
});
