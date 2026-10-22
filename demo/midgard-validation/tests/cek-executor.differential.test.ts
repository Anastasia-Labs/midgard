import { readFileSync, writeFileSync } from "node:fs";

import { beforeAll, describe, expect, it } from "vitest";

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
 *
 * `MIDGARD_WRITE_CEK_EXECUTOR_DIFFERENTIAL=1` rewrites the whole fixture from
 * the current executor before the cases are compared with it.
 */

type RecordedCase = {
  readonly proof: MidgardCekDifferentialOutcome;
  readonly verdict: MidgardCekDifferentialOutcome;
};

const FIXTURE_URL = new URL(
  "./fixtures/cek-executor-differential-v1.json",
  import.meta.url,
);

const outcomeOf = (
  entry: (typeof MIDGARD_CEK_DIFFERENTIAL_CORPUS)[number],
): RecordedCase => {
  const prepared = entry.prepare();
  return {
    proof: runMidgardCekDifferentialCase(prepared, "proof"),
    verdict: runMidgardCekDifferentialCase(prepared, "verdict"),
  };
};

let recorded: Readonly<Record<string, RecordedCase>> = {};

describe("structural executor differential", () => {
  beforeAll(() => {
    if (process.env.MIDGARD_WRITE_CEK_EXECUTOR_DIFFERENTIAL === "1") {
      const outcomes = Object.fromEntries(
        MIDGARD_CEK_DIFFERENTIAL_CORPUS.map((entry) => [
          entry.label,
          outcomeOf(entry),
        ]),
      );
      writeFileSync(
        FIXTURE_URL,
        `${JSON.stringify(outcomes, null, 2)}\n`,
        "utf8",
      );
    }
    recorded = JSON.parse(readFileSync(FIXTURE_URL, "utf8")) as Readonly<
      Record<string, RecordedCase>
    >;
  }, 1_200_000);

  it("covers exactly the recorded corpus", () => {
    expect(MIDGARD_CEK_DIFFERENTIAL_CORPUS.map((entry) => entry.label)).toEqual(
      Object.keys(recorded),
    );
  });

  for (const entry of MIDGARD_CEK_DIFFERENTIAL_CORPUS) {
    it(entry.label, { timeout: 120_000 }, () => {
      expect(outcomeOf(entry)).toEqual(recorded[entry.label]);
    });
  }
});
