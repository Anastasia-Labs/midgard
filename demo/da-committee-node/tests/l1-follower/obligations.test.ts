import { describe, expect, it } from "vitest";

import { obligations } from "../../src/l1/follower/obligations.js";
import { SIM_DEPTHS } from "./queue-sim-traffic.js";

const headerHash = "ab".repeat(28);
const endTimeMs = 1_600_000_009_999n;

/** The obligation of one signed header absent from the current chain. */
const absent = (finalBlockTimeMs: number | null, pruned = false) =>
  obligations({
    signed: [{ headerHash, blockTimeMs: endTimeMs }],
    presence: [],
    tipHeight: 100,
    finalBlockTimeMs,
    pruned,
    parameters: SIM_DEPTHS,
  })[0];

describe("the obligation of a signed header absent from the current chain", () => {
  it("is pending while the latest final block is not later than its end time", () => {
    expect(absent(null)).toMatchObject({
      state: "pending",
      retainCommitRecord: true,
      decisionDeletable: false,
    });
    // Exactly at the end time the rule still keeps it: it waits for a final
    // block strictly later than the end time (conservative by one block).
    expect(absent(Number(endTimeMs))).toMatchObject({
      state: "pending",
      retainCommitRecord: true,
      decisionDeletable: false,
    });
  });

  it("cannot land once a final block is later than its end time", () => {
    expect(absent(Number(endTimeMs) + 1)).toMatchObject({
      state: "cannot_land",
      retainCommitRecord: false,
      decisionDeletable: true,
    });
  });

  it("is beyond retention, never cannot_land, over a pruned store", () => {
    expect(absent(Number(endTimeMs) + 1, true)).toMatchObject({
      state: "beyond_retention",
      retainCommitRecord: false,
      decisionDeletable: true,
    });
    expect(absent(Number(endTimeMs), true)).toMatchObject({
      state: "pending",
    });
  });
});

it("retains a disappeared catch-up decision until its commit time has passed", () => {
  const signed = [{ headerHash, endTimeMs: 1_000n, blockTimeMs: 20_000n }];
  const at = (finalBlockTimeMs: number) =>
    obligations({
      signed,
      presence: [],
      tipHeight: 100,
      finalBlockTimeMs,
      pruned: false,
      parameters: SIM_DEPTHS,
    })[0];
  expect(at(10_000)).toMatchObject({
    state: "pending",
    decisionDeletable: false,
  });
  expect(at(20_000)).toMatchObject({
    state: "pending",
    decisionDeletable: false,
  });
  expect(at(20_001)).toMatchObject({
    state: "cannot_land",
    decisionDeletable: true,
  });
});

it("retains an old signed decision whose commit clock is unknown", () => {
  expect(
    obligations({
      signed: [{ headerHash, blockTimeMs: null }],
      presence: [],
      tipHeight: 100,
      finalBlockTimeMs: 50_000,
      pruned: false,
      parameters: SIM_DEPTHS,
    })[0],
  ).toMatchObject({ state: "pending", decisionDeletable: false });
});
