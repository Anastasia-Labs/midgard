import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { commitDaSourceContributions } from "../src/workers/commit-block-header.plan-additive-da.js";
import { commitDaPayloadForSizing } from "../src/workers/commit-block-header/submission.assert-pre-submit-da-payload-size.js";
import { selectAdditiveCommitFrame } from "../src/workers/utils/commit-block-planner.additive-frame.js";
import { additiveDaFixedBytes } from "../src/workers/utils/commit-da-material.encoding.js";
import {
  blockContentFor,
  header,
  mkCandidate,
} from "./helpers/commit-da-frame-fixtures.js";

it("the additive full source budget covers real canonical DA encoding", async () => {
  const candidates = [mkCandidate(1), mkCandidate(2), mkCandidate(3)];
  const budget = commitDaSourceContributions({
    deposits: [],
    withdrawals: [],
    forced: [],
    candidates,
    profile: MIDGARD_CONSENSUS_PROFILE,
  });
  const content = blockContentFor(candidates);
  const actual = await Effect.runPromise(
    commitDaPayloadForSizing({
      ...content,
      headerHash: "44".repeat(28),
      header: {
        ...header,
        l2TransactionCount: 3n,
        totalEventCount: 3n,
        transitionStepCount: 3n,
        validationTraceCount: 3n,
      },
    }),
  );
  expect(
    budget.fixedBytes + budget.ordinaryBytes.reduce((a, b) => a + b, 0n),
  ).toBeGreaterThanOrEqual(BigInt(SDK.daPayloadEncodedSize(actual)));
});

const counts = { deposits: 3, withdrawals: 0, forcedTransactions: 0 };
const initialWindow = {
  mode: "normal",
  endTimeMs: 100,
  counts,
  includedInstantCount: 2,
} as const;
describe("bounded additive complete-prefix selection", () => {
  const events = [
    { kind: "deposits", inclusionTimeMs: 10, bytes: 4n },
    { kind: "deposits", inclusionTimeMs: 10, bytes: 4n },
    { kind: "deposits", inclusionTimeMs: 20, bytes: 4n },
  ] as const;
  it("never cuts an instant and suppresses ordinary transactions when frame pressure lowers coverage", () => {
    expect(
      selectAdditiveCommitFrame({
        initialWindow,
        events,
        ordinaryBytes: [1n],
        fixedBytes: 2n,
        maxInnerBytes: 10,
      }),
    ).toEqual({
      kind: "selected",
      window: {
        mode: "catch_up",
        endTimeMs: 10,
        counts: { ...counts, deposits: 2 },
        includedInstantCount: 1,
      },
      candidateCount: 0,
      upperBoundBytes: 10n,
    });
  });
  it("selects the longest conservative ordinary prefix after all mandatory events", () => {
    expect(
      selectAdditiveCommitFrame({
        initialWindow,
        events,
        ordinaryBytes: [3n, 3n, 3n],
        fixedBytes: 2n,
        maxInnerBytes: 20,
      }),
    ).toMatchObject({
      kind: "selected",
      window: initialWindow,
      candidateCount: 2,
      upperBoundBytes: 20n,
    });
  });
  it("rescues exactly the first overestimated complete instant without cutting or evicting events", () => {
    expect(
      selectAdditiveCommitFrame({
        initialWindow,
        events,
        ordinaryBytes: [1n],
        fixedBytes: 2n,
        maxInnerBytes: 9,
      }),
    ).toMatchObject({
      kind: "selected",
      rescue: "whole_instant",
      candidateCount: 0,
      window: {
        endTimeMs: 10,
        includedInstantCount: 1,
        counts: { ...counts, deposits: 2 },
      },
    });
  });
  it("rescues only the first ordinary transaction when its conservative bound excludes every candidate", () => {
    expect(
      selectAdditiveCommitFrame({
        initialWindow: {
          ...initialWindow,
          counts: { ...counts, deposits: 0 },
          includedInstantCount: 0,
        },
        events: [],
        ordinaryBytes: [100n, 1n],
        fixedBytes: 2n,
        maxInnerBytes: 9,
      }),
    ).toMatchObject({
      kind: "selected",
      rescue: "ordinary",
      candidateCount: 1,
      upperBoundBytes: 102n,
    });
  });
  it("keeps arithmetic bounded for enormous program budgets", () => {
    expect(
      selectAdditiveCommitFrame({
        initialWindow: {
          ...initialWindow,
          counts: { ...counts, deposits: 1 },
          includedInstantCount: 1,
        },
        events: [{ kind: "deposits", inclusionTimeMs: 10, bytes: 2n ** 80n }],
        ordinaryBytes: [],
        fixedBytes: additiveDaFixedBytes(),
        maxInnerBytes: 64 * 1024 * 1024,
      }),
    ).toMatchObject({
      kind: "selected",
      rescue: "whole_instant",
      candidateCount: 0,
    });
  });
});
