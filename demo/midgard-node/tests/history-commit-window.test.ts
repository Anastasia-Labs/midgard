import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  HistoryProducer,
  type HistoryProducerPermit,
} from "../src/services/event-history-producer.js";
import {
  historyCommitTimingBudget,
  historyEligibilityHorizon,
} from "../src/services/history-commit-window.js";
import { refreshCommitUserEventSourcesThroughBlockEnd } from "../src/workers/commit-block-header/submission.js";

const permit: HistoryProducerPermit = {
  token: {
    deploymentIdentity: "11".repeat(32),
    ownerToken: "isolated model",
    generation: "1",
  },
  coverage: {
    bindingDigest: "22".repeat(32),
    checkpointRevision: "3",
    point: { id: "33".repeat(32), slot: 100 },
    snapshotDigest: "44".repeat(32),
    includedThroughMs: 1_000_000,
  },
};

describe("authenticated commit window", () => {
  it("keeps the observed point distinct from the conservative eligibility horizon", () => {
    const before = structuredClone(permit.coverage);
    const horizon = historyEligibilityHorizon(permit.coverage);
    // A future admission cannot have an inclusive upper bound before its
    // actual future slot; its enforced60s delay lies beyond this horizon.
    expect(horizon).toBeLessThan(permit.coverage.includedThroughMs + 60_000);
    expect(permit.coverage).toEqual(before);
    expect(() =>
      historyEligibilityHorizon({
        ...permit.coverage,
        includedThroughMs: Number.MAX_SAFE_INTEGER,
      }),
    ).toThrow();
  });

  it("uses owner coverage at final refresh without invoking deposit or withdrawal polling", async () => {
    const calls: string[] = [];
    const record = (kind: string) => (upperBound: Date) =>
      Effect.sync(() => {
        calls.push(kind);
        return upperBound;
      });
    const refresh = (end: number) =>
      refreshCommitUserEventSourcesThroughBlockEnd(end, {
        deposit: record("deposit"),
        withdrawal: record("withdrawal"),
        txOrder: record("tx-order"),
      }).pipe(Effect.provideService(HistoryProducer, permit));
    const horizon = historyEligibilityHorizon(permit.coverage);
    await Effect.runPromise(refresh(horizon));
    expect(calls).toEqual(["tx-order"]);
    calls.length = 0;
    await expect(Effect.runPromise(refresh(horizon + 1))).rejects.toThrow(
      /exceeds authenticated history coverage/,
    );
    expect(calls).toEqual([]);
  });

  it("refuses an exhausted or invalid short-window attempt instead of moving its header end", () => {
    const end = historyEligibilityHorizon(permit.coverage) + 1;
    const adequate = historyCommitTimingBudget({
      checkpoint: "pre_submit",
      resolvedEndTimeMs: end,
      nowMs: end - 10_000,
    });
    const exhausted = historyCommitTimingBudget({
      checkpoint: "pre_submit",
      resolvedEndTimeMs: end,
      nowMs: end - 9_999,
    });
    expect(adequate.satisfied).toBe(true);
    expect(exhausted.satisfied).toBe(false);
    expect(exhausted.resolvedEndTimeMs).toBe(adequate.resolvedEndTimeMs);
    expect(
      historyCommitTimingBudget({
        checkpoint: "pre_submit",
        resolvedEndTimeMs: end,
        nowMs: Number.NaN,
      }).satisfied,
    ).toBe(false);
  });
});
