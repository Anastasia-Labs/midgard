import { expect, it, vi } from "vitest";

import { reconcileLegacyFundingAbandonmentRecords } from "../src/workflow/funding-reservation-permit.reopen-legacy-abandonment.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import { createSupersededAttemptReadSchedule } from "../src/workflow/superseded-attempt-read-schedule.js";

it("reads each attempt at once, then backs off exponentially to a cap, at most perPass per pass", () => {
  const schedule = createSupersededAttemptReadSchedule({
    baseDelayMs: 10,
    maxDelayMs: 40,
    perPass: 2,
  });
  expect(schedule.due(["a", "b", "c"], 0)).toEqual(["a", "b"]);
  schedule.unresolved("a", 0);
  schedule.unresolved("b", 0);
  // c was never read, so it is due first; a and b wait.
  expect(schedule.due(["a", "b", "c"], 5)).toEqual(["c"]);
  schedule.unresolved("c", 5);
  expect(schedule.due(["a", "b", "c"], 10)).toEqual(["a", "b"]);
  schedule.unresolved("a", 10);
  // a now waits 20, then 40, and never longer.
  expect(schedule.due(["a"], 29)).toEqual([]);
  expect(schedule.due(["a"], 30)).toEqual(["a"]);
  schedule.unresolved("a", 30);
  expect(schedule.due(["a"], 69)).toEqual([]);
  schedule.unresolved("a", 70);
  expect(schedule.due(["a"], 109)).toEqual([]);
  expect(schedule.due(["a"], 110)).toEqual(["a"]);
  // Retirement or adoption forgets it: a later superseding reads at once.
  schedule.forget("a");
  expect(schedule.due(["a"], 70)).toEqual(["a"]);
  expect(() => createSupersededAttemptReadSchedule({ perPass: 0 })).toThrow(
    "out of range",
  );
});

it("bounds the L1 reads of one pass however many superseded attempts accumulate", async () => {
  const hashes = Array.from({ length: 12 }, (_, index) =>
    index.toString(16).padStart(2, "0").repeat(32),
  );
  const attempts = hashes.map(
    (transactionHash) =>
      ({
        transition: { transactionHash },
        handoff: { reconciliation: {} },
      }) as unknown as Parameters<
        typeof reconcileLegacyFundingAbandonmentRecords
      >[0]["savedAttempts"][number],
  );
  const entries = hashes.map(
    (txHash) =>
      ({
        event: { kind: "submission_intent", txHash },
      }) as unknown as FraudProofWorkflowJournalEntry,
  );
  const reconcile = vi.fn(async (_saved: (typeof attempts)[number]) => ({
    kind: "not_found" as const,
  }));
  const schedule = createSupersededAttemptReadSchedule({
    baseDelayMs: 1_000,
    perPass: 3,
  });
  const pass = async (nowMs: number) =>
    await reconcileLegacyFundingAbandonmentRecords({
      savedAttempts: attempts,
      entries,
      append: async () => undefined,
      retire: async () => undefined,
      reconcile,
      reads: { schedule, nowMs },
    });
  // Four passes at once read every attempt exactly once, three at a time.
  for (let index = 0; index < 4; index += 1) {
    await pass(0);
    expect(reconcile).toHaveBeenCalledTimes(3 * (index + 1));
  }
  expect(
    new Set(
      reconcile.mock.calls.map(([saved]) => saved.transition.transactionHash),
    ),
  ).toEqual(new Set(hashes));
  // Everything has backed off: further passes read nothing until due.
  await pass(999);
  expect(reconcile).toHaveBeenCalledTimes(12);
  await pass(1_000);
  expect(reconcile).toHaveBeenCalledTimes(15);
});
