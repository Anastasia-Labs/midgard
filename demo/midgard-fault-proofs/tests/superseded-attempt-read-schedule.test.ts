import { expect, it, vi } from "vitest";

import type { TimeoutCorrectionJournal } from "../src/remove-unattested-block.parse-timeout-correction-journal.js";
import type { TimeoutCorrectionAttemptObservation } from "../src/remove-unattested-block.reconcile-last-timeout-correction-step.js";
import { adoptLandedTimeoutCorrectionAttempts } from "../src/remove-unattested-block.supersede-timeout-correction-attempts.js";
import type { SignedWorkflowTransaction } from "../src/workflow/signed-transaction-reconciliation.js";
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

it("bounds the L1 reads of one pass however many abandoned timeout-correction attempts accumulate", async () => {
  const hashes = Array.from({ length: 12 }, (_, index) =>
    index.toString(16).padStart(2, "0").repeat(32),
  );
  const journal = {
    version: 1,
    targetHeaderHash: "ab".repeat(28),
    targetDeadlineMs: "0",
    steps: hashes.map((txHash) => ({
      txHash,
      signedCbor: "84a0",
      status: "abandoned",
    })),
    completed: false,
  } as unknown as TimeoutCorrectionJournal;
  const observe = vi.fn(
    async (_signed: SignedWorkflowTransaction) =>
      ({ status: "pending" }) as TimeoutCorrectionAttemptObservation,
  );
  const schedule = createSupersededAttemptReadSchedule({
    baseDelayMs: 1_000,
    perPass: 3,
  });
  const pass = async (nowMs: number) =>
    await adoptLandedTimeoutCorrectionAttempts({
      journal,
      queue: [],
      observe,
      nowMs,
      schedule,
    });
  // Four passes at once read every attempt exactly once, three at a time.
  for (let index = 0; index < 4; index += 1) {
    expect(await pass(0)).toBe(journal);
    expect(observe).toHaveBeenCalledTimes(3 * (index + 1));
  }
  expect(
    new Set(observe.mock.calls.map(([signed]) => signed.transactionHash)),
  ).toEqual(new Set(hashes));
  // Everything has backed off: further passes read nothing until due.
  await pass(999);
  expect(observe).toHaveBeenCalledTimes(12);
  await pass(1_000);
  expect(observe).toHaveBeenCalledTimes(15);
});
