/**
 * The coalesced runner's bound on a local verifier fault: a run that ends
 * with a block held on `landed_block_verifier_unavailable` is retried on the
 * capped backoff, and once a fixed number of runs in a row ended on that
 * fault for the same block the runner stops and reports it; the node exits
 * non-zero on that report for its supervisor to restart it. The count lives
 * in the process, so a restarted node that meets the same fault on the same
 * block retries within the same bound and exits again. A run free of the
 * fault, or one held on another block, starts the count over.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { type DriverHold, isRetriedHold } from "../src/l1-events/driver.js";
import { combineHolds, replayOutcomeHold } from "../src/landed-blocks/holds.js";
import {
  coalescedRunner,
  LOCAL_FAULT_RUN_LIMIT,
} from "../src/services/l1-follower.coalesced-runner.js";

const BLOCK = "aa".repeat(28);
const OTHER_BLOCK = "bb".repeat(28);

/** The held run processing ends on when the verifier faults on `block`. */
const faulted = (block: string): readonly DriverHold[] => [
  combineHolds([
    replayOutcomeHold(
      "verifier_unavailable",
      `block ${block}: LedgerSignatureVerifierUnavailableError: Ed25519 verification is unavailable: injected`,
      block,
    ),
  ])!,
];

/** A run free of the fault, still held (a wait) so the runner retries it. */
const waiting = (block: string): readonly DriverHold[] => [
  combineHolds([replayOutcomeHold("missing", `block ${block}: no DA yet`)])!,
];

/** A runner (one node process) over `holdsOf(run)`. */
const runner = (holdsOf: (run: number) => readonly DriverHold[]) => {
  let runs = 0;
  const reports: (readonly DriverHold[])[] = [];
  const abort = new AbortController();
  const trigger = coalescedRunner(
    () => {
      runs += 1;
      return Promise.resolve(holdsOf(runs));
    },
    abort.signal,
    { onExhausted: (holds) => reports.push(holds) },
  );
  return { trigger, runs: () => runs, reports, abort };
};

/** Drives the backoff timers until `runs` reaches `count` or the runner stops. */
const advance = async (state: ReturnType<typeof runner>, count: number) => {
  for (let step = 0; step < 4 * count; step += 1) {
    if (state.runs() >= count) return;
    await vi.advanceTimersByTimeAsync(31_000);
  }
};

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("coalescedRunner's local-fault bound", () => {
  it("is a small fixed count", () => {
    expect(Number.isSafeInteger(LOCAL_FAULT_RUN_LIMIT)).toBe(true);
    expect(LOCAL_FAULT_RUN_LIMIT).toBeGreaterThan(1);
    expect(LOCAL_FAULT_RUN_LIMIT).toBeLessThanOrEqual(10);
  });

  it("retries a local-fault hold on the backoff, then stops and reports it", async () => {
    expect(isRetriedHold(faulted(BLOCK)[0]!)).toBe(true);
    const state = runner(() => faulted(BLOCK));
    state.trigger();
    await advance(state, 3 * LOCAL_FAULT_RUN_LIMIT);
    expect(state.runs()).toBe(LOCAL_FAULT_RUN_LIMIT);
    expect(state.reports).toHaveLength(1);
    expect(state.reports[0]!.map((hold) => hold.reason)).toEqual([
      "landed_block_verifier_unavailable",
    ]);
    expect(state.reports[0]![0]!.detail).toContain("injected");
    // Stopped: no further run on a trigger or a timer.
    state.trigger();
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(LOCAL_FAULT_RUN_LIMIT);
    expect(state.reports).toHaveLength(1);
  });

  it("reports again in a restarted process meeting the same fault on the same block", async () => {
    for (let process = 0; process < 3; process += 1) {
      const state = runner(() => faulted(BLOCK));
      state.trigger();
      await advance(state, 3 * LOCAL_FAULT_RUN_LIMIT);
      expect(state.runs()).toBe(LOCAL_FAULT_RUN_LIMIT);
      expect(state.reports).toHaveLength(1);
    }
  });

  it("starts the count over after a run free of the fault or held on another block", async () => {
    const runs = 4 * LOCAL_FAULT_RUN_LIMIT;
    const state = runner((run) =>
      run === runs
        ? []
        : run % (LOCAL_FAULT_RUN_LIMIT - 1) === 0
          ? run % 2 === 0
            ? waiting(BLOCK)
            : faulted(OTHER_BLOCK)
          : faulted(BLOCK),
    );
    state.trigger();
    await advance(state, runs);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(runs);
    expect(state.reports).toEqual([]);
  });

  it("stops retrying once the fault clears", async () => {
    const state = runner((run) => (run <= 2 ? faulted(BLOCK) : []));
    state.trigger();
    await advance(state, 3);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(3);
    expect(state.reports).toEqual([]);
  });
});
