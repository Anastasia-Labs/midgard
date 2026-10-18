/**
 * The coalesced runner's bound on a local fault: a run that ends with a
 * block held on `landed_block_verifier_unavailable`, `landed_block_local_fault`,
 * `landed_block_replay_incomplete` or `landed_block_forced_order_pending` is
 * retried on the capped backoff, and
 * once a fixed number of runs ended on a local fault, of any of these kinds,
 * with no progress past the faulted blocks the runner stops and reports it;
 * the node exits non-zero on that report for its supervisor to restart it.
 * The count lives in the process, so a restarted node that meets the same
 * fault on the same block retries within the same bound and exits again.
 *
 * Only progress past a faulted block (processing commits it) starts the
 * count over. A run free of the fault (a wait), a transient failure, or a
 * fault on another block the follower reached without that progress (a
 * sibling, or a rollback onto one) keeps it. A run that ends on a local
 * fault does not start a transient failure's budget over.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  type DriverHold,
  isRetriedHold,
  localFaultSubject,
  transientFailure,
} from "../src/l1-events/driver.js";
import {
  combineHolds,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
} from "../src/landed-blocks/holds.js";
import { replayOutcomeHold } from "../src/landed-blocks/replay-outcome-hold.js";
import {
  coalescedRunner,
  type CoalescedRunnerBound,
  LOCAL_FAULT_RUN_LIMIT,
  localFaultProgress,
} from "../src/services/l1-follower.coalesced-runner.js";

const BLOCK = "aa".repeat(28);
/** A sibling of `BLOCK` (another block at its height), never committed. */
const SIBLING = "bb".repeat(28);
/** A block after `BLOCK`, reached once processing committed `BLOCK`. */
const NEXT_BLOCK = "cc".repeat(28);
const LIMIT = LOCAL_FAULT_RUN_LIMIT;

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

/** The held run processing ends on when replaying `block` fails locally as
 * `kind`. */
const failedLocally =
  (kind: "local_fault" | "incomplete" | "forced_order_pending") =>
  (block: string): readonly DriverHold[] => [
    combineHolds([
      replayOutcomeHold(kind, `block ${block}: ${kind}: injected`, block),
    ])!,
  ];
const computeFault = failedLocally("local_fault");
const incomplete = failedLocally("incomplete");
const forcedOrderPending = failedLocally("forced_order_pending");

/** A run free of the fault, still held (a wait) so the runner retries it. */
const waiting = (block: string): readonly DriverHold[] => [
  combineHolds([
    replayOutcomeHold("missing", `block ${block}: no DA yet`, block),
  ])!,
];

/** A run the database did not answer: a transient failure. */
const databaseDown = (): readonly DriverHold[] => [
  transientFailure({
    reason: "landed_block_replay_failed",
    detail: "connect ECONNREFUSED",
  }),
];

/** Processing's report of the blocks it committed, as the follower wires it. */
type Committed = (headerHash: string) => void;

/**
 * A runner (one node process) over `holdsOf(run, committed)`: a run reports
 * the blocks it committed through `committed`. `bound` overrides the
 * runner's defaults.
 */
const runner = (
  holdsOf: (run: number, committed: Committed) => readonly DriverHold[],
  bound: Partial<CoalescedRunnerBound> = {},
) => {
  let runs = 0;
  const reports: (readonly DriverHold[])[] = [];
  const abort = new AbortController();
  const progress = localFaultProgress();
  const trigger = coalescedRunner(
    () => {
      runs += 1;
      return Promise.resolve(holdsOf(runs, progress.committed));
    },
    abort.signal,
    { progress, ...bound, onExhausted: (holds) => reports.push(holds) },
  );
  return { trigger, runs: () => runs, reports, abort };
};

/** The blocks a report's local faults are on. */
const reportedBlocks = (report: readonly DriverHold[]) =>
  report.map(localFaultSubject);

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

  it("keeps the count over runs free of the fault that made no progress past the block", async () => {
    // Seven faulted runs, then a wait on the same block, repeating.
    const state = runner((run) =>
      run % LIMIT === 0 ? waiting(BLOCK) : faulted(BLOCK),
    );
    state.trigger();
    await advance(state, 4 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(LIMIT + 1);
    expect(state.reports).toHaveLength(1);
    expect(reportedBlocks(state.reports[0]!)).toEqual([BLOCK]);
  });

  it("keeps the count over transient failures between faulted runs", async () => {
    const state = runner(
      (run) => (run % 2 === 0 ? databaseDown() : faulted(BLOCK)),
      { budgetMs: Number.POSITIVE_INFINITY },
    );
    state.trigger();
    await advance(state, 4 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(2 * LIMIT - 1);
    expect(state.reports).toHaveLength(1);
    expect(reportedBlocks(state.reports[0]!)).toEqual([BLOCK]);
  });

  it("does not start a transient failure's budget over on a faulted run", async () => {
    let clock = 0;
    const state = runner(
      (run) => {
        clock += 60_000;
        return run % 2 === 0 ? faulted(BLOCK) : databaseDown();
      },
      {
        budgetMs: 5 * 60_000,
        now: () => clock,
        localFaultRuns: Number.POSITIVE_INFINITY,
      },
    );
    state.trigger();
    await advance(state, 4 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    // Transient failures at minutes 1, 3, 5, 7: the budget runs out at 7.
    expect(state.runs()).toBe(7);
    expect(state.reports).toHaveLength(1);
    expect(state.reports[0]!.map((hold) => hold.reason)).toEqual([
      "landed_block_replay_failed",
    ]);
  });

  it("keeps the count over faults alternating between a block and its sibling", async () => {
    const state = runner((run) =>
      run % 2 === 0 ? faulted(SIBLING) : faulted(BLOCK),
    );
    state.trigger();
    await advance(state, 4 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(LIMIT);
    expect(state.reports).toHaveLength(1);
  });

  it("keeps the count over a fault on a block never committed, among waits and transient failures", async () => {
    const cycle = [faulted(BLOCK), waiting(BLOCK), faulted(SIBLING)];
    const state = runner(
      (run, committed) => {
        // Committing another block (an ancestor, again) is no progress past
        // the faulted ones.
        committed("dd".repeat(28));
        return run % 4 === 0 ? databaseDown() : cycle[run % cycle.length]!;
      },
      { budgetMs: Number.POSITIVE_INFINITY },
    );
    state.trigger();
    await advance(state, 8 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    // Faults at runs 2, 3, 5, 6, 9, 11, 14 and 15.
    expect(state.runs()).toBe(15);
    expect(state.reports).toHaveLength(1);
  });

  it("starts the count over once processing committed the faulted block", async () => {
    // Seven faulted runs on a block, then a run that commits it; again for
    // each next block; the last run is free of holds.
    const runs = 4 * LIMIT;
    const blockOf = (run: number) =>
      (Math.floor((run - 1) / LIMIT) + 1)
        .toString(16)
        .padStart(2, "0")
        .repeat(28);
    const state = runner((run, committed) => {
      if (run % LIMIT !== 0) return faulted(blockOf(run));
      committed(blockOf(run));
      return run === runs ? [] : waiting(blockOf(run + 1));
    });
    state.trigger();
    await advance(state, runs);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(runs);
    expect(state.reports).toEqual([]);
  });

  it("counts from one a fault on a block reached after committing the faulted one", async () => {
    // Seven faulted runs on BLOCK; the eighth commits it and faults on the
    // block after it, which then faults until the bound.
    const state = runner((run, committed) => {
      if (run < LIMIT) return faulted(BLOCK);
      if (run === LIMIT) committed(BLOCK);
      return faulted(NEXT_BLOCK);
    });
    state.trigger();
    await advance(state, 4 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(LIMIT - 1 + LIMIT);
    expect(state.reports).toHaveLength(1);
    expect(reportedBlocks(state.reports[0]!)).toEqual([NEXT_BLOCK]);
  });

  it.each([
    ["local_fault", computeFault, LANDED_BLOCK_LOCAL_FAULT],
    ["incomplete", incomplete, LANDED_BLOCK_REPLAY_INCOMPLETE],
    [
      "forced_order_pending",
      forcedOrderPending,
      LANDED_BLOCK_FORCED_ORDER_PENDING,
    ],
  ] as const)(
    "retries the %s hold on the backoff within the same bound, then stops and reports it",
    async (_kind, fail, reason) => {
      expect(isRetriedHold(fail(BLOCK)[0]!)).toBe(true);
      const state = runner(() => fail(BLOCK));
      state.trigger();
      await advance(state, 3 * LIMIT);
      await vi.advanceTimersByTimeAsync(10 * 60_000);
      expect(state.runs()).toBe(LIMIT);
      expect(state.reports).toHaveLength(1);
      expect(state.reports[0]!.map((hold) => hold.reason)).toEqual([reason]);
      expect(reportedBlocks(state.reports[0]!)).toEqual([BLOCK]);
    },
  );

  it("keeps one count over local faults of different kinds on the same block", async () => {
    const state = runner((run) =>
      run % 2 === 0 ? computeFault(BLOCK) : incomplete(BLOCK),
    );
    state.trigger();
    await advance(state, 4 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(LIMIT);
    expect(state.reports).toHaveLength(1);
    expect(reportedBlocks(state.reports[0]!)).toEqual([BLOCK]);
  });

  it("counts from one an incomplete replay of a block reached after committing the one held", async () => {
    // Seven incomplete replays of BLOCK; the eighth run commits it and its
    // replay of the block after it is incomplete, until the bound.
    const state = runner((run, committed) => {
      if (run < LIMIT) return incomplete(BLOCK);
      if (run === LIMIT) committed(BLOCK);
      return incomplete(NEXT_BLOCK);
    });
    state.trigger();
    await advance(state, 4 * LIMIT);
    await vi.advanceTimersByTimeAsync(10 * 60_000);
    expect(state.runs()).toBe(LIMIT - 1 + LIMIT);
    expect(state.reports).toHaveLength(1);
    expect(reportedBlocks(state.reports[0]!)).toEqual([NEXT_BLOCK]);
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
