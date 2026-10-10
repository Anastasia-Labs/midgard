/**
 * The coalesced runner's transient bound (`coalescedRunner`, R2): a driver
 * run that ends on a database transient (`transientFailure`) is retried on
 * the runner's backoff for a bounded time, and once runs have ended on one
 * for the budget in a row the runner stops and reports the failures; the
 * node exits non-zero on that report. A wait on the L1 node (its transport,
 * sidecar or the follower provider's node path) has no bound, and a run free
 * of transient failures starts the budget over.
 */
import { TransportUnavailableError } from "@al-ft/l1-node-transport";
import { L1ProviderTransientError } from "@al-ft/midgard-l1-follower/provider";
import { describe, expect, it, vi } from "vitest";

import {
  type DriverHold,
  isRetriedHold,
  isTransientFailureHold,
} from "../src/l1-events/driver.js";
import { coalescedRunner } from "../src/services/l1-follower.coalesced-runner.js";
import {
  failureHold,
  isL1NodeOutage,
} from "../src/services/l1-follower.failure-hold.js";

const MINUTE = 60_000;

/** A database the driver could not reach: a transient that is not the L1 node's. */
const databaseDown = () =>
  Object.assign(new Error("connect ECONNREFUSED 127.0.0.1:5433"), {
    code: "ECONNREFUSED",
  });
/** The local L1 node's transport is down. */
const nodeDown = () =>
  new TransportUnavailableError("node_unreachable", "socket refused");

/**
 * A runner over `holdsOf(run)`, on a clock that advances a minute per run.
 * Returns the trigger, the run count and the exhaustion reports.
 */
const runner = (holdsOf: (run: number) => readonly DriverHold[]) => {
  let clock = 0;
  let runs = 0;
  const reports: (readonly DriverHold[])[] = [];
  const abort = new AbortController();
  const trigger = coalescedRunner(
    () => {
      runs += 1;
      clock += MINUTE;
      return Promise.resolve(holdsOf(runs));
    },
    abort.signal,
    {
      budgetMs: 5 * MINUTE,
      now: () => clock,
      onExhausted: (holds) => reports.push(holds),
    },
  );
  return { trigger, runs: () => runs, reports, abort };
};

/** Drives the runner's backoff timers until `runs` reaches `count` or it stops. */
const advance = async (
  state: ReturnType<typeof runner>,
  count: number,
): Promise<void> => {
  for (let step = 0; step < 4 * count; step += 1) {
    if (state.runs() >= count) return;
    await vi.advanceTimersByTimeAsync(31_000);
  }
};

describe("failure holds by source", () => {
  it("bounds a database transient, waits on the L1 node unbounded, and does not retry the rest", () => {
    const database = failureHold("r", "d", databaseDown());
    expect(isRetriedHold(database)).toBe(true);
    expect(isTransientFailureHold(database)).toBe(true);

    for (const outage of [
      nodeDown(),
      new L1ProviderTransientError("transport", "socket refused"),
      new Error("wrapped", { cause: nodeDown() }),
    ]) {
      expect(isL1NodeOutage(outage)).toBe(true);
      const hold = failureHold("r", "d", outage);
      expect(isRetriedHold(hold)).toBe(true);
      expect(isTransientFailureHold(hold)).toBe(false);
    }

    const store = failureHold(
      "r",
      "d",
      new L1ProviderTransientError("store", "database unavailable"),
    );
    expect(isL1NodeOutage(store)).toBe(false);
    expect(isTransientFailureHold(store)).toBe(true);

    const logic = failureHold("r", "d", new Error("malformed datum"));
    expect(isRetriedHold(logic)).toBe(false);
    expect(isTransientFailureHold(logic)).toBe(false);
  });
});

describe("coalescedRunner's transient bound", () => {
  it("stops and reports once database transients outlast the budget", async () => {
    vi.useFakeTimers();
    try {
      const state = runner(() => [
        failureHold("l1_events_ingestion_failed", "db", databaseDown()),
      ]);
      state.trigger();
      await advance(state, 12);
      expect(state.reports).toHaveLength(1);
      expect(state.reports[0]!.map((hold) => hold.reason)).toEqual([
        "l1_events_ingestion_failed",
      ]);
      // The first failing run starts the clock; the run at the budget stops.
      expect(state.runs()).toBe(6);
      // Stopped: no further run on a trigger or a timer.
      state.trigger();
      await vi.advanceTimersByTimeAsync(5 * MINUTE);
      expect(state.runs()).toBe(6);
      state.abort.abort();
    } finally {
      vi.useRealTimers();
    }
  });

  it("keeps retrying an L1 node outage past the budget", async () => {
    vi.useFakeTimers();
    try {
      const state = runner(() => [
        failureHold("l1_events_ingestion_failed", "node", nodeDown()),
      ]);
      state.trigger();
      await advance(state, 12);
      expect(state.runs()).toBe(12);
      expect(state.reports).toEqual([]);
      state.abort.abort();
    } finally {
      vi.useRealTimers();
    }
  });

  it("starts the budget over after a run free of transient failures", async () => {
    vi.useFakeTimers();
    try {
      // Runs 1-4 fail on the database, run 5 waits on the node only, runs
      // 6-10 fail on the database again: no stretch reaches the budget
      // until run 11 (minute 6 to minute 11).
      const state = runner((run) => [
        run === 5
          ? failureHold("l1_events_ingestion_failed", "node", nodeDown())
          : failureHold("l1_events_ingestion_failed", "db", databaseDown()),
      ]);
      state.trigger();
      await advance(state, 11);
      expect(state.runs()).toBe(11);
      expect(state.reports).toHaveLength(1);
      state.abort.abort();
    } finally {
      vi.useRealTimers();
    }
  });
});
