/**
 * The node's supervision of the settlement worker, driven with real worker
 * threads that stand in for the settlement worker's ways of dying.
 */
import { performance } from "node:perf_hooks";
import { Worker } from "node:worker_threads";

import { Effect, Fiber, Logger, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  type SettlementSupervision,
  type SettlementWorkerData,
  superviseSettlementWorker,
} from "../src/fibers/settlement.js";
import type { SettlementHealth } from "../src/services/settlement.js";
import {
  SETTLEMENT_WORKER_FAILURE_WINDOW_MS,
  SETTLEMENT_WORKER_RECOVERY_TICKS,
  settlementReadinessReason,
} from "../src/services/settlement-readiness.js";

const OWNERSHIP_ERROR =
  "Error: Settlement wallet identity changed or another node owns settlement\n    at renew (settlement.ts:45)";
const LEASE_WAIT =
  "waiting for the previous settlement ownership lease: Settlement wallet identity changed or another node owns settlement";

/** One worker run, chosen by `behaviour`. */
const WORKER = `
const { parentPort, workerData } = require("node:worker_threads");
const report = (state, detail, tickCompleted) =>
  parentPort.postMessage({ observedAt: Date.now(), state, detail, tickCompleted });
switch (workerData.behaviour) {
  case "fail":
    // What workers/settlement.ts does when its program fails.
    report("error", ${JSON.stringify(OWNERSHIP_ERROR)});
    process.exitCode = 1;
    parentPort.close();
    break;
  case "crash":
    report("error", "tick failed: provider timeout");
    setTimeout(() => {
      throw new Error("Worker terminated due to reaching memory limit: JS heap out of memory");
    }, 5);
    break;
  case "exit":
    process.exit(1);
    break;
  case "lease-wait": {
    // What settlement.build-job.ts reports while another token's lease is
    // live, until the wait runs out and the program fails.
    const wait = setInterval(
      () => report("starting", ${JSON.stringify(LEASE_WAIT)}),
      5,
    );
    setTimeout(() => {
      clearInterval(wait);
      report("error", ${JSON.stringify(OWNERSHIP_ERROR)});
      process.exitCode = 1;
      parentPort.close();
    }, workerData.leaseWaitMs);
    break;
  }
  case "silent":
    setInterval(() => {}, 1_000);
    break;
  case "tick-errors": {
    // A run that stays up while every tick fails (settlement.build-job.ts
    // reports each failed tick as 'error', never as a completed tick), for
    // longer than any stable period, then dies.
    const ticks = setInterval(
      () => report("error", "settlement withdrawal event-a#0 initialize reconcile: indexer sync: fetch failed"),
      5,
    );
    setTimeout(() => {
      clearInterval(ticks);
      process.exitCode = 1;
      parentPort.close();
    }, workerData.leaseWaitMs);
    break;
  }
  case "cheap-then-crash": {
    // Completes one tick short of recovery (a pending body's reconcile, an
    // eligibility wait), then dies building a job: the run-8 OOM shape.
    let ticks = 0;
    const cheap = setInterval(() => {
      ticks += 1;
      report("waiting", "settlement transaction abababababababababababababababababababababababababababababababab journaled; S6 sends its exact bytes until it lands", true);
      if (ticks < workerData.recoveryTicks - 1) return;
      clearInterval(cheap);
      setTimeout(() => {
        throw new Error("Worker terminated due to reaching memory limit: JS heap out of memory");
      }, 5);
    }, 5);
    break;
  }
  case "flapping": {
    // Every other tick completes, for longer than recovery would take in a
    // row, then the run dies.
    let ticks = 0;
    const flap = setInterval(() => {
      ticks += 1;
      if (ticks % 2 === 0) report("error", "settlement withdrawal event-a#0 fund: indexer sync: fetch failed");
      else report("running", "settlement progress checkpointed", true);
    }, 5);
    setTimeout(() => {
      clearInterval(flap);
      process.exitCode = 1;
      parentPort.close();
    }, workerData.leaseWaitMs);
    break;
  }
  case "healthy": {
    // Waits out a lease first, then completes a tick every 5 ms.
    const wait = setInterval(() => report("starting", ${JSON.stringify(LEASE_WAIT)}), 5);
    setTimeout(() => {
      clearInterval(wait);
      setInterval(() => report("running", "settlement queue drained", true), 5);
    }, 30);
    break;
  }
}
`;

type Run = {
  readonly token: string;
  readonly spawnedAt: number;
  exitedAt?: number;
};

const supervise = async (
  behaviour: (run: number) => string,
  done: (health: SettlementHealth, runs: readonly Run[]) => boolean,
  options: Partial<SettlementSupervision> = {},
) => {
  const runs: Run[] = [];
  const logs: string[] = [];
  const seen: SettlementHealth[] = [];
  // Runs whose thread had not exited when a later run was spawned.
  const live = new Set<Run>();
  const overlaps: Run[] = [];
  const spawn = (data: SettlementWorkerData) => {
    overlaps.push(...live);
    const run: Run = { token: data.ownerToken, spawnedAt: performance.now() };
    const worker = new Worker(WORKER, {
      eval: true,
      workerData: {
        ...data,
        behaviour: behaviour(runs.length),
        leaseWaitMs: LEASE_WAIT_MS,
        recoveryTicks: SETTLEMENT_WORKER_RECOVERY_TICKS,
      },
    });
    live.add(run);
    worker.once("exit", () => {
      run.exitedAt = performance.now();
      live.delete(run);
    });
    runs.push(run);
    return worker;
  };
  const logger = Logger.make(({ logLevel, message }) =>
    logs.push(
      `${logLevel.label} ${Array.isArray(message) ? message.join(" ") : String(message)}`,
    ),
  );
  const result = await Effect.runPromise(
    Effect.gen(function* () {
      const health = yield* Ref.make<SettlementHealth>({
        observedAt: Date.now(),
        state: "starting",
        detail: "settlement worker starting",
      });
      const fiber = yield* Effect.fork(
        superviseSettlementWorker(health, spawn, {
          spacing: "0 millis",
          watchdogMs: 60_000,
          ...options,
        }),
      );
      for (let polls = 0; polls < 2_000; polls++) {
        const current = yield* Ref.get(health);
        if (seen.at(-1) !== current) seen.push(current);
        if (done(current, runs)) {
          yield* Fiber.interrupt(fiber);
          return current;
        }
        yield* Effect.sleep("5 millis");
      }
      yield* Fiber.interrupt(fiber);
      throw new Error(
        `supervision never settled: ${JSON.stringify(seen.at(-1))}`,
      );
    }).pipe(Effect.provide(Logger.replace(Logger.defaultLogger, logger))),
  );
  return { health: result, runs, logs, seen, overlaps };
};

/** How long a lease-wait or tick-errors run stays up before it dies: longer
 * than the stable period an earlier supervisor cleared the streak after. */
const LEASE_WAIT_MS = 150;

const failed = (count: number) => (health: SettlementHealth) =>
  (health.workerFailures?.count ?? 0) >= count;

describe("settlement worker supervision", () => {
  it("reports a worker's death with the error it last reported, and logs both", async () => {
    const { health, logs } = await supervise(() => "fail", failed(1));
    expect(health.state).toBe("error");
    expect(health.detail).toBe(`${OWNERSHIP_ERROR} (worker exited 1)`);
    expect(logs).toContain(
      `WARN Settlement worker reported an error: ${OWNERSHIP_ERROR}`,
    );
    expect(logs).toContain(
      `WARN Settlement worker failed (1 in a row): ${OWNERSHIP_ERROR} (worker exited 1)`,
    );
  });

  it("keeps the last reported error beside a crash's own error", async () => {
    const { health } = await supervise(() => "crash", failed(1));
    expect(health.detail).toBe(
      "Worker terminated due to reaching memory limit: JS heap out of memory (last reported error: tick failed: provider timeout)",
    );
  });

  it("says so when a worker died without reporting anything", async () => {
    const { health } = await supervise(() => "exit", failed(1));
    expect(health.detail).toBe("no error reported (worker exited 1)");
  });

  it("stops a silent worker, awaiting each run's termination before the next, all under one token", async () => {
    const { health, runs, overlaps } = await supervise(
      () => "silent",
      (_, runs) => runs.length >= 3,
      { watchdogMs: 20 },
    );
    expect(health.detail).toBe("Settlement worker stopped reporting progress");
    expect(overlaps).toEqual([]);
    expect(new Set(runs.map((run) => run.token)).size).toBe(1);
    for (const [index, run] of runs.slice(1).entries()) {
      const previous = runs[index]!;
      expect(previous.exitedAt).toBeDefined();
      expect(run.spawnedAt).toBeGreaterThanOrEqual(previous.exitedAt!);
    }
  });

  it("never spawns a replacement before the previous thread has exited, and leaves no timer behind", async () => {
    const timers = () =>
      process
        .getActiveResourcesInfo()
        .filter((resource) => resource === "Timeout").length;
    const before = timers();
    const { runs, overlaps } = await supervise(() => "exit", failed(8));
    expect(runs.length).toBeGreaterThanOrEqual(8);
    expect(overlaps).toEqual([]);
    // Each failed run's watchdog interval is cleared, not just the last.
    expect(timers() - before).toBeLessThan(2);
  });

  it("counts a run that dies still waiting for the ownership lease, however long it waited", async () => {
    const { health, logs, seen } = await supervise(
      () => "lease-wait",
      failed(3),
    );
    const failures = health.workerFailures!;
    expect(failures.count).toBe(3);
    expect(logs.some((line) => line.includes("recovered"))).toBe(false);
    // The lease wait carries the streak, so readers see it the whole time.
    expect(
      seen.some(
        (value) =>
          value.state === "starting" && value.workerFailures?.count === 2,
      ),
    ).toBe(true);
    expect(
      settlementReadinessReason(
        health,
        failures.since + SETTLEMENT_WORKER_FAILURE_WINDOW_MS,
      ),
    ).toBe(
      "settlement_worker_failing:3:Error: Settlement wallet identity changed or another node owns settlement",
    );
  });

  it("counts a run that stays up reporting failed ticks, however long it reported", async () => {
    const { health, logs, seen } = await supervise(
      () => "tick-errors",
      failed(3),
    );
    const failures = health.workerFailures!;
    expect(failures.count).toBe(3);
    expect(health.detail).toBe(
      "settlement withdrawal event-a#0 initialize reconcile: indexer sync: fetch failed (worker exited 1)",
    );
    expect(
      settlementReadinessReason(
        health,
        failures.since + SETTLEMENT_WORKER_FAILURE_WINDOW_MS,
      ),
    ).toBe(
      "settlement_worker_failing:3:settlement withdrawal event-a#0 initialize reconcile: indexer sync: fetch failed (worker exited 1)",
    );
    expect(logs.some((line) => line.includes("recovered"))).toBe(false);
    // Its failed-tick reports carry the streak the whole time.
    expect(
      seen.some(
        (value) => value.state === "error" && value.workerFailures?.count === 2,
      ),
    ).toBe(true);
  });

  it("names a crash loop to readiness only after the streak, and forgets it once a replacement completes ticks in a row", async () => {
    const { health, seen } = await supervise(
      (run) => (run < 3 ? "exit" : "healthy"),
      (health, runs) =>
        runs.length > 3 &&
        health.state === "running" &&
        health.workerFailures === undefined,
    );
    const streaks = seen.flatMap((value) =>
      value.workerFailures === undefined ? [] : [value],
    );
    const third = streaks.find((value) => value.workerFailures!.count === 3)!;
    const { since } = third.workerFailures!;
    const failingStreaks = streaks.filter(
      (value) => value.workerFailures!.count < 3,
    );
    // One or two dead runs, or a fresh third, never take the node out.
    for (const value of [...failingStreaks, third])
      expect(
        settlementReadinessReason(
          value,
          value === third
            ? since + SETTLEMENT_WORKER_FAILURE_WINDOW_MS - 1
            : since + 10 * 60_000,
        ),
      ).toBeUndefined();
    expect(
      settlementReadinessReason(
        third,
        since + SETTLEMENT_WORKER_FAILURE_WINDOW_MS,
      ),
    ).toBe("settlement_worker_failing:3:no error reported (worker exited 1)");
    // The replacement's lease wait still carries the streak ...
    expect(
      seen.some(
        (value) =>
          value.state === "starting" && value.workerFailures?.count === 3,
      ),
    ).toBe(true);
    // ... and so do its first completed ticks; only the
    // SETTLEMENT_WORKER_RECOVERY_TICKS-th in a row clears it, and no report
    // is published with the worker's flag.
    const running = seen.filter((value) => value.state === "running");
    expect(
      running.filter((value) => value.workerFailures !== undefined).length,
    ).toBeLessThanOrEqual(SETTLEMENT_WORKER_RECOVERY_TICKS - 1);
    expect(running.at(-1)!.workerFailures).toBeUndefined();
    expect(seen.some((value) => "tickCompleted" in value)).toBe(false);
    expect(
      settlementReadinessReason(health, since + 60 * 60_000),
    ).toBeUndefined();
  });

  it("counts runs that each complete a cheap tick and then die building a job, until readiness names them", async () => {
    const { health, logs, seen } = await supervise(
      () => "cheap-then-crash",
      failed(3),
    );
    const failures = health.workerFailures!;
    expect(failures.count).toBe(3);
    expect(logs.some((line) => line.includes("recovered"))).toBe(false);
    // Each run's completed ticks are published with the streak it inherits.
    expect(
      seen.some(
        (value) =>
          value.state === "waiting" && value.workerFailures?.count === 2,
      ),
    ).toBe(true);
    expect(
      settlementReadinessReason(
        health,
        failures.since + SETTLEMENT_WORKER_FAILURE_WINDOW_MS,
      ),
    ).toBe(
      "settlement_worker_failing:3:Worker terminated due to reaching memory limit: JS heap out of memory",
    );
  });

  it("keeps the streak of runs whose completed ticks never follow one another", async () => {
    const { health, logs } = await supervise(() => "flapping", failed(3));
    expect(health.workerFailures!.count).toBe(3);
    expect(logs.some((line) => line.includes("recovered"))).toBe(false);
  });
});
