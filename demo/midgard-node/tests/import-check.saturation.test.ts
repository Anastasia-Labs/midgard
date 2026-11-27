/**
 * The import checker while admission keeps every worker busy: an entry's
 * run that cannot start within its block's cap is cancelled (never a
 * verdict) and left to the backlog lane; the sweep, on its own loop, raises
 * `unverified_deadline_missed` at the deadline of an entry whose run is
 * still waiting; and a verdict arriving after it neither erases nor
 * repeats that alarm.
 */
import type { MidgardCekVerdictRequest } from "@al-ft/midgard-validation/phase-b";
import { Clock, Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { type ImportCheckAlarm } from "../src/import-check/alarms.js";
import {
  runBacklogPass,
  runImportCheckPass,
  runUnverifiedSweep,
} from "../src/import-check/checker.js";
import { type FixedValidationWorkerPool } from "../src/services/validation-pool.js";
import {
  blockOf,
  checker,
  CONFIG,
  db,
  entries,
  firstRuns,
  hex,
  HOUR_MS,
  idsIn,
  injectedEvaluator,
  processBlock,
  useCheckerDatabase,
} from "./import-check.checker.fixture.js";

useCheckerDatabase();

const SLOW_MS = 100;

/** Admission: `loops` submitters each running `request` back to back. */
const flood = (
  pool: FixedValidationWorkerPool,
  request: MidgardCekVerdictRequest,
  loops = 6,
) => {
  let on = true;
  let admitted = 0;
  const all = Array.from({ length: loops }, async () => {
    while (on) {
      await pool.submit({
        kind: "cek_verdict",
        jobId: pool.allocateJobId(),
        request,
      });
      admitted += 1;
    }
  });
  return {
    admitted: () => admitted,
    stop: async () => {
      on = false;
      await Promise.all(all);
    },
  };
};

/** A clock that reads `ms` until the test sets it, for passes that wait on
 * a real pool: every pass run `on` it reads the same time, and its sleeps
 * are real. It starts at the wall clock's time, which the pool compares a
 * run's `startBy` with. */
const manualClock = () => {
  const live = Clock.make();
  const time = { ms: Date.now() };
  const nanos = () => BigInt(time.ms) * 1_000_000n;
  const clock: Clock.Clock = {
    [Clock.ClockTypeId]: Clock.ClockTypeId,
    unsafeCurrentTimeMillis: () => time.ms,
    currentTimeMillis: Effect.sync(() => time.ms),
    unsafeCurrentTimeNanos: nanos,
    currentTimeNanos: Effect.sync(nanos),
    sleep: (duration) => live.sleep(duration),
  };
  return Object.assign(time, {
    on: <A, E, R>(effect: Effect.Effect<A, E, R>) =>
      Effect.withClock(effect, clock),
  });
};

/** A slow run for admission, and a block of `bodies` not yet processed. */
const setUp = async (count: number) => {
  const [slow] = firstRuns(await blockOf(["accept"], { firstByte: 0x90 }));
  const block = await blockOf(
    Array.from({ length: count }, () => "accept" as const),
  );
  return { slow: slow!, block };
};

describe("import checker under saturated admission", () => {
  it.each(["accepted", "rejected"] as const)(
    "raises the missed alarm at the deadline of a run still waiting for a worker, once, and a late verdict (%s) keeps it",
    async (late) => {
      const { slow, block } = await setUp(1);
      const run = block.runs[0]![0]!;
      const injected = await injectedEvaluator(
        {
          mode: "accept",
          slowContexts: [slow.contextCbor],
          slowMs: SLOW_MS,
          ...(late === "rejected" ? { rejectContexts: [run.contextCbor] } : {}),
        },
        { size: 2, backlogShare: 1 },
      );
      const admission = flood(injected.pool, slow);
      const clock = manualClock();
      const deadlineMs = clock.ms + 1_500;
      await processBlock(block, { deadlineMs });
      const alarms: ImportCheckAlarm[] = [];
      let asked!: () => void;
      const began = new Promise<void>((resolve) => (asked = resolve));
      const { input } = checker(
        {
          ...injected.evaluator,
          evaluate: (...args) => {
            asked();
            return injected.evaluator.evaluate(...args);
          },
        },
        {
          ...CONFIG,
          // The cap does not end before the deadline: the run waits past it.
          blockCapMs: 60_000,
        },
      );
      const recorded = {
        ...input,
        alarm: (alarm: ImportCheckAlarm) =>
          Effect.sync(() => void alarms.push(alarm)),
      };
      let passDone = false;
      const pass = db(clock.on(runImportCheckPass(recorded))).then(() => {
        passDone = true;
      });
      const alarmed = new Set<number>();
      const sweepAt = (nowMs: number) => {
        clock.ms = nowMs;
        return db(clock.on(runUnverifiedSweep(recorded, alarmed)));
      };
      const missed = () =>
        alarms.filter((alarm) => alarm.alarm === "unverified_deadline_missed");
      try {
        // The run is asked for, and admission takes every worker that
        // comes free after it.
        await began;
        const admittedBefore = admission.admitted();
        await vi.waitFor(
          () =>
            expect(admission.admitted()).toBeGreaterThan(admittedBefore + 6),
          { timeout: 30_000, interval: 20 },
        );
        await sweepAt(deadlineMs - 1);
        expect(missed()).toEqual([]);
        await sweepAt(deadlineMs);
        expect(passDone).toBe(false);
        // The run never started: admission held both workers.
        expect(injected.jobs()).toBe(0);
        expect(injected.pool.stats().busyWorkers).toBe(2);
        expect(missed()).toEqual([
          expect.objectContaining({
            headerHash: block.headerHash,
            transactionId: hex(block.txIds[0]!),
            deadlineMs,
            state: "pending",
          }),
        ]);
        await sweepAt(deadlineMs + 1);
        expect(missed()).toHaveLength(1);
      } finally {
        // Nothing of this test outlives it, whatever failed above.
        await admission.stop();
        await Promise.allSettled([pass]);
      }
      await pass;
      await sweepAt(deadlineMs + 2);
      expect(missed()).toHaveLength(1);
      if (late === "rejected") {
        expect(
          alarms.filter((alarm) => alarm.alarm === "import_check_rejected"),
        ).toEqual([
          expect.objectContaining({ transactionId: hex(block.txIds[0]!) }),
        ]);
        // The late rejection was recorded; the entry then left the set
        // at its deadline.
        expect(
          alarms.findIndex((alarm) => alarm.alarm === "import_check_rejected"),
        ).toBeGreaterThan(alarms.indexOf(missed()[0]!));
      } else
        expect(
          alarms.filter((alarm) => alarm.alarm === "import_check_rejected"),
        ).toEqual([]);
      expect(injected.jobs()).toBe(late === "rejected" ? 0 : 1);
      expect(await entries()).toEqual([]);
    },
    60_000,
  );

  it("cancels runs not started within the block's cap, with no verdict, and the backlog lane runs them", async () => {
    const { slow, block } = await setUp(2);
    const injected = await injectedEvaluator(
      { mode: "accept", slowContexts: [slow.contextCbor], slowMs: SLOW_MS },
      { size: 2, backlogShare: 1 },
    );
    await processBlock(block);
    const admission = flood(injected.pool, slow);
    const check = checker(injected.evaluator, { ...CONFIG, blockCapMs: 300 });
    try {
      const startedAt = Date.now();
      const alarms = await check.importPass();
      expect(Date.now() - startedAt).toBeLessThan(2_000);
      expect(alarms).toEqual([
        {
          alarm: "import_check_cap",
          headerHash: block.headerHash,
          skipped: 2,
        },
      ]);
      expect(injected.jobs()).toBe(0);
      expect(await idsIn(["cap_skipped"])).toEqual(
        block.txIds.map((txId) => hex(txId)),
      );
      // The backlog lane keeps its share under the same flood.
      expect(await check.backlog()).toEqual([]);
    } finally {
      await admission.stop();
    }
    expect(injected.jobs()).toBe(2);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("runs no entry already past its deadline, and the sweep takes it out with the missed alarm", async () => {
    const { block } = await setUp(1);
    const injected = await injectedEvaluator(
      { mode: "accept" },
      { size: 2, backlogShare: 1 },
    );
    // Already due: no pass runs it, and the sweep takes it out.
    await processBlock(block, { deadlineMs: Date.now() - HOUR_MS });
    const check = checker(injected.evaluator);
    expect(await check.importPass()).toEqual([]);
    expect(await db(runBacklogPass(check.input))).toBe(0);
    expect(injected.jobs()).toBe(0);
    expect(await check.sweep()).toEqual([
      expect.objectContaining({
        alarm: "unverified_deadline_missed",
        state: "pending",
      }),
    ]);
    expect(await entries()).toEqual([]);
  }, 60_000);
});
