/**
 * The import checker while admission keeps every worker busy: an entry's
 * run that cannot start within its block's cap is cancelled (never a
 * verdict) and left to the backlog lane; the sweep, on its own loop, raises
 * `unverified_deadline_missed` at the deadline of an entry whose run is
 * still waiting; and a verdict arriving after it neither erases nor
 * repeats that alarm.
 */
import { type MidgardCekVerdictRequest } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

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

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

/** A slow run for admission, and a block of `bodies` not yet processed. */
const setUp = async (count: number) => {
  const [slow] = firstRuns(await blockOf(["accept"], { firstByte: 0x90 }));
  const block = await blockOf(
    Array.from({ length: count }, () => "accept" as const),
  );
  return { slow: slow!, block };
};

type Timed = ImportCheckAlarm & { atMs: number };

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
      const deadlineMs = Date.now() + 1_500;
      await processBlock(block, { deadlineMs });
      const alarms: Timed[] = [];
      const { input } = checker(injected.evaluator, {
        ...CONFIG,
        // The cap does not end before the deadline: the run waits past it.
        blockCapMs: 60_000,
      });
      const timed = {
        ...input,
        alarm: (alarm: ImportCheckAlarm) =>
          Effect.sync(() => void alarms.push({ ...alarm, atMs: Date.now() })),
      };
      let passDone = false;
      const pass = db(runImportCheckPass(timed)).then(() => {
        passDone = true;
      });
      let sweeping = true;
      const alarmed = new Set<number>();
      const sweeps = (async () => {
        while (sweeping) {
          await db(runUnverifiedSweep(timed, alarmed));
          await sleep(50);
        }
      })();
      const missed = () =>
        alarms.filter((alarm) => alarm.alarm === "unverified_deadline_missed");
      try {
        await sleep(deadlineMs + 600 - Date.now());
        expect(passDone).toBe(false);
        // The run never started: admission held both workers.
        expect(injected.jobs()).toBe(0);
        expect(injected.pool.stats().busyWorkers).toBe(2);
        expect(admission.admitted()).toBeGreaterThan(6);
        expect(missed()).toEqual([
          expect.objectContaining({
            headerHash: block.headerHash,
            transactionId: hex(block.txIds[0]!),
            deadlineMs,
            state: "pending",
          }),
        ]);
        expect(missed()[0]!.atMs).toBeGreaterThanOrEqual(deadlineMs);
        expect(missed()[0]!.atMs).toBeLessThan(deadlineMs + 500);
      } finally {
        await admission.stop();
      }
      await pass;
      await sleep(200);
      sweeping = false;
      await sweeps;
      await db(runUnverifiedSweep(timed, alarmed));
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
    await processBlock(block, { deadlineMs: Date.now() - 1 });
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
