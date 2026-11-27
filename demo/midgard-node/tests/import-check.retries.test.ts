/**
 * The import checker's runs with no verdict: a run killed at its
 * wall-clock limit, or whose worker failed before the run was sent to it.
 * Neither faults nor caches the transaction: it waits as `timed_out`, and
 * the backlog lane runs it again on spare capacity, backing off, until its
 * deadline. Rows on the scripted evaluator run on a virtual clock; rows on
 * a pool use workers whose every script job hangs or never starts, so no
 * outcome depends on how fast a healthy job answers.
 */
import { describe, expect, it } from "vitest";

import { type ImportCheckAlarm } from "../src/import-check/alarms.js";
import {
  ACCEPTED,
  blockOf,
  CANCELLED,
  checker,
  closePools,
  CONFIG,
  entries,
  FAULT,
  firstRuns,
  hex,
  HOUR_MS,
  idsIn,
  injectedEvaluator,
  KILLED,
  landBlock,
  OVER_BUDGET,
  processBlock,
  REJECTED,
  scriptedEvaluator,
} from "./import-check.checker.fixture.js";
import { useCheckerDatabase } from "./import-check.checker.fixture.js";

useCheckerDatabase();

const named = (alarms: readonly ImportCheckAlarm[], name: string) =>
  alarms.filter((alarm) => alarm.alarm === name);

const ids = (alarms: readonly ImportCheckAlarm[]) =>
  alarms.map((alarm) => [
    alarm.alarm,
    "transactionId" in alarm ? alarm.transactionId : undefined,
  ]);

describe("import checker runs with no verdict, on the pool", () => {
  it("does not fault or cache a transaction whose run is killed, and does not retry it on a fresh worker", async () => {
    // Every script job hangs, so every run is killed whatever the load.
    const injected = await injectedEvaluator({ mode: "hang" }, { size: 3 });
    const check = checker(injected.evaluator, { ...CONFIG, killFloorMs: 50 });
    const first = await landBlock(["accept"]);
    const alarms = await check.importPass();
    expect(ids(alarms)).toEqual([
      ["import_check_timeout", hex(first.txIds[0]!)],
    ]);
    expect(injected.jobs()).toBe(1);
    expect(await idsIn(["timed_out"])).toEqual([hex(first.txIds[0]!)]);
    // Not cached: the next transaction running the script is run at import.
    const next = await landBlock(["accept"], {
      parentHeaderHash: first.headerHash,
      firstByte: 0x60,
    });
    expect(ids(await check.importPass())).toEqual([
      ["import_check_timeout", hex(next.txIds[0]!)],
    ]);
    expect(injected.jobs()).toBe(2);
    expect(await idsIn(["faulted", "cache_skipped"])).toEqual([]);
  }, 60_000);

  it("does not fault or cache a transaction whose fresh worker cannot start", async () => {
    // The run crashes its worker; the worker started for its retry fails
    // before the run is sent to it, so the retry never runs.
    const size = 3;
    const injected = await injectedEvaluator(
      { mode: "crash", startsAllowed: size },
      { size },
    );
    const check = checker(injected.evaluator);
    const first = await landBlock(["accept"]);
    const alarms = await check.importPass();
    expect(ids(alarms)).toEqual([
      ["import_check_timeout", hex(first.txIds[0]!)],
    ]);
    expect(injected.jobs()).toBe(1);
    expect(await idsIn(["timed_out"])).toEqual([hex(first.txIds[0]!)]);
    // Not cached: the next transaction running the script is not skipped.
    await landBlock(["accept"], {
      parentHeaderHash: first.headerHash,
      firstByte: 0x60,
    });
    expect(named(await check.importPass(), "uplc_import_fault")).toEqual([]);
    expect(await idsIn(["faulted", "cache_skipped"])).toEqual([]);
  }, 60_000);
});

describe("import checker retries", () => {
  it("leaves a due retry to the backlog lane, which decides it once the worker recovers", async () => {
    const t0 = Date.now();
    // Killed once; the import lane then never has a worker free.
    const scripted = scriptedEvaluator((call) =>
      scripted.calls.length === 1
        ? KILLED
        : call.lane === "import"
          ? CANCELLED
          : ACCEPTED,
    );
    const check = checker(scripted.evaluator);
    const block = await landBlock(["accept"], { deadlineMs: t0 + HOUR_MS });
    expect(await check.importPass(t0)).toEqual([
      expect.objectContaining({
        alarm: "import_check_timeout",
        transactionId: hex(block.txIds[0]!),
        retryAtMs: t0 + 5_000,
      }),
    ]);
    // Its retry is due, but the import pass does not take it.
    expect(await check.importStep(t0 + 5_000)).toEqual({
      worked: false,
      alarms: [],
    });
    expect(await check.backlog(t0 + 4_999)).toEqual([]);
    expect(scripted.calls).toHaveLength(1);
    expect(await check.backlog(t0 + 5_000)).toEqual([]);
    expect(scripted.calls.map((call) => [call.lane, call.atMs])).toEqual([
      ["import", t0],
      ["backlog", t0 + 5_000],
    ]);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("runs every first check before any due retry, and never retries a killed run on a fresh worker", async () => {
    const t0 = Date.now();
    const flood = await blockOf(["accept", "accept", "accept", "accept"]);
    const honest = await blockOf(["accept"], { firstByte: 0x60 });
    const honestContext = hex(firstRuns(honest)[0]!.contextCbor);
    const scripted = scriptedEvaluator((call) =>
      call.context === honestContext ? ACCEPTED : KILLED,
    );
    const check = checker(scripted.evaluator);
    await processBlock(flood, { deadlineMs: t0 + HOUR_MS });
    expect(
      named(await check.importPass(t0), "import_check_timeout"),
    ).toHaveLength(4);
    // An honest transaction due after the killed ones, while all their
    // retries are due.
    await processBlock(honest, {
      parentHeaderHash: flood.headerHash,
      deadlineMs: t0 + 2 * HOUR_MS,
    });
    expect(await check.backlog(t0 + 600_000)).toEqual([]);
    const backlog = scripted.calls.slice(4);
    expect(backlog.map((call) => call.context)).toEqual([
      honestContext,
      ...firstRuns(flood).map((run) => hex(run.contextCbor)),
    ]);
    expect(scripted.calls.filter((call) => call.fresh)).toEqual([]);
    expect(await idsIn(["timed_out"])).toHaveLength(4);
    expect(await idsIn(["pending", "cap_skipped"])).toEqual([]);
  }, 60_000);

  it("runs a first check that arrives while retries are due after at most one retry per backlog worker", async () => {
    const t0 = Date.now();
    const killed = await blockOf(["accept", "accept", "accept"]);
    const later = await blockOf(["accept"], { firstByte: 0x60 });
    const laterContext = hex(firstRuns(later)[0]!.contextCbor);
    let landed = false;
    const scripted = scriptedEvaluator(
      (call) => (call.context === laterContext ? ACCEPTED : KILLED),
      {
        // A block lands during the first retry, with a first check.
        during: async (call) => {
          if (call.lane !== "backlog" || landed) return;
          landed = true;
          await processBlock(later, {
            parentHeaderHash: killed.headerHash,
            deadlineMs: t0 + 2 * HOUR_MS,
          });
        },
      },
    );
    const check = checker(scripted.evaluator);
    await processBlock(killed, { deadlineMs: t0 + HOUR_MS });
    expect(
      named(await check.importPass(t0), "import_check_timeout"),
    ).toHaveLength(3);
    // The backlog's share is one worker: the first check runs right after
    // the retry it arrived during, before the other due retries.
    expect(await check.backlog(t0 + 5_000)).toEqual([]);
    const [first, second, third] = firstRuns(killed).map((run) =>
      hex(run.contextCbor),
    );
    expect(
      scripted.calls.slice(3).map((call) => [call.lane, call.context]),
    ).toEqual([
      ["backlog", first],
      ["backlog", laterContext],
      ["backlog", second],
      ["backlog", third],
    ]);
    expect(await idsIn(["timed_out"])).toHaveLength(3);
    expect(await idsIn(["pending"])).toEqual([]);
  }, 60_000);

  it("backs off a killed transaction's retries, doubling from 5 s up to 10 minutes, with one alarm", async () => {
    const t0 = Date.now();
    const scripted = scriptedEvaluator(() => KILLED);
    const check = checker(scripted.evaluator);
    await landBlock(["accept"], { deadlineMs: t0 + 4 * HOUR_MS });
    expect(
      named(await check.importPass(t0), "import_check_timeout"),
    ).toHaveLength(1);
    const waits = [
      5_000, 10_000, 20_000, 40_000, 80_000, 160_000, 320_000, 600_000, 600_000,
      600_000,
    ];
    let lastMs = t0;
    for (const wait of waits) {
      expect(await check.backlog(lastMs + wait - 1)).toEqual([]);
      expect(scripted.calls.at(-1)!.atMs).toBe(lastMs);
      expect(await check.backlog(lastMs + wait)).toEqual([]);
      lastMs += wait;
      expect(scripted.calls.at(-1)!.atMs).toBe(lastMs);
    }
    expect(scripted.calls).toHaveLength(waits.length + 1);
    expect(await idsIn(["timed_out"])).toHaveLength(1);
  }, 60_000);

  it("drops a killed transaction at its deadline, alarming once", async () => {
    const t0 = Date.now();
    const deadlineMs = t0 + 20_000;
    const scripted = scriptedEvaluator(() => KILLED);
    const check = checker(scripted.evaluator);
    const block = await landBlock(["accept"], { deadlineMs });
    await check.importPass(t0);
    expect(await check.backlog(t0 + 5_000)).toEqual([]);
    expect(await check.backlog(t0 + 15_000)).toEqual([]);
    // Its next retry would be past its deadline.
    expect(await check.backlog(deadlineMs - 1)).toEqual([]);
    expect(scripted.calls).toHaveLength(3);
    expect(await check.sweep(deadlineMs - 1)).toEqual([]);
    expect(await idsIn(["timed_out"])).toEqual([hex(block.txIds[0]!)]);
    expect(await check.sweep(deadlineMs)).toEqual([
      {
        alarm: "unverified_deadline_missed",
        headerHash: block.headerHash,
        transactionId: hex(block.txIds[0]!),
        deadlineMs,
        state: "timed_out",
      },
    ]);
    expect(await entries()).toEqual([]);
    expect(await check.sweep(deadlineMs + 1)).toEqual([]);
    expect(await check.backlog(deadlineMs + 600_000)).toEqual([]);
    expect(scripted.calls).toHaveLength(3);
  }, 60_000);

  it("still rejects a run over its declared budget, and faults and caches a failing run's script, while a killed run waits", async () => {
    const t0 = Date.now();
    const block = await blockOf(["accept", "accept", "accept"]);
    const [killed, overBudget, failing] = firstRuns(block).map((run) =>
      hex(run.contextCbor),
    );
    const scripted = scriptedEvaluator((call) =>
      call.context === killed
        ? KILLED
        : call.context === overBudget
          ? OVER_BUDGET
          : call.context === failing
            ? FAULT
            : ACCEPTED,
    );
    await processBlock(block, { deadlineMs: t0 + HOUR_MS });
    const check = checker(scripted.evaluator);
    const txIds = block.txIds.map((txId) => hex(txId));
    expect(ids(await check.importPass(t0))).toEqual([
      ["import_check_timeout", txIds[0]],
      ["import_check_rejected", txIds[1]],
      ["uplc_import_fault", txIds[2]],
    ]);
    // The failing run is retried once on a fresh instance; the others not.
    expect(
      scripted.calls.map((call) => [
        [killed, overBudget, failing].indexOf(call.context),
        call.fresh,
      ]),
    ).toEqual([
      [0, false],
      [1, false],
      [2, false],
      [2, true],
    ]);
    expect(await idsIn(["timed_out"])).toEqual([txIds[0]]);
    expect(await idsIn(["rejected"])).toEqual([txIds[1]]);
    expect(await idsIn(["faulted"])).toEqual([txIds[2]]);
    // The failing run's script is cached: a later transaction running it
    // is not run at import.
    const later = await landBlock(["accept"], {
      parentHeaderHash: block.headerHash,
      firstByte: 0x60,
    });
    expect(await check.importPass(t0)).toEqual([]);
    expect(await idsIn(["cache_skipped"])).toEqual([hex(later.txIds[0]!)]);
  }, 60_000);

  it("projects a killed transaction's retry from its retry time, and leaves out one that cannot run again before its deadline", async () => {
    const t0 = Date.now();
    const scripted = scriptedEvaluator(() => KILLED);
    // Two kills of one entry's run project 2,002 ms of backlog work.
    const check = checker(scripted.evaluator, {
      ...CONFIG,
      killFloorMs: 1_000,
    });
    // Its retry (t0 + 5 s) is past its deadline: it does not run again.
    const early = await landBlock(["accept"], { deadlineMs: t0 + 4_000 });
    // Its retry is projected to end at t0 + 7,002 ms, past its deadline.
    const lateMs = t0 + 6_000;
    await landBlock(["accept"], {
      deadlineMs: lateMs,
      parentHeaderHash: early.headerHash,
      firstByte: 0x60,
    });
    expect(
      named(await check.importPass(t0), "import_check_timeout"),
    ).toHaveLength(2);
    expect(await check.sweep(t0)).toEqual([
      {
        alarm: "unverified_near_deadline",
        deadlineMs: lateMs,
        entries: 1,
        projectedMs: t0 + 7_002,
      },
    ]);
  }, 60_000);

  it("keeps a killed transaction's retry time across a restart, and serves the transactions behind it first", async () => {
    const t0 = Date.now();
    const killedCount = 3;
    const filler = 3;
    const block = await blockOf(
      Array.from({ length: killedCount + filler + 1 }, () => "accept" as const),
    );
    const contexts = firstRuns(block).map((run) => hex(run.contextCbor));
    const killed = contexts.slice(0, killedCount);
    const rejected = contexts[killedCount + filler]!;
    await processBlock(block, { deadlineMs: t0 + HOUR_MS });
    // Before the restart: the killed runs, then the block's cap is reached
    // (no import-lane worker is free).
    const before = scriptedEvaluator((call) =>
      killed.includes(call.context) ? KILLED : CANCELLED,
    );
    const importAlarms = await checker(before.evaluator).importPass(t0);
    expect(named(importAlarms, "import_check_timeout")).toHaveLength(
      killedCount,
    );
    expect(named(importAlarms, "import_check_cap")).toEqual([
      expect.objectContaining({ skipped: filler + 1 }),
    ]);
    // A restart: new in-memory state; the set is read back as it was.
    await closePools();
    const after = scriptedEvaluator((call) =>
      killed.includes(call.context)
        ? KILLED
        : call.context === rejected
          ? REJECTED
          : ACCEPTED,
    );
    const check = checker(after.evaluator);
    expect(await check.sweep(t0 + 1_000)).toEqual([]);
    const txIds = block.txIds.map((txId) => hex(txId));
    expect(ids(await check.backlog(t0 + 1_000))).toEqual([
      ["import_check_rejected", txIds[killedCount + filler]],
    ]);
    // The killed ones' retries are not due yet.
    expect(after.calls.map((call) => call.context)).toEqual(
      contexts.slice(killedCount),
    );
    expect(await idsIn(["timed_out"])).toEqual(txIds.slice(0, killedCount));
    // Once due, they are run again, with no further alarm.
    expect(await check.backlog(t0 + 5_000)).toEqual([]);
    expect(after.calls.slice(filler + 1).map((call) => call.context)).toEqual(
      killed,
    );
    expect(await idsIn(["faulted"])).toEqual([]);
  }, 60_000);
});
