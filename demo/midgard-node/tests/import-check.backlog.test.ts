/**
 * The import checker's bounds past the import pass: the negative cache, the
 * backlog lane and the projected-completion alarm. A transaction skipped at
 * import (past its block's cap, or running a cached script) still gets its
 * runs from the backlog lane, earliest deadline first; the sweep alarms
 * while every entry due by a deadline can still be served.
 */
import { describe, expect, it } from "vitest";

import { type ImportCheckAlarm } from "../src/import-check/alarms.js";
import { DEFAULT_IMPORT_CHECK_CONFIG } from "../src/services/config.import-check.js";
import {
  blockOf,
  checker,
  checksOf,
  closePools,
  CONFIG,
  entries,
  firstRuns,
  hex,
  idsIn,
  injectedEvaluator,
  type InjectedWorker,
  landBlock,
  processBlock,
  realEvaluator,
  useCheckerDatabase,
} from "./import-check.checker.fixture.js";

useCheckerDatabase();

const accepts = (count: number) =>
  Array.from({ length: count }, () => "accept" as const);

const named = (alarms: readonly ImportCheckAlarm[], name: string) =>
  alarms.filter((alarm) => alarm.alarm === name);

describe("import checker negative cache", () => {
  it.each(["hang", "crash"] as const)(
    "skips the import-time runs of a script that faulted twice (%s), and the backlog lane still gives each transaction a verdict",
    async (mode) => {
      const first = await landBlock(["accept"]);
      // The first two runs fault, every later one is accepted.
      const injected = await injectedEvaluator({ mode, faults: 2 });
      const check = checker(injected.evaluator, {
        ...CONFIG,
        killFloorMs: 300,
      });
      expect(named(await check.importPass(), "uplc_import_fault")).toEqual([
        expect.objectContaining({ transactionId: hex(first.txIds[0]!) }),
      ]);
      expect(injected.jobs()).toBe(2);
      // A second block reusing the script: no import-time run.
      const second = await landBlock(["accept", "accept"], {
        parentHeaderHash: first.headerHash,
        firstByte: 0x60,
      });
      expect(await check.importPass()).toEqual([]);
      expect(injected.jobs()).toBe(2);
      expect(await idsIn(["cache_skipped"])).toEqual(
        second.txIds.map((txId) => hex(txId)),
      );
      // The backlog lane runs them.
      expect(await check.backlog()).toEqual([]);
      expect(injected.jobs()).toBe(4);
      expect(await idsIn(["cache_skipped"])).toEqual([]);
      expect(await idsIn(["faulted"])).toEqual([hex(first.txIds[0]!)]);
    },
    60_000,
  );

  it("does not fault or cache an honest transaction with a large script and a low declared budget, under a loaded pool too", async () => {
    const real = await realEvaluator(4);
    // Three workers busy with long runs; the checker gets the fourth.
    const [loopRun] = firstRuns(
      await blockOf(["loop"], { firstByte: 0x90, cpu: 10_000_000_000n }),
    );
    const load = Array.from({ length: 3 }, () =>
      real.pool
        .submit({
          kind: "cek_verdict",
          jobId: real.pool.allocateJobId(),
          request: loopRun!,
        })
        .catch(() => undefined),
    );
    const check = checker(real, DEFAULT_IMPORT_CHECK_CONFIG);
    const first = await landBlock(["large"], { cpu: 1_000_000n });
    expect(await check.importPass()).toEqual([]);
    expect(real.pool.stats().busyWorkers).toBe(3);
    expect(await entries()).toEqual([]);
    // Not cached: the next transaction running it is run at import.
    await landBlock(["large"], {
      cpu: 1_000_000n,
      parentHeaderHash: first.headerHash,
      firstByte: 0x60,
    });
    expect(await check.importPass()).toEqual([]);
    expect(await entries()).toEqual([]);
    await closePools();
    await Promise.all(load);
  }, 60_000);

  it("faults and caches the same large script when the kill has no byte term", async () => {
    const real = await realEvaluator(2);
    const check = checker(real, {
      ...DEFAULT_IMPORT_CHECK_CONFIG,
      killFloorMs: 1,
      killUsPerKib: 0,
    });
    const first = await landBlock(["large"], { cpu: 1_000_000n });
    expect(named(await check.importPass(), "uplc_import_fault")).toHaveLength(
      1,
    );
    const second = await landBlock(["large"], {
      cpu: 1_000_000n,
      parentHeaderHash: first.headerHash,
      firstByte: 0x60,
    });
    expect(await check.importPass()).toEqual([]);
    expect(await idsIn(["cache_skipped"])).toEqual([hex(second.txIds[0]!)]);
  }, 60_000);

  it("probes a cached script again once its entry expires", async () => {
    const ttl = 10_000;
    const t0 = Date.now();
    const injected = await injectedEvaluator({ mode: "crash", faults: 2 });
    const check = checker(injected.evaluator, {
      ...CONFIG,
      negativeCacheTtlMs: ttl,
    });
    const first = await landBlock(["accept"]);
    expect(named(await check.importPass(t0), "uplc_import_fault")).toHaveLength(
      1,
    );
    const cached = await landBlock(["accept"], {
      parentHeaderHash: first.headerHash,
      firstByte: 0x60,
    });
    expect(await check.importPass(t0 + ttl - 1)).toEqual([]);
    expect(injected.jobs()).toBe(2);
    expect(await idsIn(["cache_skipped"])).toEqual([hex(cached.txIds[0]!)]);
    await landBlock(["accept"], {
      parentHeaderHash: cached.headerHash,
      firstByte: 0x68,
    });
    expect(await check.importPass(t0 + ttl)).toEqual([]);
    expect(injected.jobs()).toBe(3);
    // The new transaction was run at import and accepted: it left the set.
    expect(await idsIn(["pending", "cache_skipped"])).toEqual([
      hex(cached.txIds[0]!),
    ]);
  }, 60_000);
});

describe("import checker backlog lane", () => {
  const FLOOD = 4;

  /** A cached script (its first transaction hung twice), then a block of
   * `FLOOD` transactions hanging under it and one rejected, last, due
   * `deadlineInMs` after it is processed. */
  const floodUnderCachedScript = async (deadlineInMs: number) => {
    const seed = await blockOf(["accept"]);
    const flood = await blockOf(accepts(FLOOD + 1), { firstByte: 0x60 });
    const fraud = firstRuns(flood)[FLOOD]!;
    const injected = await injectedEvaluator({
      mode: "hang",
      rejectContexts: [fraud.contextCbor],
    });
    const check = checker(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 100,
    });
    await processBlock(seed);
    expect(named(await check.importPass(), "uplc_import_fault")).toHaveLength(
      1,
    );
    const deadlineMs = Date.now() + deadlineInMs;
    await processBlock(flood, {
      parentHeaderHash: seed.headerHash,
      deadlineMs,
    });
    expect(await check.importPass()).toEqual([]);
    expect(await entries(["cache_skipped"])).toHaveLength(FLOOD + 1);
    return { check, flood, deadlineMs };
  };

  it("gets a transaction under a flooded cached script its verdict before its deadline", async () => {
    const { check, flood, deadlineMs } = await floodUnderCachedScript(15_000);
    expect(await check.sweep()).toEqual([]);
    const alarms = await check.backlog();
    expect(Date.now()).toBeLessThan(deadlineMs);
    expect(named(alarms, "uplc_import_fault")).toHaveLength(FLOOD);
    expect(named(alarms, "import_check_rejected")).toEqual([
      expect.objectContaining({ transactionId: hex(flood.txIds[FLOOD]!) }),
    ]);
    expect(await idsIn(["rejected"])).toEqual([hex(flood.txIds[FLOOD]!)]);
  }, 60_000);

  it("raises the projected alarm before the deadline when the flood cannot be served in time", async () => {
    // Two kills of each of the five entries take about a second.
    const { check, deadlineMs } = await floodUnderCachedScript(600);
    const alarms = await check.sweep();
    expect(Date.now()).toBeLessThan(deadlineMs);
    expect(alarms).toEqual([
      expect.objectContaining({
        alarm: "unverified_near_deadline",
        deadlineMs,
        entries: FLOOD + 1,
      }),
    ]);
  }, 60_000);

  it("alarms for entries sharing one deadline while all of them can still be served", async () => {
    const count = 8;
    const soon = await blockOf(accepts(count));
    const later = await blockOf(accepts(count), { firstByte: 0x60 });
    await checksOf(soon);
    await checksOf(later);
    const soonMs = Date.now() + 6_000;
    const laterMs = Date.now() + 60_000;
    await processBlock(soon, { deadlineMs: soonMs });
    await processBlock(later, {
      deadlineMs: laterMs,
      parentHeaderHash: soon.headerHash,
    });
    const injected = await injectedEvaluator({ mode: "accept" });
    // Two kills of every entry due by `soonMs` (16 s) run past it; with
    // the later block's entries (32 s) they do not run past `laterMs`.
    const check = checker(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 1_000,
      blockCapMs: 0,
    });
    expect(named(await check.importPass(), "import_check_cap")).toHaveLength(2);
    const alarms = await check.sweep();
    expect(alarms).toEqual([
      expect.objectContaining({
        alarm: "unverified_near_deadline",
        deadlineMs: soonMs,
        entries: count,
      }),
    ]);
    expect(await check.sweep()).toEqual([]);
    // All of them are still served before the deadline.
    expect(await check.backlog()).toEqual([]);
    expect(Date.now()).toBeLessThan(soonMs);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("serves the transaction behind hanging transactions and capped filler before its deadline, across a restart", async () => {
    const hanging = 3;
    const filler = 3;
    const block = await blockOf(accepts(hanging + filler + 1));
    const runs = firstRuns(block);
    const worker: InjectedWorker = {
      mode: "hang",
      acceptContexts: runs
        .slice(hanging, hanging + filler)
        .map((run) => run.contextCbor),
      rejectContexts: [runs[hanging + filler]!.contextCbor],
    };
    const config = {
      ...CONFIG,
      killFloorMs: 400,
      blockCapMs: 700,
    };
    // Three workers: a killed run's retry takes the third at once instead of
    // waiting for the killed worker's replacement.
    const before = await injectedEvaluator(worker, { size: 3 });
    const deadlineMs = Date.now() + 20_000;
    await processBlock(block, { deadlineMs });
    // The first hanging transaction's two kills use up the block's cap.
    const importAlarms = await checker(before.evaluator, config).importPass();
    expect(named(importAlarms, "uplc_import_fault")).toHaveLength(1);
    expect(named(importAlarms, "import_check_cap")).toEqual([
      expect.objectContaining({ skipped: hanging - 1 + filler + 1 }),
    ]);
    // A restart: a new pool and cache; the set is read back as it was.
    await closePools();
    expect(await entries(["cap_skipped"])).toHaveLength(hanging + filler);
    expect(await entries(["faulted"])).toHaveLength(1);
    const after = await injectedEvaluator(worker, { size: 3 });
    const check = checker(after.evaluator, config);
    expect(await check.sweep()).toEqual([]);
    const alarms = await check.backlog();
    expect(Date.now()).toBeLessThan(deadlineMs);
    expect(named(alarms, "import_check_rejected")).toEqual([
      expect.objectContaining({
        transactionId: hex(block.txIds[hanging + filler]!),
      }),
    ]);
    const ids = block.txIds.map((txId) => hex(txId));
    expect(await idsIn(["faulted"])).toEqual(ids.slice(0, hanging));
    expect(await idsIn(["rejected"])).toEqual([ids[hanging + filler]]);
    expect(await entries()).toHaveLength(hanging + 1);
  }, 60_000);
});
