/**
 * The import checker's bounds past the import pass: the negative cache, the
 * backlog lane and the projected-completion alarm. A transaction skipped at
 * import (past its block's cap, or running a cached script) still gets its
 * runs from the backlog lane, earliest deadline first; the sweep alarms
 * while every entry due by a deadline can still be served.
 */
import { describe, expect, it } from "vitest";

import { type ImportCheckAlarm } from "../src/import-check/alarms.js";
import { IMPORT_CHECK_TIMEOUT_RETRIES } from "../src/import-check/checker.js";
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
  it("skips the import-time runs of a script whose worker failed twice, and the backlog lane still gives each transaction a verdict", async () => {
    const first = await landBlock(["accept"]);
    // The first two runs fail, every later one is accepted.
    const injected = await injectedEvaluator({ mode: "crash", faults: 2 });
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
  }, 60_000);

  it("does not fault or cache a transaction whose runs are killed while its worker is stalled, and decides it once the worker recovers", async () => {
    const t0 = Date.now();
    // The worker stalls on the first two jobs, the run and its retry on a
    // fresh worker, so both are killed; it answers every later job.
    const injected = await injectedEvaluator({ mode: "hang", faults: 2 });
    const check = checker(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 300,
    });
    const first = await landBlock(["accept"]);
    const alarms = await check.importPass(t0);
    expect(injected.jobs()).toBe(2);
    expect(named(alarms, "uplc_import_fault")).toEqual([]);
    expect(named(alarms, "import_check_timeout")).toEqual([
      expect.objectContaining({ transactionId: hex(first.txIds[0]!) }),
    ]);
    expect(await idsIn(["faulted", "rejected"])).toEqual([]);
    // Not cached: the next transaction running the script is run at import.
    await landBlock(["accept"], {
      parentHeaderHash: first.headerHash,
      firstByte: 0x60,
    });
    expect(await check.importPass(t0)).toEqual([]);
    expect(injected.jobs()).toBe(3);
    expect(await idsIn(["cache_skipped"])).toEqual([]);
    // Undecided, it waits for its retry; once that is due, the recovered
    // worker decides it.
    expect(await idsIn(["pending"])).toEqual([hex(first.txIds[0]!)]);
    expect(await check.backlog(t0 + 60_000)).toEqual([]);
    expect(injected.jobs()).toBe(4);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("runs a transaction whose runs are always killed a bounded number of times, then leaves it unrun, neither faulted nor cached", async () => {
    // The worker never answers a run of this transaction.
    const injected = await injectedEvaluator({ mode: "hang" }, { size: 3 });
    const check = checker(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 200,
    });
    const block = await landBlock(["accept"]);
    // Backlog passes on a virtual clock, each past the previous retry and
    // all before the entry's deadline.
    const timeouts: ImportCheckAlarm[] = [];
    let nowMs = Date.now();
    for (let pass = 0; pass <= IMPORT_CHECK_TIMEOUT_RETRIES + 1; pass += 1) {
      timeouts.push(
        ...named(await check.backlog(nowMs), "import_check_timeout"),
      );
      nowMs += 100_000;
    }
    expect(
      timeouts.map(
        (alarm) => alarm.alarm === "import_check_timeout" && alarm.timeouts,
      ),
    ).toEqual(
      Array.from(
        { length: IMPORT_CHECK_TIMEOUT_RETRIES + 1 },
        (_, index) => index + 1,
      ),
    );
    expect(timeouts.at(-1)).not.toHaveProperty("retryAtMs");
    expect(injected.jobs()).toBe(2 * (IMPORT_CHECK_TIMEOUT_RETRIES + 1));
    expect(await idsIn(["pending"])).toEqual([hex(block.txIds[0]!)]);
    expect(await idsIn(["faulted", "cache_skipped"])).toEqual([]);
  }, 60_000);

  it("still rejects a run over its declared budget and faults and caches a failing worker's script, while a killed run waits for its retry", async () => {
    const block = await blockOf(["accept", "accept", "accept"]);
    const [, overBudget, failing] = firstRuns(block);
    const injected = await injectedEvaluator(
      {
        mode: "hang",
        overBudgetContexts: [overBudget!.contextCbor],
        crashContexts: [failing!.contextCbor],
      },
      { size: 3 },
    );
    await processBlock(block);
    const check = checker(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 300,
    });
    const ids = block.txIds.map((txId) => hex(txId));
    const t0 = Date.now();
    const alarms = await check.importPass(t0);
    expect(
      alarms.map((alarm) => [
        alarm.alarm,
        "transactionId" in alarm ? alarm.transactionId : undefined,
      ]),
    ).toEqual([
      ["import_check_timeout", ids[0]],
      ["import_check_rejected", ids[1]],
      ["uplc_import_fault", ids[2]],
    ]);
    expect(await idsIn(["pending"])).toEqual([ids[0]]);
    expect(await idsIn(["rejected"])).toEqual([ids[1]]);
    expect(await idsIn(["faulted"])).toEqual([ids[2]]);
    // The failing worker's script is cached: a later transaction running
    // it is not run at import.
    const later = await landBlock(["accept"], {
      parentHeaderHash: block.headerHash,
      firstByte: 0x60,
    });
    expect(await check.importPass(t0)).toEqual([]);
    expect(await idsIn(["cache_skipped"])).toEqual([hex(later.txIds[0]!)]);
  }, 60_000);

  it("does not fault or cache an honest transaction with a large script and a low declared budget", async () => {
    const real = await realEvaluator(2);
    // A kill with no byte term: the large script's runs are killed before
    // they can decode it.
    const check = checker(real, {
      ...DEFAULT_IMPORT_CHECK_CONFIG,
      killFloorMs: 1,
      killUsPerKib: 0,
    });
    const first = await landBlock(["large"], { cpu: 1_000_000n });
    expect(named(await check.importPass(), "uplc_import_fault")).toEqual([]);
    expect(await idsIn(["faulted", "rejected"])).toEqual([]);
    // Not cached: the next transaction running it is run at import.
    await landBlock(["large"], {
      cpu: 1_000_000n,
      parentHeaderHash: first.headerHash,
      firstByte: 0x60,
    });
    expect(named(await check.importPass(), "uplc_import_fault")).toEqual([]);
    expect(await idsIn(["faulted", "rejected", "cache_skipped"])).toEqual([]);
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

  /** A cached script (its first transaction's worker failed twice), then a
   * block of `FLOOD` transactions failing under it and one rejected, last,
   * due `deadlineInMs` after `processedAtMs`, when it is processed. */
  const floodUnderCachedScript = async (deadlineInMs: number) => {
    const seed = await blockOf(["accept"]);
    const flood = await blockOf(accepts(FLOOD + 1), { firstByte: 0x60 });
    const fraud = firstRuns(flood)[FLOOD]!;
    const injected = await injectedEvaluator({
      mode: "crash",
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
    const processedAtMs = Date.now();
    const deadlineMs = processedAtMs + deadlineInMs;
    await processBlock(flood, {
      parentHeaderHash: seed.headerHash,
      deadlineMs,
    });
    expect(await check.importPass()).toEqual([]);
    expect(await entries(["cache_skipped"])).toHaveLength(FLOOD + 1);
    return { check, flood, deadlineMs, processedAtMs };
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
    // Two kills of each of the five entries take about a second. The sweep
    // runs on a virtual clock at the instant the flood is processed, 600 ms
    // before its deadline, however long the setup took.
    const { check, deadlineMs, processedAtMs } =
      await floodUnderCachedScript(600);
    const alarms = await check.sweep(processedAtMs);
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
    // The first hanging transaction's two kills use up the block's cap; a
    // kill is no verdict, so it stays pending.
    const importAlarms = await checker(before.evaluator, config).importPass();
    expect(named(importAlarms, "uplc_import_fault")).toEqual([]);
    expect(named(importAlarms, "import_check_timeout")).toHaveLength(1);
    expect(named(importAlarms, "import_check_cap")).toEqual([
      expect.objectContaining({ skipped: hanging - 1 + filler + 1 }),
    ]);
    // A restart: a new pool and cache; the set is read back as it was.
    await closePools();
    expect(await entries(["cap_skipped"])).toHaveLength(hanging + filler);
    expect(await entries(["pending"])).toHaveLength(1);
    const after = await injectedEvaluator(worker, { size: 3 });
    const check = checker(after.evaluator, config);
    expect(await check.sweep()).toEqual([]);
    // On a virtual clock: no killed run's retry falls due during the pass.
    const alarms = await check.backlog(Date.now());
    expect(Date.now()).toBeLessThan(deadlineMs);
    expect(named(alarms, "import_check_rejected")).toEqual([
      expect.objectContaining({
        transactionId: hex(block.txIds[hanging + filler]!),
      }),
    ]);
    const ids = block.txIds.map((txId) => hex(txId));
    expect(
      named(alarms, "import_check_timeout").map((alarm) =>
        "transactionId" in alarm ? alarm.transactionId : undefined,
      ),
    ).toEqual(ids.slice(0, hanging));
    expect(await idsIn(["faulted"])).toEqual([]);
    expect(await idsIn(["pending", "cap_skipped"])).toEqual(
      ids.slice(0, hanging),
    );
    expect(await idsIn(["rejected"])).toEqual([ids[hanging + filler]]);
    expect(await entries()).toHaveLength(hanging + 1);
  }, 60_000);
});
