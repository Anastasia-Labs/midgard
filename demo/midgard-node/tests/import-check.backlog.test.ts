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
  CONFIG,
  entries,
  firstRuns,
  hex,
  HOUR_MS,
  idsIn,
  injectedEvaluator,
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
    const check = checker(injected.evaluator);
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
    const check = checker(injected.evaluator);
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
    const { check, flood, processedAtMs } =
      await floodUnderCachedScript(HOUR_MS);
    expect(await check.sweep(processedAtMs)).toEqual([]);
    // On a virtual clock at the instant the flood is processed: every
    // verdict comes before the deadline.
    const alarms = await check.backlog(processedAtMs);
    expect(named(alarms, "unverified_deadline_missed")).toEqual([]);
    expect(named(alarms, "uplc_import_fault")).toHaveLength(FLOOD);
    expect(named(alarms, "import_check_rejected")).toEqual([
      expect.objectContaining({ transactionId: hex(flood.txIds[FLOOD]!) }),
    ]);
    expect(await idsIn(["rejected"])).toEqual([hex(flood.txIds[FLOOD]!)]);
  }, 60_000);

  it("raises the projected alarm before the deadline when the flood cannot be served in time", async () => {
    // Two kills of each of the five entries take about 50 s. The sweep runs
    // on a virtual clock at the instant the flood is processed, 600 ms
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
    // On a virtual clock at `t0`, however long the setup takes.
    const t0 = Date.now();
    const soonMs = t0 + 6_000;
    const laterMs = t0 + 60_000;
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
    expect(named(await check.importPass(t0), "import_check_cap")).toHaveLength(
      2,
    );
    const alarms = await check.sweep(t0);
    expect(alarms).toEqual([
      expect.objectContaining({
        alarm: "unverified_near_deadline",
        deadlineMs: soonMs,
        entries: count,
      }),
    ]);
    expect(await check.sweep(t0)).toEqual([]);
    // All of them are still served before the deadline.
    expect(await check.backlog(t0)).toEqual([]);
    expect(await entries()).toEqual([]);
  }, 60_000);
});
