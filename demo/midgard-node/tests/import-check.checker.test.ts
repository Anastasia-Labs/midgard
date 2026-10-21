/**
 * The import checker on the node database and the validation pool: a
 * foreign block's processing records its transactions' script runs in the
 * durable set, and the checker evaluates them off the import path. Its
 * verdicts only raise alarms; an entry leaves the set on an accepting
 * verdict, its block's removal, or its deadline, and the set survives a
 * restart.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { importCheckDeadlineMs } from "../src/import-check/deadline.js";
import { type ImportCheckEvaluator } from "../src/import-check/evaluator.js";
import { rollBackRows } from "../src/landed-blocks/settlements.js";
import {
  markApplied,
  retrieveRows,
  setState,
} from "../src/landed-blocks/store.js";
import { testWrite } from "./helpers/driver-recompute.js";
import {
  CANCELLED,
  checker,
  closePools,
  CONFIG,
  db,
  entries,
  hex,
  idsIn,
  injectedEvaluator,
  KILLED,
  landBlock,
  processBlock,
  realEvaluator,
  REJECTED,
  scriptedEvaluator,
  useCheckerDatabase,
} from "./import-check.checker.fixture.js";
import { committedBlock, fundedBy } from "./import-check.fixture.js";

useCheckerDatabase();

describe("import checker", () => {
  it("alarms on a committed transaction the local evaluator rejects, and keeps it as rejected", async () => {
    const block = await landBlock(["reject", "accept"]);
    expect(await entries(["pending"])).toHaveLength(2);
    const alarms = await checker(await realEvaluator()).importPass();
    expect(alarms).toEqual([
      expect.objectContaining({
        alarm: "import_check_rejected",
        headerHash: block.headerHash,
        transactionId: hex(block.txIds[0]!),
      }),
    ]);
    // The rejected transaction stays for a challenge; the accepted one left.
    expect(await idsIn(["rejected"])).toEqual([hex(block.txIds[0]!)]);
    expect(await entries()).toHaveLength(1);
  }, 60_000);

  it("rejects a run that does not finish within its declared budget", async () => {
    const block = await landBlock(["loop"], { cpu: 10_000_000n });
    expect(await checker(await realEvaluator()).importPass()).toEqual([
      expect.objectContaining({
        alarm: "import_check_rejected",
        transactionId: hex(block.txIds[0]!),
      }),
    ]);
    expect(await idsIn(["rejected"])).toEqual([hex(block.txIds[0]!)]);
  }, 60_000);

  it("rejects a run its evaluator accepts with more cpu than it declared", async () => {
    const block = await landBlock(["accept"], { cpu: 1_000_000_000n });
    const overBudget: ImportCheckEvaluator = {
      backlogShare: 1,
      evaluate: () =>
        Effect.succeed({
          kind: "evaluated",
          result: {
            kind: "accepted",
            budget: { cpu: 1_000_000_001n, memory: 0n },
          },
        }),
    };
    expect(await checker(overBudget).importPass()).toEqual([
      expect.objectContaining({
        alarm: "import_check_rejected",
        transactionId: hex(block.txIds[0]!),
      }),
    ]);
    expect(await idsIn(["rejected"])).toEqual([hex(block.txIds[0]!)]);
  }, 60_000);

  it("checks an honest block with no alarm and leaves no entry", async () => {
    await landBlock(["accept", "accept"]);
    expect(await checker(await realEvaluator()).importPass()).toEqual([]);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("retries a faulting run once on a fresh worker, then alarms and keeps the entry as faulted", async () => {
    const block = await landBlock(["accept"]);
    const injected = await injectedEvaluator({ mode: "crash" });
    const check = checker(injected.evaluator);
    expect(await check.importPass()).toEqual([
      expect.objectContaining({
        alarm: "uplc_import_fault",
        transactionId: hex(block.txIds[0]!),
      }),
    ]);
    expect(injected.jobs()).toBe(2);
    const [first, retry] = injected.threads();
    expect(retry).not.toBe(first);
    expect(await idsIn(["faulted"])).toEqual([hex(block.txIds[0]!)]);
    // A faulted entry is not run again: later passes leave it as it is.
    expect(await check.importPass()).toEqual([]);
    expect(await check.backlog()).toEqual([]);
    expect(await idsIn(["faulted"])).toEqual([hex(block.txIds[0]!)]);
  }, 60_000);

  it("accepts a run that faults once and succeeds on the fresh worker", async () => {
    await landBlock(["accept"]);
    const injected = await injectedEvaluator({ mode: "crash", faults: 1 });
    expect(await checker(injected.evaluator).importPass()).toEqual([]);
    expect(injected.jobs()).toBe(2);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("gives each run of a transaction its own retry on a fresh worker", async () => {
    // One transaction, two script runs; each run's first try faults and its
    // retry is accepted.
    const parent = [fundedBy(0x61, "accept"), fundedBy(0x62, "accept")];
    const block = await committedBlock(parent, [
      {
        input: parent[0]!.outRef,
        extra: [parent[1]!.outRef],
        body: "accept",
        next: "accept",
      },
    ]);
    const processed = await processBlock(block);
    expect(processed.scriptChecks.checks[0]!.runs).toBe(2);
    const injected = await injectedEvaluator({ mode: "crash-even" });
    expect(await checker(injected.evaluator).importPass()).toEqual([]);
    expect(injected.jobs()).toBe(4);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("starts a fresh worker before its kill runs", async () => {
    await landBlock(["accept"]);
    const injected = await injectedEvaluator({
      mode: "crash",
      faults: 1,
      startDelayMs: 1_500,
    });
    // The fresh worker's start takes three times the kill.
    expect(
      await checker(injected.evaluator, {
        ...CONFIG,
        killFloorMs: 500,
      }).importPass(),
    ).toEqual([]);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("kills hanging runs without a verdict, stops at the block's cap, and keeps the skipped transactions", async () => {
    const block = await landBlock(["accept", "accept", "accept", "accept"]);
    // Every script job hangs, so every run that starts is killed.
    const injected = await injectedEvaluator({ mode: "hang" }, { size: 3 });
    const alarms = await checker(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 500,
      blockCapMs: 900,
    }).importPass();
    const killed = alarms.filter(
      (alarm) => alarm.alarm === "import_check_timeout",
    );
    const cap = alarms.filter((alarm) => alarm.alarm === "import_check_cap");
    expect(killed.length).toBeGreaterThanOrEqual(1);
    expect(
      alarms.filter((alarm) => alarm.alarm === "uplc_import_fault"),
    ).toEqual([]);
    expect(cap).toEqual([
      {
        alarm: "import_check_cap",
        headerHash: block.headerHash,
        skipped: 4 - killed.length,
      },
    ]);
    // Each hanging transaction costs one kill, with no retry on a fresh
    // worker, and waits as timed out: a kill is no verdict.
    expect(injected.jobs()).toBe(killed.length);
    expect(await entries(["faulted"])).toEqual([]);
    expect(await entries(["timed_out"])).toHaveLength(killed.length);
    expect(await entries(["cap_skipped"])).toHaveLength(4 - killed.length);
  }, 60_000);

  it("imports and processes the next block, spending a checked block's output, while a pass is still running", async () => {
    const first = await landBlock(["reject", "accept"]);
    const injected = await injectedEvaluator({ mode: "hang" });
    let passDone = false;
    const running = checker(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 2_000,
      blockCapMs: 1_000,
    })
      .importPass()
      .then((alarms) => {
        passDone = true;
        return alarms;
      });
    await new Promise((resolve) => setTimeout(resolve, 200));
    const next = await committedBlock(first.ledger, [
      { input: first.produced[0]!, body: "accept", next: "accept" },
    ]);
    const startedAt = Date.now();
    await processBlock(next, { parentHeaderHash: first.headerHash });
    expect(Date.now() - startedAt).toBeLessThan(2_000);
    expect(passDone).toBe(false);
    expect(
      (await entries(["pending"])).map((entry) => entry.headerHash),
    ).toContain(next.headerHash);
    expect((await running).length).toBeGreaterThan(0);
  }, 60_000);

  it("keeps every entry and its material across a restart", async () => {
    const block = await landBlock(["reject"]);
    const before = await entries(["pending"]);
    // A new pool and new database connections: nothing in memory carries
    // over.
    await closePools();
    const after = await entries(["pending"]);
    expect(after).toHaveLength(1);
    expect(hex(after[0]!.transactionId)).toBe(hex(block.txIds[0]!));
    expect(after).toEqual(before);
    const alarms = await checker(await realEvaluator()).importPass();
    expect(alarms.map((alarm) => alarm.alarm)).toEqual([
      "import_check_rejected",
    ]);
  }, 60_000);

  it("keeps killed, skipped and rejected entries across a restart", async () => {
    const block = await landBlock(["accept", "accept", "accept"]);
    const [rejected, killed] = block.runs.map((runs) =>
      hex(runs[0]!.contextCbor),
    );
    // The first transaction is rejected, the second's run killed, and the
    // third does not start before the block's cap.
    const scripted = scriptedEvaluator((call) =>
      call.context === rejected
        ? REJECTED
        : call.context === killed
          ? KILLED
          : CANCELLED,
    );
    await checker(scripted.evaluator).importPass();
    await closePools();
    const states = (await entries()).map((entry) => entry.state);
    expect(states).toEqual(["rejected", "timed_out", "cap_skipped"]);
  }, 60_000);

  it("drops entries at their deadline, half the block maturity after the block's end, alarming only for unverified ones", async () => {
    const endTime = BigInt(Date.now());
    const deadlineMs = importCheckDeadlineMs(endTime);
    expect(deadlineMs).toBe(
      Number(endTime) +
        Math.floor(MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs / 2),
    );
    const rejected = await landBlock(["reject"], { deadlineMs });
    // `rejected`'s transaction is rejected locally.
    await checker(await realEvaluator()).importPass();
    expect(await idsIn(["rejected"])).toEqual([hex(rejected.txIds[0]!)]);
    const unverified = await landBlock(["accept"], {
      deadlineMs,
      parentHeaderHash: rejected.headerHash,
      firstByte: 0x70,
    });
    // `removed`'s block leaves the landed chain.
    const removed = await landBlock(["accept"], {
      deadlineMs,
      parentHeaderHash: unverified.headerHash,
      firstByte: 0x78,
    });
    await db(testWrite(markApplied([removed.headerHash])));
    await db(testWrite(setState([removed.headerHash], "removed")));
    expect(await entries(["pending"])).toHaveLength(1);
    const sweep = checker(undefined);
    expect(await sweep.sweep(deadlineMs - 1)).toEqual([]);
    expect(await sweep.sweep(deadlineMs)).toEqual([
      {
        alarm: "unverified_deadline_missed",
        headerHash: unverified.headerHash,
        transactionId: hex(unverified.txIds[0]!),
        deadlineMs,
        state: "pending",
      },
    ]);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("drops the entries of a block that leaves the landed chain", async () => {
    await landBlock(["reject"]);
    const rows = await db(retrieveRows);
    await db(testWrite(rollBackRows(rows, [])));
    expect(await entries()).toEqual([]);
    expect(await checker(await realEvaluator()).importPass()).toEqual([]);
  }, 60_000);
});
