/**
 * The import checker's alarms around a verdict that comes late: after its
 * entry's deadline, after its block left the landed chain, or with a stop
 * between its alarm and its state. Each alarm is raised once, for an entry
 * still in the set, and never lost.
 */
import { type LocalScriptEvalResult } from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { type ImportCheckAlarmSink } from "../src/import-check/alarms.js";
import { runImportCheckPass } from "../src/import-check/checker.js";
import { type ImportCheckEvaluator } from "../src/import-check/evaluator.js";
import { rollBackRows } from "../src/landed-blocks/settlements.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import { testWrite } from "./helpers/driver-recompute.js";
import {
  blockOf,
  checker,
  db,
  hex,
  HOUR_MS,
  idsIn,
  landBlock,
  processBlock,
  useCheckerDatabase,
} from "./import-check.checker.fixture.js";

useCheckerDatabase();

const ACCEPT: LocalScriptEvalResult = {
  kind: "accepted",
  budget: { cpu: 0n, memory: 0n },
};
const REJECT: LocalScriptEvalResult = {
  kind: "script_invalid",
  detail: "refused",
};

/** An evaluator whose runs give `result` once released; `began` resolves
 * when the first run starts. */
const gated = (result: LocalScriptEvalResult, backlogShare = 1) => {
  let release!: () => void;
  const gate = new Promise<void>((resolve) => (release = resolve));
  let started!: () => void;
  const began = new Promise<void>((resolve) => (started = resolve));
  const evaluator: ImportCheckEvaluator = {
    backlogShare,
    evaluate: () =>
      Effect.promise(async () => {
        started();
        await gate;
        return { kind: "evaluated" as const, result };
      }),
  };
  return { evaluator, release, began };
};

const sleep = (ms: number) =>
  new Promise((resolve) => setTimeout(resolve, Math.max(0, ms)));

describe("import checker late verdicts", () => {
  it("raises the near-deadline alarm once, not again each sweep after the deadline passes while its entry is checked", async () => {
    const deadlineMs = Date.now() + HOUR_MS;
    await landBlock(["accept"], { deadlineMs });
    const { evaluator, release, began } = gated(ACCEPT);
    const check = checker(evaluator);
    const pass = check.importPass();
    await began;
    const near = [];
    for (const atMs of [
      deadlineMs - 1,
      deadlineMs + 1_000,
      deadlineMs + 2_000,
      deadlineMs + 3_000,
    ])
      near.push(
        ...(await check.sweep(atMs)).filter(
          (alarm) => alarm.alarm === "unverified_near_deadline",
        ),
      );
    release();
    await pass;
    expect(near).toEqual([expect.objectContaining({ deadlineMs })]);
  }, 60_000);

  it("marks the near-deadline alarm structural on a pool with no backlog worker", async () => {
    const deadlineMs = Date.now() + HOUR_MS;
    await landBlock(["accept"], { deadlineMs });
    const [structural] = await checker(gated(ACCEPT, 0).evaluator).sweep();
    expect(structural).toEqual({
      alarm: "unverified_near_deadline",
      deadlineMs,
      entries: 1,
      projectedMs: Number.MAX_SAFE_INTEGER,
      structural: true,
    });
    const [loaded] = await checker(gated(ACCEPT, 1).evaluator).sweep(
      deadlineMs - 1,
    );
    expect(loaded).toMatchObject({ alarm: "unverified_near_deadline" });
    expect(loaded).not.toHaveProperty("structural");
  }, 60_000);

  it("raises the rejection again after a stop between its alarm and its state", async () => {
    const block = await landBlock(["accept"]);
    const rejecting: ImportCheckEvaluator = {
      backlogShare: 1,
      evaluate: () =>
        Effect.succeed({ kind: "evaluated" as const, result: REJECT }),
    };
    // The node stops as the rejection is raised.
    const stopping: ImportCheckAlarmSink = (alarm) =>
      alarm.alarm === "import_check_rejected"
        ? Effect.die(new Error("stopped"))
        : Effect.void;
    await expect(
      db(runImportCheckPass({ ...checker(rejecting).input, alarm: stopping })),
    ).rejects.toThrow("stopped");
    // After the restart the rejection is raised, and the entry kept.
    expect(await checker(rejecting).importPass()).toEqual([
      {
        alarm: "import_check_rejected",
        headerHash: block.headerHash,
        transactionId: hex(block.txIds[0]!),
        detail: expect.any(String),
      },
    ]);
    expect(await idsIn(["rejected"])).toEqual([hex(block.txIds[0]!)]);
  }, 60_000);

  it("raises no rejection for a block a rebase removed while it was checked", async () => {
    await landBlock(["accept"]);
    const { evaluator, release, began } = gated(REJECT);
    const pass = checker(evaluator).importPass();
    await began;
    await db(testWrite(rollBackRows(await db(retrieveRows), [])));
    release();
    expect(await pass).toEqual([]);
  }, 60_000);

  it("raises the missed alarm for a verdict that lands after its deadline with no sweep in between", async () => {
    const block = await blockOf(["accept"]);
    const deadlineMs = Date.now() + 3_000;
    await processBlock(block, { deadlineMs });
    const { evaluator, release, began } = gated(ACCEPT);
    const pass = checker(evaluator).importPass();
    await began;
    await sleep(deadlineMs - Date.now() + 50);
    release();
    expect(await pass).toEqual([
      {
        alarm: "unverified_deadline_missed",
        headerHash: block.headerHash,
        transactionId: hex(block.txIds[0]!),
        deadlineMs,
        state: "pending",
      },
    ]);
  }, 60_000);
});
