/**
 * The import checker on the node database and the validation pool: a
 * foreign block's processing records its transactions' script runs in the
 * durable set, and the checker evaluates them off the import path. Its
 * verdicts only raise alarms; an entry leaves the set by its verdict, its
 * block's removal, or its deadline, and the set survives a restart.
 */
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect, TestClock, TestContext } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  type ImportCheckState,
  retrieveChecks,
} from "../src/database/importChecks.js";
import { type ImportCheckAlarm } from "../src/import-check/alarms.js";
import { runImportCheckPass } from "../src/import-check/checker.js";
import { importCheckDeadlineMs } from "../src/import-check/deadline.js";
import {
  type ImportCheckEvaluator,
  poolImportCheckEvaluator,
} from "../src/import-check/evaluator.js";
import { processRow, rollBackRows } from "../src/landed-blocks/settlements.js";
import { retrieveRows } from "../src/landed-blocks/store.js";
import { type ImportCheckConfig } from "../src/services/config.import-check.js";
import { Globals } from "../src/services/globals.js";
import {
  evaluateImportCheckOn,
  FixedValidationWorkerPool,
} from "../src/services/validation-pool.js";
import type { ValidationWorkerInit } from "../src/workers/utils/validation-pool.js";
import { testWrite } from "./helpers/driver-recompute.js";
import {
  committedBlock,
  fundedBy,
  type ScriptBody,
} from "./import-check.fixture.js";
import { landedRow, run } from "./landed-blocks-rebase.fixture.js";
import { resetApplicationTables } from "./utils.js";
import {
  init,
  workerEntry,
} from "./validation-worker-pool.build-adversarial-corpus.js";

let globals: Globals;
const db = <A, E>(effect: Parameters<typeof run<A, E>>[1]) =>
  run(globals, effect);

const pools: FixedValidationWorkerPool[] = [];
beforeEach(async () => {
  globals = await Effect.runPromise(Effect.provide(Globals, Globals.Default));
  await db(resetApplicationTables);
});
afterEach(() => Promise.all(pools.splice(0).map((pool) => pool.close())));

const HOUR_MS = 3_600_000;
const CONFIG: ImportCheckConfig = {
  killFloorMs: 5_000,
  killMsPerGigaCpu: 0,
  killUsPerKib: 0,
  blockCapMs: 60_000,
};

/** The production pool on the production worker. */
const realEvaluator = async () => {
  const pool = new FixedValidationWorkerPool(2, 8, 30_000, workerEntry, init);
  pools.push(pool);
  await pool.start();
  return poolImportCheckEvaluator({
    evaluateImportCheck: evaluateImportCheckOn(pool),
  })!;
};

/** A pool on the test worker (`fixtures/import-check-worker.mjs`); `log`
 * holds the number of script jobs, then each job's worker thread id. */
const injectedEvaluator = async (
  mode: "hang" | "crash" | "crash-first" | "accept",
  options: { readonly size?: number; readonly startDelayMs?: number } = {},
) => {
  const log = new Int32Array(new SharedArrayBuffer(4 * 256));
  const pool = new FixedValidationWorkerPool(
    options.size ?? 2,
    8,
    30_000,
    pathToFileURL(resolve("tests/fixtures/import-check-worker.mjs")),
    {
      mode,
      log,
      startDelayMs: options.startDelayMs ?? 0,
    } as unknown as ValidationWorkerInit,
  );
  pools.push(pool);
  await pool.start();
  const evaluator = poolImportCheckEvaluator({
    evaluateImportCheck: evaluateImportCheckOn(pool),
  })!;
  return {
    evaluator,
    jobs: () => Atomics.load(log, 0),
    threads: () => [...log.slice(1, Atomics.load(log, 0) + 1)],
  };
};

/** Imports `block` and processes it as a landed child of `parentHeaderHash`,
 * with its checks due at `deadlineMs`. */
const processBlock = async (
  block: Awaited<ReturnType<typeof committedBlock>>,
  options: {
    readonly parentHeaderHash?: string;
    readonly deadlineMs?: number;
  } = {},
) => {
  const result = await block.imported();
  if (result._tag === "Left") throw result.left;
  await db(
    testWrite(
      processRow(
        landedRow({
          headerHash: block.headerHash,
          ...(options.parentHeaderHash === undefined
            ? {}
            : { parentHeaderHash: options.parentHeaderHash }),
        }),
        {
          deadlineMs: options.deadlineMs ?? Date.now() + HOUR_MS,
          checks: result.right.scriptChecks,
        },
      ),
    ),
  );
  return { ...block, scriptChecks: result.right.scriptChecks };
};

/** A foreign block of `bodies` spends, imported and processed. */
const landBlock = async (
  bodies: readonly ScriptBody[],
  options: { readonly deadlineMs?: number } = {},
) => {
  const parent = bodies.map((body, index) => fundedBy(0x50 + index, body));
  const block = await committedBlock(
    parent,
    bodies.map((body, index) => ({
      input: parent[index]!.outRef,
      body,
      next: "accept" as const,
    })),
  );
  return processBlock(block, options);
};

const pass = async (
  evaluator: ImportCheckEvaluator | undefined,
  config: ImportCheckConfig = CONFIG,
) => {
  const alarms: ImportCheckAlarm[] = [];
  await db(
    runImportCheckPass({
      evaluator,
      config,
      alarm: (alarm) => Effect.sync(() => void alarms.push(alarm)),
    }),
  );
  return alarms;
};

const entries = (
  states: readonly ImportCheckState[] = ["pending", "cap_skipped", "faulted"],
) => db(retrieveChecks(states));

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

describe("import checker", () => {
  it("alarms on a committed transaction the local evaluator rejects, and the entry leaves the set", async () => {
    const block = await landBlock(["reject", "accept"]);
    expect(await entries(["pending"])).toHaveLength(2);
    const alarms = await pass(await realEvaluator());
    expect(alarms).toEqual([
      expect.objectContaining({
        alarm: "import_check_rejected",
        headerHash: block.headerHash,
        transactionId: hex(block.txIds[0]!),
      }),
    ]);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("checks an honest block with no alarm and leaves no entry", async () => {
    await landBlock(["accept", "accept"]);
    expect(await pass(await realEvaluator())).toEqual([]);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("retries a faulting run once on a fresh worker, then alarms and keeps the entry as faulted", async () => {
    const block = await landBlock(["accept"]);
    const injected = await injectedEvaluator("crash");
    const alarms = await pass(injected.evaluator);
    expect(alarms).toEqual([
      expect.objectContaining({
        alarm: "uplc_import_fault",
        transactionId: hex(block.txIds[0]!),
      }),
    ]);
    expect(injected.jobs()).toBe(2);
    const [first, retry] = injected.threads();
    expect(retry).not.toBe(first);
    expect(
      (await entries(["faulted"])).map((entry) => hex(entry.transactionId)),
    ).toEqual([hex(block.txIds[0]!)]);
    // A kill or crash is never a verdict: a later pass leaves it as it is.
    expect(await pass(injected.evaluator)).toEqual([]);
    expect(await entries(["faulted"])).toHaveLength(1);
  }, 60_000);

  it("accepts a run that faults once and succeeds on the fresh worker", async () => {
    await landBlock(["accept"]);
    const injected = await injectedEvaluator("crash-first");
    expect(await pass(injected.evaluator)).toEqual([]);
    expect(injected.jobs()).toBe(2);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("starts a fresh worker before its kill runs", async () => {
    await landBlock(["accept"]);
    const injected = await injectedEvaluator("crash-first", {
      startDelayMs: 1_500,
    });
    // The fresh worker's start takes three times the kill.
    expect(
      await pass(injected.evaluator, { ...CONFIG, killFloorMs: 500 }),
    ).toEqual([]);
    expect(await entries()).toEqual([]);
  }, 60_000);

  it("kills hanging runs, stops at the block's cap, and keeps the skipped transactions", async () => {
    const block = await landBlock(["accept", "accept", "accept", "accept"]);
    const injected = await injectedEvaluator("hang");
    const startedAt = Date.now();
    const alarms = await pass(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 300,
      blockCapMs: 500,
    });
    expect(Date.now() - startedAt).toBeLessThan(10_000);
    const faults = alarms.filter(
      (alarm) => alarm.alarm === "uplc_import_fault",
    );
    const cap = alarms.filter((alarm) => alarm.alarm === "import_check_cap");
    expect(faults.length).toBeGreaterThanOrEqual(1);
    expect(cap).toEqual([
      {
        alarm: "import_check_cap",
        headerHash: block.headerHash,
        skipped: 4 - faults.length,
      },
    ]);
    // Each hanging transaction costs at most one kill and one retry.
    expect(injected.jobs()).toBe(2 * faults.length);
    expect(await entries(["faulted"])).toHaveLength(faults.length);
    expect(await entries(["cap_skipped"])).toHaveLength(4 - faults.length);
  }, 60_000);

  it("imports and processes the next block, spending a checked block's output, while a pass is still running", async () => {
    const first = await landBlock(["reject", "accept"]);
    const injected = await injectedEvaluator("hang");
    let passDone = false;
    const running = pass(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 2_000,
      blockCapMs: 1_000,
    }).then((alarms) => {
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

  it("keeps every entry and its runs across a restart", async () => {
    const block = await landBlock(["reject"]);
    const before = await entries(["pending"]);
    // A new pool and new database connections: nothing in memory carries
    // over.
    await Promise.all(pools.splice(0).map((pool) => pool.close()));
    const after = await entries(["pending"]);
    expect(after).toHaveLength(1);
    expect(hex(after[0]!.transactionId)).toBe(hex(block.txIds[0]!));
    expect(after[0]!.runs.map((run) => hex(run.scriptBytes))).toEqual(
      block.scriptChecks[0]!.runs.map((run) => hex(run.scriptBytes)),
    );
    expect(after).toEqual(before);
    const alarms = await pass(await realEvaluator());
    expect(alarms.map((alarm) => alarm.alarm)).toEqual([
      "import_check_rejected",
    ]);
  }, 60_000);

  it("keeps faulted and skipped entries across a restart", async () => {
    await landBlock(["accept", "accept", "accept"]);
    const injected = await injectedEvaluator("hang");
    await pass(injected.evaluator, {
      ...CONFIG,
      killFloorMs: 200,
      blockCapMs: 100,
    });
    await Promise.all(pools.splice(0).map((pool) => pool.close()));
    const states = (await entries()).map((entry) => entry.state).sort();
    expect(states).toEqual(["cap_skipped", "cap_skipped", "faulted"]);
  }, 60_000);

  it("drops an entry at its deadline, half the block maturity after the block's end, with an alarm", async () => {
    const endTime = BigInt(Date.now());
    const deadlineMs = importCheckDeadlineMs(endTime);
    expect(deadlineMs).toBe(
      Number(endTime) +
        Math.floor(MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs / 2),
    );
    const block = await landBlock(["accept"], { deadlineMs });
    const at = async (nowMs: number) => {
      const alarms: ImportCheckAlarm[] = [];
      await db(
        TestClock.setTime(nowMs).pipe(
          Effect.zipRight(
            runImportCheckPass({
              evaluator: undefined,
              config: CONFIG,
              alarm: (alarm) => Effect.sync(() => void alarms.push(alarm)),
            }),
          ),
          Effect.provide(TestContext.TestContext),
        ),
      );
      return alarms;
    };
    expect(await at(deadlineMs - 1)).toEqual([]);
    expect(await entries()).toHaveLength(1);
    expect(await at(deadlineMs)).toEqual([
      {
        alarm: "unverified_deadline_missed",
        headerHash: block.headerHash,
        transactionId: hex(block.txIds[0]!),
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
    expect(await pass(await realEvaluator())).toEqual([]);
  }, 60_000);
});
