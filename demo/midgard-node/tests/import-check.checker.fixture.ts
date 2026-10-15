/**
 * The import checker on the node database and the validation pool: foreign
 * blocks imported and processed (their checks recorded in the durable set),
 * evaluators on the production worker or on the test worker
 * (`fixtures/import-check-worker.mjs`), and the checker's three passes.
 * Call `useCheckerDatabase()` once per suite.
 */
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

import { type MidgardCekVerdictRequest } from "@al-ft/midgard-validation";
import { Effect, TestClock, TestContext } from "effect";
import { afterEach, beforeEach } from "vitest";

import {
  type ImportCheckState,
  retrieveChecks,
} from "../src/database/importChecks.js";
import { type ImportCheckAlarm } from "../src/import-check/alarms.js";
import {
  BACKLOG_BATCH,
  type ImportCheckPassInput,
  runBacklogPass,
  runImportCheckPass,
  runUnverifiedSweep,
} from "../src/import-check/checker.js";
import {
  type ImportCheckEvaluator,
  poolImportCheckEvaluator,
} from "../src/import-check/evaluator.js";
import { makeNegativeCache } from "../src/import-check/negative-cache.js";
import { processRow } from "../src/landed-blocks/settlements.js";
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
  type Spend,
} from "./import-check.fixture.js";
import { landedRow, run } from "./landed-blocks-rebase.fixture.js";
import { resetApplicationTables } from "./utils.js";
import {
  init,
  workerEntry,
} from "./validation-worker-pool.build-adversarial-corpus.js";

let globals: Globals;
export const db = <A, E>(effect: Parameters<typeof run<A, E>>[1]) =>
  run(globals, effect);

const pools: FixedValidationWorkerPool[] = [];

/** A fresh database per test, and every pool closed after it. */
export const useCheckerDatabase = () => {
  beforeEach(async () => {
    globals = await Effect.runPromise(Effect.provide(Globals, Globals.Default));
    await db(resetApplicationTables);
  });
  afterEach(() => closePools());
};

/** Closes every pool: with new database connections, a restart. */
export const closePools = () =>
  Promise.all(pools.splice(0).map((pool) => pool.close()));

export const HOUR_MS = 3_600_000;
export const CONFIG: ImportCheckConfig = {
  killFloorMs: 5_000,
  killMsPerGigaCpu: 0,
  killUsPerKib: 0,
  blockCapMs: 60_000,
  negativeCacheTtlMs: HOUR_MS,
  backlogWorkers: 1,
  openerMarginMs: 0,
};

/** The production pool on the production worker. */
export const realEvaluator = async (size = 2) => {
  const pool = new FixedValidationWorkerPool(
    size,
    8,
    30_000,
    workerEntry,
    init,
  );
  pools.push(pool);
  await pool.start();
  const evaluator = poolImportCheckEvaluator({
    evaluateImportCheck: evaluateImportCheckOn(pool),
  })!;
  return Object.assign(evaluator, { pool });
};

export type InjectedWorker = Readonly<{
  mode: "hang" | "crash" | "crash-even" | "accept";
  faults?: number;
  startDelayMs?: number;
  rejectContexts?: readonly Uint8Array[];
  acceptContexts?: readonly Uint8Array[];
  slowContexts?: readonly Uint8Array[];
  slowMs?: number;
}>;

/** A pool on the test worker; `jobs()` counts the script jobs decided by
 * the worker's mode, `threads()` their worker thread ids. */
export const injectedEvaluator = async (
  worker: InjectedWorker,
  options: { readonly size?: number; readonly backlogShare?: number } = {},
) => {
  const log = new Int32Array(new SharedArrayBuffer(4 * 256));
  const hexes = (contexts: readonly Uint8Array[] = []) =>
    contexts.map((context) => Buffer.from(context).toString("hex"));
  const pool = new FixedValidationWorkerPool(
    options.size ?? 2,
    8,
    30_000,
    pathToFileURL(resolve("tests/fixtures/import-check-worker.mjs")),
    {
      ...worker,
      log,
      rejectContexts: hexes(worker.rejectContexts),
      acceptContexts: hexes(worker.acceptContexts),
      slowContexts: hexes(worker.slowContexts),
    } as unknown as ValidationWorkerInit,
    options.backlogShare,
  );
  pools.push(pool);
  await pool.start();
  const evaluator = poolImportCheckEvaluator({
    evaluateImportCheck: evaluateImportCheckOn(pool),
  })!;
  return {
    evaluator,
    pool,
    jobs: () => Atomics.load(log, 0),
    threads: () => [...log.slice(1, Atomics.load(log, 0) + 1)],
  };
};

export type Block = Awaited<ReturnType<typeof committedBlock>>;

/** `block`'s import result: its script checks. */
export const checksOf = async (block: Block) => {
  const result = await block.imported();
  if (result._tag === "Left") throw result.left;
  return result.right.scriptChecks;
};

/** The first run of each of `block`'s transactions. */
export const firstRuns = async (
  block: Block,
): Promise<MidgardCekVerdictRequest[]> =>
  (await checksOf(block)).map((check) => check.runs[0]!);

/** Imports `block` and processes it as a landed child of `parentHeaderHash`,
 * with its checks due at `deadlineMs`. */
export const processBlock = async (
  block: Block,
  options: {
    readonly parentHeaderHash?: string;
    readonly deadlineMs?: number;
  } = {},
) => {
  const scriptChecks = await checksOf(block);
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
          checks: scriptChecks,
        },
      ),
    ),
  );
  return { ...block, scriptChecks };
};

/** A foreign block of one spend per body, each from its own funded input
 * (`firstByte` onwards), not yet processed. */
export const blockOf = (
  bodies: readonly ScriptBody[],
  options: { readonly firstByte?: number; readonly cpu?: bigint } = {},
) => {
  const parent = bodies.map((body, index) =>
    fundedBy((options.firstByte ?? 0x50) + index, body),
  );
  return committedBlock(
    parent,
    bodies.map(
      (body, index): Spend => ({
        input: parent[index]!.outRef,
        body,
        next: "accept",
        ...(options.cpu === undefined ? {} : { cpu: options.cpu }),
      }),
    ),
  );
};

/** A foreign block of `bodies` spends, imported and processed. */
export const landBlock = async (
  bodies: readonly ScriptBody[],
  options: {
    readonly deadlineMs?: number;
    readonly parentHeaderHash?: string;
    readonly firstByte?: number;
    readonly cpu?: bigint;
  } = {},
) => processBlock(await blockOf(bodies, options), options);

/** A checker on `evaluator` with one negative cache across its passes;
 * each pass returns the alarms it raised. `at` runs a pass at a test clock
 * time. */
export const checker = (
  evaluator: ImportCheckEvaluator | undefined,
  config: ImportCheckConfig = CONFIG,
) => {
  const alarms: ImportCheckAlarm[] = [];
  const input: ImportCheckPassInput = {
    evaluator,
    config,
    alarm: (alarm) => Effect.sync(() => void alarms.push(alarm)),
    cache: makeNegativeCache(config.negativeCacheTtlMs),
  };
  const alarmed = new Set<number>();
  const raised = async <A, E>(effect: Parameters<typeof run<A, E>>[1]) => {
    await db(effect);
    return alarms.splice(0);
  };
  const at = <A, E, R>(nowMs: number, effect: Effect.Effect<A, E, R>) =>
    TestClock.setTime(nowMs).pipe(
      Effect.zipRight(effect),
      Effect.provide(TestContext.TestContext),
    );
  return {
    importPass: (nowMs?: number) =>
      raised(
        nowMs === undefined
          ? runImportCheckPass(input)
          : at(nowMs, runImportCheckPass(input)),
      ),
    /** Backlog passes until one reads less than a full batch. */
    backlog: () =>
      raised(
        runBacklogPass(input).pipe(
          Effect.repeat({ while: (read) => read === BACKLOG_BATCH }),
        ),
      ),
    sweep: (nowMs?: number) =>
      raised(
        nowMs === undefined
          ? runUnverifiedSweep(input, alarmed)
          : at(nowMs, runUnverifiedSweep(input, alarmed)),
      ),
  };
};

export const ALL_STATES: readonly ImportCheckState[] = [
  "pending",
  "cap_skipped",
  "cache_skipped",
  "faulted",
  "rejected",
];

export const entries = (states: readonly ImportCheckState[] = ALL_STATES) =>
  db(retrieveChecks(states));

/** The transaction ids of the entries in `states`, in set order. */
export const idsIn = async (states: readonly ImportCheckState[]) =>
  (await entries(states)).map((entry) => hex(entry.transactionId));

export const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
