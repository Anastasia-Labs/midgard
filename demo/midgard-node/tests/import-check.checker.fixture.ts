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
import { Clock, Effect, TestClock, TestContext } from "effect";
import { afterEach, beforeEach } from "vitest";

import {
  type ImportCheckState,
  retrieveChecks,
} from "../src/database/importChecks.js";
import { type ImportCheckAlarm } from "../src/import-check/alarms.js";
import {
  type ImportCheckPassInput,
  makeImportCheckRuns,
  runBacklogPass,
  runImportCheckPass,
  runUnverifiedSweep,
} from "../src/import-check/checker.js";
import {
  type ImportCheckEvaluator,
  type ImportCheckOutcome,
  poolImportCheckEvaluator,
} from "../src/import-check/evaluator.js";
import { makeNegativeCache } from "../src/import-check/negative-cache.js";
import { processRow } from "../src/landed-blocks/settlements.js";
import { type ImportCheckConfig } from "../src/services/config.import-check.js";
import { Globals } from "../src/services/globals.js";
import {
  evaluateImportCheckOn,
  FixedValidationWorkerPool,
  type ImportCheckLane,
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
    importCheckBacklogShare: pool.backlogShare,
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
  overBudgetContexts?: readonly Uint8Array[];
  crashContexts?: readonly Uint8Array[];
  /** Only this many worker starts succeed; every later one fails. */
  startsAllowed?: number;
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
      overBudgetContexts: hexes(worker.overBudgetContexts),
      crashContexts: hexes(worker.crashContexts),
      starts: new Int32Array(new SharedArrayBuffer(4)),
    } as unknown as ValidationWorkerInit,
    options.backlogShare,
  );
  pools.push(pool);
  await pool.start();
  const evaluator = poolImportCheckEvaluator({
    evaluateImportCheck: evaluateImportCheckOn(pool),
    importCheckBacklogShare: pool.backlogShare,
  })!;
  return {
    evaluator,
    pool,
    jobs: () => Atomics.load(log, 0),
    threads: () => [...log.slice(1, Atomics.load(log, 0) + 1)],
  };
};

/** One run the scripted evaluator was asked for: its context (hex), lane,
 * whether on a fresh instance, and the (test) clock time. */
export type ScriptedCall = Readonly<{
  context: string;
  lane: ImportCheckLane;
  fresh: boolean;
  atMs: number;
}>;

export const ACCEPTED: ImportCheckOutcome = {
  kind: "evaluated",
  result: { kind: "accepted", budget: { cpu: 0n, memory: 0n } },
};
export const OVER_BUDGET: ImportCheckOutcome = {
  kind: "evaluated",
  result: { kind: "accepted", budget: { cpu: 10n ** 12n, memory: 0n } },
};
export const REJECTED: ImportCheckOutcome = {
  kind: "evaluated",
  result: { kind: "script_invalid", detail: "scripted rejection" },
};
export const KILLED: ImportCheckOutcome = {
  kind: "timeout",
  detail: "scripted kill",
};
export const FAULT: ImportCheckOutcome = {
  kind: "fault",
  detail: "scripted fault",
};
export const CANCELLED: ImportCheckOutcome = { kind: "cancelled" };

/** An evaluator with no pool: each run's outcome is `decide(call)`, and
 * `calls` records every run asked for, so a test controls the outcome and
 * the clock with no wall-clock wait. */
export const scriptedEvaluator = (
  decide: (call: ScriptedCall) => ImportCheckOutcome,
) => {
  const calls: ScriptedCall[] = [];
  const evaluator: ImportCheckEvaluator = {
    backlogShare: 1,
    evaluate: (run, options) =>
      Effect.gen(function* () {
        const call = {
          context: hex(run.contextCbor),
          lane: options.lane,
          fresh: options.fresh,
          atMs: yield* Clock.currentTimeMillis,
        };
        calls.push(call);
        return decide(call);
      }),
  };
  return { evaluator, calls };
};

export type Block = Awaited<ReturnType<typeof committedBlock>>;

/** `block`'s import result: its script checks. */
export const checksOf = async (block: Block) => {
  const result = await block.imported();
  if (result._tag === "Left") throw result.left;
  return result.right.scriptChecks;
};

/** The first run of each of `block`'s transactions. */
export const firstRuns = (block: Block): MidgardCekVerdictRequest[] =>
  block.runs.map((runs) => runs[0]!);

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
    runs: makeImportCheckRuns(),
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
  const imports = runImportCheckPass(input).pipe(
    Effect.repeat({ while: (worked) => worked }),
  );
  return {
    input,
    /** Import passes until no block has a pending entry. */
    importPass: (nowMs?: number) =>
      raised(nowMs === undefined ? imports : at(nowMs, imports)),
    /** One import pass at `nowMs`: whether it found a block. */
    importStep: async (nowMs: number) => {
      const worked = await db(at(nowMs, runImportCheckPass(input)));
      return { worked, alarms: alarms.splice(0) };
    },
    /** Backlog passes until one reads no entry. */
    backlog: (nowMs?: number) => {
      const backlog = runBacklogPass(input).pipe(
        Effect.repeat({ while: (read) => read > 0 }),
      );
      return raised(nowMs === undefined ? backlog : at(nowMs, backlog));
    },
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
  "timed_out",
  "faulted",
  "rejected",
];

export const entries = (states: readonly ImportCheckState[] = ALL_STATES) =>
  db(retrieveChecks(states, { limit: 100_000 }));

/** The transaction ids of the entries in `states`, in set order. */
export const idsIn = async (states: readonly ImportCheckState[]) =>
  (await entries(states)).map((entry) => hex(entry.transactionId));

export const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
