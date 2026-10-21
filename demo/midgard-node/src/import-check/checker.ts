/**
 * The import checker. A foreign block's import applies its Plutus
 * transactions by their committed verdicts and records them in the durable
 * set (`database/importChecks.ts`); the checker evaluates them afterwards,
 * off the import path, and its verdicts feed only alarms (`alarms.ts`).
 *
 * Checking an entry rebuilds its runs (`rebuild.ts`) and runs them in
 * order, each killed at `importCheckKillMs`. A run that faults is retried
 * once on a fresh instance. Neither a kill (nor a worker that failed before
 * the run was sent to it) nor a run cancelled because it was not started
 * in time is a verdict: the kill is wall-clock time, which machine load
 * stretches, so only the deterministic outcomes below decide an entry.
 * - every run accepted: the entry leaves the set;
 * - a run rejected, or over its declared budget: the entry stays as
 *   `rejected` (`import_check_rejected`);
 * - a run faulted again (crashed or failed to evaluate): the entry stays as
 *   `faulted` (`uplc_import_fault`), and its script enters the negative
 *   cache;
 * - a run killed: no verdict, and no retry on a fresh instance. The entry
 *   waits as `timed_out` (`import_check_timeout`, raised for its first
 *   kill only) and the backlog lane runs it again from its retry time,
 *   `IMPORT_CHECK_TIMEOUT_RETRY_MS` after the kill, doubled with each
 *   further kill up to `IMPORT_CHECK_TIMEOUT_RETRY_CAP_MS`, until its
 *   deadline. The retry state is durable, so a restart keeps it.
 * A verdict that arrives at or after the entry's deadline first raises
 * `unverified_deadline_missed` (once per entry), then applies as above.
 *
 * Three passes, each run by its own loop (`fiber.ts`):
 * - the import pass (`runImportCheckPass`), below admission: one block at a
 *   time, earliest deadline first, its `pending` entries a page at a time
 *   within the block's wall-time cap. No run starts after the cap: a run not
 *   started by then is cancelled, and the block's entries not yet run wait
 *   as `cap_skipped` (`import_check_cap`). An entry running a cached script
 *   is not run and waits as `cache_skipped`;
 * - the backlog pass (`runBacklogPass`), on the pool's reserved backlog
 *   share: every unrun entry (`pending` ones included), earliest deadline
 *   first, cached scripts included; only when none waits, the `timed_out`
 *   entries whose retry is due, at most one per backlog worker before it
 *   reads unrun entries again, so a first check waits at most for the
 *   retries already running. A run not started by its entry's deadline is
 *   cancelled;
 * - the sweep (`runUnverifiedSweep`), which never waits on a run: entries
 *   past their deadline leave the set (a `timed_out` one included), and
 *   entries being checked past their deadline raise
 *   `unverified_deadline_missed`; the backlog's projected completion is
 *   checked against every deadline (`unverified_near_deadline`).
 *
 * With no evaluator (the validation pool is disabled) nothing is run, and
 * entries leave the set only by deadline or block removal.
 */
import { Clock, Effect } from "effect";

import {
  deferCheck,
  deleteCheck,
  expireChecks,
  type ImportCheckEntry,
  type ImportCheckKey,
  type ImportCheckState,
  isCheckHeld,
  retrieveCheckMaterial,
  retrieveChecks,
  retrieveNextPendingBlock,
  setCheckState,
  skipPendingChecks,
  UNRUN_STATES,
} from "../database/importChecks.js";
import {
  type ImportCheckLoad,
  retrieveCheckLoads,
} from "../database/importChecks.loads.js";
import { type ImportCheckConfig } from "../services/config.import-check.js";
import { type ImportCheckLane } from "../services/validation-pool.js";
import { type ImportCheckAlarmSink } from "./alarms.js";
import { type ImportCheckEvaluator } from "./evaluator.js";
import { importCheckKillMs, importCheckLoadKillMs } from "./kill.js";
import { type NegativeCache } from "./negative-cache.js";
import { importCheckOutRefs, rebuildAndEvaluate } from "./rebuild.js";

/**
 * The passes' shared state: the entries being checked now (no other pass
 * runs them, and the sweep leaves them in the set), and the entries
 * `unverified_deadline_missed` was raised for, by deadline.
 */
export type ImportCheckRuns = Readonly<{
  inFlight: Map<string, ImportCheckEntry>;
  /**
   * In memory only: after a restart, an entry alarmed while it was still
   * being checked past its deadline is alarmed once more when the sweep
   * drops it. Known and accepted: one duplicate alarm per such entry.
   */
  missed: Map<string, number>;
}>;

export const makeImportCheckRuns = (): ImportCheckRuns => ({
  inFlight: new Map(),
  missed: new Map(),
});

export type ImportCheckPassInput = Readonly<{
  evaluator: ImportCheckEvaluator | undefined;
  config: ImportCheckConfig;
  alarm: ImportCheckAlarmSink;
  cache: NegativeCache;
  runs: ImportCheckRuns;
}>;

/** The most backlog entries one backlog pass reads. */
export const BACKLOG_BATCH = 64;

/** How long an entry whose check was killed waits before the backlog lane
 * runs it again; doubled with each further kill, up to
 * `IMPORT_CHECK_TIMEOUT_RETRY_CAP_MS`. */
export const IMPORT_CHECK_TIMEOUT_RETRY_MS = 5_000;
export const IMPORT_CHECK_TIMEOUT_RETRY_CAP_MS = 600_000;

const keyOf = (key: ImportCheckKey) =>
  `${key.headerHash}:${key.transactionId.toString("hex")}`;

const inFlight = (input: ImportCheckPassInput) => [
  ...input.runs.inFlight.values(),
];

/** Records a killed check of `entry`: it waits as `timed_out` until its
 * retry time. Only its first kill is alarmed. */
const deferTimedOut = (
  input: ImportCheckPassInput,
  entry: ImportCheckEntry,
  detail: string,
) =>
  Effect.gen(function* () {
    const timeouts = entry.timeouts + 1;
    const retryAtMs =
      (yield* Clock.currentTimeMillis) +
      Math.min(
        IMPORT_CHECK_TIMEOUT_RETRY_CAP_MS,
        IMPORT_CHECK_TIMEOUT_RETRY_MS * 2 ** (timeouts - 1),
      );
    // The alarm comes first, as for a rejection.
    if (timeouts === 1 && (yield* isCheckHeld(entry)))
      yield* input.alarm({
        alarm: "import_check_timeout",
        headerHash: entry.headerHash,
        transactionId: entry.transactionId.toString("hex"),
        detail,
        retryAtMs,
      });
    yield* deferCheck(entry, timeouts, retryAtMs);
  });

/** Raises `unverified_deadline_missed` for `entry` unless it was raised
 * already or the entry left the set or the landed chain. */
const raiseMissed = (
  input: ImportCheckPassInput,
  entry: ImportCheckKey & Readonly<{ deadlineMs: number }>,
  state: ImportCheckState,
) =>
  Effect.gen(function* () {
    const key = keyOf(entry);
    if (input.runs.missed.has(key)) return;
    input.runs.missed.set(key, entry.deadlineMs);
    if (!(yield* isCheckHeld(entry))) return;
    yield* input.alarm({
      alarm: "unverified_deadline_missed",
      headerHash: entry.headerHash,
      transactionId: entry.transactionId.toString("hex"),
      deadlineMs: entry.deadlineMs,
      state,
    });
  });

const runEntry = (
  input: ImportCheckPassInput,
  evaluator: ImportCheckEvaluator,
  entry: ImportCheckEntry,
  lane: ImportCheckLane,
  startBy: number | undefined,
) =>
  Effect.gen(function* () {
    const material = yield* retrieveCheckMaterial(entry, importCheckOutRefs);
    if (material === undefined) return "done" as const;
    const verdict = yield* rebuildAndEvaluate(
      entry.transactionId,
      material,
      (run) =>
        Effect.gen(function* () {
          const options = {
            killMs: importCheckKillMs(run, input.config),
            lane,
            ...(startBy === undefined ? {} : { startBy }),
          };
          const first = yield* evaluator.evaluate(run, {
            ...options,
            fresh: false,
          });
          if (first.kind !== "fault") return first;
          return yield* evaluator.evaluate(run, { ...options, fresh: true });
        }),
    );
    if (verdict.kind === "cancelled") return "cancelled" as const;
    if (verdict.kind === "timeout") {
      yield* deferTimedOut(input, entry, verdict.detail);
      return "deferred" as const;
    }
    if ((yield* Clock.currentTimeMillis) >= entry.deadlineMs)
      yield* raiseMissed(input, entry, entry.state);
    const transactionId = entry.transactionId.toString("hex");
    if (verdict.kind === "accepted") yield* deleteCheck(entry);
    else if (verdict.kind === "rejected") {
      // The alarm comes first: a stop between the two re-runs the entry
      // (still unrun) and alarms again, where the other order would lose
      // it. An entry that left the set or the landed chain is not alarmed.
      if (yield* isCheckHeld(entry))
        yield* input.alarm({
          alarm: "import_check_rejected",
          headerHash: entry.headerHash,
          transactionId,
          detail: verdict.detail,
        });
      yield* setCheckState(entry, "rejected");
    } else {
      yield* setCheckState(entry, "faulted");
      if (verdict.scriptHash !== undefined)
        yield* input.cache.add(verdict.scriptHash);
      yield* input.alarm({
        alarm: "uplc_import_fault",
        headerHash: entry.headerHash,
        transactionId,
        detail: verdict.detail,
      });
    }
    return "done" as const;
  });

/**
 * Checks `entry` (see the module comment); `busy` when another pass is
 * checking it.
 */
const checkEntry = (
  input: ImportCheckPassInput,
  evaluator: ImportCheckEvaluator,
  entry: ImportCheckEntry,
  lane: ImportCheckLane,
  startBy?: number,
) =>
  Effect.suspend(() => {
    const key = keyOf(entry);
    if (input.runs.inFlight.has(key)) return Effect.succeed("busy" as const);
    input.runs.inFlight.set(key, entry);
    return runEntry(input, evaluator, entry, lane, startBy).pipe(
      Effect.ensuring(Effect.sync(() => input.runs.inFlight.delete(key))),
    );
  });

const runsCachedScript = (cache: NegativeCache, entry: ImportCheckEntry) =>
  Effect.exists(entry.scriptHashes, (hash) => cache.holds(hash));

/** Runs one block's pending entries within its cap. */
const checkBlock = (
  input: ImportCheckPassInput,
  evaluator: ImportCheckEvaluator,
  headerHash: string,
) =>
  Effect.gen(function* () {
    const capEnd = (yield* Clock.currentTimeMillis) + input.config.blockCapMs;
    let capped = false;
    let progressed = true;
    while (!capped && progressed) {
      const now = yield* Clock.currentTimeMillis;
      const page = yield* retrieveChecks(["pending"], {
        headerHash,
        dueAfterMs: now,
        excluding: inFlight(input),
      });
      progressed = false;
      for (const entry of page) {
        capped = (yield* Clock.currentTimeMillis) >= capEnd;
        if (capped) break;
        if (yield* runsCachedScript(input.cache, entry)) {
          yield* setCheckState(entry, "cache_skipped");
          progressed = true;
          continue;
        }
        const result = yield* checkEntry(
          input,
          evaluator,
          entry,
          "import",
          capEnd,
        );
        capped = result === "cancelled";
        if (capped) break;
        progressed ||= result === "done" || result === "deferred";
      }
    }
    if (!capped) return;
    const skipped = yield* skipPendingChecks(headerHash, inFlight(input));
    if (skipped > 0)
      yield* input.alarm({ alarm: "import_check_cap", headerHash, skipped });
  });

/**
 * The import pass (see the module comment): the block of the earliest-due
 * pending entry. Returns whether there was one.
 */
export const runImportCheckPass = (input: ImportCheckPassInput) =>
  Effect.gen(function* () {
    const evaluator = input.evaluator;
    if (evaluator === undefined) return false;
    const now = yield* Clock.currentTimeMillis;
    const headerHash = yield* retrieveNextPendingBlock(now, inFlight(input));
    if (headerHash === undefined) return false;
    yield* checkBlock(input, evaluator, headerHash);
    return true;
  });

/**
 * The backlog pass: up to `BACKLOG_BATCH` unrun entries due after now,
 * earliest deadline first, or, when there is none, as many `timed_out`
 * entries whose retry is due as the backlog's share (one on a
 * single-worker pool); as many at a time as that share. Returns the
 * number of entries it read; the fiber runs it again at once while it
 * reads any.
 */
export const runBacklogPass = (input: ImportCheckPassInput) =>
  Effect.gen(function* () {
    const evaluator = input.evaluator;
    if (evaluator === undefined) return 0;
    const now = yield* Clock.currentTimeMillis;
    const options = {
      dueAfterMs: now,
      excluding: inFlight(input),
      limit: BACKLOG_BATCH,
    };
    const unrun = yield* retrieveChecks(UNRUN_STATES, options);
    const share = Math.max(1, evaluator.backlogShare);
    const entries =
      unrun.length > 0
        ? unrun
        : yield* retrieveChecks(["timed_out"], {
            ...options,
            limit: share,
            retryDueByMs: now,
          });
    yield* Effect.forEach(
      entries,
      (entry) =>
        checkEntry(input, evaluator, entry, "backlog", entry.deadlineMs),
      { concurrency: share, discard: true },
    );
    return entries.length;
  });

/**
 * The deadlines the backlog cannot serve in time, over its `share` of
 * workers, each entry costing two kills. For each deadline d:
 * - the unrun entries due by d (`unrun`) end past d minus the opener's
 *   margin; or
 * - the `timed_out` entries due by d (`retries`; only those that can still
 *   run) end past it. They run after every unrun entry and not before
 *   their latest retry time.
 * With no share (a single-worker pool) no completion is projected, and
 * every deadline with an entry is at risk. Faulted entries are not
 * counted: each already raised `uplc_import_fault`.
 */
export const projectUnverified = (
  unrun: readonly ImportCheckLoad[],
  retries: readonly ImportCheckLoad[],
  config: ImportCheckConfig,
  nowMs: number,
  share: number,
) => {
  const killsMs = (load: ImportCheckLoad) =>
    2 * importCheckLoadKillMs(load, config);
  const endMs = (startMs: number, workMs: number) =>
    share > 0 ? Math.ceil(startMs + workMs / share) : Number.MAX_SAFE_INTEGER;
  const unrunEndMs = endMs(
    nowMs,
    unrun.reduce((sum, load) => sum + killsMs(load), 0),
  );
  const byDeadline = new Map<
    number,
    { unrun?: ImportCheckLoad; retry?: ImportCheckLoad }
  >();
  for (const load of unrun) byDeadline.set(load.deadlineMs, { unrun: load });
  for (const load of retries)
    byDeadline.set(load.deadlineMs, {
      ...byDeadline.get(load.deadlineMs),
      retry: load,
    });
  const atRisk: { deadlineMs: number; entries: number; projectedMs: number }[] =
    [];
  let entries = 0;
  let unrunMs = 0;
  let retryMs = 0;
  let retryStartMs: number | undefined;
  for (const deadlineMs of [...byDeadline.keys()].sort((a, b) => a - b)) {
    const { unrun: first, retry } = byDeadline.get(deadlineMs)!;
    if (first !== undefined) {
      entries += first.entries;
      unrunMs += killsMs(first);
    }
    if (retry !== undefined) {
      entries += retry.entries;
      retryMs += killsMs(retry);
      retryStartMs = Math.max(
        retryStartMs ?? unrunEndMs,
        retry.retryAtMs ?? nowMs,
      );
    }
    const projectedMs = Math.max(
      endMs(nowMs, unrunMs),
      retryStartMs === undefined ? 0 : endMs(retryStartMs, retryMs),
    );
    if (projectedMs > deadlineMs - config.openerMarginMs)
      atRisk.push({ deadlineMs, entries, projectedMs });
  }
  return atRisk;
};

/**
 * The sweep (see the module comment). `alarmed` holds the deadlines
 * `unverified_near_deadline` was raised for, so each is raised once.
 */
export const runUnverifiedSweep = (
  input: ImportCheckPassInput,
  alarmed: Set<number>,
) =>
  Effect.gen(function* () {
    const now = yield* Clock.currentTimeMillis;
    const running = inFlight(input);
    const kept = new Set(running.map(keyOf));
    const missedBefore = [...input.runs.missed.keys()];
    for (const expired of yield* expireChecks(now, running)) {
      const key = keyOf(expired);
      if (input.runs.missed.has(key)) continue;
      input.runs.missed.set(key, expired.deadlineMs);
      if (expired.removed || expired.state === "rejected") continue;
      yield* input.alarm({
        alarm: "unverified_deadline_missed",
        headerHash: expired.headerHash,
        transactionId: expired.transactionId.toString("hex"),
        deadlineMs: expired.deadlineMs,
        state: expired.state,
      });
    }
    for (const entry of running)
      if (entry.deadlineMs <= now)
        yield* raiseMissed(input, entry, entry.state);
    // An entry neither running nor kept at this sweep's expiry has left the
    // set, so no later pass or sweep reaches it again.
    for (const key of missedBefore)
      if (!kept.has(key) && !input.runs.inFlight.has(key))
        input.runs.missed.delete(key);
    for (const deadlineMs of alarmed)
      if (deadlineMs <= now) alarmed.delete(deadlineMs);
    const evaluator = input.evaluator;
    if (evaluator === undefined) return;
    for (const risk of projectUnverified(
      yield* retrieveCheckLoads(UNRUN_STATES),
      yield* retrieveCheckLoads(["timed_out"]),
      input.config,
      now,
      evaluator.backlogShare,
    )) {
      // A deadline already past (an entry still being checked) is not
      // near: `unverified_deadline_missed` covers it.
      if (risk.deadlineMs <= now || alarmed.has(risk.deadlineMs)) continue;
      alarmed.add(risk.deadlineMs);
      yield* input.alarm({
        alarm: "unverified_near_deadline",
        ...risk,
        ...(evaluator.backlogShare === 0 ? { structural: true } : {}),
      });
    }
  });
