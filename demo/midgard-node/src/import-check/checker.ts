/**
 * The import checker. A foreign block's import applies its Plutus
 * transactions by their committed verdicts and records them in the durable
 * set (`database/importChecks.ts`); the checker evaluates them afterwards,
 * off the import path, and its verdicts feed only alarms (`alarms.ts`).
 *
 * Checking an entry runs its runs in order, each killed at
 * `importCheckKillMs`. A run that faults is retried once on a fresh
 * instance; a kill is never a verdict.
 * - every run accepted: the entry leaves the set;
 * - a run rejected: the entry stays as `rejected` (`import_check_rejected`);
 * - a run faulted again: the entry stays as `faulted`
 *   (`uplc_import_fault`), and its script enters the negative cache.
 *
 * Three passes, each run by its own loop (`fiber.ts`):
 * - the import pass (`runImportCheckPass`), below admission: each block's
 *   `pending` entries, earliest deadline first, within the block's wall-time
 *   cap. An entry running a cached script is not run and waits as
 *   `cache_skipped`; the entries not started within the cap wait as
 *   `cap_skipped` (`import_check_cap`);
 * - the backlog pass (`runBacklogPass`), on the pool's reserved backlog
 *   share: the skipped entries, earliest deadline first, cached scripts
 *   included;
 * - the sweep (`runUnverifiedSweep`): entries past their deadline leave the
 *   set (`unverified_deadline_missed`), and the backlog's projected
 *   completion is checked against every deadline
 *   (`unverified_near_deadline`).
 *
 * With no evaluator (the validation pool is disabled) nothing is run, and
 * entries leave the set only by deadline or block removal.
 */
import { Clock, Effect } from "effect";

import {
  BACKLOG_STATES,
  deleteCheck,
  expireChecks,
  type ImportCheckEntry,
  type ImportCheckLoad,
  retrieveCheckLoads,
  retrieveChecks,
  setCheckState,
  UNRUN_STATES,
} from "../database/importChecks.js";
import { type ImportCheckConfig } from "../services/config.import-check.js";
import { type ImportCheckLane } from "../services/validation-pool.js";
import { type ImportCheckAlarmSink } from "./alarms.js";
import {
  type ImportCheckEvaluator,
  importCheckKillMs,
  importCheckLoadKillMs,
} from "./evaluator.js";
import { type NegativeCache } from "./negative-cache.js";

export type ImportCheckPassInput = Readonly<{
  evaluator: ImportCheckEvaluator | undefined;
  config: ImportCheckConfig;
  alarm: ImportCheckAlarmSink;
  cache: NegativeCache;
}>;

/** The most backlog entries one backlog pass reads. */
export const BACKLOG_BATCH = 64;

const checkEntry = (
  input: ImportCheckPassInput,
  evaluator: ImportCheckEvaluator,
  entry: ImportCheckEntry,
  lane: ImportCheckLane,
) =>
  Effect.gen(function* () {
    const transactionId = entry.transactionId.toString("hex");
    for (const run of entry.runs) {
      const killMs = importCheckKillMs(run, input.config);
      let outcome = yield* evaluator.evaluate(run, {
        killMs,
        fresh: false,
        lane,
      });
      if (outcome.kind === "fault")
        outcome = yield* evaluator.evaluate(run, { killMs, fresh: true, lane });
      if (outcome.kind === "fault") {
        yield* setCheckState(entry, "faulted");
        yield* input.cache.add(run);
        yield* input.alarm({
          alarm: "uplc_import_fault",
          headerHash: entry.headerHash,
          transactionId,
          detail: outcome.detail,
        });
        return;
      }
      if (outcome.kind === "rejected") {
        yield* setCheckState(entry, "rejected");
        yield* input.alarm({
          alarm: "import_check_rejected",
          headerHash: entry.headerHash,
          transactionId,
          detail: outcome.detail,
        });
        return;
      }
    }
    yield* deleteCheck(entry);
  });

const runsCachedScript = (cache: NegativeCache, entry: ImportCheckEntry) =>
  Effect.exists(entry.runs, (run) => cache.holds(run));

/** Runs one block's pending entries within its cap. */
const checkBlock = (
  input: ImportCheckPassInput,
  evaluator: ImportCheckEvaluator,
  entries: readonly ImportCheckEntry[],
) =>
  Effect.gen(function* () {
    const startedAt = yield* Clock.currentTimeMillis;
    let next = 0;
    for (; next < entries.length; next++) {
      const now = yield* Clock.currentTimeMillis;
      if (now - startedAt >= input.config.blockCapMs) break;
      const entry = entries[next]!;
      if (yield* runsCachedScript(input.cache, entry))
        yield* setCheckState(entry, "cache_skipped");
      else yield* checkEntry(input, evaluator, entry, "import");
    }
    const skipped = entries.slice(next);
    if (skipped.length === 0) return;
    for (const entry of skipped) yield* setCheckState(entry, "cap_skipped");
    yield* input.alarm({
      alarm: "import_check_cap",
      headerHash: entries[0]!.headerHash,
      skipped: skipped.length,
    });
  });

/** The import pass (see the module comment). */
export const runImportCheckPass = (input: ImportCheckPassInput) =>
  Effect.gen(function* () {
    const evaluator = input.evaluator;
    if (evaluator === undefined) return;
    const pending = yield* retrieveChecks(["pending"]);
    const blocks = new Map<string, ImportCheckEntry[]>();
    for (const entry of pending) {
      const block = blocks.get(entry.headerHash) ?? [];
      block.push(entry);
      blocks.set(entry.headerHash, block);
    }
    for (const entries of blocks.values())
      yield* checkBlock(input, evaluator, entries);
  });

/**
 * The backlog pass: up to `BACKLOG_BATCH` skipped entries, earliest
 * deadline first, `backlogWorkers` at a time; an entry already past its
 * deadline is left to the sweep. Returns the number of entries it checked;
 * a full batch means more may wait.
 */
export const runBacklogPass = (input: ImportCheckPassInput) =>
  Effect.gen(function* () {
    const evaluator = input.evaluator;
    if (evaluator === undefined) return 0;
    const now = yield* Clock.currentTimeMillis;
    const entries = (yield* retrieveChecks(
      BACKLOG_STATES,
      BACKLOG_BATCH,
    )).filter((entry) => now < entry.deadlineMs);
    yield* Effect.forEach(
      entries,
      (entry) => checkEntry(input, evaluator, entry, "backlog"),
      { concurrency: input.config.backlogWorkers, discard: true },
    );
    return entries.length;
  });

/**
 * The deadlines the backlog cannot serve in time: for each deadline d, two
 * kills of every entry due by d, over the backlog's reserved workers, end
 * past d minus the opener's margin.
 */
export const projectUnverified = (
  loads: readonly ImportCheckLoad[],
  config: ImportCheckConfig,
  nowMs: number,
) => {
  const atRisk: { deadlineMs: number; entries: number; projectedMs: number }[] =
    [];
  let killsMs = 0;
  for (let index = 0; index < loads.length; ) {
    const deadlineMs = loads[index]!.deadlineMs;
    for (; loads[index]?.deadlineMs === deadlineMs; index++)
      killsMs += 2 * importCheckLoadKillMs(loads[index]!, config);
    const projectedMs = Math.ceil(nowMs + killsMs / config.backlogWorkers);
    if (projectedMs > deadlineMs - config.openerMarginMs)
      atRisk.push({ deadlineMs, entries: index, projectedMs });
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
    for (const expired of yield* expireChecks(now)) {
      if (expired.removed || expired.state === "rejected") continue;
      yield* input.alarm({
        alarm: "unverified_deadline_missed",
        headerHash: expired.headerHash,
        transactionId: expired.transactionId.toString("hex"),
        deadlineMs: expired.deadlineMs,
        state: expired.state,
      });
    }
    for (const deadlineMs of alarmed)
      if (deadlineMs <= now) alarmed.delete(deadlineMs);
    if (input.evaluator === undefined) return;
    const loads = yield* retrieveCheckLoads(UNRUN_STATES);
    for (const risk of projectUnverified(loads, input.config, now)) {
      if (alarmed.has(risk.deadlineMs)) continue;
      alarmed.add(risk.deadlineMs);
      yield* input.alarm({ alarm: "unverified_near_deadline", ...risk });
    }
  });
