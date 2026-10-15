/**
 * The import checker. A foreign block's import applies its Plutus
 * transactions by their committed verdicts and records them in the durable
 * set (`database/importChecks.ts`); the checker evaluates them afterwards,
 * off the import path, and its verdicts feed only alarms (`alarms.ts`).
 *
 * One pass:
 * 1. every entry past its deadline leaves the set (`unverified_deadline_missed`);
 * 2. each block's `pending` entries, earliest deadline first, run within the
 *    block's wall-time cap. A transaction's runs go in order, each killed at
 *    `importCheckKillMs`. Accepted: it leaves the set. Rejected: alarm, and
 *    it leaves the set. A fault is retried once on a fresh instance; a second
 *    fault in the transaction leaves it `faulted` in the set
 *    (`uplc_import_fault`). A kill is never a verdict. The entries not
 *    started within the cap stay as `cap_skipped` (`import_check_cap`).
 *
 * With no evaluator (the validation pool is disabled) nothing is run, and
 * entries leave the set only by deadline or block removal.
 */
import { Clock, Effect } from "effect";

import {
  deleteCheck,
  expireChecks,
  type ImportCheckEntry,
  retrieveChecks,
  setCheckState,
} from "../database/importChecks.js";
import { type ImportCheckConfig } from "../services/config.import-check.js";
import { type ImportCheckAlarmSink } from "./alarms.js";
import { type ImportCheckEvaluator, importCheckKillMs } from "./evaluator.js";

export type ImportCheckPassInput = Readonly<{
  evaluator: ImportCheckEvaluator | undefined;
  config: ImportCheckConfig;
  alarm: ImportCheckAlarmSink;
}>;

type EntryVerdict = "accepted" | "rejected" | "faulted";

const checkEntry = (input: ImportCheckPassInput, entry: ImportCheckEntry) =>
  Effect.gen(function* () {
    const evaluator = input.evaluator!;
    let retried = false;
    for (const run of entry.runs) {
      const killMs = importCheckKillMs(run, input.config);
      let outcome = yield* evaluator.evaluate(run, { killMs, fresh: false });
      if (outcome.kind === "fault" && !retried) {
        retried = true;
        outcome = yield* evaluator.evaluate(run, { killMs, fresh: true });
      }
      if (outcome.kind === "fault") {
        yield* setCheckState(entry, "faulted");
        yield* input.alarm({
          alarm: "uplc_import_fault",
          headerHash: entry.headerHash,
          transactionId: entry.transactionId.toString("hex"),
          detail: outcome.detail,
        });
        return "faulted" satisfies EntryVerdict;
      }
      if (outcome.kind === "rejected") {
        yield* deleteCheck(entry);
        yield* input.alarm({
          alarm: "import_check_rejected",
          headerHash: entry.headerHash,
          transactionId: entry.transactionId.toString("hex"),
          detail: outcome.detail,
        });
        return "rejected" satisfies EntryVerdict;
      }
    }
    yield* deleteCheck(entry);
    return "accepted" satisfies EntryVerdict;
  });

/** Runs one block's pending entries within its cap. */
const checkBlock = (
  input: ImportCheckPassInput,
  entries: readonly ImportCheckEntry[],
) =>
  Effect.gen(function* () {
    const startedAt = yield* Clock.currentTimeMillis;
    let next = 0;
    for (; next < entries.length; next++) {
      const now = yield* Clock.currentTimeMillis;
      if (now - startedAt >= input.config.blockCapMs) break;
      yield* checkEntry(input, entries[next]!);
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

/** One checker pass (see the module comment). */
export const runImportCheckPass = (input: ImportCheckPassInput) =>
  Effect.gen(function* () {
    const now = yield* Clock.currentTimeMillis;
    for (const expired of yield* expireChecks(now))
      yield* input.alarm({
        alarm: "unverified_deadline_missed",
        headerHash: expired.headerHash,
        transactionId: expired.transactionId.toString("hex"),
        deadlineMs: expired.deadlineMs,
        state: expired.state,
      });
    if (input.evaluator === undefined) return;
    const pending = yield* retrieveChecks(["pending"]);
    const blocks = new Map<string, ImportCheckEntry[]>();
    for (const entry of pending) {
      const block = blocks.get(entry.headerHash) ?? [];
      block.push(entry);
      blocks.set(entry.headerHash, block);
    }
    for (const entries of blocks.values()) yield* checkBlock(input, entries);
  });
