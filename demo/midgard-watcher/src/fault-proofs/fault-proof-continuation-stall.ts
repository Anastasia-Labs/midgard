import { FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED } from "@al-ft/midgard-fault-proofs";

import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import { holdWatcherFaultProofOnFunding } from "./fault-proof-funding-hold.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import { holdWatcherFaultProofForResubmission } from "./fault-proof-resubmission-hold.js";

/**
 * An objective whose runner session journaled no progress within its
 * continuation bound. It resumes on the next observation; the hold is the
 * operator's alarm until a later run journals progress.
 */
export const WATCHER_FAULT_PROOF_CONTINUATION_STALLED =
  "fault_proof_continuation_stalled";

/**
 * An objective awaiting its counterparty past the response deadline its run
 * reported, plus `WATCHER_COUNTERPARTY_DEADLINE_SLACK_MS`, or with no
 * usable deadline. By then this side was due to move; the hold is the
 * operator's alarm until a later run reports something else.
 */
export const WATCHER_FAULT_PROOF_COUNTERPARTY_DEADLINE_PASSED =
  "fault_proof_counterparty_deadline_passed";

/**
 * How long past a counterparty's response deadline an objective may still
 * await it: the observation that lets this side move, and its provider's
 * lag, arrive within this.
 */
export const WATCHER_COUNTERPARTY_DEADLINE_SLACK_MS = 10 * 60_000;

/** Whether a result or outcome is a pending run handed back by its bound. */
export const isWatcherFaultProofContinuationStall = (
  result: unknown,
): result is Readonly<{ kind: "pending"; reason: string }> =>
  typeof result === "object" &&
  result !== null &&
  "kind" in result &&
  result.kind === "pending" &&
  "reason" in result &&
  typeof result.reason === "string" &&
  result.reason.startsWith(FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED);

/** The alarm detail of an objective awaiting its counterparty, if due. */
const counterpartyOverdue = (
  result: Readonly<{ kind: unknown; reason: unknown }>,
  nowMs: number,
): string | undefined => {
  if (result.kind !== "awaiting_counterparty") return undefined;
  const deadline =
    "responseDeadline" in result ? result.responseDeadline : undefined;
  if (typeof deadline !== "number" || !Number.isSafeInteger(deadline))
    return `awaiting its counterparty with no response deadline: ${String(result.reason)}`;
  const overdueMs = nowMs - deadline - WATCHER_COUNTERPARTY_DEADLINE_SLACK_MS;
  return overdueMs > 0
    ? `awaiting its counterparty ${overdueMs.toString()} ms past the response deadline ${deadline.toString()} and its slack: ${String(result.reason)}`
    : undefined;
};

/** The state a pending run's family named for readiness, if any. */
const namedStateOf = (
  result: Readonly<{ kind: unknown; reason: unknown }>,
): Readonly<{ name: string; alarm: boolean }> | undefined => {
  if (result.kind !== "pending" || !("namedState" in result)) return undefined;
  const state: unknown = result.namedState;
  return typeof state === "object" &&
    state !== null &&
    "name" in state &&
    typeof state.name === "string" &&
    state.name.length > 0 &&
    "alarm" in state &&
    typeof state.alarm === "boolean"
    ? { name: state.name, alarm: state.alarm }
    : undefined;
};

/** The reasons of the actions a run skipped as unbuilt, each an alarm. */
const alarmsOf = (result: object): readonly string[] => {
  const alarms: unknown = "alarms" in result ? result.alarms : undefined;
  return Array.isArray(alarms)
    ? alarms.flatMap((alarm: unknown) =>
        typeof alarm === "object" &&
        alarm !== null &&
        "reason" in alarm &&
        typeof alarm.reason === "string"
          ? [alarm.reason]
          : [],
      )
    : [];
};

const logError = (hold: Readonly<{ reason: string; detail: string }>) =>
  process.stderr.write(
    `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "error", ...hold })}\n`,
  );

/** `hold` naming every action the run skipped as unbuilt, other than the
 * run's own reason. */
const withAlarms = (
  hold: Readonly<{ reason: string; detail: string }>,
  result: Readonly<{ kind: unknown; reason: unknown }>,
): Readonly<{ reason: string; detail: string }> => {
  const others = alarmsOf(result).filter(
    (alarm) => alarm !== String(result.reason),
  );
  return { ...hold, detail: [hold.detail, ...others].join("; ") };
};

/**
 * Holds a pending objective under its name: one that needs a new submission
 * after its latest safe start, one paused because the prover wallet cannot
 * fund it, one in a state its family named, one whose session stalled, or
 * one still awaiting its counterparty past the response deadline (as of
 * `nowMs`) and its slack. The last two, and a named state its family marks
 * as an alarm, are also logged at error level. A funding, named-state or
 * stall hold also names every action the run skipped as unbuilt, each an
 * alarm, and is then logged at error level even when the run went on to land
 * another move. An objective awaiting its counterparty before then is healthy
 * and holds nothing.
 */
export const holdWatcherFaultProofPending = (
  holds: Map<string, Readonly<{ reason: string; detail: string }>>,
  job: WatcherProofObjective,
  result: Readonly<{ kind: unknown; reason: unknown }>,
  nowMs: number = Date.now(),
): Readonly<{ kind: "pending"; reason: string }> | undefined => {
  const resubmission = holdWatcherFaultProofForResubmission(holds, job, result);
  if (resubmission !== undefined) return resubmission;
  const objective = `${job.category}/${job.headerHash}`;
  const alarms = alarmsOf(result);
  if (result.kind === "pending") {
    const funding = holdWatcherFaultProofOnFunding(holds, job, result.reason);
    if (funding !== undefined) {
      const fundingHold = holds.get(objective);
      if (alarms.length > 0 && fundingHold !== undefined) {
        const hold = withAlarms(fundingHold, result);
        holds.set(objective, hold);
        logError(hold);
      }
      return funding;
    }
  }
  const named = namedStateOf(result);
  if (named !== undefined) {
    const hold = withAlarms(
      { reason: named.name, detail: `${objective}: ${String(result.reason)}` },
      result,
    );
    holds.set(objective, hold);
    if (named.alarm || alarms.length > 0) logError(hold);
    return { kind: "pending", reason: String(result.reason) };
  }
  const overdue = counterpartyOverdue(result, nowMs);
  if (overdue === undefined && !isWatcherFaultProofContinuationStall(result))
    return undefined;
  const hold =
    overdue === undefined
      ? withAlarms(
          {
            reason: WATCHER_FAULT_PROOF_CONTINUATION_STALLED,
            detail: `${objective}: ${String(result.reason)}`,
          },
          result,
        )
      : {
          reason: WATCHER_FAULT_PROOF_COUNTERPARTY_DEADLINE_PASSED,
          detail: `${objective}: ${overdue}`,
        };
  holds.set(objective, hold);
  logError(hold);
  return { kind: "pending", reason: String(result.reason) };
};

/**
 * The progress callback of an objective's run: its first journaled progress
 * clears a stall hold left by an earlier run, without waiting for this run to
 * end. Any other hold is left to the run's outcome.
 */
export const releaseWatcherFaultProofStallOnProgress =
  (
    holds: Map<string, Readonly<{ reason: string; detail: string }>>,
    job: WatcherProofObjective,
  ) =>
  (): void => {
    const objective = `${job.category}/${job.headerHash}`;
    if (
      holds.get(objective)?.reason === WATCHER_FAULT_PROOF_CONTINUATION_STALLED
    )
      holds.delete(objective);
  };
