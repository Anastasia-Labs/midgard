import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import {
  isWatcherJournalIntegrityError,
  isWatcherJournalUnavailableError,
} from "./watcher-journal-database.js";

/**
 * An objective whose run stalled outside preflight, or whose funding or run
 * met an integrity error of its own (a reconcile that refused its durable
 * transaction, or a reservation the recovery authorization cannot match to
 * the objective's journal, for example). Only that objective is held: the
 * process keeps running, every other objective keeps proving, and the
 * supervisor runs this one again on a bounded backoff. Nothing is signed
 * before a failed funding mint, and each later run reconciles its journal
 * before it acts, so it re-checks the cause rather than resending anything;
 * the hold ends with the first run that meets something else.
 */
export const WATCHER_FAULT_PROOF_QUARANTINED =
  "fault_proof_objective_quarantined";

type Holds = Map<string, Readonly<{ reason: string; detail: string }>>;
type AlertSink = Pick<WatcherOperationsSink, "setAlert">;

const objectiveOf = ({ category, headerHash }: WatcherProofObjective) =>
  `${category}/${headerHash}`;

/**
 * The cause a failure of the objective's own run is quarantined under, or
 * undefined for a watcher journal database that is corrupt or unavailable:
 * that database is shared by every objective, and the supervisor already
 * holds the whole watcher unready on it without failing the process.
 */
export const quarantineCause = (error: unknown): string | undefined =>
  isWatcherJournalIntegrityError(error) ||
  isWatcherJournalUnavailableError(error)
    ? undefined
    : error instanceof Error
      ? error.message
      : String(error);

/** Whether `objective` is held in quarantine. */
export const isWatcherFaultProofQuarantined = (
  holds: Holds,
  objective: WatcherProofObjective,
): boolean =>
  holds.get(objectiveOf(objective))?.reason === WATCHER_FAULT_PROOF_QUARANTINED;

/**
 * Holds the objective in quarantine under `cause`. Entering it raises the
 * objective's `proof_submission_failure` alert and logs at error level; the
 * outcome hands the objective back to the supervisor's bounded backoff.
 */
export const quarantineWatcherFaultProof = (input: {
  readonly holds: Holds;
  readonly job: WatcherProofObjective;
  readonly decisionDigest: string;
  readonly sink: AlertSink;
  readonly cause: string;
}) => {
  const objective = objectiveOf(input.job);
  const entering = !isWatcherFaultProofQuarantined(input.holds, input.job);
  const hold = {
    reason: WATCHER_FAULT_PROOF_QUARANTINED,
    detail: `${objective}: ${input.cause}`,
  };
  input.holds.set(objective, hold);
  if (entering) {
    process.stderr.write(
      `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "error", ...hold })}\n`,
    );
    input.sink.setAlert({
      code: "proof_submission_failure",
      subjectDigest: input.decisionDigest,
      active: true,
      observedAtMs: Date.now().toString(),
    });
  }
  return Object.freeze({
    kind: "retryable" as const,
    resume: "backoff" as const,
    reason: `${WATCHER_FAULT_PROOF_QUARANTINED}: ${input.cause}`,
    retryAfterMs: 1_000,
  });
};

/** Clears the objective's alert once a run that began quarantined ends out of it. */
export const settleWatcherFaultProofQuarantine = (input: {
  readonly holds: Holds;
  readonly job: WatcherProofObjective;
  readonly decisionDigest: string;
  readonly sink: AlertSink;
  readonly wasQuarantined: boolean;
}): void => {
  if (
    !input.wasQuarantined ||
    isWatcherFaultProofQuarantined(input.holds, input.job)
  )
    return;
  input.sink.setAlert({
    code: "proof_submission_failure",
    subjectDigest: input.decisionDigest,
    active: false,
    observedAtMs: Date.now().toString(),
  });
};
