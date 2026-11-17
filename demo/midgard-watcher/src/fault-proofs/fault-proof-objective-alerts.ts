import { createHash } from "node:crypto";

import { activeCollateralForfeitures } from "@al-ft/midgard-fault-proofs";

import type {
  WatcherAlertCode,
  WatcherOperationsSink,
} from "../runtime/operations-observability.js";
import { WATCHER_PACKAGE_NAME } from "../runtime/scaffold.js";
import { WATCHER_PROVER_FUNDING_UNAVAILABLE } from "./fault-proof-funding-hold.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import type { WatcherFaultProofDeadline } from "./fault-proof-supervisor.validate-job.js";
import type { WatcherWorkflowJournal } from "./watcher-workflow-journal.js";

/**
 * The readiness reason of an objective whose run started after its latest
 * safe start, as `<category>/<headerHash>: ...`. It is listed until the
 * objective's correction is included, another record resolves it, or the
 * supervisor stops driving it (its header merged, was removed, or matured
 * and is held by name).
 */
export const WATCHER_FAULT_PROOF_STARTED_LATE = "fault_proof_started_late";

/** One alert subject per code and proof objective. */
export const watcherProofObjectiveAlertSubject = (
  code: WatcherAlertCode,
  { category, headerHash }: WatcherProofObjective,
): string =>
  createHash("sha256")
    .update("midgard-watcher-proof-objective-alert-v1\u0000")
    .update(`${code}\u0000${category}/${headerHash.toLowerCase()}`)
    .digest("hex");

/**
 * The execution's per-objective alerts:
 *
 * - `maturity_deadline_risk`: a run started past the objective's latest safe
 *   start (the supervisor marks it `startedLate`), signed or not, before or
 *   after maturity. Before maturity the proof can still land, so the watcher
 *   keeps trying, and says so loudly: an error line naming the objective,
 *   the active alert, and the `fault_proof_started_late` readiness entry.
 *   The mark comes from the deadline and the clock at each run, so a
 *   restarted watcher raises it again on the objective's first run.
 * - `prover_funding_unavailable`: the objective's last run was paused
 *   because the prover wallet could not fund it. It clears once a run meets
 *   something else.
 * - `prover_collateral_forfeited`: an attempt of the objective landed
 *   failing phase 2, so the chain took its collateral: local evaluation
 *   disagreed with the chain. Each such attempt writes one error line
 *   naming the objective, the attempt and the collateral. It clears once a
 *   rollback drops every such landing.
 *
 * All clear when the objective settles.
 */
export const createWatcherFaultProofObjectiveAlerts = ({
  operationsSink: sink,
  workflowJournal,
}: {
  readonly operationsSink: Pick<WatcherOperationsSink, "setAlert">;
  readonly workflowJournal: Pick<WatcherWorkflowJournal, "readExecution">;
}) => {
  const late = new Map<string, Readonly<{ reason: string; detail: string }>>();
  const unfunded = new Set<string>();
  /** Per objective, the attempts whose forfeiture stands. */
  const forfeited = new Map<string, ReadonlySet<string>>();
  const objectiveOf = ({ category, headerHash }: WatcherProofObjective) =>
    `${category}/${headerHash}`;
  const set = (
    code: WatcherAlertCode,
    objective: WatcherProofObjective,
    active: boolean,
  ) =>
    sink.setAlert({
      code,
      subjectDigest: watcherProofObjectiveAlertSubject(code, objective),
      active,
      observedAtMs: Date.now().toString(),
    });
  const finished = (objective: WatcherProofObjective): void => {
    if (!late.delete(objectiveOf(objective))) return;
    set("maturity_deadline_risk", objective, false);
  };
  return Object.freeze({
    /** A run of `job` begins; a start past the latest safe start of
     * `startedLate` raises its alert once. */
    started: (
      job: WatcherProofObjective,
      startedLate: WatcherFaultProofDeadline | undefined,
    ): void => {
      const objective = objectiveOf(job);
      if (startedLate === undefined || late.has(objective)) return;
      const entry = Object.freeze({
        reason: WATCHER_FAULT_PROOF_STARTED_LATE,
        detail: `${objective}: running after its latest safe start ${startedLate.latestSafeStartAtMs}; the proof must land before maturity at ${startedLate.maturityAtMs}`,
      });
      late.set(objective, entry);
      process.stderr.write(
        `${JSON.stringify({ packageName: WATCHER_PACKAGE_NAME, level: "error", ...entry })}\n`,
      );
      set("maturity_deadline_risk", job, true);
    },
    /** After a run: whether `holds` (the execution's) holds it paused on
     * prover funding. */
    funding: (
      job: WatcherProofObjective,
      holds: ReadonlyMap<string, Readonly<{ reason: string }>>,
    ): void => {
      const objective = objectiveOf(job);
      const paused =
        holds.get(objective)?.reason === WATCHER_PROVER_FUNDING_UNAVAILABLE;
      if (paused === unfunded.has(objective)) return;
      if (paused) unfunded.add(objective);
      else unfunded.delete(objective);
      set("prover_funding_unavailable", job, paused);
    },
    /** After a run: the forfeitures that stand in the objective's journal. */
    collateral: (job: WatcherProofObjective): void => {
      const objective = objectiveOf(job);
      const standing = activeCollateralForfeitures(
        workflowJournal.readExecution(job)?.entries ?? [],
      );
      const raised = forfeited.get(objective) ?? new Set<string>();
      for (const [txHash, event] of standing) {
        if (raised.has(txHash)) continue;
        process.stderr.write(
          `${JSON.stringify({
            packageName: WATCHER_PACKAGE_NAME,
            level: "error",
            reason: "prover_collateral_forfeited",
            detail: `${objective}: attempt ${txHash} landed failing phase 2; the chain took collateral ${event.collateralOutRefs.join(", ")}`,
          })}\n`,
        );
      }
      if (standing.size === 0) {
        if (forfeited.delete(objective))
          set("prover_collateral_forfeited", job, false);
        return;
      }
      forfeited.set(objective, new Set(standing.keys()));
      if (raised.size === 0) set("prover_collateral_forfeited", job, true);
    },
    /** The objective's correction was included or another record resolved
     * it: the race against maturity is over. */
    finished,
    /** The supervisor no longer drives the objective. */
    settled: (objective: WatcherProofObjective): void => {
      finished(objective);
      if (unfunded.delete(objectiveOf(objective)))
        set("prover_funding_unavailable", objective, false);
      if (forfeited.delete(objectiveOf(objective)))
        set("prover_collateral_forfeited", objective, false);
    },
    readiness: () => [...late.values()],
  });
};
