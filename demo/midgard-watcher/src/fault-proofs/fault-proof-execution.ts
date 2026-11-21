import {
  assertWorkflowActuationPermitIdentity,
  FraudProofL1CheckpointChangedError,
  type FraudProofWorkflowTerminal,
  isSqliteBusyError,
  isWorkflowActuationRevokedError,
  type WorkflowActuationPermit,
  type WorkflowActuationRevokedError,
} from "@al-ft/midgard-fault-proofs";
import { createWorkflowObjectiveWallet } from "@al-ft/midgard-fault-proofs/workflow/workflow-run";

import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";
import type { WatcherOperationsSink } from "../runtime/operations-observability.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.canonical-json.js";
import { isWatcherRetainedDaTransportUnavailable } from "../storage/retained-da-transport-unavailable.js";
import type {
  WatcherCompletedFaultProofVerification,
  WatcherFaultProofApplication,
} from "./fault-proof-application.js";
import * as stall from "./fault-proof-continuation-stall.js";
import {
  holdWatcherFaultProofOnReplay,
  l1RefusalOf,
  WATCHER_FAULT_PROOF_L1_REFUSED,
} from "./fault-proof-execution-holds.js";
import { createWatcherFaultProofObjectiveAlerts } from "./fault-proof-objective-alerts.js";
import type { WatcherProofObjective } from "./fault-proof-objective-journal.js";
import type { WatcherFaultProofDepartureStep } from "./fault-proof-progress-authority.types.js";
import {
  holdsReadiness,
  quarantineCause,
  quarantineSettlement,
  quarantineWatcherFaultProof,
  type WatcherFaultProofHolds,
} from "./fault-proof-quarantine.js";
import type {
  WatcherFaultProofDeadline,
  WatcherFaultProofJob,
} from "./fault-proof-supervisor.js";
import { isWatcherPreflightStalledResult } from "./preflight-stall-retry.js";
import { isWatcherProofDecisionMissingError } from "./watcher-decision-hold.js";

/** Chosen by the supervisor for one run. */
export type WatcherFaultProofExecutionAdmission = Readonly<{
  mode: "run" | "resume";
  /** The deadline whose latest safe start the run is past, before or after
   * maturity (a resumed job carries none). */
  startedLate?: WatcherFaultProofDeadline;
}>;

export type WatcherFaultProofExecutionOutcome =
  | Readonly<{ kind: "completed" | "terminal_included"; result: unknown }>
  /** The header was removed against a record this objective's run did not
   * create, with every transaction the run sent settled: the objective is
   * done, but not as our conviction. */
  | Readonly<{ kind: "resolved_by_another_record"; result: unknown }>
  | stall.WatcherFaultProofPendingOutcome
  | Readonly<{
      kind: "authority_revoked";
      error: WorkflowActuationRevokedError;
    }>
  | Readonly<{
      kind: "retryable";
      resume: "backoff";
      reason: string;
      retryAfterMs: number;
    }>;

export type WatcherFaultProofExecution = Readonly<{
  /** Rechecks the objective's completion on L1; `terminal` is a runner's
   * fresh result, absent when only the chain says what completed. */
  verifyCompleted(input: {
    readonly job: WatcherFaultProofJob;
    readonly actuationPermit: WorkflowActuationPermit;
    readonly terminal?: FraudProofWorkflowTerminal;
  }): Promise<
    | WatcherCompletedFaultProofVerification
    | Readonly<{ kind: "pending"; reason: string }>
    | Extract<WatcherFaultProofExecutionOutcome, { kind: "retryable" }>
  >;
  execute(input: {
    readonly job: WatcherFaultProofJob;
    readonly actuationPermit: WorkflowActuationPermit;
    readonly admission: WatcherFaultProofExecutionAdmission;
  }): Promise<WatcherFaultProofExecutionOutcome>;
  /**
   * Objectives held on a refused L1 read, a stalled session, unfunded prover
   * wallet, quarantined run or held script re-execution
   * (`fault_proof_l1_refused:<reason>`, `fault_proof_continuation_stalled`,
   * `prover_funding_unavailable`, `fault_proof_objective_quarantined`,
   * `validation_trace_replay_held`), until a run or completion check
   * meets something else, a stalled one's later run makes progress, or it
   * is settled.
   */
  readiness(): readonly Readonly<{ reason: string; detail: string }>[];
  /**
   * The supervisor no longer drives the objective (held under another name,
   * completed, or its header left the queue, merged or removed), so no run
   * or completion check would clear its hold: this does.
   */
  objectiveSettled(objective: WatcherProofObjective): void;
  /** Records the proof step an objective whose header left the queue takes
   * from the chain alone: no run happens for it. */
  departureStep(step: WatcherFaultProofDepartureStep): void;
}>;

/** The identity a proof step of the objective's turn is recorded under. */
const proofActionIdentity = ({
  category,
  headerHash,
  decisionDigest,
  rollbackGeneration,
}: Omit<WatcherFaultProofDepartureStep, "step">): string =>
  watcherSha256CanonicalJson({
    category,
    headerHash,
    decisionDigest,
    rollbackGeneration,
  });

/** Executes one admitted turn. Scheduling and retries belong to the
 * supervisor; this adapter owns invocation and its holds. */
export const createWatcherFaultProofExecution = (dependencies: {
  readonly application: Readonly<{
    runners: Partial<WatcherFaultProofApplication["runners"]>;
    runOrResume: WatcherFaultProofApplication["runOrResume"];
    verifyCompleted: WatcherFaultProofApplication["verifyCompleted"];
  }>;
  /** Forwarded to the shared workflow runner, whose contract carries it; the
   * watcher's loader runs on the configuration startup admitted. */
  readonly runtimeConfigPath: string;
  readonly deploymentFingerprint: string;
  readonly operationsSink: Pick<
    WatcherOperationsSink,
    "recordProofStep" | "setAlert"
  >;
}): WatcherFaultProofExecution => {
  // A refused L1 read says nothing about the fault and is not a process
  // failure: the objective is held by name and runs again on the next
  // observation, which clears or renews the hold.
  const holds: WatcherFaultProofHolds = new Map();
  const alerts = createWatcherFaultProofObjectiveAlerts(dependencies);
  const objectiveOf = ({ category, headerHash }: WatcherProofObjective) =>
    `${category}/${headerHash}`;
  const holdOnRefusal = (
    job: WatcherFaultProofJob,
    error: unknown,
  ): Readonly<{ kind: "pending"; reason: string }> | undefined => {
    const refusal = l1RefusalOf(error);
    if (refusal === undefined) return undefined;
    const reason = `${WATCHER_FAULT_PROOF_L1_REFUSED}:${refusal.reason}`;
    holds.set(objectiveOf(job), {
      reason,
      detail: `${objectiveOf(job)}: ${refusal.detail}`,
    });
    return { kind: "pending", reason: `${reason}: ${refusal.detail}` };
  };
  return Object.freeze({
    readiness: () =>
      Object.freeze([...holdsReadiness(holds), ...alerts.readiness()]),
    objectiveSettled: (objective) => {
      const settle = quarantineSettlement(holds, objective, dependencies);
      holds.delete(objectiveOf(objective));
      settle();
      alerts.settled(objective);
    },
    departureStep: ({ step, ...action }) =>
      dependencies.operationsSink.recordProofStep({
        decisionDigest: action.decisionDigest,
        stage: "terminal",
        actionIdentityDigest: proofActionIdentity(action),
        status: step,
        updatedAtMs: Date.now().toString(),
      }),
    verifyCompleted: async ({ job, actuationPermit, terminal }) => {
      const identity = assertWorkflowActuationPermitIdentity({
        permit: actuationPermit,
        category: job.category,
        rollbackGeneration: job.rollbackGeneration,
      });
      if (
        identity.headerHash !== job.headerHash ||
        identity.decisionDigest !== job.decisionDigest ||
        identity.deploymentFingerprint !== dependencies.deploymentFingerprint
      )
        throw new Error(
          "completed proof verification changed its admitted objective",
        );
      const settle = quarantineSettlement(holds, job, dependencies);
      try {
        const verification = await dependencies.application.verifyCompleted({
          category: job.category,
          headerHash: job.headerHash,
          decisionDigest: job.decisionDigest,
          ...(terminal === undefined ? {} : { terminal }),
        });
        holds.delete(objectiveOf(job));
        return verification;
      } catch (cause) {
        const held = holdOnRefusal(job, cause);
        if (held !== undefined) return held;
        holds.delete(objectiveOf(job));
        if (isWatcherL1TransientFailure(cause))
          return {
            kind: "retryable",
            resume: "backoff",
            reason: cause.message,
            retryAfterMs: 1_000,
          };
        throw cause;
      } finally {
        settle();
      }
    },
    execute: async ({ job, actuationPermit, admission }) => {
      const { category, headerHash, decisionDigest } = job;
      const actionIdentityDigest = proofActionIdentity(job);
      const record = (
        status: Parameters<
          WatcherOperationsSink["recordProofStep"]
        >[0]["status"],
        stage: "prepare" | "terminal" = "terminal",
      ) =>
        dependencies.operationsSink.recordProofStep({
          decisionDigest,
          stage,
          actionIdentityDigest,
          status,
          updatedAtMs: Date.now().toString(),
        });
      record("preflight", "prepare");
      // Any outcome but a refusal or quarantine ends the hold.
      let held: ReturnType<typeof holdOnRefusal>;
      const settleQuarantine = quarantineSettlement(holds, job, dependencies);
      // Set while the objective's own run is in hand: a failure there
      // quarantines this objective alone (fault-proof-quarantine.ts).
      let objectiveWork = false;
      const quarantine = (cause: string) => {
        held = { kind: "pending", reason: cause };
        record("failed");
        return quarantineWatcherFaultProof({
          holds,
          job,
          decisionDigest,
          sink: dependencies.operationsSink,
          cause,
        });
      };
      try {
        const authority = assertWorkflowActuationPermitIdentity({
          permit: actuationPermit,
          category,
          rollbackGeneration: job.rollbackGeneration,
        });
        if (
          authority.deploymentFingerprint !==
            dependencies.deploymentFingerprint ||
          authority.headerHash !== headerHash ||
          authority.decisionDigest !== decisionDigest ||
          admission.mode !== job.mode ||
          (authority.authority === "reconciliation" &&
            admission.mode !== "resume")
        )
          throw new Error(
            "fault-proof execution admission changed its objective or authority",
          );
        const runner = dependencies.application.runners[category];
        if (runner === undefined)
          throw new Error(
            `fault-proof execution has no installed ${category} runner`,
          );
        objectiveWork = true;
        alerts.started(job, admission.startedLate);
        const result = await dependencies.application.runOrResume({
          category,
          deploymentFingerprint: dependencies.deploymentFingerprint,
          headerHash,
          decisionDigest,
          actuationPermit,
          objectiveWallet: createWorkflowObjectiveWallet({
            category,
            headerHash,
          }),
          runtimeConfigPath: dependencies.runtimeConfigPath,
          onProgress: stall.releaseWatcherFaultProofStallOnProgress(holds, job),
        });
        if (typeof result === "object" && result !== null && "kind" in result) {
          if (
            result.kind === "completed" ||
            result.kind === "terminal_included"
          ) {
            record(result.kind === "completed" ? "completed" : "confirmed");
            alerts.finished(job);
            return { kind: result.kind, result };
          }
          // Not our conviction: the proof step ends cancelled, not completed.
          if (result.kind === "resolved_by_another_record") {
            record("cancelled");
            alerts.finished(job);
            return { kind: result.kind, result };
          }
          // A dispute that made its move and awaits the other party's response
          // (`awaiting_counterparty`) is healthy: each observation re-drives it
          // until the response lands or its deadline lets this side move again.
          if (
            "reason" in result &&
            typeof result.reason === "string" &&
            (result.kind === "pending" ||
              result.kind === "awaiting_counterparty" ||
              isWatcherPreflightStalledResult(result))
          ) {
            held = stall.holdWatcherFaultProofPending(holds, job, result);
            record("reconciling");
            return stall.watcherFaultProofPendingOutcome(result);
          }
          if (
            result.kind === "stalled" &&
            "reason" in result &&
            typeof result.reason === "string"
          )
            return quarantine(`workflow stalled: ${result.reason}`);
        }
        // Everything else contradicts this objective's own decision: a family
        // that re-classifies the watcher's admitted fault as
        // `no_fault_detected` or `unprovable_gap`, or an outcome of no known
        // kind. That holds this objective alone; the process keeps running.
        const reason =
          typeof result === "object" &&
          result !== null &&
          "reason" in result &&
          typeof result.reason === "string"
            ? result.reason
            : typeof result === "object" &&
                result !== null &&
                "kind" in result &&
                (result.kind === "no_fault_detected" ||
                  result.kind === "unprovable_gap")
              ? `the family's own classification returned ${result.kind} for an admitted fault`
              : "invalid execution outcome";
        return quarantine(`workflow did not progress: ${reason}`);
      } catch (error) {
        if (isWorkflowActuationRevokedError(error)) {
          record("cancelled");
          return { kind: "authority_revoked", error };
        }
        // The supervisor holds the objective by name, or
        // holds on a busy database and requeues it (journal_busy).
        if (
          isWatcherProofDecisionMissingError(error) ||
          isSqliteBusyError(error)
        ) {
          record("reconciling");
          throw error;
        }
        held = holdOnRefusal(job, error);
        if (held !== undefined) {
          record("reconciling");
          return { ...held, resume: "await_observation" };
        }
        if (error instanceof FraudProofL1CheckpointChangedError) {
          record("reconciling");
          return {
            kind: "pending",
            resume: "await_observation",
            reason: error.message,
          };
        }
        const replay = holdWatcherFaultProofOnReplay(holds, job, error);
        if (replay !== undefined) {
          held = { kind: "pending", reason: replay.reason };
          record("reconciling");
          return replay;
        }
        // Any read that only failed to reach the follower, the node or a
        // provider, or found this watcher's own DA node not up yet, waits and
        // runs again; it says nothing about the fault.
        if (
          isWatcherL1TransientFailure(error) ||
          isWatcherRetainedDaTransportUnavailable(error)
        ) {
          record("reconciling");
          return {
            kind: "retryable",
            resume: "backoff",
            reason: error.message,
            retryAfterMs: 1_000,
          };
        }
        const cause = objectiveWork ? quarantineCause(error) : undefined;
        if (cause !== undefined) return quarantine(cause);
        record("failed");
        dependencies.operationsSink.setAlert({
          code: "proof_submission_failure",
          subjectDigest: decisionDigest,
          active: true,
          observedAtMs: Date.now().toString(),
        });
        throw error;
      } finally {
        if (held === undefined) holds.delete(objectiveOf(job));
        alerts.funding(job, holds);
        settleQuarantine();
      }
    },
  });
};
