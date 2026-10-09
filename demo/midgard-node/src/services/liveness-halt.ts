import { Duration, Effect, Metric, Option, Ref, Schedule } from "effect";

import type { Globals } from "./globals.globals.js";
import { l1ControlPlaneLivenessReasons } from "./globals.l1-control-plane.js";
import {
  clearLivenessReason,
  livenessReasonRaisedSinceMs,
  publishLivenessReasonAge,
  setLivenessReason,
} from "./globals.liveness-reasons.js";

/**
 * Conditions that once exited the process from a fiber now stay inside it: the
 * fiber that detects one raises a liveness reason under its source, keeps
 * re-evaluating on its own schedule, and clears the reason once the condition
 * is gone. While a halting source's reason is raised, the fibers whose effects
 * it must stop are held; ingestion, readiness and every other fiber keep
 * running.
 */
export const HaltSource = {
  /** This operator is removed (`operator_removed`, plan §7.5 R7): retired,
   * or in no list after having been active, in the follower's operator set
   * (see `publishOperatorMembership`). Holds every operator duty; a rollback
   * that undoes the removal clears it. */
  operatorMembership: "operator_membership",
  /** The node's instance lock is suspended or held by another process
   * (`node-instance-lock.ts`): another node may hold this database. Holds
   * every operator duty; the lock taken again clears it. */
  instanceLock: "node_instance_lock",
} as const;

export type HaltSource = (typeof HaltSource)[keyof typeof HaltSource];

/** The fibers that build or submit block commitments. */
export const COMMIT_HALT_SOURCES: readonly HaltSource[] = [
  HaltSource.operatorMembership,
  HaltSource.instanceLock,
];

/** The settlement worker. */
export const SETTLEMENT_HALT_SOURCES: readonly HaltSource[] = [
  HaltSource.operatorMembership,
  HaltSource.instanceLock,
];

/** The merge fiber, which folds the state queue into the confirmed state. */
export const MERGE_HALT_SOURCES: readonly HaltSource[] = [
  HaltSource.operatorMembership,
  HaltSource.instanceLock,
];

/** The operator watchdog, which takes over other operators' slots. */
export const WATCHDOG_HALT_SOURCES: readonly HaltSource[] = [
  HaltSource.operatorMembership,
  HaltSource.instanceLock,
];

/**
 * The fibers each halt holds, by their name in the node's fiber roster
 * (`nodeFibers`). Confirmation and state-queue correction are never held:
 * they re-derive the conditions they raise, and clear them.
 */
export const FIBER_HALT_SOURCES = {
  settlement: SETTLEMENT_HALT_SOURCES,
  blockCommitment: COMMIT_HALT_SOURCES,
  merge: MERGE_HALT_SOURCES,
  operatorWatchdog: WATCHDOG_HALT_SOURCES,
} as const satisfies Record<string, readonly HaltSource[]>;

export type HeldFiber = keyof typeof FIBER_HALT_SOURCES;

/** The source of the reasons the L1 control plane derives (see
 * `l1ControlPlaneLivenessReasons`); it raises none into `LIVENESS_REASONS`. */
export const L1_CONTROL_PLANE_LIVENESS_SOURCE = "l1_control_plane";

/** How often a held fiber re-reads whether its halt cleared. */
export const HALT_POLL_MS = 1_000;

export const livenessIncidentCounter = Metric.counter(
  "midgard_liveness_incident_total",
  {
    description:
      "Liveness incidents raised in place of a process exit, by reason. A raised reason stays in LIVENESS_REASONS, and holds the fibers its source halts, until its source clears it.",
    incremental: true,
  },
);

type Incident = {
  reason: string;
  since: number;
  escalated: boolean;
  escalateAfterMs: number | undefined;
};

type LivenessGlobals = Pick<Globals, "LIVENESS_REASONS">;

/** Per Globals instance, so each node (and each test) has its own. */
const incidents = new WeakMap<object, Map<string, Incident>>();

const incidentsOf = (globals: LivenessGlobals) => {
  let map = incidents.get(globals.LIVENESS_REASONS);
  if (map === undefined) {
    map = new Map();
    incidents.set(globals.LIVENESS_REASONS, map);
  }
  return map;
};

/**
 * Raises `reason` under `source` in `LIVENESS_REASONS`. A new or changed reason counts
 * once in `midgard_liveness_incident_total` and logs one warning; while it
 * persists it logs at debug, except once at error when it is still raised
 * `escalateAfterMs` after it was first raised. Never fails.
 */
export const raiseLivenessIncident = (
  globals: LivenessGlobals,
  source: string,
  reason: string,
  detail: string,
  options: { readonly escalateAfterMs?: number; readonly nowMs?: number } = {},
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const now = options.nowMs ?? Date.now();
    const map = incidentsOf(globals);
    const current = map.get(source);
    if (current === undefined || current.reason !== reason) {
      map.set(source, {
        reason,
        since: now,
        escalated: false,
        escalateAfterMs: options.escalateAfterMs,
      });
      yield* Metric.increment(
        Metric.tagged(livenessIncidentCounter, "reason", reason),
      );
      yield* Effect.logWarning(`${reason}: ${detail}`);
    } else if (
      !current.escalated &&
      options.escalateAfterMs !== undefined &&
      now - current.since >= options.escalateAfterMs
    ) {
      current.escalated = true;
      yield* Effect.logError(
        `${reason} unresolved for ${(now - current.since).toString()} ms: ${detail}`,
      );
    } else yield* Effect.logDebug(`${reason}: ${detail}`);
    if (current !== undefined && current.reason === reason)
      current.escalateAfterMs = options.escalateAfterMs;
    yield* setLivenessReason(globals, source, reason);
  });

/** Clears the reason `source` raised, logging once that it cleared. */
export const clearLivenessIncident = (
  globals: LivenessGlobals,
  source: string,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    const map = incidentsOf(globals);
    const current = map.get(source);
    if (current !== undefined) {
      map.delete(source);
      yield* Effect.logInfo(`${current.reason} cleared`);
    }
    yield* clearLivenessReason(globals, source);
  });

/** Clears `reason` under `source` only if it is the reason raised there, so
 * one evaluation never clears another's reason under a shared source. */
export const clearLivenessReasonIf = (
  globals: LivenessGlobals,
  source: string,
  reason: string,
): Effect.Effect<void> =>
  Effect.flatMap(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
    reasons.get(source) === reason
      ? clearLivenessIncident(globals, source)
      : Effect.void,
  );

/** One reason the node raises right now, as readiness reports it. */
export type ActiveLivenessReason = Readonly<{
  source: string;
  reason: string;
  /** Since the source last went from no raised reason to one; null when it
   * is not known (a derived reason, or one written straight into the Ref). */
  ageMs: number | null;
  /** The bound its source escalates it after, if it gave one. */
  escalateAfterMs: number | null;
  /** Raised for at least `escalateAfterMs`. Escalation is only reported: it
   * never stops the node and is never terminal. */
  escalated: boolean;
}>;

/**
 * Every liveness reason the node raises at `nowMs` (the same set as
 * `currentLivenessReasons`), each with its source, age and escalation, sorted
 * by reason. Also refreshes each raised source's age gauge.
 */
export const activeLivenessReasons = (
  globals: Pick<Globals, "LIVENESS_REASONS" | "L1_CONTROL_PLANE_ACTIVITY">,
  nowMs: number = Date.now(),
): Effect.Effect<readonly ActiveLivenessReason[]> =>
  Effect.gen(function* () {
    const raised = yield* Ref.get(globals.LIVENESS_REASONS);
    const activity = yield* Ref.get(globals.L1_CONTROL_PLANE_ACTIVITY);
    const map = incidentsOf(globals);
    const active: ActiveLivenessReason[] = [];
    for (const [source, reason] of raised) {
      const sinceMs = livenessReasonRaisedSinceMs(globals, source);
      const ageMs = sinceMs === undefined ? null : Math.max(0, nowMs - sinceMs);
      if (ageMs !== null) yield* publishLivenessReasonAge(source, ageMs);
      const incident = map.get(source);
      const escalateAfterMs =
        incident?.reason === reason ? (incident.escalateAfterMs ?? null) : null;
      active.push({
        source,
        reason,
        ageMs,
        escalateAfterMs,
        escalated:
          escalateAfterMs !== null &&
          ageMs !== null &&
          ageMs >= escalateAfterMs,
      });
    }
    for (const reason of l1ControlPlaneLivenessReasons(activity, nowMs))
      active.push({
        source: L1_CONTROL_PLANE_LIVENESS_SOURCE,
        reason,
        ageMs: null,
        escalateAfterMs: null,
        escalated: false,
      });
    return active.sort((a, b) =>
      a.reason < b.reason ? -1 : a.reason > b.reason ? 1 : 0,
    );
  });

const isHalted = (globals: LivenessGlobals, sources: readonly string[]) =>
  Effect.map(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
    sources.some((source) => reasons.has(source)),
  );

const poll = Schedule.spaced(Duration.millis(HALT_POLL_MS));

/** Completes once no source in `sources` has a raised reason. */
export const awaitHaltCleared = (
  globals: LivenessGlobals,
  sources: readonly string[],
): Effect.Effect<void> =>
  Effect.asVoid(
    Effect.repeat(isHalted(globals, sources), {
      schedule: poll,
      until: (halted) => !halted,
    }),
  );

/** Completes once a source in `sources` has a raised reason. */
const awaitHaltRaised = (
  globals: LivenessGlobals,
  sources: readonly string[],
): Effect.Effect<void> =>
  Effect.asVoid(
    Effect.repeat(isHalted(globals, sources), {
      schedule: poll,
      until: (halted) => halted,
    }),
  );

/**
 * `schedule`, held at each tick boundary while a source in `sources` has a
 * raised reason: the next tick starts only once the halt clears, and a tick
 * already running finishes. For a fiber that repeats one action on a
 * schedule, the halt never interrupts that action.
 */
export const pausedWhileHalted = <Out, In>(
  schedule: Schedule.Schedule<Out, In>,
  globals: LivenessGlobals,
  sources: readonly string[],
): Schedule.Schedule<Out, In> =>
  Schedule.modifyDelayEffect(schedule, (_, delay) =>
    Effect.sleep(delay).pipe(
      Effect.zipRight(awaitHaltCleared(globals, sources)),
      Effect.as(Duration.zero),
    ),
  );

/**
 * Runs `fiber` while no source in `sources` has a raised reason. A raised
 * reason interrupts it, and it starts again from the beginning once the
 * reason clears. For a fiber that already tolerates being stopped at any
 * point and starting again (a supervisor that restarts its worker, a fiber
 * whose start recovers from a stopped predecessor). Its own outcome is
 * returned unchanged.
 */
export const restartedAcrossHalts = <A, E, R>(
  globals: LivenessGlobals,
  sources: readonly string[],
  fiber: Effect.Effect<A, E, R>,
): Effect.Effect<A, E, R> =>
  Effect.gen(function* () {
    for (;;) {
      yield* awaitHaltCleared(globals, sources);
      const outcome = yield* Effect.raceFirst(
        Effect.map(fiber, Option.some),
        Effect.as(awaitHaltRaised(globals, sources), Option.none<A>()),
      );
      if (Option.isSome(outcome)) return outcome.value;
      yield* Effect.logWarning(
        `A fiber held by ${sources.join(", ")} stopped until the halt clears`,
      );
    }
  });

/** The landed-block rebase's native restore was refused because the native
 * MPF store retains no root of the processed landed chain in full (every
 * `restoreCanonicalRoot` it tried returned `NativeMpfRootNotRetained`).
 * Raised under the rebase's source (`landed_block_rebase`). The refusal
 * changes nothing: the rebase holds with native MPF, the SQL root and the
 * journals as they are and the follower write gate closed, and the
 * follower-change driver retries it on its backoff; the next rebase evaluation that runs, or
 * finds no rebase due, clears it. The node cannot put the root's closure back
 * itself: the operator stops it, installs a native MPF store that retains
 * the root in full, and restarts it. */
export const NATIVE_MPF_RESTORE_ROOT_NOT_RETAINED =
  "native_mpf_restore_root_not_retained";

/** The landed-block rebase's native restore was refused because the target
 * root's node closure is in the native MPF store but its full index is over
 * a full-index cap
 * (`NativeMpfFullIndexCapExceeded`, whose message names the cap,
 * `FULL_INDEX_MAX_RECORDS` or `FULL_INDEX_MAX_BYTES`, and its value). Raised
 * under the rebase's source. The refusal changes nothing: the rebase holds
 * with native MPF, the SQL root and the journals as they are, and the
 * follower write gate closed; every evaluation retries the restore, and the next
 * rebase evaluation that runs, or finds no rebase due, clears it. The caps
 * are fixed in the node build (the TypeScript owner and its native child
 * each enforce them), so the node cannot load the root
 * until it runs a build whose caps cover it. */
export const NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED =
  "native_mpf_restore_index_cap_exceeded";

/** The landed-block rebase's native restore was refused because reading the
 * target root's node closure from the native MPF store failed
 * (`NativeMpfRestoreReadFailed`): a LevelDB read error other than a missing,
 * corrupt or undecodable record. Raised under the rebase's source.
 * The refusal changes nothing, and every evaluation retries the restore on
 * the follower-change driver's backoff; the first that reads the closure clears it
 * (or raises the refusal that read finds). Escalated, in the log and on
 * readiness, after `NATIVE_MPF_RESTORE_READ_ESCALATION_MS`. */
export const NATIVE_MPF_RESTORE_READ_TRANSIENT =
  "native_mpf_restore_read_transient";

/** How long `NATIVE_MPF_RESTORE_READ_TRANSIENT` stays raised before it is
 * escalated: ten minutes (thirty nominal L1 blocks), well past the
 * follower-change driver's maximum backoff,
 * so a read that fails for that long is not a passing blip. */
export const NATIVE_MPF_RESTORE_READ_ESCALATION_MS = 10 * 60_000;

/** The source the commit worker's DA frame notices raise under. It holds no
 * fiber: the block is already refused on every tick (by the pre-submit frame
 * check, or by committing nothing while the transactions stay pending), so
 * the reason only makes that refusal visible on /readyz. The next measured
 * tick whose block fits the frame, or the next tick with no transaction or
 * user event pending, clears it; a block left unmeasured, or a tick that
 * returns before the step-down for another reason, leaves it as it is. */
export const COMMIT_DA_FRAME_SOURCE = "commit_da_frame";

/** A block's events alone overflow the DA frame although its empty block
 * fits: the step-down has no transaction left to drop. */
export const COMMIT_DA_FRAME_EVENTS_OVERFLOW =
  "commit_da_frame_events_overflow";

/** The base ledger's empty block alone exceeds the DA frame (every payload
 * carries the full post-block ledger). */
export const COMMIT_DA_FRAME_LEDGER_CEILING = "commit_da_frame_ledger_ceiling";
