import { SqlClient } from "@effect/sql/SqlClient";
import type { SqlError } from "@effect/sql/SqlError";
import { Cause, Clock, Effect, Metric, Ref, Schedule } from "effect";

import * as SettlementJournal from "../database/settlement.js";
import {
  EVENT_IDENTITY_CONFLICT,
  EVENT_UNDECODABLE,
} from "../l1-events/driver.js";
import { Globals } from "../services/globals.js";
import { ContractDeploymentIdentity } from "../services/midgard-contracts.js";
import { SETTLEMENT_JOB_FAILURE_CAP } from "../services/settlement-retry.js";
import { commitPlanningAgeMs } from "../workers/commit-planning-notice.js";

/**
 * Gauges the alert rules (`demo/midgard-node/alert-rules.yml`) read for state
 * the node otherwise reports only on `/readyz`: the local block frontier and
 * whether the commit pipeline has work (commit lag), the last planning mode
 * (catch-up), the follower's event refusals, and stuck settlement work. One
 * sampler publishes them under `listen --with-monitoring`; it reads, never
 * changes, the state it reports.
 */
const localBlockEndTimeGauge = Metric.gauge("midgard_local_block_end_time_ms", {
  description:
    "End time (POSIX ms) of the latest block the node accepted as its local frontier; 0 until startup reads it",
});

const commitPipelineIdleGauge = Metric.gauge("midgard_commit_pipeline_idle", {
  description:
    "1 when the last block-commitment tick found no work to commit, 0 when it did",
  bigint: true,
});

const commitPlanningCatchUpGauge = Metric.gauge(
  "midgard_commit_planning_catch_up",
  {
    description:
      "1 when the last selected commit plan is in catch-up mode, the commit pipeline has work and the plan is within the readiness age limit; 0 otherwise",
    bigint: true,
  },
);

const l1EventRefusalsGauge = Metric.gauge("midgard_l1_event_refusals", {
  description:
    "L1 events the follower driver's last applied view left out, by reason",
  bigint: true,
});

const settlementJobsStuckGauge = Metric.gauge("midgard_settlement_jobs_stuck", {
  description:
    "Unfinished settlement jobs whose consecutive failures reached the retry cap",
  bigint: true,
});

const settlementReconcileStuckGauge = Metric.gauge(
  "midgard_settlement_reconcile_stuck",
  {
    description:
      "1 when a settlement attempt's reconcile has failed up to the retry cap and blocks new settlement work",
    bigint: true,
  },
);

/** How often the sampler publishes, well inside a 15 s scrape interval. */
export const ALERT_SIGNALS_SAMPLE_INTERVAL_MS = 5_000;

const flag = (value: boolean) => Effect.succeed(value ? 1n : 0n);

/**
 * The in-memory signals: frontier, pipeline, planning mode, follower.
 * `planMaxAgeMs` is the age past which `/readyz` expires a selected plan.
 */
const sampleInMemorySignals = (planMaxAgeMs: number) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* localBlockEndTimeGauge(
      Effect.succeed(yield* Ref.get(globals.LATEST_LOCAL_BLOCK_END_TIME_MS)),
    );
    const idle = yield* Ref.get(globals.COMMIT_PIPELINE_IDLE);
    yield* commitPipelineIdleGauge(flag(idle));
    // An idle pipeline plans nothing, and readiness gives an expired plan no
    // active mode, so neither reports catch-up. A new planning attempt clears
    // the plan; while it runs the gauge keeps the last plan's mode.
    const planning = yield* Ref.get(globals.COMMIT_PLANNING);
    if (idle) yield* commitPlanningCatchUpGauge(flag(false));
    else if (planning !== null)
      yield* commitPlanningCatchUpGauge(
        flag(
          planning.selection.mode === "catch_up" &&
            commitPlanningAgeMs(planning, yield* Clock.currentTimeMillis) <=
              planMaxAgeMs,
        ),
      );
    // Without a running follower there is no driver view, so no refusals;
    // readiness names the follower's own state.
    const follower = yield* Ref.get(globals.L1_FOLLOWER);
    const running = follower.kind === "running" ? follower : undefined;
    const conflicts =
      running
        ?.holds()
        .filter(({ reason }) => reason === EVENT_IDENTITY_CONFLICT).length ?? 0;
    for (const [reason, count] of [
      [EVENT_UNDECODABLE, running?.refused?.().length ?? 0],
      [EVENT_IDENTITY_CONFLICT, conflicts],
    ] as const)
      yield* Metric.tagged(
        l1EventRefusalsGauge,
        "reason",
        reason,
      )(Effect.succeed(BigInt(count)));
    const settlement = yield* Ref.get(globals.SETTLEMENT_HEALTH);
    yield* settlementReconcileStuckGauge(
      flag(settlement.reconcileStuck !== undefined),
    );
  });

/** Stuck settlement jobs, read as `/readyz` reads them. */
const sampleSettlementJobs: Effect.Effect<
  void,
  SqlError,
  SqlClient | ContractDeploymentIdentity
> = Effect.gen(function* () {
  const { manifestId } = yield* ContractDeploymentIdentity;
  const stuck =
    manifestId === undefined
      ? { count: 0 }
      : yield* SettlementJournal.stuckJobs(
          manifestId,
          SETTLEMENT_JOB_FAILURE_CAP,
        );
  yield* settlementJobsStuckGauge(Effect.succeed(BigInt(stuck.count)));
});

/** Publishes every alert signal once. */
export const sampleAlertSignals = (
  planMaxAgeMs: number,
): Effect.Effect<
  void,
  SqlError,
  Globals | SqlClient | ContractDeploymentIdentity
> => Effect.zipRight(sampleInMemorySignals(planMaxAgeMs), sampleSettlementJobs);

/**
 * Repeats `sampleAlertSignals` on `schedule`. A failed sample is logged and
 * the next one runs: a database outage leaves the settlement gauge at its
 * last value while the in-memory gauges keep updating.
 */
export const alertSignalsFiber = (
  schedule: Schedule.Schedule<number>,
  planMaxAgeMs: number,
): Effect.Effect<
  void,
  never,
  Globals | SqlClient | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("Alert signal sampler fiber started.");
    yield* Effect.repeat(
      sampleInMemorySignals(planMaxAgeMs).pipe(
        Effect.zipRight(sampleSettlementJobs),
        Effect.withSpan("alert-signals-fiber"),
        Effect.catchAllCause((cause) =>
          Cause.isInterruptedOnly(cause)
            ? Effect.interrupt
            : Effect.logWarning(cause),
        ),
      ),
      schedule,
    );
  });
