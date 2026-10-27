import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { sampleAlertSignals } from "../src/fibers/alert-signals.js";
import {
  EVENT_IDENTITY_CONFLICT,
  EVENT_UNDECODABLE,
  type EventRefusal,
} from "../src/l1-events/driver.js";
import { Globals } from "../src/services/globals.js";
import {
  L1_NODE_CONFIG_FAILED,
  type L1FollowerHandle,
  startingStatus,
} from "../src/services/l1-follower.readiness.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import { SETTLEMENT_JOB_FAILURE_CAP } from "../src/services/settlement-retry.js";
import {
  applyCommitPlanningNotice,
  makeCommitPlanningNotice,
} from "../src/workers/commit-planning-notice.js";
import { commitPlanningSelection } from "./helpers/commit-planning-selection.js";
import {
  sampleValue,
  startPrometheusExporter,
} from "./prometheus-scrape.fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const deploymentId = "a7".repeat(32);

/** The node's default `READINESS_MAX_HEARTBEAT_AGE_MS`, which `/readyz`
 * applies to a selected plan. */
const PLAN_MAX_AGE_MS = 120_000;

/** A plan selected just now, inside the readiness age limit. */
const freshSelection = (mode: "normal" | "catch_up") => ({
  ...commitPlanningSelection(mode),
  observedAtMs: Date.now(),
});

const refusal = (
  reason: EventRefusal["reason"],
  key: string,
): EventRefusal => ({
  kind: "deposit",
  key,
  idCbor: "d8799f00ff",
  reason,
  detail: "refused",
});

/** A running follower whose driver reports `refused` and `conflicts`. */
const runningFollower = (
  refused: readonly EventRefusal[],
  conflicts: number,
): L1FollowerHandle => ({
  kind: "running",
  status: () => startingStatus,
  holds: () => [
    { reason: "l1_follower_catching_up", detail: "behind" },
    ...Array.from({ length: conflicts }, (_, index) => ({
      reason: EVENT_IDENTITY_CONFLICT,
      detail: `deposit ${index.toString()}`,
    })),
  ],
  planCurrent: async () => ({ kind: "none", detail: "no view" }),
  refused: () => refused,
});

const insertStuckJob = (eventId: string, failures: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO settlement_jobs
      (deployment_id, kind, event_id, phase, failures, last_error)
      VALUES (${deploymentId}, 'withdrawal', ${eventId}, 'conclude',
        ${failures}, 'builder refused')`;
  });

// Only the manifest id the sampler reads settlement jobs under.
const identity = {
  kind: "manifest",
  manifestId: deploymentId,
} as unknown as ContractDeploymentIdentity;

let exporter: Awaited<ReturnType<typeof startPrometheusExporter>>;
beforeAll(async () => {
  exporter = await startPrometheusExporter();
});
afterAll(async () => {
  await exporter.stop();
});

/** Runs `body` against the production sampler and a real exporter scrape. */
const run = <A>(
  body: (
    sampleAndScrape: Effect.Effect<string, unknown, unknown>,
  ) => Effect.Effect<A, unknown, unknown>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        return yield* body(
          sampleAlertSignals(PLAN_MAX_AGE_MS).pipe(
            Effect.zipRight(Effect.promise(() => exporter.scrape())),
          ),
        );
      }),
    ).pipe(
      Effect.provideService(ContractDeploymentIdentity, identity),
      Effect.provide(Globals.Default),
    ) as Effect.Effect<A, unknown, never>,
  );

describe("alert signal metrics", () => {
  it("exports the local block frontier, the commit pipeline's idleness and the planning mode", async () => {
    await run((sampleAndScrape) =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          1_700_000_000_000,
        );
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, false);
        yield* applyCommitPlanningNotice(
          globals,
          makeCommitPlanningNotice(freshSelection("catch_up")),
        );
        const catchingUp = yield* sampleAndScrape;
        expect(sampleValue(catchingUp, "midgard_local_block_end_time_ms")).toBe(
          1_700_000_000_000,
        );
        expect(sampleValue(catchingUp, "midgard_commit_pipeline_idle")).toBe(0);
        expect(
          sampleValue(catchingUp, "midgard_commit_planning_catch_up"),
        ).toBe(1);

        // Back in normal mode the gauge clears.
        yield* applyCommitPlanningNotice(
          globals,
          makeCommitPlanningNotice(freshSelection("normal")),
        );
        yield* Ref.set(
          globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
          1_700_000_060_000,
        );
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, true);
        const normal = yield* sampleAndScrape;
        expect(sampleValue(normal, "midgard_commit_planning_catch_up")).toBe(0);
        expect(sampleValue(normal, "midgard_local_block_end_time_ms")).toBe(
          1_700_000_060_000,
        );
        expect(sampleValue(normal, "midgard_commit_pipeline_idle")).toBe(1);

        // A new planning attempt clears the plan; the gauge keeps the mode
        // of the last plan selected rather than guessing.
        yield* applyCommitPlanningNotice(globals, {
          type: "CommitPlanningNotice",
          status: "unavailable",
          reason: "planning_started",
        });
        expect(
          sampleValue(
            yield* sampleAndScrape,
            "midgard_commit_planning_catch_up",
          ),
        ).toBe(0);
      }),
    );
  });

  it("reports catch-up only while the pipeline has work and the plan is current", async () => {
    await run((sampleAndScrape) =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        const catchUp = () =>
          Effect.map(sampleAndScrape, (scrape) =>
            sampleValue(scrape, "midgard_commit_planning_catch_up"),
          );
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, false);
        yield* applyCommitPlanningNotice(
          globals,
          makeCommitPlanningNotice(freshSelection("catch_up")),
        );
        expect(yield* catchUp()).toBe(1);

        // The backlog is committed and the pipeline goes idle: no plan runs,
        // so the last catch-up plan no longer describes the node.
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, true);
        expect(yield* catchUp()).toBe(0);

        // Work returns while the catch-up plan is still current.
        yield* Ref.set(globals.COMMIT_PIPELINE_IDLE, false);
        expect(yield* catchUp()).toBe(1);

        // A catch-up plan older than readiness's age limit asserts nothing.
        yield* applyCommitPlanningNotice(
          globals,
          makeCommitPlanningNotice(commitPlanningSelection("catch_up")),
        );
        expect(yield* catchUp()).toBe(0);
      }),
    );
  });

  it("exports the follower's event refusals by reason, and clears them", async () => {
    await run((sampleAndScrape) =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.set(
          globals.L1_FOLLOWER,
          runningFollower(
            [
              refusal(EVENT_UNDECODABLE, "01"),
              refusal(EVENT_UNDECODABLE, "02"),
            ],
            1,
          ),
        );
        const refusing = yield* sampleAndScrape;
        expect(
          sampleValue(refusing, "midgard_l1_event_refusals", {
            reason: EVENT_UNDECODABLE,
          }),
        ).toBe(2);
        expect(
          sampleValue(refusing, "midgard_l1_event_refusals", {
            reason: EVENT_IDENTITY_CONFLICT,
          }),
        ).toBe(1);

        yield* Ref.set(globals.L1_FOLLOWER, runningFollower([], 0));
        const cleared = yield* sampleAndScrape;
        expect(
          sampleValue(cleared, "midgard_l1_event_refusals", {
            reason: EVENT_UNDECODABLE,
          }),
        ).toBe(0);
        expect(
          sampleValue(cleared, "midgard_l1_event_refusals", {
            reason: EVENT_IDENTITY_CONFLICT,
          }),
        ).toBe(0);

        // A follower that stops running has no driver view: its last
        // refusals no longer describe the node.
        yield* Ref.set(
          globals.L1_FOLLOWER,
          runningFollower([refusal(EVENT_UNDECODABLE, "03")], 2),
        );
        yield* sampleAndScrape;
        yield* Ref.set(globals.L1_FOLLOWER, {
          kind: "failed",
          reason: L1_NODE_CONFIG_FAILED,
          detail: "node config refused",
        });
        const stopped = yield* sampleAndScrape;
        expect(
          sampleValue(stopped, "midgard_l1_event_refusals", {
            reason: EVENT_UNDECODABLE,
          }),
        ).toBe(0);
        expect(
          sampleValue(stopped, "midgard_l1_event_refusals", {
            reason: EVENT_IDENTITY_CONFLICT,
          }),
        ).toBe(0);
      }),
    );
  });

  it("exports stuck settlement jobs and a stuck reconcile, and clears them once resolved", async () => {
    await run((sampleAndScrape) =>
      Effect.gen(function* () {
        const globals = yield* Globals;
        const sql = yield* SqlClient.SqlClient;
        // Failing below the cap is not stuck.
        yield* insertStuckJob("cc".repeat(34), SETTLEMENT_JOB_FAILURE_CAP - 1);
        const failing = yield* sampleAndScrape;
        expect(sampleValue(failing, "midgard_settlement_jobs_stuck")).toBe(0);
        expect(sampleValue(failing, "midgard_settlement_reconcile_stuck")).toBe(
          0,
        );

        yield* insertStuckJob("dd".repeat(34), SETTLEMENT_JOB_FAILURE_CAP);
        yield* insertStuckJob("ee".repeat(34), SETTLEMENT_JOB_FAILURE_CAP + 3);
        yield* Ref.update(globals.SETTLEMENT_HEALTH, (health) => ({
          ...health,
          reconcileStuck: {
            txHash: "ff".repeat(32),
            failures: SETTLEMENT_JOB_FAILURE_CAP,
            lastError: "provider refused",
          },
        }));
        const stuck = yield* sampleAndScrape;
        expect(sampleValue(stuck, "midgard_settlement_jobs_stuck")).toBe(2);
        expect(sampleValue(stuck, "midgard_settlement_reconcile_stuck")).toBe(
          1,
        );

        yield* sql`UPDATE settlement_jobs SET phase = 'complete'
          WHERE deployment_id = ${deploymentId}`;
        yield* Ref.update(globals.SETTLEMENT_HEALTH, (health) => ({
          ...health,
          reconcileStuck: undefined,
        }));
        const resolved = yield* sampleAndScrape;
        expect(sampleValue(resolved, "midgard_settlement_jobs_stuck")).toBe(0);
        expect(
          sampleValue(resolved, "midgard_settlement_reconcile_stuck"),
        ).toBe(0);
      }),
    );
  });
});
