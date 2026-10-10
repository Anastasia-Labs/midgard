import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import {
  Deferred,
  Duration,
  Effect,
  Fiber,
  Logger,
  Option,
  Ref,
  Schedule,
} from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import {
  READINESS_FAILURE_MAX_CHARS,
  readinessFailureSummary,
  refreshExactL1ProviderEvidence,
  runL1ProviderReadinessRefresher,
} from "../src/fibers/l1-provider-readiness-refresher.js";
import { NodeConfig } from "../src/services/config.js";
import {
  Globals,
  type L1ProviderHealthEvidence,
  withL1ControlPlane,
} from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { provideDatabaseLayers } from "./utils.js";

// Only the settings /readyz reads. None of these cases reaches the direct
// provider preflight: evidence is either fresh or blocked on exact evidence.
const nodeConfig = {
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 30_000,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  READINESS_MAX_HEARTBEAT_AGE_MS: 60_000,
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: 1_000,
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: 60_000,
  UNCONFIRMED_BLOCK_MAX_AGE_MS: 60_000,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: 60_000,
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: 60_000,
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} as unknown as NodeConfig["Type"];

const slot = () => ({
  source: "l1_node_tip" as const,
  currentSlot: 42,
  observedAtMs: Date.now(),
  slotLengthMs: 1_000,
});

type ProviderReadiness = {
  readonly reasons: readonly string[];
  readonly details: readonly string[];
  readonly providerQueryHealthy: boolean;
  readonly providerQueryMode: string;
  readonly providerQueryError: string | null;
};

/** Serves one /readyz request. */
const readyz = buildListenRouter().pipe(
  Effect.provideService(
    HttpServerRequest.HttpServerRequest,
    HttpServerRequest.fromWeb(new Request("http://midgard.test/readyz")),
  ),
  Effect.flatMap((response) =>
    Effect.promise(() =>
      HttpServerResponse.toWeb(
        response as HttpServerResponse.HttpServerResponse,
      ).json(),
    ),
  ),
  Effect.map((body) => body as ProviderReadiness),
);

const runWithNode = <A, E, R>(program: Effect.Effect<A, E, R>): Promise<A> =>
  Effect.runPromise(
    provideDatabaseLayers(
      program.pipe(
        Effect.provideService(NodeConfig, nodeConfig),
        Effect.provideService(ValidationPool, {
          poolSize: 1,
          stats: Effect.succeed({
            oldestInFlightAgeMs: 0,
            liveWorkers: 1,
            restartingWorkers: 0,
          }),
        } as unknown as ValidationPool["Type"]),
        Effect.provideService(Lucid, {} as Lucid),
        Effect.provideService(MidgardContracts, {} as MidgardContracts),
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provide(Globals.Default),
      ),
    ) as Effect.Effect<A, unknown, never>,
  );

/**
 * A commit-like worker that re-acquires the control plane every millisecond
 * and holds it for 25ms, so an opportunistic acquire almost never finds it
 * free.
 */
const busyWorker = (globals: Globals) =>
  Effect.repeat(
    withL1ControlPlane(
      globals,
      { scope: "block_commitment", maxHoldMs: 5_000 },
      Effect.sleep(Duration.millis(25)),
    ),
    Schedule.spaced(Duration.millis(1)),
  );

const provider = (body: ProviderReadiness) => ({
  healthy: body.providerQueryHealthy,
  error: body.providerQueryError,
  providerReasons: body.reasons.filter((reason) =>
    reason.startsWith("provider_query_unhealthy"),
  ),
});

/**
 * Runs the refresher over one exact success until the probe has run three
 * more times, then serves /readyz while it is still repeating.
 */
const refreshRounds = ({
  probe,
  logger = Logger.none,
}: {
  readonly probe: Effect.Effect<ReturnType<typeof slot>, Error>;
  readonly logger?: Logger.Logger<unknown, void>;
}) =>
  runWithNode(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* refreshExactL1ProviderEvidence({
        globals,
        probe: Effect.sync(slot),
        maxHoldMs: 1_000,
        waitTimeoutMs: 1_000,
      });
      const probes = yield* Ref.make(0);
      const refresher = yield* Effect.fork(
        runL1ProviderReadinessRefresher({
          globals,
          probe: Ref.update(probes, (count) => count + 1).pipe(
            Effect.zipRight(probe),
          ),
          maxHoldMs: 1_000,
          waitTimeoutMs: 1_000,
          schedule: Schedule.spaced(Duration.millis(10)),
        }).pipe(Effect.provide(Logger.add(logger))),
      );
      yield* Ref.get(probes).pipe(
        Effect.repeat({
          schedule: Schedule.spaced(Duration.millis(5)),
          until: (count) => count >= 3 || refresher.unsafePoll() !== null,
        }),
      );
      const evidence = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
      const body = yield* readyz;
      const stillRunning = Option.isNone(yield* Fiber.poll(refresher));
      yield* Fiber.interrupt(refresher);
      return { probes: yield* Ref.get(probes), evidence, body, stillRunning };
    }),
  );

/** A path rooted at `/`, as a stack frame or module path prints it. */
const ABSOLUTE_PATH = /(?:^|[\s(])\/[^\s/]+\//u;

/** Collects every logged message. */
const logSink = () => {
  const messages: string[] = [];
  const logger = Logger.make(({ message }) => {
    messages.push((Array.isArray(message) ? message : [message]).join(" "));
  });
  return { messages, logger };
};

describe("L1 provider readiness refresher", () => {
  it("publishes a probe defect as a one-line exact failure over the last exact success", async () => {
    const sink = logSink();
    const result = await refreshRounds({
      probe: Effect.sync(() => {
        throw new Error("lucid.config() threw");
      }),
      logger: sink.logger,
    });

    expect(result.stillRunning).toBe(true);
    expect(result.probes).toBeGreaterThanOrEqual(3);
    expect(result.evidence).toMatchObject({
      lastObservationKind: "exact_failure",
      lastExactObservationKind: "exact_failure",
      lastExactFailure: "Error: lucid.config() threw",
    });
    expect(provider(result.body)).toEqual({
      healthy: false,
      error: "Error: lucid.config() threw",
      providerReasons: [],
    });
    // Within the provider bound of the last success, a failure is a
    // degradation detail, not an unready reason.
    expect(result.body.details).toContainEqual(
      expect.stringMatching(/^provider_query_degraded:l1-provider:\d+$/u),
    );
    // The public /readyz carries no stack trace and no install path.
    for (const published of [
      result.body.providerQueryError,
      JSON.stringify(result.body),
    ]) {
      expect(published).not.toMatch(/\n|\\n/u);
      expect(published).not.toMatch(ABSOLUTE_PATH);
    }
    // The node log keeps the whole defect, stack trace included.
    const logged = sink.messages.find((message) =>
      message.startsWith(
        "L1 provider readiness refresh failed: Error: lucid.config() threw\n",
      ),
    );
    expect(logged).toMatch(/\n\s+at /u);
    expect(logged).toMatch(ABSOLUTE_PATH);
  });

  it("publishes the first line of a multi-line typed failure, bounded", async () => {
    const long = `HubOracle read failed: ${"x".repeat(400)}`;
    const result = await refreshRounds({
      probe: Effect.fail(new Error(`${long}\nresponse body line two`)),
    });

    expect(result.evidence.lastExactFailure).toBe(
      `Error: ${long}`.slice(0, READINESS_FAILURE_MAX_CHARS),
    );
    expect(result.body.providerQueryError).toBe(
      result.evidence.lastExactFailure,
    );
  });

  it("keeps a short one-line failure whole", () => {
    expect(readinessFailureSummary("Error: kupo 503")).toBe("Error: kupo 503");
    expect(readinessFailureSummary("\n  Error: kupo 503  \r\n    at f")).toBe(
      "Error: kupo 503",
    );
    expect(readinessFailureSummary("")).toBe(
      "L1 provider readiness probe failed",
    );
  });

  it("publishes a typed probe success as exact success", async () => {
    const result = await refreshRounds({ probe: Effect.sync(slot) });

    expect(result.stillRunning).toBe(true);
    expect(result.probes).toBeGreaterThanOrEqual(3);
    expect(result.evidence).toMatchObject({
      lastObservationKind: "exact_success",
      lastExactObservationKind: "exact_success",
      lastExactFailure: null,
    });
    expect(provider(result.body)).toEqual({
      healthy: true,
      error: null,
      providerReasons: [],
    });
  });

  it("keeps refreshing after a round dies past the probe", async () => {
    let sinkFailures = 0;
    // A log sink that throws once on the refresh-failure warning.
    const flakySink = Logger.make(({ message }) => {
      if (
        sinkFailures === 0 &&
        String(message).includes("L1 provider readiness refresh failed")
      ) {
        sinkFailures += 1;
        throw new Error("log sink unavailable");
      }
    });
    const result = await refreshRounds({
      probe: Effect.fail(new Error("HubOracle read failed: kupo 503")),
      logger: flakySink,
    });

    expect(sinkFailures).toBe(1);
    expect(result.stillRunning).toBe(true);
    expect(result.probes).toBeGreaterThanOrEqual(3);
    expect(result.evidence.lastExactFailure).toBe(
      "Error: HubOracle read failed: kupo 503",
    );
  });

  it("keeps exact evidence fresh while a worker keeps the control plane busy", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const worker = yield* Effect.fork(busyWorker(globals));
        yield* Effect.sleep(Duration.millis(5));
        const probes = yield* Ref.make(0);
        const refresher = yield* Effect.fork(
          runL1ProviderReadinessRefresher({
            globals,
            probe: Ref.update(probes, (count) => count + 1).pipe(
              Effect.map(slot),
            ),
            maxHoldMs: 1_000,
            waitTimeoutMs: 5_000,
            schedule: Schedule.spaced(Duration.millis(40)),
          }),
        );
        yield* Effect.sleep(Duration.millis(400));
        yield* Fiber.interrupt(refresher);
        const evidence = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
        const exactAgeMs = Date.now() - evidence.lastExactSuccessAtMs;
        // The worker still holds the control plane while /readyz is served.
        const body = yield* readyz;
        yield* Fiber.interrupt(worker);
        return {
          probes: yield* Ref.get(probes),
          evidence,
          exactAgeMs,
          body,
        };
      }),
    );

    // Several refreshes landed between the worker's holds, each one exact.
    expect(result.probes).toBeGreaterThanOrEqual(3);
    expect(result.evidence).toMatchObject({
      lastObservationKind: "exact_success",
      lastExactObservationKind: "exact_success",
      lastExactEvidenceRevision: result.evidence.evidenceRevision,
    });
    expect(result.exactAgeMs).toBeLessThan(200);
    expect(provider(result.body)).toEqual({
      healthy: true,
      error: null,
      providerReasons: [],
    });
    expect(result.body.providerQueryMode).toBe("cached_fresh");
  });

  it("reports the real provider error when the exact read fails", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const succeeded = yield* refreshExactL1ProviderEvidence({
          globals,
          probe: Effect.sync(slot),
          maxHoldMs: 1_000,
          waitTimeoutMs: 1_000,
        });
        const failed = yield* refreshExactL1ProviderEvidence({
          globals,
          probe: Effect.fail(new Error("HubOracle read failed: kupo 503")),
          maxHoldMs: 1_000,
          waitTimeoutMs: 1_000,
        });
        return { succeeded, failed, body: yield* readyz };
      }),
    );

    expect(result.succeeded.kind).toBe("published");
    expect(result.failed).toMatchObject({
      kind: "published",
      evidence: {
        lastObservationKind: "exact_failure",
        lastExactFailure: "Error: HubOracle read failed: kupo 503",
      },
    });
    expect(provider(result.body)).toEqual({
      healthy: false,
      error: "Error: HubOracle read failed: kupo 503",
      providerReasons: [],
    });
    // Within the provider bound of the last success, a failure is a
    // degradation detail, not an unready reason.
    expect(result.body.details).toContainEqual(
      expect.stringMatching(/^provider_query_degraded:l1-provider:\d+$/u),
    );
  });

  it("reports expired exact evidence it cannot refresh, as a degradation within the provider bound", async () => {
    const result = await runWithNode(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const nowMs = Date.now();
        // The live shape: a direct success 64s old on an exact success past
        // its 180s max age, so the direct probe may no longer refresh it.
        const stale: L1ProviderHealthEvidence = {
          evidenceRevision: 2,
          lastObservationKind: "direct_success",
          lastExactEvidenceRevision: 1,
          lastExactObservationKind: "exact_success",
          lastSuccessAtMs: nowMs - 64_000,
          lastExactSuccessAtMs: nowMs - 181_000,
          lastExactFailureAtMs: 0,
          lastExactFailure: null,
          lastSuccessKind: "direct",
          lastFailureAtMs: 0,
          lastFailure: null,
          lastLedgerSlot: slot(),
        };
        yield* Ref.set(globals.L1_PROVIDER_HEALTH, stale);
        const entered = yield* Deferred.make<void>();
        const holder = yield* Effect.fork(
          withL1ControlPlane(
            globals,
            { scope: "block_commitment", maxHoldMs: 5_000 },
            Deferred.succeed(entered, undefined).pipe(
              Effect.zipRight(Effect.never),
            ),
          ),
        );
        yield* Deferred.await(entered);
        const probes = yield* Ref.make(0);
        const outcome = yield* refreshExactL1ProviderEvidence({
          globals,
          probe: Ref.update(probes, (count) => count + 1).pipe(
            Effect.map(slot),
          ),
          maxHoldMs: 1_000,
          waitTimeoutMs: 50,
        });
        const evidence = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
        const body = yield* readyz;
        yield* Fiber.interrupt(holder);
        return {
          outcome,
          probes: yield* Ref.get(probes),
          unchanged: evidence === stale,
          body,
        };
      }),
    );

    // A wait that never acquired observed nothing and publishes nothing.
    expect(result.outcome).toEqual({ kind: "control_plane_unavailable" });
    expect(result.probes).toBe(0);
    expect(result.unchanged).toBe(true);
    expect(provider(result.body)).toMatchObject({
      healthy: false,
      providerReasons: [],
    });
    // Within the provider bound of the last success, a failure is a
    // degradation detail, not an unready reason.
    expect(result.body.details).toContainEqual(
      expect.stringMatching(/^provider_query_degraded:l1-provider:\d+$/u),
    );
    expect(result.body.providerQueryError).toMatch(
      /^Exact HubOracle prerequisite is 18\d{4}ms old \(max 180000ms\)$/,
    );
  });

  it("bounds its hold so a hung probe cannot keep a commit waiting", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const refresh = yield* Effect.fork(
          refreshExactL1ProviderEvidence({
            globals,
            probe: Effect.never,
            maxHoldMs: 50,
            waitTimeoutMs: 1_000,
          }),
        );
        yield* Effect.sleep(Duration.millis(5));
        const commitStartedAtMs = Date.now();
        yield* withL1ControlPlane(
          globals,
          { scope: "block_commitment", maxHoldMs: 5_000 },
          Effect.void,
        );
        const commitWaitMs = Date.now() - commitStartedAtMs;
        return { outcome: yield* Fiber.join(refresh), commitWaitMs };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.commitWaitMs).toBeLessThan(1_000);
    expect(result.outcome).toMatchObject({
      kind: "published",
      evidence: {
        lastObservationKind: "exact_failure",
        lastExactFailure:
          "L1ControlPlaneTimeoutError: L1 control-plane scope l1_provider_readiness_refresh exceeded 50ms",
      },
    });
  });
});
