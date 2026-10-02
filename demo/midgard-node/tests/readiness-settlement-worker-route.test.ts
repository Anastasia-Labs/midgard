import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { NodeConfig } from "../src/services/config.js";
import {
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import type { SettlementHealth } from "../src/services/settlement.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { provideDatabaseLayers } from "./utils.js";

// Only the settings /readyz reads. Fresh exact provider evidence keeps the
// handler off Lucid and the contracts, so those are never dereferenced.
const nodeConfig = {
  READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS: 60_000,
  L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: 1_000,
  READINESS_MAX_HEARTBEAT_AGE_MS: 60_000,
  READINESS_MAX_DURABLE_ADMISSION_BACKLOG: 1_000,
  READINESS_MAX_DURABLE_ADMISSION_AGE_MS: 60_000,
  UNCONFIRMED_BLOCK_MAX_AGE_MS: 60_000,
  VALIDATION_WORKER_JOB_TIMEOUT_MS: 60_000,
  STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS: 60_000,
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} as unknown as NodeConfig["Type"];

type ReadyzSettlement = {
  readonly settlementReasons: readonly string[];
  readonly settlement: SettlementHealth;
};

const readyz = (health: SettlementHealth) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* Ref.update(globals.L1_PROVIDER_HEALTH, (current) =>
          nextL1ProviderHealthEvidence({
            current,
            healthy: true,
            observedAtMs: Date.now(),
            successKind: "exact",
          }),
        );
        yield* Ref.set(globals.SETTLEMENT_HEALTH, health);
        const response = (yield* buildListenRouter().pipe(
          Effect.provideService(
            HttpServerRequest.HttpServerRequest,
            HttpServerRequest.fromWeb(
              new Request("http://midgard.test/readyz"),
            ),
          ),
        )) as HttpServerResponse.HttpServerResponse;
        const body = (yield* Effect.promise(() =>
          HttpServerResponse.toWeb(response).json(),
        )) as {
          readonly reasons: readonly string[];
          readonly settlement: SettlementHealth;
        };
        return {
          settlementReasons: body.reasons.filter((reason) =>
            reason.startsWith("settlement"),
          ),
          settlement: body.settlement,
        };
      }).pipe(
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
      ) as unknown as Effect.Effect<ReadyzSettlement, unknown, never>,
    ),
  );

const health = (
  state: SettlementHealth["state"],
  detail: string,
  workerFailures?: SettlementHealth["workerFailures"],
): SettlementHealth => ({
  observedAt: Date.now(),
  state,
  detail,
  ...(workerFailures === undefined ? {} : { workerFailures }),
});

describe("GET /readyz settlement worker", () => {
  it("stays ready through eligibility waits, pending confirmations and failing jobs", async () => {
    for (const value of [
      health(
        "waiting",
        "settlement eligibility wait until 2026-09-30T20:00:00.000Z",
      ),
      health("waiting", "waiting for authenticated confirmation depth"),
      health("waiting", "waiting for the previous settlement ownership lease"),
      health(
        "error",
        "settlement deferred until 2026-09-30: No spendable reserve UTxO",
      ),
      // Two dead runs, or three within the window, are not yet a crash loop.
      health("error", "no error reported (worker exited 1)", {
        count: 2,
        since: Date.now() - 10 * 60_000,
        last: "no error reported (worker exited 1)",
      }),
      health("error", "no error reported (worker exited 1)", {
        count: 3,
        since: Date.now() - 60_000,
        last: "no error reported (worker exited 1)",
      }),
    ])
      expect((await readyz(value)).settlementReasons).toEqual([]);
  });

  it("goes unready naming a worker that keeps dying, until a replacement stays up", async () => {
    const last =
      "Worker terminated due to reaching memory limit: JS heap out of memory\n    at stack";
    const failing = {
      count: 4,
      since: Date.now() - 3 * 60_000,
      last,
    };
    const dying = await readyz(health("error", last, failing));
    expect(dying.settlementReasons).toEqual([
      "settlement_worker_failing:4:Worker terminated due to reaching memory limit: JS heap out of memory",
    ]);
    expect(dying.settlement.workerFailures).toEqual(failing);
    // A replacement on probation still carries the streak.
    expect(
      (await readyz(health("running", "settlement queue drained", failing)))
        .settlementReasons,
    ).toHaveLength(1);
    expect(
      (await readyz(health("running", "settlement queue drained")))
        .settlementReasons,
    ).toEqual([]);
  });
});
