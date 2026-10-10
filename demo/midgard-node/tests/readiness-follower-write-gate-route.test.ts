/**
 * `/readyz` names this process's side of the follower write gate (plan
 * §8.1): unready until its follower-change driver applied a view, and while
 * a recompute of that driver is under way or held. The follower's own
 * readiness (catching up, lagging) is its reasons, read from the follower.
 */
import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { NodeConfig } from "../src/services/config.js";
import {
  DRIVER_RECOMPUTE_PENDING,
  FOLLOWER_VIEW_UNAPPLIED,
  type FollowerWriteGateLocal,
} from "../src/services/follower-write-gate.local.js";
import {
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import {
  runningFollower,
  seedCaughtUpL1Follower,
} from "./readiness-l1-follower.fixture.js";
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

type GateBody = {
  readonly epoch: string | null;
  readonly recomputing: boolean;
  readonly producers: number;
};

/** `/readyz` of a node whose follower is at the tip, with the gate's
 * local side `gate` (undefined: no driver applied a view). */
const readyz = (gate: Partial<FollowerWriteGateLocal> | undefined) =>
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
        if (gate === undefined)
          yield* Ref.set(globals.L1_FOLLOWER, runningFollower());
        else {
          yield* seedCaughtUpL1Follower(globals);
          yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
            ...local,
            ...gate,
          }));
        }
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
          readonly followerWriteGate: GateBody;
        };
        return {
          gate: body.reasons.filter(
            (reason) =>
              reason === FOLLOWER_VIEW_UNAPPLIED ||
              reason === DRIVER_RECOMPUTE_PENDING,
          ),
          followerWriteGate: body.followerWriteGate,
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
        // /readyz scopes DA removal outcomes by the verified deployment.
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provide(Globals.Default),
      ) as Effect.Effect<
        { gate: string[]; followerWriteGate: GateBody },
        unknown,
        never
      >,
    ),
  );

describe("GET /readyz follower write gate", () => {
  it("is unready until the driver applied a view", async () => {
    expect(await readyz(undefined)).toEqual({
      gate: [FOLLOWER_VIEW_UNAPPLIED],
      followerWriteGate: { epoch: null, recomputing: false, producers: 0 },
    });
  });

  it("is unready while the driver's recompute is under way or held", async () => {
    expect(await readyz({ epoch: "4", recomputing: true })).toEqual({
      gate: [DRIVER_RECOMPUTE_PENDING],
      followerWriteGate: { epoch: "4", recomputing: true, producers: 0 },
    });
  });

  it("names no gate reason once the driver's view is applied", async () => {
    expect(await readyz({ epoch: "5", recomputing: false })).toEqual({
      gate: [],
      followerWriteGate: { epoch: "5", recomputing: false, producers: 0 },
    });
  });
});
