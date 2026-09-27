import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { buildListenRouter } from "../src/commands/listen-router.js";
import { attestationTimeoutCorrectionReadinessBounds } from "../src/fibers/attestation-timeout-correction.js";
import { NodeConfig } from "../src/services/config.js";
import {
  type AttestationTimeoutCorrectionHealth,
  Globals,
  nextL1ProviderHealthEvidence,
} from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../src/services/midgard-contracts.js";
import { ValidationPool } from "../src/services/validation-pool.js";
import { provideDatabaseLayers } from "./utils.js";

const TICK_MS = 10_000;

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
  WAIT_BETWEEN_MERGE_TXS: TICK_MS,
  MPF_ENGINE: "legacy",
  MIN_QUEUE_LENGTH_FOR_MERGING: 1,
} as unknown as NodeConfig["Type"];

type ReadyzResult = {
  readonly status: number;
  readonly correctionReasons: readonly string[];
  readonly attestationTimeoutCorrection: AttestationTimeoutCorrectionHealth;
};

/** GET /readyz with the correction health the fiber would have recorded,
 * given the moment of the request. */
const readyz = (
  healthAt: (
    nowMs: number,
    current: AttestationTimeoutCorrectionHealth,
  ) => AttestationTimeoutCorrectionHealth,
) =>
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
        yield* Ref.update(
          globals.ATTESTATION_TIMEOUT_CORRECTION_HEALTH,
          (current) => healthAt(Date.now(), current),
        );
        const response = HttpServerResponse.toWeb(
          (yield* buildListenRouter().pipe(
            Effect.provideService(
              HttpServerRequest.HttpServerRequest,
              HttpServerRequest.fromWeb(
                new Request("http://midgard.test/readyz"),
              ),
            ),
          )) as HttpServerResponse.HttpServerResponse,
        );
        const body = (yield* Effect.promise(() => response.json())) as {
          readonly reasons: readonly string[];
          readonly attestationTimeoutCorrection: AttestationTimeoutCorrectionHealth;
        };
        return {
          status: response.status,
          correctionReasons: body.reasons.filter((reason) =>
            reason.startsWith("attestation_timeout_"),
          ),
          attestationTimeoutCorrection: body.attestationTimeoutCorrection,
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
      ) as unknown as Effect.Effect<ReadyzResult, unknown, never>,
    ),
  );

const headerHash = "ab".repeat(32);
const { stallBoundMs, queueUnknownBoundMs } =
  attestationTimeoutCorrectionReadinessBounds(TICK_MS);

describe("GET /readyz attestation-timeout correction", () => {
  it("reports no correction reason for a fresh node with nothing to correct", async () => {
    const result = await readyz((_nowMs, current) => current);
    expect(result.correctionReasons).toEqual([]);
    expect(result.attestationTimeoutCorrection).toMatchObject({
      consecutiveFailures: 0,
      oldestUnattestedHeader: null,
    });
  });

  it("is unready while failing steps leave a timed-out header uncorrected", async () => {
    const result = await readyz((nowMs, current) => ({
      ...current,
      consecutiveFailures: 3,
      lastError: "Kupo unavailable",
      oldestUnattestedHeader: { headerHash, deadlineMs: nowMs - 5_000 },
    }));
    expect(result.status).toBe(503);
    expect(result.correctionReasons).toHaveLength(1);
    expect(result.correctionReasons[0]).toMatch(
      new RegExp(`^attestation_timeout_correction_failing:${headerHash}:3:`),
    );
    expect(result.attestationTimeoutCorrection).toMatchObject({
      consecutiveFailures: 3,
      lastError: "Kupo unavailable",
      oldestUnattestedHeader: { headerHash },
    });
  });

  it("is unready while a step past the deadline stalls, against the bound the node's tick derives", async () => {
    const result = await readyz((nowMs, current) => ({
      ...current,
      lastProgressAtMs: nowMs - stallBoundMs - 60_000,
      oldestUnattestedHeader: { headerHash, deadlineMs: nowMs - 5_000 },
    }));
    expect(result.status).toBe(503);
    expect(result.correctionReasons).toHaveLength(1);
    expect(result.correctionReasons[0]).toMatch(
      new RegExp(
        `^attestation_timeout_correction_stalled:${headerHash}:\\d+:${stallBoundMs}$`,
      ),
    );
  });

  it("is unready once the queue has gone unread past its bound", async () => {
    const result = await readyz((nowMs, current) => ({
      ...current,
      lastQueueReadAtMs: nowMs - queueUnknownBoundMs - 60_000,
    }));
    expect(result.status).toBe(503);
    expect(result.correctionReasons).toHaveLength(1);
    expect(result.correctionReasons[0]).toMatch(
      new RegExp(
        `^attestation_timeout_queue_unknown:\\d+:${queueUnknownBoundMs}$`,
      ),
    );
  });
});
