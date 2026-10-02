import {
  deserializePhaseACandidate,
  type PhaseAResult,
  type RejectedTx,
} from "@al-ft/midgard-validation";
import { Duration, Effect, Layer, Metric } from "effect";

import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import {
  packPhaseAJob,
  type ValidationJobRequest,
  type ValidationWorkerResponse,
} from "../workers/utils/validation-pool.js";
import { NodeConfig } from "./config.js";
import { ContractDeploymentIdentity } from "./midgard-contracts.js";
import {
  busyWorkersGauge,
  deserializeDuration,
  phaseAJobDuration,
  poolSizeGauge,
  queueDepthGauge,
  serializeDuration,
  ValidationPool,
  type ValidationPoolService,
  ValidationWorkerError,
} from "./validation-pool.bundled-worker-exec-argv.js";
import { FixedValidationWorkerPool } from "./validation-pool.fixed-validation-worker-pool.js";

const makeValidationPool = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const deploymentIdentity = yield* ContractDeploymentIdentity;
  const size = config.VALIDATION_WORKER_POOL_SIZE;
  if (size === 0) {
    return {
      poolSize: 0,
      consensusProfile: deploymentIdentity.consensusProfile,
      runPhaseAChunk: () =>
        Effect.fail(
          new ValidationWorkerError({ message: "validation pool is disabled" }),
        ),
      ready: Effect.void,
      stats: Effect.succeed({
        busyWorkers: 0,
        queueDepth: 0,
        oldestInFlightAgeMs: 0,
        liveWorkers: 0,
        restartingWorkers: 0,
      }),
    } satisfies ValidationPoolService;
  }

  const entry = resolveWorkerEntry(import.meta.url, "validation.js");
  const pool = new FixedValidationWorkerPool(
    size,
    size * 4,
    config.VALIDATION_WORKER_JOB_TIMEOUT_MS,
    entry,
    {
      config: {
        expectedNetworkId: config.NETWORK === "Mainnet" ? 1n : 0n,
        minFeeA: config.MIN_FEE_A,
        minFeeB: config.MIN_FEE_B,
        strictnessProfile: config.VALIDATION_STRICTNESS_PROFILE,
        consensusProfile: deploymentIdentity.consensusProfile,
      },
      signatureVerifier: config.VALIDATION_WORKER_NODE_ED25519 ? "node" : "cml",
    },
  );

  const reportStats = Effect.sync(() => pool.stats()).pipe(
    Effect.tap((stats) => busyWorkersGauge(Effect.succeed(stats.busyWorkers))),
    Effect.tap((stats) => queueDepthGauge(Effect.succeed(stats.queueDepth))),
    Effect.asVoid,
  );
  const runJob = (
    request: ValidationJobRequest,
  ): Effect.Effect<ValidationWorkerResponse, ValidationWorkerError> =>
    Effect.tryPromise({
      try: () => pool.submit(request),
      catch: (cause) =>
        cause instanceof ValidationWorkerError
          ? cause
          : new ValidationWorkerError({
              message: "validation worker job failed",
              cause,
            }),
    }).pipe(Effect.ensuring(reportStats));

  const ready = Effect.tryPromise({
    try: () => pool.start(),
    catch: (cause) =>
      cause instanceof ValidationWorkerError
        ? cause
        : new ValidationWorkerError({
            message: "validation worker pool startup failed",
            cause,
          }),
  }).pipe(
    Effect.tap(() => poolSizeGauge(Effect.succeed(size))),
    Effect.ensuring(reportStats),
  );

  const service: ValidationPoolService = {
    poolSize: size,
    consensusProfile: deploymentIdentity.consensusProfile,
    ready,
    stats: Effect.sync(() => pool.stats()),
    runPhaseAChunk: (txs) => {
      const serializeStartedAt = Date.now();
      const request = packPhaseAJob(pool.allocateJobId(), txs);
      const jobStartedAt = Date.now();
      return Metric.update(
        serializeDuration,
        Duration.millis(Date.now() - serializeStartedAt),
      ).pipe(
        Effect.zipRight(
          runJob(request).pipe(
            Effect.flatMap((response) => {
              if (response.kind !== "phase_a") {
                return Effect.fail(
                  new ValidationWorkerError({
                    message: `expected phase_a response, got ${response.kind}`,
                  }),
                );
              }
              const deserializeStartedAt = Date.now();
              const accepted = [];
              const rejected: RejectedTx[] = [];
              for (const result of response.results) {
                if (result.ok) {
                  accepted.push(deserializePhaseACandidate(result.candidate));
                } else {
                  rejected.push({
                    txId: Buffer.from(
                      result.txId.buffer,
                      result.txId.byteOffset,
                      result.txId.byteLength,
                    ),
                    code: result.code,
                    detail: result.detail,
                  });
                }
              }
              return Metric.update(
                deserializeDuration,
                Duration.millis(Date.now() - deserializeStartedAt),
              ).pipe(Effect.as({ accepted, rejected } satisfies PhaseAResult));
            }),
            Effect.ensuring(
              Metric.update(
                phaseAJobDuration,
                Duration.millis(Date.now() - jobStartedAt),
              ).pipe(Effect.asVoid),
            ),
          ),
        ),
      );
    },
  };
  return yield* Effect.acquireRelease(ready.pipe(Effect.as(service)), () =>
    Effect.promise(() => pool.close()),
  );
});

export const validationPoolLayer = Layer.scoped(
  ValidationPool,
  makeValidationPool,
);
