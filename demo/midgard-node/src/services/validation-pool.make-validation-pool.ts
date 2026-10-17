import { findLedgerSignatureVerifierUnavailable } from "@al-ft/midgard-core";
import {
  deserializePhaseACandidate,
  type PhaseAResult,
  type RejectedTx,
} from "@al-ft/midgard-validation";
import { Duration, Effect, Either, Layer, Metric } from "effect";

import {
  LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS,
  LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
} from "../commands/listen-startup.await-ledger-signature-verifier.js";
import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import {
  packPhaseAJob,
  type ValidationJobRequest,
  type ValidationWorkerResponse,
} from "../workers/utils/validation-pool.js";
import { NodeConfig } from "./config.js";
import { ContractDeploymentIdentity } from "./midgard-contracts.js";
import { retryStartupStep, startupStepFailed } from "./startup-waiting.js";
import {
  busyWorkersGauge,
  deserializeDuration,
  phaseAJobDuration,
  poolSizeGauge,
  queueDepthGauge,
  serializeDuration,
  ValidationJobTimeoutError,
  ValidationPool,
  type ValidationPoolService,
  ValidationWorkerError,
  ValidationWorkerStoppedError,
} from "./validation-pool.bundled-worker-exec-argv.js";
import { FixedValidationWorkerPool } from "./validation-pool.fixed-validation-worker-pool.js";

const poolStartFailure = (cause: unknown): ValidationWorkerError =>
  cause instanceof ValidationWorkerError
    ? cause
    : new ValidationWorkerError({
        message: "validation worker pool startup failed",
        cause,
      });

/** The startup step the validation workers' readiness reports under. */
const VALIDATION_WORKERS_STEP = "validation_workers";

/** A validation worker stopped while it answered its readiness job. */
const VALIDATION_WORKER_CRASHED = "validation_worker_crashed";

/** A validation worker's module, or one it imports, did not load. */
const VALIDATION_WORKER_LOAD_FAILED = "validation_worker_load_failed";

/** The validation workers did not start, for a reason not named above. */
const VALIDATION_WORKER_START_FAILED = "validation_worker_start_failed";

/** Node's codes for a worker entry, or a module it imports, that cannot be
 * resolved or loaded. */
const MODULE_LOAD_CODES: ReadonlySet<unknown> = new Set([
  "ERR_MODULE_NOT_FOUND",
  "MODULE_NOT_FOUND",
  "ERR_UNSUPPORTED_DIR_IMPORT",
  "ERR_UNKNOWN_FILE_EXTENSION",
  "ERR_UNKNOWN_MODULE_FORMAT",
  "ERR_WORKER_PATH",
  "ERR_INVALID_URL",
  "ERR_INVALID_URL_SCHEME",
  "ERR_REQUIRE_ESM",
]);

/** `error` and the causes it carries, outermost first. */
const causes = (error: unknown): readonly unknown[] => {
  const chain: unknown[] = [];
  for (let next = error; next !== undefined && chain.length < 8; ) {
    chain.push(next);
    next =
      typeof next === "object" && next !== null && "cause" in next
        ? (next as { readonly cause?: unknown }).cause
        : undefined;
  }
  return chain;
};

/**
 * A pool start that failed because a worker did not become ready to verify:
 * its Ed25519 verifier did not initialise (the worker's typed fault), or it
 * did not answer its readiness job within the job timeout.
 */
const workerNotReady = (error: ValidationWorkerError): boolean =>
  findLedgerSignatureVerifierUnavailable(error) !== undefined ||
  error.cause instanceof ValidationJobTimeoutError;

/** A pool start that failed because a worker's module, or one it imports,
 * did not load: it cannot be resolved, or it does not parse or link. */
const workerModuleNotLoaded = (error: ValidationWorkerError): boolean =>
  causes(error).some(
    (cause) =>
      (cause instanceof Error && cause.name === "SyntaxError") ||
      (typeof cause === "object" &&
        cause !== null &&
        MODULE_LOAD_CODES.has((cause as { readonly code?: unknown }).code)),
  );

/** A pool start that failed because a worker stopped while it answered its
 * readiness job (`ValidationWorkerStoppedError`: an error it did not handle,
 * its heap limit reached, a non-zero exit), other than a module that did not
 * load. */
const workerStopped = (error: ValidationWorkerError): boolean =>
  causes(error).some(
    (cause) => cause instanceof ValidationWorkerStoppedError,
  ) && !workerModuleNotLoaded(error);

/**
 * Starts the validation workers. Each worker initialises its own Ed25519
 * verifier, which the parent's readiness does not establish, and the pool
 * starts once every worker answered its readiness job.
 *
 * A start that fails on what is plausibly transient is retried on a fresh
 * pool under the ledger signature verifier's startup bound
 * (`LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS`), reported under
 * `validation_workers`:
 *
 * - a worker that did not become ready (`workerNotReady`), under
 *   `ledger_signature_verifier_unavailable`;
 * - a worker that stopped (`workerStopped`: an error it did not handle, its
 *   heap limit reached, `ERR_WORKER_OUT_OF_MEMORY`, or a non-zero exit),
 *   under `validation_worker_crashed`. A worker whose module throws while it
 *   is evaluated stops the same way and is retried the same way.
 *
 * Past the bound the step fails exhausted (`StartupStepFailedError`), so the
 * process exits non-zero for its supervisor to restart it. Any other start
 * failure is not retried: the step fails at once, marked terminal, so the
 * process exits non-zero rather than holds, under
 * `validation_worker_load_failed` for a worker module that did not load and
 * `validation_worker_start_failed` otherwise.
 */
const startValidationWorkers = (makePool: () => FixedValidationWorkerPool) => {
  const start = Effect.acquireRelease(
    Effect.suspend(() => {
      const pool = makePool();
      return Effect.tryPromise({
        try: () => pool.start(),
        catch: poolStartFailure,
      }).pipe(Effect.as(pool));
    }),
    (pool) => Effect.promise(() => pool.close()),
  ).pipe(
    Effect.map(
      (pool): Either.Either<FixedValidationWorkerPool, ValidationWorkerError> =>
        Either.right(pool),
    ),
    Effect.catchIf(
      (error) => !workerNotReady(error) && !workerStopped(error),
      (error) => Effect.succeed(Either.left(error)),
    ),
  );
  const retryDelayMs = Duration.toMillis(
    Duration.decode(LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS.retryDelay),
  );
  return retryStartupStep(start, {
    key: VALIDATION_WORKERS_STEP,
    reason: (error) =>
      workerStopped(error)
        ? VALIDATION_WORKER_CRASHED
        : LEDGER_SIGNATURE_VERIFIER_UNAVAILABLE,
    retryable: () => true,
    budget: {
      maxAttempts: LEDGER_SIGNATURE_VERIFIER_STARTUP_BOUNDS.maxAttempts,
    },
    initialMs: retryDelayMs,
    maxMs: retryDelayMs,
  }).pipe(
    Effect.flatMap((started) =>
      Either.isLeft(started)
        ? Effect.fail(
            startupStepFailed({
              step: VALIDATION_WORKERS_STEP,
              reason: workerModuleNotLoaded(started.left)
                ? VALIDATION_WORKER_LOAD_FAILED
                : VALIDATION_WORKER_START_FAILED,
              cause: started.left,
              terminal: true,
            }),
          )
        : Effect.succeed(started.right),
    ),
  );
};

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
      evaluateCekVerdict: () =>
        Effect.fail(
          new ValidationWorkerError({ message: "validation pool is disabled" }),
        ),
      tryEvaluateCekVerdict: () => Effect.succeed(undefined),
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
  const pool = yield* startValidationWorkers(
    () =>
      new FixedValidationWorkerPool(
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
        },
      ),
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
    catch: poolStartFailure,
  }).pipe(
    Effect.tap(() => poolSizeGauge(Effect.succeed(size))),
    Effect.ensuring(reportStats),
  );

  const service: ValidationPoolService = {
    poolSize: size,
    consensusProfile: deploymentIdentity.consensusProfile,
    ready,
    stats: Effect.sync(() => pool.stats()),
    evaluateCekVerdict: (request) =>
      runJob({
        kind: "cek_verdict",
        jobId: pool.allocateJobId(),
        request,
      }).pipe(
        Effect.flatMap((response) =>
          response.kind === "cek_verdict"
            ? Effect.succeed(response.result)
            : Effect.fail(
                new ValidationWorkerError({
                  message: `expected cek_verdict response, got ${response.kind}`,
                }),
              ),
        ),
      ),
    tryEvaluateCekVerdict: (request) =>
      Effect.suspend(() => {
        const pending = pool.trySubmitAdvisory({
          kind: "cek_verdict",
          jobId: pool.allocateJobId(),
          request,
        });
        if (pending === undefined) return Effect.succeed(undefined);
        return Effect.tryPromise({
          try: () => pending,
          catch: (cause) =>
            new ValidationWorkerError({
              message: "advisory evaluation worker failed",
              cause,
            }),
        }).pipe(
          Effect.flatMap((response) =>
            response.kind === "cek_verdict"
              ? Effect.succeed(response.result)
              : Effect.fail(
                  new ValidationWorkerError({
                    message: "unexpected advisory evaluation response",
                  }),
                ),
          ),
          Effect.ensuring(reportStats),
        );
      }),
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
  return yield* ready.pipe(Effect.as(service));
});

export const validationPoolLayer = Layer.scoped(
  ValidationPool,
  makeValidationPool,
);
