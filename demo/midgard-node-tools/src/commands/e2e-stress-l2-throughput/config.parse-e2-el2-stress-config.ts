import { join } from "node:path";

import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
  parseAddressArgument,
  parseNodeEndpoint,
} from "midgard-node/commands/command-utils";

import {
  parseCorpusShape,
  parseLoadModel,
  parseMode,
  parseNonNegativeBigInt,
  parseNonNegativeInteger,
  parsePositiveBigInt,
  parsePositiveInteger,
  parsePositiveNumber,
  parseWorkloadProfile,
} from "./config.parse-workload-profile.js";
import {
  DEFAULT_ACCEPTANCE_TIMEOUT_MS,
  DEFAULT_AGGREGATE_OBSERVER_INTERVAL_MS,
  DEFAULT_COMMIT_OBSERVATION_TIMEOUT_MS,
  DEFAULT_CONCURRENCY,
  DEFAULT_COUNT,
  DEFAULT_FEE_HEADROOM_LOVELACE,
  DEFAULT_FINALITY_OBSERVER_MAX_CONCURRENT_REQUESTS,
  DEFAULT_LOVELACE,
  DEFAULT_MAX_SUBMISSION_FAILURES,
  DEFAULT_NO_OP_CALIBRATION_DURATION_MS,
  DEFAULT_OPEN_LOOP_DURATION_MS,
  DEFAULT_OPEN_LOOP_MAX_IN_FLIGHT,
  DEFAULT_OPEN_LOOP_TARGET_RATE_TPS,
  DEFAULT_POLL_INITIAL_INTERVAL_MS,
  DEFAULT_POLL_INTERVAL_MS,
  DEFAULT_POLL_MAX_INTERVAL_MS,
  DEFAULT_SUBMIT_REQUEST_TIMEOUT_MS,
  MAX_DEFAULT_CONCURRENCY,
  MAX_DEFAULT_COUNT,
} from "./constants.js";
import { timestampForPath } from "./runtime.js";
import {
  type E2EL2StressConfig,
  type ParseE2EL2StressOptions,
} from "./types.js";
import {
  resolveStressWallet,
  resolveStressWallets,
  validateDistinctStressWallets,
} from "./wallets.js";

export const parseE2EL2StressConfig = ({
  endpoint,
  loadModel: rawLoadModel,
  workloadProfile: rawWorkloadProfile,
  mode: rawMode,
  corpusShape: rawCorpusShape,
  corpusPath,
  corpusSliceId,
  targetRateTps: rawTargetRateTps,
  openLoopDurationMs: rawOpenLoopDurationMs,
  openLoopWarmupCount: rawOpenLoopWarmupCount,
  openLoopCooldownCount: rawOpenLoopCooldownCount,
  openLoopMaxInFlight: rawOpenLoopMaxInFlight,
  noOpCalibrationEndpoint,
  requireNoOpCalibration = false,
  noOpCalibrationDurationMs: rawNoOpCalibrationDurationMs,
  aggregateObserverIntervalMs: rawAggregateObserverIntervalMs,
  count: rawCount,
  concurrency: rawConcurrency,
  lovelace: rawLovelace,
  feeHeadroomLovelace: rawFeeHeadroomLovelace,
  walletSeedPhrase,
  walletSeedPhraseEnv = DEFAULT_WALLET_SEED_ENV,
  stressWalletSeedPhraseEnvs = [],
  l2Address,
  runId,
  outDir,
  pollIntervalMs: rawPollIntervalMs,
  pollInitialIntervalMs: rawPollInitialIntervalMs,
  pollMaxIntervalMs: rawPollMaxIntervalMs,
  submitRequestTimeoutMs: rawSubmitRequestTimeoutMs,
  acceptanceTimeoutMs: rawAcceptanceTimeoutMs,
  commitObservationTimeoutMs: rawCommitObservationTimeoutMs,
  finalityObserverMaxConcurrentRequests:
    rawFinalityObserverMaxConcurrentRequests,
  maxSubmissionFailures: rawMaxSubmissionFailures,
  network = "Preprod",
  env = process.env,
  allowUnsafeBounds = false,
}: ParseE2EL2StressOptions): E2EL2StressConfig => {
  const loadModel = parseLoadModel(rawLoadModel);
  const workloadProfile = parseWorkloadProfile({
    value: rawWorkloadProfile,
    loadModel,
  });
  const mode = parseMode(rawMode);
  const corpusShape = parseCorpusShape(rawCorpusShape);
  const count = parsePositiveInteger(rawCount, "--count", DEFAULT_COUNT);
  const targetRateTps = parsePositiveNumber(
    rawTargetRateTps,
    "--target-rate-tps",
    DEFAULT_OPEN_LOOP_TARGET_RATE_TPS,
  );
  const openLoopDurationMs = parsePositiveInteger(
    rawOpenLoopDurationMs,
    "--open-loop-duration-ms",
    DEFAULT_OPEN_LOOP_DURATION_MS,
  );
  const openLoopWarmupCount = parseNonNegativeInteger(
    rawOpenLoopWarmupCount,
    "--open-loop-warmup-count",
    0,
  );
  const openLoopCooldownCount = parseNonNegativeInteger(
    rawOpenLoopCooldownCount,
    "--open-loop-cooldown-count",
    0,
  );
  const openLoopMaxInFlight = parsePositiveInteger(
    rawOpenLoopMaxInFlight,
    "--open-loop-max-in-flight",
    DEFAULT_OPEN_LOOP_MAX_IN_FLIGHT,
  );
  const noOpCalibrationDurationMs = parsePositiveInteger(
    rawNoOpCalibrationDurationMs,
    "--no-op-calibration-duration-ms",
    DEFAULT_NO_OP_CALIBRATION_DURATION_MS,
  );
  const aggregateObserverIntervalMs = parsePositiveInteger(
    rawAggregateObserverIntervalMs,
    "--aggregate-observer-interval-ms",
    DEFAULT_AGGREGATE_OBSERVER_INTERVAL_MS,
  );
  const concurrency = parsePositiveInteger(
    rawConcurrency,
    "--concurrency",
    DEFAULT_CONCURRENCY,
  );
  const lovelace = parsePositiveBigInt(
    rawLovelace,
    "--lovelace",
    DEFAULT_LOVELACE,
  );
  const feeHeadroomLovelace = parseNonNegativeBigInt(
    rawFeeHeadroomLovelace,
    "--fee-headroom-lovelace",
    DEFAULT_FEE_HEADROOM_LOVELACE,
  );
  const pollIntervalMs =
    rawPollIntervalMs === undefined || rawPollIntervalMs.trim().length === 0
      ? undefined
      : parsePositiveInteger(
          rawPollIntervalMs,
          "--poll-interval-ms",
          DEFAULT_POLL_INTERVAL_MS,
        );
  const pollInitialIntervalMs = parsePositiveInteger(
    rawPollInitialIntervalMs,
    "--poll-initial-interval-ms",
    DEFAULT_POLL_INITIAL_INTERVAL_MS,
  );
  const pollMaxIntervalMs = parsePositiveInteger(
    rawPollMaxIntervalMs,
    "--poll-max-interval-ms",
    DEFAULT_POLL_MAX_INTERVAL_MS,
  );
  const submitRequestTimeoutMs = parsePositiveInteger(
    rawSubmitRequestTimeoutMs,
    "--submit-request-timeout-ms",
    DEFAULT_SUBMIT_REQUEST_TIMEOUT_MS,
  );
  const acceptanceTimeoutMs = parsePositiveInteger(
    rawAcceptanceTimeoutMs,
    "--acceptance-timeout-ms",
    DEFAULT_ACCEPTANCE_TIMEOUT_MS,
  );
  const commitObservationTimeoutMs = parsePositiveInteger(
    rawCommitObservationTimeoutMs,
    "--commit-observation-timeout-ms",
    DEFAULT_COMMIT_OBSERVATION_TIMEOUT_MS,
  );
  const finalityObserverMaxConcurrentRequests = parsePositiveInteger(
    rawFinalityObserverMaxConcurrentRequests,
    "--finality-observer-max-concurrent-requests",
    DEFAULT_FINALITY_OBSERVER_MAX_CONCURRENT_REQUESTS,
  );
  const maxSubmissionFailures = parseNonNegativeInteger(
    rawMaxSubmissionFailures,
    "--max-submission-failures",
    DEFAULT_MAX_SUBMISSION_FAILURES,
  );
  const normalizedEndpoint = parseNodeEndpoint(
    endpoint ?? defaultMidgardNodeEndpoint(env),
  );
  const resolvedRunId = runId?.trim() || `e2e-run-${timestampForPath()}`;
  const primaryWallet =
    loadModel === "closed-loop-smoke" && mode === "serial-chain"
      ? resolveStressWallet({
          walletSeedPhrase,
          walletSeedPhraseEnv,
          env,
          network,
        })
      : undefined;
  const stressWallets =
    loadModel === "closed-loop-smoke"
      ? resolveStressWallets({
          envNames: stressWalletSeedPhraseEnvs,
          env,
          network,
        })
      : [];
  validateDistinctStressWallets(stressWallets);

  if (count > MAX_DEFAULT_COUNT && !allowUnsafeBounds) {
    throw new Error(
      `--count ${count.toString()} exceeds the default cap ${MAX_DEFAULT_COUNT.toString()}; pass --unsafe-allow-large-stress to make that choice explicit.`,
    );
  }
  if (concurrency > count) {
    throw new Error("--concurrency must be less than or equal to --count.");
  }
  if (concurrency > MAX_DEFAULT_CONCURRENCY && !allowUnsafeBounds) {
    throw new Error(
      `--concurrency ${concurrency.toString()} exceeds the default cap ${MAX_DEFAULT_CONCURRENCY.toString()}; pass --unsafe-allow-large-stress to make that choice explicit.`,
    );
  }
  if (
    loadModel === "closed-loop-smoke" &&
    concurrency > 1 &&
    mode !== "parallel-fanout"
  ) {
    throw new Error(
      "--concurrency > 1 requires --mode parallel-fanout and independent stress wallet seed env vars.",
    );
  }
  if (
    loadModel === "closed-loop-smoke" &&
    mode === "parallel-fanout" &&
    stressWallets.length < concurrency
  ) {
    throw new Error(
      `--mode parallel-fanout requires at least ${concurrency.toString()} independent --stress-wallet-seed-phrase-env values with spendable L2 UTxOs; no submissions were made.`,
    );
  }
  if (loadModel === "open-loop-upper-bound") {
    if (corpusPath === undefined || corpusPath.trim().length === 0) {
      throw new Error(
        "--load-model open-loop-upper-bound requires --tx-corpus with prebuilt canonical CBOR rows.",
      );
    }
    if (
      workloadProfile === "production-end-user" &&
      noOpCalibrationEndpoint === undefined
    ) {
      throw new Error(
        "production-end-user open-loop runs must still provide --no-op-calibration-endpoint before upper-bound claims.",
      );
    }
  }
  if (requireNoOpCalibration && noOpCalibrationEndpoint === undefined) {
    throw new Error(
      "--require-no-op-calibration requires --no-op-calibration-endpoint.",
    );
  }

  return {
    runId: resolvedRunId,
    loadModel,
    workloadProfile,
    mode,
    corpusShape,
    count,
    concurrency,
    lovelace,
    feeHeadroomLovelace,
    nodeEndpoint: normalizedEndpoint,
    ...(corpusPath === undefined || corpusPath.trim().length === 0
      ? {}
      : { corpusPath: corpusPath.trim() }),
    corpusSliceId: corpusSliceId?.trim() || "default",
    targetRateTps,
    openLoopDurationMs,
    openLoopWarmupCount,
    openLoopCooldownCount,
    openLoopMaxInFlight,
    ...(noOpCalibrationEndpoint === undefined ||
    noOpCalibrationEndpoint.trim().length === 0
      ? {}
      : {
          noOpCalibrationEndpoint: parseNodeEndpoint(noOpCalibrationEndpoint),
        }),
    requireNoOpCalibration,
    noOpCalibrationDurationMs,
    aggregateObserverIntervalMs,
    ...(l2Address === undefined || l2Address.trim().length === 0
      ? {}
      : { destinationAddress: parseAddressArgument(l2Address) }),
    pollIntervalMs,
    pollInitialIntervalMs,
    pollMaxIntervalMs,
    submitRequestTimeoutMs,
    acceptanceTimeoutMs,
    commitObservationTimeoutMs,
    finalityObserverMaxConcurrentRequests,
    maxSubmissionFailures,
    outDir: outDir?.trim() || join("logs", resolvedRunId, "stress"),
    network,
    allowUnsafeBounds,
    primaryWallet,
    stressWallets,
  };
};
