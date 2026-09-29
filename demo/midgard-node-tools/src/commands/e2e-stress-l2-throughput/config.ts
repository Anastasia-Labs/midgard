import { join } from "node:path";

import {
  DEFAULT_WALLET_SEED_ENV,
  defaultMidgardNodeEndpoint,
  parseAddressArgument,
  parseNodeEndpoint,
} from "midgard-node/commands/command-utils";

import {
  type OpenLoopCorpusShape,
  type OpenLoopWorkloadProfile,
} from "../stress-open-loop.js";
import { parseE2EL2StressConfigArtifact } from "./config-artifact.js";
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
  E2E_L2_STRESS_CONFIG_SCHEMA_VERSION,
  MAX_DEFAULT_CONCURRENCY,
  MAX_DEFAULT_COUNT,
} from "./constants.js";
import { measurementPolicyForConfig } from "./policy.js";
import { timestampForPath } from "./runtime.js";
import {
  type E2EL2StressConfig,
  type E2EL2StressLoadModel,
  type E2EL2StressMode,
  type ParseE2EL2StressOptions,
} from "./types.js";
import {
  requirePrimaryWallet,
  resolveStressWallet,
  resolveStressWallets,
  validateDistinctStressWallets,
} from "./wallets.js";

const parsePositiveInteger = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a safe positive integer.`);
  }
  return parsed;
};

const parseNonNegativeInteger = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a safe non-negative integer.`);
  }
  return parsed;
};

const parsePositiveNumber = (
  value: string | undefined,
  label: string,
  defaultValue: number,
): number => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  const parsed = Number(raw);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    throw new Error(`${label} must be a positive number.`);
  }
  return parsed;
};

const parsePositiveBigInt = (
  value: string | undefined,
  label: string,
  defaultValue: bigint,
): bigint => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a positive integer.`);
  }
  const parsed = BigInt(raw);
  if (parsed <= 0n) {
    throw new Error(`${label} must be greater than zero.`);
  }
  return parsed;
};

const parseNonNegativeBigInt = (
  value: string | undefined,
  label: string,
  defaultValue: bigint,
): bigint => {
  const raw = value?.trim();
  if (raw === undefined || raw.length === 0) {
    return defaultValue;
  }
  if (!/^\d+$/.test(raw)) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return BigInt(raw);
};

const parseMode = (value: string | undefined): E2EL2StressMode => {
  const normalized = value?.trim() || "serial-chain";
  if (normalized === "serial-chain" || normalized === "parallel-fanout") {
    return normalized;
  }
  throw new Error(
    `--mode must be "serial-chain" or "parallel-fanout", got "${value}".`,
  );
};

const parseLoadModel = (value: string | undefined): E2EL2StressLoadModel => {
  const normalized = value?.trim() || "closed-loop-smoke";
  if (
    normalized === "closed-loop-smoke" ||
    normalized === "open-loop-upper-bound"
  ) {
    return normalized;
  }
  throw new Error(
    `--load-model must be "closed-loop-smoke" or "open-loop-upper-bound", got "${value}".`,
  );
};

const parseWorkloadProfile = ({
  value,
  loadModel,
}: {
  readonly value: string | undefined;
  readonly loadModel: E2EL2StressLoadModel;
}): OpenLoopWorkloadProfile => {
  const normalized =
    value?.trim() ||
    (loadModel === "open-loop-upper-bound"
      ? "synthetic-admission"
      : "production-end-user");
  if (
    normalized === "synthetic-admission" ||
    normalized === "production-end-user"
  ) {
    return normalized;
  }
  throw new Error(
    `--workload-profile must be "synthetic-admission" or "production-end-user", got "${value}".`,
  );
};

const parseCorpusShape = (value: string | undefined): OpenLoopCorpusShape => {
  const normalized = value?.trim() || "fanout";
  if (
    normalized === "fanout" ||
    normalized === "chain" ||
    normalized === "mixed"
  ) {
    return normalized;
  }
  throw new Error(
    `--corpus-shape must be "fanout", "chain", or "mixed", got "${value}".`,
  );
};

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

const rawArtifactConfig = (config: E2EL2StressConfig) => ({
  schemaVersion: E2E_L2_STRESS_CONFIG_SCHEMA_VERSION,
  runId: config.runId,
  loadModel: config.loadModel,
  workloadProfile: config.workloadProfile,
  corpusShape: config.corpusShape,
  mode: config.mode,
  measurementPolicy: measurementPolicyForConfig(config),
  count: config.count,
  concurrency: config.concurrency,
  lovelace: config.lovelace.toString(10),
  feeHeadroomLovelace: config.feeHeadroomLovelace.toString(10),
  nodeEndpoint: config.nodeEndpoint,
  corpusPath: config.corpusPath ?? null,
  corpusSliceId: config.corpusSliceId,
  targetRateTps: config.targetRateTps,
  openLoopDurationMs: config.openLoopDurationMs,
  openLoopWarmupCount: config.openLoopWarmupCount,
  openLoopCooldownCount: config.openLoopCooldownCount,
  openLoopMaxInFlight: config.openLoopMaxInFlight,
  noOpCalibrationEndpoint: config.noOpCalibrationEndpoint ?? null,
  requireNoOpCalibration: config.requireNoOpCalibration,
  noOpCalibrationDurationMs: config.noOpCalibrationDurationMs,
  aggregateObserverIntervalMs: config.aggregateObserverIntervalMs,
  destination:
    config.destinationAddress === undefined
      ? { mode: "self" }
      : { mode: "explicit", address: config.destinationAddress },
  pollIntervalMs: config.pollIntervalMs,
  pollInitialIntervalMs: config.pollInitialIntervalMs,
  pollMaxIntervalMs: config.pollMaxIntervalMs,
  submitRequestTimeoutMs: config.submitRequestTimeoutMs,
  acceptanceTimeoutMs: config.acceptanceTimeoutMs,
  commitObservationTimeoutMs: config.commitObservationTimeoutMs,
  finalityObserverMaxConcurrentRequests:
    config.finalityObserverMaxConcurrentRequests,
  maxSubmissionFailures: config.maxSubmissionFailures,
  network: config.network,
  allowUnsafeBounds: config.allowUnsafeBounds,
  wallets:
    config.loadModel === "open-loop-upper-bound"
      ? []
      : config.mode === "parallel-fanout"
        ? config.stressWallets.map((wallet) => ({
            seedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
            address: wallet.address,
          }))
        : [
            {
              seedSource:
                requirePrimaryWallet(config).resolvedWalletSeedPhrase
                  .resolvedFrom,
              address: requirePrimaryWallet(config).address,
            },
          ],
});

export type E2EL2StressConfigArtifact = ReturnType<typeof rawArtifactConfig>;

export const artifactConfig = (
  config: E2EL2StressConfig,
): E2EL2StressConfigArtifact =>
  parseE2EL2StressConfigArtifact(rawArtifactConfig(config));
