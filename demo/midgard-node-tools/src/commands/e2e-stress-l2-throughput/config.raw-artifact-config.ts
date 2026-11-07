import { parseE2EL2StressConfigArtifact } from "./config-artifact.js";
import { E2E_L2_STRESS_CONFIG_SCHEMA_VERSION } from "./constants.js";
import { measurementPolicyForConfig } from "./policy.js";
import { type E2EL2StressConfig } from "./types.js";
import { requirePrimaryWallet } from "./wallets.js";

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
