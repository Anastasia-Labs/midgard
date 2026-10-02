import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect } from "effect";

import { isRetryableProviderError } from "../provider-retry.js";
import { Lucid, MidgardContracts, NodeConfig } from "../services/index.js";
import { formatStateQueueTopology } from "../services/state-queue-topology.js";
import { assertAvailabilityChallengeRewardAccountsRegisteredProgram } from "../transactions/availability-challenge-registration.js";
import * as Initialization from "../transactions/initialization.js";
import {
  ensureNodeRuntimeReferenceScriptsProgram,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "../transactions/reference-scripts.js";
import * as ContractDeploymentInfo from "./contract-deployment-info.js";
import { shouldRunGenesisOnStartup } from "./startup-policy.js";

const STARTUP_BACKGROUND_PROVIDER_TIMEOUT = Duration.seconds(90);

const verifyNodeRuntimeReferenceScriptsInBackground = Effect.gen(function* () {
  yield* ensureNodeRuntimeReferenceScriptsOnStartup(false);
}).pipe(
  Effect.timeoutFail({
    duration: STARTUP_BACKGROUND_PROVIDER_TIMEOUT,
    onTimeout: () =>
      new Error(
        "startup node-runtime reference-script background verification exceeded its bounded provider window",
      ),
  }),
  Effect.catchAll((error) =>
    Effect.logWarning(
      `Startup node-runtime reference-script background verification did not complete after bounded provider retries; startup continues with the configured deployment manifest. cause=${formatUnknownError(error)}`,
    ),
  ),
  Effect.forkDaemon,
  Effect.asVoid,
);

const writeStartupContractDeploymentInfoAfterFreshInit = (initTxHash: string) =>
  Effect.gen(function* () {
    const outputPath =
      ContractDeploymentInfo.defaultContractDeploymentInfoOutputPath();
    const manifestPath =
      yield* ContractDeploymentInfo.writeLiveContractDeploymentInfoProgram(
        outputPath,
        {
          hubOracleOneShotStatus: "consumed_by_init",
          steps: {
            initProtocol: {
              status: "complete",
              txHash: initTxHash,
            },
          },
        },
      );
    yield* Effect.logInfo(
      `Startup contract deployment info written after fresh initialization: ${manifestPath}`,
    );
  }).pipe(
    Effect.timeoutFail({
      duration: STARTUP_BACKGROUND_PROVIDER_TIMEOUT,
      onTimeout: () =>
        new Error(
          "startup contract deployment info write exceeded its bounded provider window",
        ),
    }),
  );

export const fetchProtocolDeploymentStatusWithStartupRetry = (
  fetchStatus: () => Effect.Effect<
    Initialization.ProtocolDeploymentStatus,
    SDK.LucidError
  >,
  options: {
    readonly maxAttempts: number;
    readonly retryDelayMs: number;
  },
): Effect.Effect<Initialization.ProtocolDeploymentStatus, SDK.LucidError> =>
  Effect.gen(function* () {
    const maxAttempts = Math.max(1, Math.floor(options.maxAttempts));
    const retryDelayMs = Math.max(0, Math.floor(options.retryDelayMs));
    let lastError: SDK.LucidError | undefined;

    for (let attempt = 1; attempt <= maxAttempts; attempt += 1) {
      const statusAttempt = yield* Effect.either(fetchStatus());
      if (statusAttempt._tag === "Right") {
        if (attempt > 1) {
          yield* Effect.logInfo(
            `Startup protocol deployment status query became available after ${attempt.toString()} attempt(s).`,
          );
        }
        return statusAttempt.right;
      }

      lastError = statusAttempt.left;
      // The read's own typed retryability decides (a Kupo 503 retries, a
      // Kupo 400 does not), whatever text the SDK wrapper around it carries.
      if (!isRetryableProviderError(lastError)) {
        return yield* Effect.fail(lastError);
      }
      if (attempt < maxAttempts) {
        yield* Effect.logWarning(
          `Startup protocol deployment status query failed (attempt ${attempt.toString()}/${maxAttempts.toString()}); retrying in ${retryDelayMs.toString()}ms. cause=${formatUnknownError(lastError)}`,
        );
        if (retryDelayMs > 0) {
          yield* Effect.sleep(Duration.millis(retryDelayMs));
        }
      }
    }

    return yield* Effect.fail(
      new SDK.LucidError({
        message:
          "Startup protocol deployment status query failed after bounded retries",
        cause: `attempts=${maxAttempts.toString()},last_cause=${formatUnknownError(lastError)}`,
      }),
    );
  });

const ensureNodeRuntimeReferenceScriptsOnStartup = (shouldBootstrap: boolean) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    if (shouldBootstrap) {
      yield* lucid.switchToOperatorsMainWallet;
      const publications = yield* ensureNodeRuntimeReferenceScriptsProgram(
        lucid.referenceScriptsApi,
        contracts,
        contracts.referenceScriptAuth,
        lucid.api,
        lucid.referenceScriptsAddress,
      );
      yield* Effect.logInfo(
        `Startup node-runtime reference-script preflight completed: count=${publications.length.toString()},address=${lucid.referenceScriptsAddress}`,
      );
      return publications;
    }
    const publications = yield* verifyNodeRuntimeReferenceScriptsProgram(
      lucid.api,
      lucid.referenceScriptsAddress,
      contracts,
      contracts.referenceScriptAuth,
    );
    yield* Effect.logInfo(
      `Startup node-runtime reference-script verification completed: count=${publications.length.toString()},address=${lucid.referenceScriptsAddress}`,
    );
    return publications;
  });

/**
 * Verifies protocol deployment state at startup and optionally auto-initializes
 * an empty deployment.
 */
export const ensureProtocolInitializedOnStartup = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const manifestReport =
    yield* ContractDeploymentInfo.verifyConfiguredDeploymentManifestIfPresentProgram;
  if (manifestReport !== null && !manifestReport.ok) {
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Startup deployment manifest verification failed; refusing to attach to a mismatched deployment",
        cause: `manifest_id=${manifestReport.manifestId ?? "unknown"},path=${manifestReport.path ?? "unknown"},recommendation=${manifestReport.recommendation},mismatches=[${manifestReport.mismatches.join(";")}]`,
      }),
    );
  }
  const shouldBootstrap = shouldRunGenesisOnStartup({
    network: nodeConfig.NETWORK,
    runGenesisOnStartup: nodeConfig.RUN_GENESIS_ON_STARTUP,
  });
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const deploymentStatus = yield* fetchProtocolDeploymentStatusWithStartupRetry(
    () => Initialization.fetchProtocolDeploymentStatus(lucid.api, contracts),
    {
      maxAttempts: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS,
      retryDelayMs: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS,
    },
  );
  const details = formatStateQueueTopology(deploymentStatus.stateQueueTopology);

  if (!deploymentStatus.stateQueueTopology.healthy) {
    if (deploymentStatus.stateQueueTopology.initialized) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Startup initialization check failed: configured state_queue policy has invalid topology",
          cause: `${details}; reason=${deploymentStatus.stateQueueTopology.reason ?? "unknown"}`,
        }),
      );
    }
  }

  if (deploymentStatus.complete) {
    if (manifestReport === null) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Startup deployment manifest verification failed; refusing to attach without a finalized contract deployment manifest",
          cause:
            "manifest_id=unknown,path=unknown,recommendation=fresh_redeploy_required,mismatches=[contract deployment manifest file not found]",
        }),
      );
    }
    yield* assertAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid.api,
      contracts,
    );
    yield* Effect.logInfo(
      `Startup initialization check: protocol deployment already present (state_queue=${details}).`,
    );
    if (shouldBootstrap) {
      yield* ensureNodeRuntimeReferenceScriptsOnStartup(true);
    } else {
      yield* verifyNodeRuntimeReferenceScriptsInBackground;
    }
    yield* Effect.logInfo(
      `Startup contract deployment manifest verified: manifest_id=${manifestReport.manifestId ?? "unknown"},path=${manifestReport.path ?? "unknown"}`,
    );
    return;
  }

  if (!deploymentStatus.empty) {
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Startup initialization check found a partial deployment; refusing to auto-initialize over externally provisioned state",
        cause: `state_queue=${details}; missing=[${deploymentStatus.missingComponents.join(",")}]; hub_oracle_present=${deploymentStatus.hubOracleWitness !== null}; scheduler_initialized=${deploymentStatus.schedulerInitialized}; registered_initialized=${deploymentStatus.registeredOperatorsInitialized}; active_initialized=${deploymentStatus.activeOperatorsInitialized}; retired_initialized=${deploymentStatus.retiredOperatorsInitialized}; phas_reward_address=${deploymentStatus.phasMembershipRewardAddress}`,
      }),
    );
  }

  if (!shouldBootstrap) {
    yield* Effect.logInfo(
      "Skipping protocol initialization on startup (disabled or mainnet).",
    );
    return;
  }

  yield* Effect.logInfo(
    "No existing protocol deployment found for configured contracts. Running protocol initialization...",
  );
  const initTxHash = yield* Initialization.program;
  yield* Effect.logInfo(
    `Startup protocol initialization submitted successfully: ${initTxHash}`,
  );
  yield* ensureNodeRuntimeReferenceScriptsOnStartup(false);
  yield* writeStartupContractDeploymentInfoAfterFreshInit(initTxHash);
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Startup protocol initialization failed: ${formatUnknownError(e)}`,
    ),
  ),
  Effect.orDie,
);
