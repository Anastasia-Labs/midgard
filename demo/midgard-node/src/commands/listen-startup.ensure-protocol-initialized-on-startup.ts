import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect } from "effect";

import { isRetryableProviderError } from "../provider-retry.js";
import { Lucid, MidgardContracts, NodeConfig } from "../services/index.js";
import {
  PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE,
  PROTOCOL_INITIALIZATION_FAILED,
  retryStartupStep,
} from "../services/startup-waiting.js";
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

/**
 * Fetches the protocol deployment status, waiting out a retryable provider
 * failure (`isRetryableProviderError`) every `retryDelayMs` with no deadline
 * under `protocol_deployment_status_unavailable`; any other failure fails at
 * once.
 */
export const fetchProtocolDeploymentStatusWithStartupRetry = (
  fetchStatus: () => Effect.Effect<
    Initialization.ProtocolDeploymentStatus,
    SDK.LucidError
  >,
  options: { readonly retryDelayMs: number },
): Effect.Effect<Initialization.ProtocolDeploymentStatus, SDK.LucidError> => {
  const retryDelayMs = Math.max(0, Math.floor(options.retryDelayMs));
  return retryStartupStep(Effect.suspend(fetchStatus), {
    key: "protocol_deployment_status",
    reason: PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE,
    // The read's own typed retryability decides (a Kupo 503 retries, a
    // Kupo 400 does not), whatever text the SDK wrapper around it carries.
    retryable: isRetryableProviderError,
    initialMs: retryDelayMs,
    maxMs: retryDelayMs,
  });
};

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
const ensureProtocolInitializedOnce = Effect.gen(function* () {
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
    { retryDelayMs: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS },
  );
  // The queue's health is the landed queue's (P1): once the follower runs,
  // an unhealthy queue fails `/readyz` with its reason and stops proposals.
  const details = `initialized=${String(deploymentStatus.stateQueueInitialized)}`;

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
});

/**
 * `ensureProtocolInitializedOnce` until it passes: a failed run (a provider
 * read, a deployment or manifest verdict, an unregistered reward account,
 * a failed initialization) is logged and run again from the start on a
 * capped backoff, the startup waiting under `protocol_initialization_failed`
 * with no deadline. A run again re-reads the deployment status, so an
 * initialization a failed run submitted is seen as present, not submitted
 * twice. Never fails.
 */
export const ensureProtocolInitializedOnStartup = retryStartupStep(
  ensureProtocolInitializedOnce,
  { key: "protocol_initialization", reason: PROTOCOL_INITIALIZATION_FAILED },
).pipe(
  // Every failure is retried above; nothing reaches here.
  Effect.catchAll(() => Effect.never),
);
