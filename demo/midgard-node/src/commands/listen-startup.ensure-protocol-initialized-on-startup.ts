import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect } from "effect";

import { isRetryableProviderError } from "../provider-retry.js";
import { Lucid, MidgardContracts, NodeConfig } from "../services/index.js";
import {
  AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED,
  DEPLOYMENT_MANIFEST_MISMATCH,
  DEPLOYMENT_MANIFEST_MISSING,
  DEPLOYMENT_MANIFEST_UNVERIFIABLE,
  PROTOCOL_DEPLOYMENT_PARTIAL,
  PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE,
  PROTOCOL_INITIALIZATION_FAILED,
  retryStartupStep,
  REWARD_ACCOUNT_STATUS_UNAVAILABLE,
  RUNTIME_REFERENCE_SCRIPTS_FAILED,
  startupStepFailed,
  StartupStepFailedError,
} from "../services/startup-waiting.js";
import {
  assertAvailabilityChallengeRewardAccountsRegisteredProgram,
  AvailabilityRewardAccountUnregisteredError,
} from "../transactions/availability-challenge-registration.js";
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
 * failure (`isRetryableProviderError`) every `retryDelayMs` for at most
 * `maxAttempts` reads under `protocol_deployment_status_unavailable`. Any
 * other failure, or the last read's, fails the step
 * (`StartupStepFailedError`).
 */
export const fetchProtocolDeploymentStatusWithStartupRetry = (
  fetchStatus: () => Effect.Effect<
    Initialization.ProtocolDeploymentStatus,
    SDK.LucidError
  >,
  options: { readonly maxAttempts: number; readonly retryDelayMs: number },
): Effect.Effect<
  Initialization.ProtocolDeploymentStatus,
  StartupStepFailedError
> => {
  const retryDelayMs = Math.max(0, Math.floor(options.retryDelayMs));
  return retryStartupStep(Effect.suspend(fetchStatus), {
    key: "protocol_deployment_status",
    reason: PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE,
    // The read's own typed retryability decides (a Kupo 503 retries, a
    // Kupo 400 does not), whatever text the SDK wrapper around it carries.
    retryable: isRetryableProviderError,
    budget: { maxAttempts: options.maxAttempts },
    initialMs: retryDelayMs,
    maxMs: retryDelayMs,
  });
};

/**
 * The availability-challenge reward accounts' registration, its reads
 * waiting out a retryable provider failure under the deployment-status
 * budget. An unregistered account is a verdict, not a read failure, and
 * fails the step at once.
 */
const assertRewardAccountsRegisteredWithStartupRetry = (
  assertRegistered: Effect.Effect<
    void,
    SDK.StateQueueError | AvailabilityRewardAccountUnregisteredError
  >,
  options: { readonly maxAttempts: number; readonly retryDelayMs: number },
): Effect.Effect<void, StartupStepFailedError> => {
  const retryDelayMs = Math.max(0, Math.floor(options.retryDelayMs));
  return retryStartupStep(assertRegistered, {
    key: "availability_reward_accounts",
    reason: (error) =>
      error instanceof AvailabilityRewardAccountUnregisteredError
        ? AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED
        : REWARD_ACCOUNT_STATUS_UNAVAILABLE,
    retryable: (error) =>
      !(error instanceof AvailabilityRewardAccountUnregisteredError) &&
      isRetryableProviderError(error),
    budget: { maxAttempts: options.maxAttempts },
    initialMs: retryDelayMs,
    maxMs: retryDelayMs,
  });
};

/** Fails the protocol startup check under `reason`, never retried. */
const failProtocolStartup = (reason: string, cause: unknown) =>
  Effect.fail(
    startupStepFailed({ step: "protocol_initialization", reason, cause }),
  );

/** Runs `effect`; its failure fails the protocol startup check under
 * `reason`, never retried. */
const asProtocolStartupStep =
  (reason: string) =>
  <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ): Effect.Effect<A, StartupStepFailedError, R> =>
    Effect.catchAll(effect, (cause) => failProtocolStartup(reason, cause));

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
 * Verifies the protocol deployment at startup and, where configured,
 * initializes an empty one. Only the provider reads (the deployment status,
 * the reward accounts) wait out a transient failure, each under its budget.
 * A deployment or manifest verdict, an unregistered reward account, a
 * failed reference-script preflight and a failed initialization each fail
 * the startup with its named reason (`StartupStepFailedError`): none of
 * them clears by checking again, and the process's restart runs the whole
 * check again from the deployment status, so an initialization a failed run
 * submitted is seen as present, not submitted twice.
 */
export const ensureProtocolInitializedOnStartup = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const providerRetry = {
    maxAttempts: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS,
    retryDelayMs: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS,
  };
  const manifestReport =
    yield* ContractDeploymentInfo.verifyConfiguredDeploymentManifestIfPresentProgram.pipe(
      asProtocolStartupStep(DEPLOYMENT_MANIFEST_UNVERIFIABLE),
    );
  if (manifestReport !== null && !manifestReport.ok) {
    return yield* failProtocolStartup(
      DEPLOYMENT_MANIFEST_MISMATCH,
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
    providerRetry,
  );
  // The queue's health is the landed queue's (P1): once the follower runs,
  // an unhealthy queue fails `/readyz` with its reason and stops proposals.
  const details = `initialized=${String(deploymentStatus.stateQueueInitialized)}`;

  if (deploymentStatus.complete) {
    if (manifestReport === null) {
      return yield* failProtocolStartup(
        DEPLOYMENT_MANIFEST_MISSING,
        new SDK.StateQueueError({
          message:
            "Startup deployment manifest verification failed; refusing to attach without a finalized contract deployment manifest",
          cause:
            "manifest_id=unknown,path=unknown,recommendation=fresh_redeploy_required,mismatches=[contract deployment manifest file not found]",
        }),
      );
    }
    yield* assertRewardAccountsRegisteredWithStartupRetry(
      assertAvailabilityChallengeRewardAccountsRegisteredProgram(
        lucid.api,
        contracts,
      ),
      providerRetry,
    );
    yield* Effect.logInfo(
      `Startup initialization check: protocol deployment already present (state_queue=${details}).`,
    );
    if (shouldBootstrap) {
      yield* ensureNodeRuntimeReferenceScriptsOnStartup(true).pipe(
        asProtocolStartupStep(RUNTIME_REFERENCE_SCRIPTS_FAILED),
      );
    } else {
      yield* verifyNodeRuntimeReferenceScriptsInBackground;
    }
    yield* Effect.logInfo(
      `Startup contract deployment manifest verified: manifest_id=${manifestReport.manifestId ?? "unknown"},path=${manifestReport.path ?? "unknown"}`,
    );
    return;
  }

  if (!deploymentStatus.empty) {
    return yield* failProtocolStartup(
      PROTOCOL_DEPLOYMENT_PARTIAL,
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
  yield* Effect.gen(function* () {
    const initTxHash = yield* Initialization.program;
    yield* Effect.logInfo(
      `Startup protocol initialization submitted successfully: ${initTxHash}`,
    );
    yield* ensureNodeRuntimeReferenceScriptsOnStartup(false);
    yield* writeStartupContractDeploymentInfoAfterFreshInit(initTxHash);
  }).pipe(asProtocolStartupStep(PROTOCOL_INITIALIZATION_FAILED));
}).pipe(
  Effect.tapError((error) =>
    Effect.logError(`Startup protocol check failed: ${error.message}`),
  ),
);
