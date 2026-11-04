import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import { isRetryableProviderError } from "../provider-retry.js";
import {
  assertLucidL1SlotConfigMatchesDeployment,
  ContractDeploymentIdentity,
  deploymentL1SlotConfig,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  AVAILABILITY_REWARD_ACCOUNT_UNREGISTERED,
  DEPLOYMENT_MANIFEST_MISMATCH,
  DEPLOYMENT_MANIFEST_MISSING,
  DEPLOYMENT_MANIFEST_UNVERIFIABLE,
  L1_SLOT_CONFIG_MISMATCH,
  retryStartupStep,
  REWARD_ACCOUNT_STATUS_UNAVAILABLE,
  startupStepFailed,
  StartupStepFailedError,
} from "../services/startup-waiting.js";
import {
  assertAvailabilityChallengeRewardAccountsRegisteredProgram,
  AvailabilityRewardAccountUnregisteredError,
} from "../transactions/availability-challenge-registration.js";
import * as ContractDeploymentInfo from "./contract-deployment-info.js";

/**
 * The availability-challenge reward accounts' registration, its reads
 * waiting out a retryable provider failure under the startup status-query
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

/**
 * Verifies the protocol deployment the node attaches to. The node never
 * deploys: the deployment commands do, and leave the finalized deployment
 * manifest this check requires. A slot mapping other than the deployment's
 * (`l1_slot_config_mismatch`), a missing, unreadable or mismatched manifest
 * and an unregistered reward account each fail the startup with its named
 * reason (`StartupStepFailedError`); only the reward-account reads wait out a
 * transient provider failure, under their budget.
 */
export const ensureProtocolInitializedOnStartup = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  // Phase-B and the validation trace build script contexts from the Lucid
  // slot mapping; the deployed txInfo validator builds them from its own.
  const identity = yield* ContractDeploymentIdentity;
  const lucid = yield* Lucid;
  yield* Effect.try(() =>
    assertLucidL1SlotConfigMatchesDeployment(
      deploymentL1SlotConfig(identity),
      lucid.api,
    ),
  ).pipe(asProtocolStartupStep(L1_SLOT_CONFIG_MISMATCH));
  const manifestReport =
    yield* ContractDeploymentInfo.verifyConfiguredDeploymentManifestIfPresentProgram.pipe(
      asProtocolStartupStep(DEPLOYMENT_MANIFEST_UNVERIFIABLE),
    );
  if (manifestReport === null) {
    return yield* failProtocolStartup(
      DEPLOYMENT_MANIFEST_MISSING,
      new SDK.StateQueueError({
        message:
          "Startup deployment manifest verification failed; refusing to attach without a finalized contract deployment manifest (the node never deploys: run the deployment commands first)",
        cause:
          "manifest_id=unknown,path=unknown,recommendation=fresh_redeploy_required,mismatches=[contract deployment manifest file not found]",
      }),
    );
  }
  if (!manifestReport.ok) {
    return yield* failProtocolStartup(
      DEPLOYMENT_MANIFEST_MISMATCH,
      new SDK.StateQueueError({
        message:
          "Startup deployment manifest verification failed; refusing to attach to a mismatched deployment",
        cause: `manifest_id=${manifestReport.manifestId ?? "unknown"},path=${manifestReport.path ?? "unknown"},recommendation=${manifestReport.recommendation},mismatches=[${manifestReport.mismatches.join(";")}]`,
      }),
    );
  }
  yield* assertRewardAccountsRegisteredWithStartupRetry(
    assertAvailabilityChallengeRewardAccountsRegisteredProgram(
      lucid.api,
      yield* MidgardContracts,
    ),
    {
      maxAttempts: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_MAX_ATTEMPTS,
      retryDelayMs: nodeConfig.STARTUP_PROTOCOL_STATUS_QUERY_RETRY_DELAY_MS,
    },
  );
  yield* Effect.logInfo(
    `Startup contract deployment manifest verified: manifest_id=${manifestReport.manifestId ?? "unknown"},path=${manifestReport.path ?? "unknown"}`,
  );
}).pipe(
  Effect.tapError((error) =>
    Effect.logError(`Startup protocol check failed: ${error.message}`),
  ),
);
