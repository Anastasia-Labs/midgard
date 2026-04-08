/**
 * Startup-only invariant checks and bootstrap seeding for the node process.
 * This module isolates safety checks that must run before serving traffic from
 * the steady-state wiring in the main listen entrypoint.
 */
import { Globals, Lucid, MidgardContracts, NodeConfig } from "@/services/index.js";
import { formatStateQueueTopology } from "@/services/state-queue-topology.js";
import { shouldRunGenesisOnStartup } from "@/commands/startup-policy.js";
import * as Initialization from "@/transactions/initialization.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";

/**
 * Resolves the end-time boundary represented by the current state-queue tip.
 */
const resolveStateQueueTipEndTimeMs = (
  datum: SDK.StateQueueDatum,
): Effect.Effect<number, SDK.DataCoercionError, never> =>
  Effect.gen(function* () {
    if (datum.key === "Empty") {
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(datum);
      return Number(data.endTime);
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(datum);
    return Number(header.endTime);
  });

/**
 * Verifies protocol deployment state at startup and optionally auto-initializes
 * an empty deployment.
 */
export const ensureProtocolInitializedOnStartup = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const shouldBootstrap = shouldRunGenesisOnStartup({
    network: nodeConfig.NETWORK,
    runGenesisOnStartup: nodeConfig.RUN_GENESIS_ON_STARTUP,
  });
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const deploymentStatus = yield* Initialization.fetchProtocolDeploymentStatus(
    lucid.api,
    contracts,
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
    yield* Effect.logInfo(
      `Startup initialization check: protocol deployment already present (state_queue=${details}).`,
    );
    return;
  }

  if (!deploymentStatus.empty) {
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message:
          "Startup initialization check found a partial deployment; refusing to auto-initialize over externally provisioned state",
        cause: `state_queue=${details}; missing=[${deploymentStatus.missingComponents.join(",")}]; hub_oracle_present=${deploymentStatus.hubOracleWitness !== null}; scheduler_initialized=${deploymentStatus.schedulerInitialized}; registered_initialized=${deploymentStatus.registeredOperatorsInitialized}; active_initialized=${deploymentStatus.activeOperatorsInitialized}; retired_initialized=${deploymentStatus.retiredOperatorsInitialized}`,
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
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Startup protocol initialization failed: ${JSON.stringify(e)}`,
    ),
  ),
  Effect.orDie,
);

/**
 * Seeds the in-memory local block-boundary cache from the current state-queue
 * tip during startup.
 */
export const seedLatestLocalBlockBoundaryOnStartup = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const globals = yield* Globals;

  const latestBlock = yield* SDK.fetchLatestCommittedBlockProgram(lucid.api, {
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: contracts.stateQueue.policyId,
  });
  const latestEndTimeMs = yield* resolveStateQueueTipEndTimeMs(
    latestBlock.datum,
  );
  yield* Ref.set(globals.LATEST_LOCAL_BLOCK_END_TIME_MS, latestEndTimeMs);
  yield* Effect.logInfo(
    `Seeded latest local block boundary from state-queue tip: ${new Date(latestEndTimeMs).toISOString()}`,
  );
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Failed to seed latest local block boundary on startup: ${JSON.stringify(e)}`,
    ),
  ),
  Effect.orDie,
);
