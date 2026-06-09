/**
 * Startup-only invariant checks and bootstrap seeding for the node process.
 * This module isolates safety checks that must run before serving traffic from
 * the steady-state wiring in the main listen entrypoint.
 */
import { PendingBlockFinalizationsDB } from "@/database/index.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { formatStateQueueTopology } from "@/services/state-queue-topology.js";
import { shouldRunGenesisOnStartup } from "@/commands/startup-policy.js";
import * as ContractDeploymentInfo from "@/commands/contract-deployment-info.js";
import * as Initialization from "@/transactions/initialization.js";
import {
  ensureNodeRuntimeReferenceScriptsProgram,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "@/transactions/reference-scripts.js";
import { serializeStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option, Ref } from "effect";

const writeStartupContractDeploymentInfo = Effect.gen(function* () {
  const outputPath =
    ContractDeploymentInfo.defaultContractDeploymentInfoOutputPath();
  const manifestPath =
    yield* ContractDeploymentInfo.writeLiveContractDeploymentInfoProgram(
      outputPath,
    );
  yield* Effect.logInfo(
    `Startup contract deployment info written: ${manifestPath}`,
  );
}).pipe(
  Effect.catchAll((error) =>
    Effect.logError(
      `Failed to write startup contract deployment info: ${JSON.stringify(error)}`,
    ),
  ),
);

const ensureNodeRuntimeReferenceScriptsOnStartup = (
  shouldBootstrap: boolean,
) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    if (shouldBootstrap) {
      yield* lucid.switchToOperatorsMainWallet;
      const publications = yield* ensureNodeRuntimeReferenceScriptsProgram(
        lucid.referenceScriptsApi,
        contracts,
        lucid.api,
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
    yield* ensureNodeRuntimeReferenceScriptsOnStartup(shouldBootstrap);
    yield* writeStartupContractDeploymentInfo;
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
  yield* ensureNodeRuntimeReferenceScriptsOnStartup(false);
  yield* writeStartupContractDeploymentInfo;
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Startup protocol initialization failed: ${JSON.stringify(e)}`,
    ),
  ),
  Effect.orDie,
);

/**
 * Seeds AVAILABLE_CONFIRMED_BLOCK from the current state-queue tip so the
 * commitment fiber can start producing the next block.
 */
export const seedAvailableConfirmedBlockOnStartup = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const globals = yield* Globals;

  const latestBlock = yield* SDK.fetchLatestCommittedBlockProgram(lucid.api, {
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: contracts.stateQueue.policyId,
  });
  const serialized = yield* serializeStateQueueUTxO(latestBlock);
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, serialized);
  yield* Effect.logInfo(
    "Seeded AVAILABLE_CONFIRMED_BLOCK from current state-queue tip on startup.",
  );
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Failed to seed AVAILABLE_CONFIRMED_BLOCK on startup: ${JSON.stringify(e)}`,
    ),
  ),
  Effect.orDie,
);

/**
 * Replays any persisted pending-finalization journal into the in-memory
 * globals so the confirmation fiber can resume after restarts.
 */
export const hydratePendingBlockFinalizationOnStartup = Effect.gen(
  function* () {
    const globals = yield* Globals;
    const pending = yield* PendingBlockFinalizationsDB.retrieveActive();
    if (Option.isNone(pending)) {
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
      return;
    }
    const record = pending.value;
    const submittedTxHash =
      record[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH];
    yield* Ref.set(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
      submittedTxHash === null ? "" : submittedTxHash.toString("hex"),
    );
    yield* Ref.set(
      globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
      record[PendingBlockFinalizationsDB.Columns.UPDATED_AT].getTime(),
    );
    yield* Effect.logInfo(
      `Hydrated pending block-finalization journal on startup for header ${record[
        PendingBlockFinalizationsDB.Columns.HEADER_HASH
      ].toString("hex")}.`,
    );
  },
).pipe(
  Effect.tapError((error) =>
    Effect.logError(
      `Failed to hydrate pending block-finalization journal on startup: ${JSON.stringify(error)}`,
    ),
  ),
  Effect.orDie,
);
