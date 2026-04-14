/**
 * Startup-only invariant checks and bootstrap seeding for the node process.
 * This module isolates safety checks that must run before serving traffic from
 * the steady-state wiring in the main listen entrypoint.
 */
import { PendingBlockFinalizationsDB } from "@/database/index.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { formatStateQueueTopology } from "@/services/state-queue-topology.js";
import { shouldRunGenesisOnStartup } from "@/commands/startup-policy.js";
import * as ContractDeploymentInfo from "@/commands/contract-deployment-info.js";
import * as Initialization from "@/transactions/initialization.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option, Ref } from "effect";

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
  let seededBoundaryMs = latestEndTimeMs;
  if (latestBlock.datum.key !== "Empty") {
    const latestHeader = yield* SDK.getHeaderFromStateQueueDatum(
      latestBlock.datum,
    );
    const latestHeaderHash = Buffer.from(
      yield* SDK.hashBlockHeader(latestHeader),
      "hex",
    );
    const finalizedJournal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(latestHeaderHash);
    if (Option.isSome(finalizedJournal)) {
      seededBoundaryMs =
        finalizedJournal.value[
          PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
        ].getTime();
      yield* Effect.logInfo(
        `Seeded latest local block boundary from pending-finalization journal for header ${latestHeaderHash.toString("hex")}: ${new Date(seededBoundaryMs).toISOString()}`,
      );
    }
  }
  yield* Ref.set(globals.LATEST_LOCAL_BLOCK_END_TIME_MS, seededBoundaryMs);
  yield* Effect.logInfo(
    `Seeded latest local block boundary from startup state: ${new Date(seededBoundaryMs).toISOString()}`,
  );
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Failed to seed latest local block boundary on startup: ${JSON.stringify(e)}`,
    ),
  ),
  Effect.orDie,
);

export const hydratePendingBlockFinalizationOnStartup = Effect.gen(
  function* () {
    const globals = yield* Globals;
    const pending = yield* PendingBlockFinalizationsDB.retrieveActive();
    if (Option.isNone(pending)) {
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH, "");
      yield* Ref.set(globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS, 0);
      yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
      yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
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
    yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
    const status = record[PendingBlockFinalizationsDB.Columns.STATUS];
    yield* Ref.set(
      globals.LOCAL_FINALIZATION_PENDING,
      status === PendingBlockFinalizationsDB.Status.PendingSubmission ||
        status ===
          PendingBlockFinalizationsDB.Status
            .SubmittedLocalFinalizationPending ||
        status === PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
    );
    yield* Effect.logInfo(
      `Hydrated pending block-finalization journal on startup for header ${record[
        PendingBlockFinalizationsDB.Columns.HEADER_HASH
      ].toString("hex")} (status=${status}, submitted_tx=${
        submittedTxHash === null ? "unknown" : submittedTxHash.toString("hex")
      }).`,
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
