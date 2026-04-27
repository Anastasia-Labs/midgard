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
import {
  ensureNodeRuntimeReferenceScriptsProgram,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "@/transactions/reference-scripts.js";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option, Ref } from "effect";

/**
 * Resolves the end-time boundary represented by the current state-queue tip.
 */
const resolveStateQueueTipEndTimeMs = (
  datum: SDK.StateQueueNodeView,
): Effect.Effect<number, SDK.DataCoercionError, never> =>
  Effect.gen(function* () {
    if (datum.key === "Empty") {
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(datum);
      return Number(data.endTime);
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(datum);
    return Number(header.endTime);
  });

type CanonicalCommittedHeader = {
  readonly headerHash: Buffer;
  readonly endTimeMs: number;
  readonly journal: Option.Option<PendingBlockFinalizationsDB.Record>;
};

const localJournalHasPayloadMembers = (
  journal: PendingBlockFinalizationsDB.Record,
): boolean =>
  journal.depositEventIds.length > 0 || journal.mempoolTxIds.length > 0;

const fetchCanonicalCommittedHeaders = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const committedBlocks = yield* SDK.fetchSortedStateQueueUTxOsProgram(
    lucid.api,
    {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    },
  );
  const headers: CanonicalCommittedHeader[] = [];
  for (const block of committedBlocks) {
    if (block.datum.key === "Empty") {
      continue;
    }
    const header = yield* SDK.getHeaderFromStateQueueDatum(block.datum);
    const headerHash = Buffer.from(yield* SDK.hashBlockHeader(header), "hex");
    const journal =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
    headers.push({
      headerHash,
      endTimeMs: Number(header.endTime),
      journal,
    });
  }
  return headers;
});

const reviveEarliestCanonicalPayloadJournalOnStartup = (
  canonicalHeaders: readonly CanonicalCommittedHeader[],
) =>
  Effect.gen(function* () {
    const candidateIndex = canonicalHeaders.findIndex(
      ({ journal }) =>
        Option.isSome(journal) &&
        journal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
          PendingBlockFinalizationsDB.Status.Abandoned &&
        localJournalHasPayloadMembers(journal.value),
    );
    if (candidateIndex < 0) {
      return Option.none<CanonicalCommittedHeader>();
    }

    const candidate = canonicalHeaders[candidateIndex]!;
    const active = yield* PendingBlockFinalizationsDB.retrieveActive();
    if (Option.isSome(active)) {
      const activeHeaderHash =
        active.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH];
      if (activeHeaderHash.equals(candidate.headerHash)) {
        return Option.some(candidate);
      }

      const activeCanonicalIndex = canonicalHeaders.findIndex(
        ({ headerHash }) => headerHash.equals(activeHeaderHash),
      );
      if (
        activeCanonicalIndex > candidateIndex &&
        !localJournalHasPayloadMembers(active.value)
      ) {
        yield* PendingBlockFinalizationsDB.markAbandoned(activeHeaderHash);
        yield* Effect.logWarning(
          `Demoted active empty canonical pending-finalization journal ${activeHeaderHash.toString("hex")} so earlier payload-bearing canonical block ${candidate.headerHash.toString("hex")} can recover local finalization first.`,
        );
      } else {
        yield* Effect.logInfo(
          `Skipping abandoned canonical payload journal revival for ${candidate.headerHash.toString("hex")}; active pending-finalization journal ${activeHeaderHash.toString("hex")} must resolve first.`,
        );
        return Option.none<CanonicalCommittedHeader>();
      }
    }

    yield* PendingBlockFinalizationsDB.reviveAbandonedCanonical(
      candidate.headerHash,
      BigInt(Date.now()),
    );
    yield* Effect.logWarning(
      `Revived abandoned pending-finalization journal for canonical payload-bearing block ${candidate.headerHash.toString("hex")}; local finalization recovery will replay that block before later canonical descendants.`,
    );
    return Option.some(candidate);
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

const ensureNodeRuntimeReferenceScriptsOnStartup = (shouldBootstrap: boolean) =>
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
  const canonicalHeaders = yield* fetchCanonicalCommittedHeaders;
  const revivedPayloadJournal =
    yield* reviveEarliestCanonicalPayloadJournalOnStartup(canonicalHeaders);
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
      const journalBoundaryMs =
        finalizedJournal.value[
          PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
        ].getTime();
      seededBoundaryMs = Math.max(journalBoundaryMs, latestEndTimeMs);
      if (
        finalizedJournal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
          PendingBlockFinalizationsDB.Status.Abandoned &&
        localJournalHasPayloadMembers(finalizedJournal.value) &&
        Option.isNone(revivedPayloadJournal)
      ) {
        yield* PendingBlockFinalizationsDB.reviveAbandonedCanonical(
          latestHeaderHash,
          BigInt(Date.now()),
        );
        yield* Effect.logWarning(
          `Revived abandoned pending-finalization journal for canonical payload-bearing state-queue tip ${latestHeaderHash.toString("hex")}; local finalization recovery will replay the block payload.`,
        );
      }
      yield* Effect.logInfo(
        `Seeded latest local block boundary from pending-finalization journal for header ${latestHeaderHash.toString("hex")}: ${new Date(seededBoundaryMs).toISOString()}`,
      );
    }
  }
  if (Option.isSome(revivedPayloadJournal)) {
    seededBoundaryMs = Math.max(
      seededBoundaryMs,
      revivedPayloadJournal.value.endTimeMs,
      revivedPayloadJournal.value.journal.pipe(
        Option.match({
          onNone: () => 0,
          onSome: (journal) =>
            journal[
              PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
            ].getTime(),
        }),
      ),
    );
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
