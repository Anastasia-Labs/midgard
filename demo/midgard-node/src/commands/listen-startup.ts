/**
 * Startup-only invariant checks and bootstrap seeding for the node process.
 * This module isolates safety checks that must run before serving traffic from
 * the steady-state wiring in the main listen entrypoint.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Duration, Effect, Option, Ref } from "effect";

import {
  ConfirmedLedgerDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  synchronizeCommitMpfStoresFromLedgerEntries,
} from "../mpf/index.js";
import {
  fetchCanonicalCommittedHeaders,
  journalAbandonment,
  localJournalHasPayloadMembers,
  reviveEarliestCanonicalPayloadJournal,
} from "../services/canonical-journal-recovery.js";
import {
  DatabaseInitializationError,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  formatStateQueueTopology,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/state-queue-topology.js";
import { assertAvailabilityChallengeRewardAccountsRegisteredProgram } from "../transactions/availability-challenge-registration.js";
import * as Initialization from "../transactions/initialization.js";
import {
  ensureNodeRuntimeReferenceScriptsProgram,
  verifyNodeRuntimeReferenceScriptsProgram,
} from "../transactions/reference-scripts.js";
import {
  applyConfirmedLedgerDeltaChainTransaction,
  materializeConfirmedLedgerSnapshot,
} from "../transactions/state-queue/confirmed-ledger-snapshot.js";
import { deserializeStateQueueUTxO } from "../workers/utils/commit-block-header.js";
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

const isRetryableProtocolStatusError = (error: SDK.LucidError): boolean =>
  error.message.startsWith("Failed to fetch ") ||
  error.message.startsWith("Failed to query ");

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
      if (!isRetryableProtocolStatusError(lastError)) {
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

/**
 * Seeds the in-memory local block-boundary cache from the current state-queue
 * tip during startup.
 */
export const seedLatestLocalBlockBoundaryOnStartup = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const globals = yield* Globals;
  const config = yield* NodeConfig;

  const snapshot = yield* fetchStateQueueSnapshotProgram(
    lucid.api,
    contracts.stateQueue,
    "startup",
  );
  yield* refreshStateQueueGlobalsFromSnapshot(globals, snapshot);
  const latestBlock = yield* deserializeStateQueueUTxO(
    snapshot.tailCommitBase.utxo,
  );
  const latestEndTimeMs = snapshot.tailCommitBase.blockEndTimeMs;
  yield* Effect.logInfo(
    `Startup state-queue snapshot hydrated: tail=${snapshot.tailCommitBase.outRef},snapshot=${snapshot.snapshotId}`,
  );
  if (snapshot.topology.parsedNodeCount <= 1) {
    const confirmedLedgerEntries = yield* ConfirmedLedgerDB.retrieve;
    const confirmedLedgerRoot = yield* computeLedgerMpfRootFromLedgerEntries(
      confirmedLedgerEntries,
    );
    const onChainUtxoRoot = snapshot.tailCommitBase.roots.utxosRoot;
    if (confirmedLedgerRoot === onChainUtxoRoot) {
      if (config.MPF_ENGINE === "architecture_g") {
        yield* Effect.logInfo(
          "Startup verified confirmed ledger against the clean state queue; native owner will establish the durable MPF root before Ready.",
        );
      } else {
        const syncResult = yield* synchronizeCommitMpfStoresFromLedgerEntries(
          confirmedLedgerEntries,
        );
        yield* Effect.logInfo(
          `Startup synchronized clean-queue commit MPFs from confirmed ledger (ledger_entries=${syncResult.ledgerEntryCount.toString()},ledger_root=${syncResult.ledgerRoot}).`,
        );
      }
    } else {
      const finalizedJournal =
        snapshot.tailCommitBase.headerHash === null
          ? Option.none<PendingBlockFinalizationsDB.Record>()
          : yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              Buffer.from(snapshot.tailCommitBase.headerHash, "hex"),
            );
      if (
        Option.isSome(finalizedJournal) &&
        finalizedJournal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
          PendingBlockFinalizationsDB.Status.Finalized
      ) {
        const finalizedSnapshot = yield* materializeConfirmedLedgerSnapshot(
          finalizedJournal.value,
        );
        if (finalizedSnapshot.root === onChainUtxoRoot) {
          const recoveredEntries =
            yield* applyConfirmedLedgerDeltaChainTransaction(finalizedSnapshot);
          if (config.MPF_ENGINE === "architecture_g") {
            yield* Effect.logInfo(
              "Startup repaired confirmed ledger from authenticated journals; native owner must recover the corresponding durable root before Ready.",
            );
          } else {
            const syncResult =
              yield* synchronizeCommitMpfStoresFromLedgerEntries(
                recoveredEntries,
              );
            yield* Effect.logWarning(
              `Startup applied ${finalizedSnapshot.deltaChain.length.toString()} authenticated finalized-journal delta(s) to confirmed_ledger and synchronized commit MPFs (header=${snapshot.tailCommitBase.headerHash},ledger_entries=${syncResult.ledgerEntryCount.toString()},ledger_root=${syncResult.ledgerRoot},previous_confirmed_ledger_root=${confirmedLedgerRoot}).`,
            );
          }
        } else if (confirmedLedgerEntries.length > 0) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Startup clean-queue confirmed ledger root does not match the on-chain state queue root",
              cause: `confirmed_ledger_entries=${confirmedLedgerEntries.length.toString()},confirmed_ledger_root=${confirmedLedgerRoot},journal_snapshot_root=${finalizedSnapshot.root},on_chain_utxo_root=${onChainUtxoRoot},snapshot=${snapshot.snapshotId}`,
            }),
          );
        } else {
          yield* Effect.logInfo(
            `Startup skipped clean-queue commit MPF synchronization because confirmed_ledger is empty and the finalized journal root (${finalizedSnapshot.root}) does not match the on-chain root (${onChainUtxoRoot}).`,
          );
        }
      } else if (confirmedLedgerEntries.length > 0) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Startup clean-queue confirmed ledger root does not match the on-chain state queue root",
            cause: `confirmed_ledger_entries=${confirmedLedgerEntries.length.toString()},confirmed_ledger_root=${confirmedLedgerRoot},on_chain_utxo_root=${onChainUtxoRoot},snapshot=${snapshot.snapshotId}`,
          }),
        );
      } else {
        yield* Effect.logInfo(
          `Startup skipped clean-queue commit MPF synchronization because confirmed_ledger is empty and its root (${confirmedLedgerRoot}) does not match the on-chain root (${onChainUtxoRoot}).`,
        );
      }
    }
  }
  const canonicalHeaders = yield* fetchCanonicalCommittedHeaders;
  const revivedPayloadJournal = yield* reviveEarliestCanonicalPayloadJournal({
    canonicalHeaders,
    logPrefix: "Startup",
  });
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
      // Only an unattributed abandonment is revived bare here. A replaced
      // journal needs its members taken back, which the earliest-journal
      // revival above does; a correction-abandoned one is never revived.
      if (
        finalizedJournal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
          PendingBlockFinalizationsDB.Status.Abandoned &&
        localJournalHasPayloadMembers(finalizedJournal.value) &&
        journalAbandonment(finalizedJournal.value) === "unattributed" &&
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
  const deletedSupersededPreSubmitJournals =
    yield* PendingBlockFinalizationsDB.deleteSupersededAbandonedUnsubmitted();
  if (deletedSupersededPreSubmitJournals > 0) {
    yield* Effect.logInfo(
      `Deleted ${deletedSupersededPreSubmitJournals.toString()} superseded pre-submit pending-finalization journal(s) already covered by finalized state-queue roots.`,
    );
  }
  yield* Ref.set(globals.LATEST_LOCAL_BLOCK_END_TIME_MS, seededBoundaryMs);
  yield* Effect.logInfo(
    `Seeded latest local block boundary from startup state: ${new Date(seededBoundaryMs).toISOString()}`,
  );
}).pipe(
  Effect.tapError((e) =>
    Effect.logError(
      `Failed to seed latest local block boundary on startup: ${formatUnknownError(e)}`,
    ),
  ),
);

export const hydratePendingBlockFinalizationOnStartup = Effect.gen(
  function* () {
    const globals = yield* Globals;
    yield* PendingBlockFinalizationsDB.assertActiveJournalPayloadsComplete;
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
      (
        submittedTxHash ??
        record[PendingBlockFinalizationsDB.Columns.INTENDED_TX_HASH]
      )?.toString("hex") ?? "",
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
      `Failed to hydrate pending block-finalization journal on startup: ${formatUnknownError(error)}`,
    ),
  ),
  Effect.orDie,
);

/**
 * Journal statuses of a submitted block whose local finalization has not
 * completed. A failed finalization attempt for such a block is owned by the
 * runtime: the commit worker retries it while the block is live, and if the
 * block is removed on L1 the correction path abandons the journal and removes
 * the moot job with it. Refusing startup here would stop the correction observer from ever
 * admitting that removal.
 */
const RUNTIME_OWNED_FAILED_FINALIZATION_JOURNAL_STATUSES: readonly PendingBlockFinalizationsDB.Status[] =
  [
    PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
    PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
    PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
  ];

const LOCAL_FINALIZATION_JOB_ID_PATTERN = new RegExp(
  `^${MutationJobsDB.Kind.LocalBlockFinalization}:([0-9a-f]{56})$`,
);

/** The header of a failed local-finalization job, or none for any other job. */
const failedLocalFinalizationHeader = (
  job: MutationJobsDB.Entry,
): Buffer | undefined => {
  if (
    job[MutationJobsDB.Columns.KIND] !==
      MutationJobsDB.Kind.LocalBlockFinalization ||
    job[MutationJobsDB.Columns.STATUS] !== MutationJobsDB.Status.Failed
  )
    return undefined;
  const match = LOCAL_FINALIZATION_JOB_ID_PATTERN.exec(
    job[MutationJobsDB.Columns.JOB_ID],
  );
  return match === null ? undefined : Buffer.from(match[1]!, "hex");
};

/**
 * Whether startup may hand an unfinished job to the runtime. Only a failed
 * local-finalization job whose own journal still records its submitted block
 * as awaiting local finalization qualifies. Every other unfinished job
 * refuses, exactly as before: a running job (a crash mid-mutation), a failed
 * merge finalization, and a failed local finalization whose journal is
 * missing, finalized, abandoned or never submitted.
 */
export const classifyUnfinishedMutationJobOnStartup = (
  job: MutationJobsDB.Entry,
  journalStatus: PendingBlockFinalizationsDB.Status | undefined,
): "runtime" | "refuse" =>
  failedLocalFinalizationHeader(job) !== undefined &&
  journalStatus !== undefined &&
  RUNTIME_OWNED_FAILED_FINALIZATION_JOURNAL_STATUSES.includes(journalStatus)
    ? "runtime"
    : "refuse";

/**
 * Startup gate over unfinished local mutation jobs. It runs after pending
 * history reconciliation (a correction rewind that abandons a removed block's
 * journal also removes its job) and refuses to serve while any job needs
 * operator recovery; see classifyUnfinishedMutationJobOnStartup.
 */
export const assertStartupMutationJobsRecoverable = Effect.gen(function* () {
  const unfinished = yield* MutationJobsDB.retrieveUnfinished;
  const refused: MutationJobsDB.Entry[] = [];
  for (const job of unfinished) {
    const header = failedLocalFinalizationHeader(job);
    const journal =
      header === undefined
        ? Option.none()
        : yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(header);
    const journalStatus = Option.isSome(journal)
      ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
      : undefined;
    if (
      classifyUnfinishedMutationJobOnStartup(job, journalStatus) === "refuse"
    ) {
      refused.push(job);
      continue;
    }
    yield* Effect.logWarning(
      `Startup left failed local mutation job ${job[MutationJobsDB.Columns.JOB_ID]} (attempts=${job[MutationJobsDB.Columns.ATTEMPTS].toString()},journal_status=${journalStatus ?? "none"}) to the runtime: finalization is retried while its block is live, and a correction removing the block also removes the job. last_error=${job[MutationJobsDB.Columns.LAST_ERROR] ?? "none"}`,
    );
  }
  if (refused.length > 0)
    return yield* Effect.fail(
      new DatabaseInitializationError({
        message:
          "Startup found unfinished local mutation jobs; refusing to serve until recovery is performed",
        cause: refused.map((job) => ({
          jobId: job[MutationJobsDB.Columns.JOB_ID],
          kind: job[MutationJobsDB.Columns.KIND],
          status: job[MutationJobsDB.Columns.STATUS],
          updatedAt: job[MutationJobsDB.Columns.UPDATED_AT].toISOString(),
          lastError: job[MutationJobsDB.Columns.LAST_ERROR],
        })),
      }),
    );
});
