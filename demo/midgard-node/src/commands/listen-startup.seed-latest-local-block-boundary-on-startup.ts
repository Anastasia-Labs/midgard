import "./listen-startup.ensure-protocol-initialized-on-startup.js";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option, Ref } from "effect";

import {
  ConfirmedLedgerDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/index.js";
import {
  fetchCanonicalCommittedHeaders,
  journalAbandonment,
  localJournalHasPayloadMembers,
  reviveEarliestCanonicalPayloadJournal,
} from "../services/canonical-journal-recovery.js";
import { signedCommitNode } from "../services/history-expired-intent-release.js";
import { Globals, Lucid, MidgardContracts } from "../services/index.js";
import {
  fetchStateQueueSnapshotProgram,
  refreshStateQueueGlobalsFromSnapshot,
} from "../services/state-queue-topology.js";
import {
  applyConfirmedLedgerDeltaChainTransaction,
  materializeConfirmedLedgerSnapshot,
} from "../transactions/state-queue/confirmed-ledger-snapshot.js";
import {
  deserializeStateQueueUTxO,
  serializeStateQueueUTxO,
} from "../workers/utils/commit-block-header.js";

/**
 * Seeds the in-memory local block-boundary cache from the current state-queue
 * tip during startup.
 */
export const seedLatestLocalBlockBoundaryOnStartup = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const globals = yield* Globals;

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
      yield* Effect.logInfo(
        "Startup verified confirmed ledger against the clean state queue; native owner will establish the durable MPF root before Ready.",
      );
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
          yield* applyConfirmedLedgerDeltaChainTransaction(finalizedSnapshot);
          yield* Effect.logInfo(
            "Startup repaired confirmed ledger from authenticated journals; native owner must recover the corresponding durable root before Ready.",
          );
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
      // Only an unattributed abandonment is revived here, as in the
      // earliest-journal revival above. A replaced journal is revived only by
      // the history owner from its authenticated view; a correction-abandoned
      // one is never revived.
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
    const status = record[PendingBlockFinalizationsDB.Columns.STATUS];
    // An observed block's node may already be merged into the confirmed state
    // (or removed), so no queue read ever yields it again: re-derive the node
    // its retained signed commit created, which local finalization binds to
    // the journal by every header root.
    yield* Ref.set(
      globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
      status === PendingBlockFinalizationsDB.Status.ObservedWaitingStability
        ? yield* signedCommitNode(record, yield* MidgardContracts).pipe(
            Effect.flatMap(({ node }) => serializeStateQueueUTxO(node)),
            Effect.catchAll((cause) =>
              Effect.logWarning(
                `Observed journal ${record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex")} retains no node its signed commit creates; local finalization waits for the confirmation path: ${formatUnknownError(cause)}`,
              ).pipe(Effect.as("" as const)),
            ),
          )
        : "",
    );
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

export const LOCAL_FINALIZATION_JOB_ID_PATTERN = new RegExp(
  `^${MutationJobsDB.Kind.LocalBlockFinalization}:([0-9a-f]{56})$`,
);

/** The header of a failed local-finalization job, or none for any other job. */
export const failedLocalFinalizationHeader = (
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
 * Whether startup may hand an unfinished job to the runtime. Two kinds
 * qualify:
 *
 * - a confirmed-merge finalization, failed or running (a crash mid-way): it
 *   is idempotent, and every merge attempt first finalizes each merge L1
 *   confirmed that this database has not (finalizeLandedMergesProgram);
 * - a failed local-finalization job whose own journal still records its
 *   submitted block as awaiting local finalization.
 *
 * Every other unfinished job refuses: a running local finalization (a crash
 * mid-mutation), and a failed one whose journal is missing, finalized,
 * abandoned or never submitted.
 */
export const classifyUnfinishedMutationJobOnStartup = (
  job: MutationJobsDB.Entry,
  journalStatus: PendingBlockFinalizationsDB.Status | undefined,
): "runtime" | "refuse" =>
  job[MutationJobsDB.Columns.KIND] ===
    MutationJobsDB.Kind.ConfirmedMergeFinalization ||
  (failedLocalFinalizationHeader(job) !== undefined &&
    journalStatus !== undefined &&
    RUNTIME_OWNED_FAILED_FINALIZATION_JOURNAL_STATUSES.includes(journalStatus))
    ? "runtime"
    : "refuse";
