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
  type CanonicalCommittedHeader,
  fetchCanonicalCommittedHeaders,
  findSignedIntentReplacementIntegrityError,
  reviveEarliestCanonicalPayloadJournal,
} from "../services/canonical-journal-recovery.js";
import { signedCommitNode } from "../services/history-expired-intent-release.js";
import { Globals, Lucid, MidgardContracts } from "../services/index.js";
import {
  HaltSource,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";
import {
  SIGNED_INTENT_UNDECIDED,
  SIGNED_INTENT_UNDECIDED_ESCALATION_MS,
} from "../services/signed-intent-undecided.js";
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
 * Startup's recovery over the journals of the canonical queue, and the local
 * block boundary it seeds: the latest of the tip's end time, the tip's
 * journal's and the revived journal's.
 *
 * Only the earliest unattributed abandoned payload journal is revived, under
 * the single-active guard of reviveEarliestCanonicalPayloadJournal. A
 * replaced journal is revived only by the history owner from its
 * authenticated view; a correction-abandoned one is never revived.
 *
 * A replaced block that won its slot after the node moved past its base
 * (`SignedIntentReplacementIntegrityError`, refused before anything is
 * written) cannot be decided from this one view either. As in steady state
 * (blockConfirmationStep), it raises `signed_intent_undecided`, which holds
 * block commitment before any commit fiber starts; confirmation re-derives it
 * on every tick and clears it. Every other failure still fails startup.
 */
export const recoverCanonicalJournalsOnStartup = ({
  globals,
  canonicalHeaders,
  latestHeaderHash,
  latestEndTimeMs,
}: {
  readonly globals: Pick<Globals, "LIVENESS_REASONS">;
  readonly canonicalHeaders: readonly CanonicalCommittedHeader[];
  readonly latestHeaderHash: Option.Option<Buffer>;
  readonly latestEndTimeMs: number;
}) =>
  Effect.gen(function* () {
    const revivedPayloadJournal = yield* reviveEarliestCanonicalPayloadJournal({
      canonicalHeaders,
      logPrefix: "Startup",
    }).pipe(
      Effect.catchAllCause((cause) => {
        const integrity = findSignedIntentReplacementIntegrityError(cause);
        return integrity === undefined
          ? Effect.failCause(cause)
          : raiseLivenessIncident(
              globals,
              HaltSource.blockConfirmationSignedIntent,
              SIGNED_INTENT_UNDECIDED,
              `${integrity.message} Startup revived no journal; block commitment is held, the signed intent stays in place, and confirmation re-derives it on every tick.`,
              { escalateAfterMs: SIGNED_INTENT_UNDECIDED_ESCALATION_MS },
            ).pipe(Effect.as(Option.none<CanonicalCommittedHeader>()));
      }),
    );
    let seededBoundaryMs = latestEndTimeMs;
    if (Option.isSome(latestHeaderHash)) {
      const latestJournal =
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
          latestHeaderHash.value,
        );
      if (Option.isSome(latestJournal)) {
        seededBoundaryMs = Math.max(
          latestJournal.value[
            PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME
          ].getTime(),
          latestEndTimeMs,
        );
        yield* Effect.logInfo(
          `Seeded latest local block boundary from pending-finalization journal for header ${latestHeaderHash.value.toString("hex")}: ${new Date(seededBoundaryMs).toISOString()}`,
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
    return seededBoundaryMs;
  });

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
  const latestHeaderHash =
    latestBlock.datum.key === "Empty"
      ? Option.none<Buffer>()
      : Option.some(
          Buffer.from(
            yield* SDK.hashBlockHeader(
              yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum),
            ),
            "hex",
          ),
        );
  const seededBoundaryMs = yield* recoverCanonicalJournalsOnStartup({
    globals,
    canonicalHeaders: yield* fetchCanonicalCommittedHeaders,
    latestHeaderHash,
    latestEndTimeMs,
  });
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
 * completed. An unfinished finalization attempt for such a block, failed or
 * killed mid-way, is owned by the runtime: the commit worker retries it while
 * the block is live (`start` re-arms the row, and the retry is idempotent:
 * ImmutableDB rows already written are filtered, every later step is a
 * guarded status update), and if the block is removed on L1 the correction
 * path abandons the journal and removes the moot job with it. Refusing
 * startup here would stop the correction observer from ever admitting that
 * removal.
 */
const RUNTIME_OWNED_UNFINISHED_FINALIZATION_JOURNAL_STATUSES: readonly PendingBlockFinalizationsDB.Status[] =
  [
    PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
    PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
    PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
  ];

export const LOCAL_FINALIZATION_JOB_ID_PATTERN = new RegExp(
  `^${MutationJobsDB.Kind.LocalBlockFinalization}:([0-9a-f]{56})$`,
);

/** The header of an unfinished (running or failed) local-finalization job,
 * or none for any other job. */
export const unfinishedLocalFinalizationHeader = (
  job: MutationJobsDB.Entry,
): Buffer | undefined => {
  if (
    job[MutationJobsDB.Columns.KIND] !==
      MutationJobsDB.Kind.LocalBlockFinalization ||
    (job[MutationJobsDB.Columns.STATUS] !== MutationJobsDB.Status.Failed &&
      job[MutationJobsDB.Columns.STATUS] !== MutationJobsDB.Status.Running)
  )
    return undefined;
  const match = LOCAL_FINALIZATION_JOB_ID_PATTERN.exec(
    job[MutationJobsDB.Columns.JOB_ID],
  );
  return match === null ? undefined : Buffer.from(match[1]!, "hex");
};

/**
 * What startup does with an unfinished job. It runs under this process's
 * acquired history authority, so no other node process is mid-way through
 * any job: a running row is a process that died (SIGKILL, OOM, power loss)
 * before recording the outcome.
 *
 * "runtime" hands the job over:
 * - a confirmed-merge finalization, failed or running: it is idempotent, and
 *   every merge attempt first finalizes each merge L1 confirmed that this
 *   database has not (finalizeLandedMergesProgram);
 * - a failed or running local-finalization job whose own journal still
 *   records its submitted block as awaiting local finalization.
 *
 * "complete" closes a running or failed local-finalization job whose
 * journal is finalized: marking the journal finalized is the job's last
 * durable step, so only its completion record was lost, to a process that
 * died before markCompleted or to a transient that failed markCompleted (or
 * the ack of markFinalized's commit) after it. The commit worker closes the
 * same job the same way at runtime.
 *
 * Every other unfinished job refuses: a local finalization whose journal is
 * missing, abandoned or never submitted, and a malformed job id.
 */
export const classifyUnfinishedMutationJobOnStartup = (
  job: MutationJobsDB.Entry,
  journalStatus: PendingBlockFinalizationsDB.Status | undefined,
): "runtime" | "complete" | "refuse" => {
  if (
    job[MutationJobsDB.Columns.KIND] ===
    MutationJobsDB.Kind.ConfirmedMergeFinalization
  )
    return "runtime";
  if (
    unfinishedLocalFinalizationHeader(job) === undefined ||
    journalStatus === undefined
  )
    return "refuse";
  if (
    RUNTIME_OWNED_UNFINISHED_FINALIZATION_JOURNAL_STATUSES.includes(
      journalStatus,
    )
  )
    return "runtime";
  return journalStatus === PendingBlockFinalizationsDB.Status.Finalized
    ? "complete"
    : "refuse";
};
