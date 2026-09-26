import {
  parseStateQueueAuthenticatedTransition,
  type StateQueueAuthenticatedTransition,
} from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { correctionRewindRemovedHeaders } from "../database/eventHistoryRecoveryPlans.js";
import {
  BlocksDB,
  DepositsDB,
  ForcedTransactionsDB,
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  WithdrawalsDB,
} from "../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import { Database } from "./database.js";
import { withHistoryWrite } from "./event-history-producer.js";
import {
  restoreSpeculativeLedgerAfterCorrection,
  type WithdrawalLedgerRestore,
} from "./state-queue-correction-ledger-restore.js";
import { StateQueueCorrectionRewindIntegrityError } from "./state-queue-correction-observer.js";

export type CorrectedBlockReinclusionResult = {
  readonly headerHash: string;
  readonly journalFound: boolean;
  readonly restoredMempoolTransactions: number;
  readonly restoredProcessedTransactions: number;
  readonly reopenedEvents: number;
  /** Status the journal held when this reinclusion abandoned it. */
  readonly abandonedFromStatus?: PendingBlockFinalizationsDB.Status;
  /** Pending transactions rejected because they depended on reopened state;
   * reported on the last block of the batch only. */
  readonly rejectedDependentTransactions?: readonly Buffer[];
  readonly restoredWithdrawalOutputs?: number;
};

export type CorrectedBlockRollbackRestoreResult = Readonly<{
  headerHash: string;
  journalFound: boolean;
  restoredCanonicalBlock: boolean;
}>;

export type StateQueueCorrectionReinclusionAuthority = Readonly<{
  expectedDeploymentIdentityDigest: string;
  requiredFinalityDepth: bigint;
}>;

/**
 * The database recovery boundary accepts only the shared, digest-bound full
 * transition envelope after the caller's own L1 admission/finality engine has
 * authenticated it. Timeout correction and fraud removal both invalidate the
 * removed block's locally projected payloads; a normal merge is terminal but
 * must never reopen them. A hash list or transaction-status response is never
 * sufficient authority to mutate the database.
 */
export const authorizeStateQueueCorrectionReinclusion = (
  transitionInput: unknown,
  authority: StateQueueCorrectionReinclusionAuthority,
): StateQueueAuthenticatedTransition => {
  const transition = parseStateQueueAuthenticatedTransition(transitionInput);
  if (transition === null) {
    throw new Error(
      "State-queue correction reinclusion requires a canonical digest-bound authenticated transition V1.",
    );
  }
  if (
    authority.requiredFinalityDepth <= 0n ||
    BigInt(transition.finalityDepth) < authority.requiredFinalityDepth
  ) {
    throw new Error(
      `State-queue correction transition ${transition.transitionDigest} has finality depth ${transition.finalityDepth}, below required release depth ${authority.requiredFinalityDepth.toString()}.`,
    );
  }
  if (
    transition.deploymentIdentityDigest !==
    authority.expectedDeploymentIdentityDigest
  ) {
    throw new Error(
      `State-queue correction transition deployment ${transition.deploymentIdentityDigest} does not match configured deployment ${authority.expectedDeploymentIdentityDigest}.`,
    );
  }
  if (
    transition.transitionKind !== "timeout_correction" &&
    transition.transitionKind !== "fraud_removal"
  ) {
    throw new Error(
      `State-queue ${transition.transitionKind} transition must not reinclude corrected payloads.`,
    );
  }
  return transition;
};

/** One block reopened by an admitted correction: a block the correction
 * removed, or a descendant proven never to reach L1 because its commit spends
 * the removed block's consumed queue node. */
export type StateQueueCorrectedBlock = Readonly<{
  headerHash: string;
  transitionDigest: string;
  kind: "removed" | "unlanded";
}>;

const Status = PendingBlockFinalizationsDB.Status;
const J = PendingBlockFinalizationsDB.Columns;
/** A removed block's journal always recorded a submission; one still reading
 * pending_submission stopped between handing the signed commit to L1 and
 * recording it, so it must retain the signed intent. */
const REMOVED_STATUSES: readonly PendingBlockFinalizationsDB.Status[] = [
  Status.SubmittedLocalFinalizationPending,
  Status.SubmittedUnconfirmed,
  Status.ObservedWaitingStability,
  Status.Finalized,
];
/** An unlanded descendant was never observed on L1. */
const UNLANDED_STATUSES: readonly PendingBlockFinalizationsDB.Status[] = [
  Status.PendingSubmission,
  Status.SubmittedLocalFinalizationPending,
  Status.SubmittedUnconfirmed,
];
/** Statuses reached only after local finalization wrote ImmutableDB and
 * BlocksDB and applied the block's withdrawal ledger effects. */
const LOCALLY_FINALIZED_STATUSES: readonly PendingBlockFinalizationsDB.Status[] =
  [
    Status.SubmittedUnconfirmed,
    Status.ObservedWaitingStability,
    Status.Finalized,
  ];

/** Terminal abandonment of a reopened journal that never reached a submitted
 * status (removed pending_submission with a signed intent) or never landed
 * (unlanded descendant). Exactly one row, from exactly the admitted states. */
const abandonReopenedJournal = (
  headerHash: Buffer,
  transitionDigest: string,
  allowed: readonly PendingBlockFinalizationsDB.Status[],
  requireSignedIntent: boolean,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`
      UPDATE pending_block_finalizations
      SET status = ${Status.Abandoned},
        correction_transition_digest = ${transitionDigest},
        updated_at = NOW()
      WHERE header_hash = ${headerHash}
        AND status IN ${sql.in(allowed)}
        ${requireSignedIntent ? sql`AND intended_tx_hash IS NOT NULL AND signed_tx_cbor IS NOT NULL` : sql``}
      RETURNING header_hash`;
    if (rows.length !== 1)
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: "Failed to abandon a reopened block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
  });

/**
 * Reopens every locally journaled payload only after its exact L1 header has
 * been confirmed removed, in the given order. All database mutations are one
 * transaction, so a crash cannot expose half-reincluded events or
 * transactions. Callers must already hold correction authority for every
 * block (see authorizeStateQueueCorrectionReinclusion).
 */
export const reincludeStateQueueCorrectedBlocks = (
  removedBlocks: readonly StateQueueCorrectedBlock[],
): Effect.Effect<
  readonly CorrectedBlockReinclusionResult[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const withdrawalRestores: WithdrawalLedgerRestore[] = [];
    const reopenedDepositEventIds: Buffer[] = [];
    return yield* sql.withTransaction(
      Effect.forEach(
        removedBlocks,
        ({ headerHash: headerHashHex, transitionDigest, kind }) =>
          Effect.gen(function* () {
            const headerHash = Buffer.from(headerHashHex, "hex");
            const journal =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                headerHash,
                true,
              );
            if (Option.isNone(journal)) {
              return {
                headerHash: headerHashHex,
                journalFound: false,
                restoredMempoolTransactions: 0,
                restoredProcessedTransactions: 0,
                reopenedEvents: 0,
              } satisfies CorrectedBlockReinclusionResult;
            }
            const record = journal.value;
            yield* PendingBlockFinalizationsDB.assertCanonicalEventMembers(
              record,
            );
            if (
              record[PendingBlockFinalizationsDB.Columns.STATUS] ===
              PendingBlockFinalizationsDB.Status.Abandoned
            ) {
              if (
                record[
                  PendingBlockFinalizationsDB.Columns
                    .CORRECTION_TRANSITION_DIGEST
                ] !== transitionDigest
              ) {
                return yield* Effect.fail(
                  new DatabaseError({
                    table: PendingBlockFinalizationsDB.tableName,
                    message:
                      "Abandoned journal does not identify this correction",
                    cause: headerHashHex,
                  }),
                );
              }
              return {
                headerHash: headerHashHex,
                journalFound: true,
                restoredMempoolTransactions: 0,
                restoredProcessedTransactions: 0,
                reopenedEvents: 0,
              } satisfies CorrectedBlockReinclusionResult;
            }
            const unknownMember = record.txMembers.find(
              (member) =>
                member[
                  PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE
                ] !== MempoolDB.tableName &&
                member[
                  PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE
                ] !== ProcessedMempoolDB.tableName,
            );
            if (unknownMember !== undefined) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: PendingBlockFinalizationsDB.tableName,
                  message:
                    "Cannot reinclude corrected block with unknown transaction source",
                  cause: `header_hash=${headerHashHex},source=${unknownMember[PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE]}`,
                }),
              );
            }
            const mempoolEntries = record.txMembers
              .filter(
                (member) =>
                  member[
                    PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE
                  ] === MempoolDB.tableName,
              )
              .map(PendingBlockFinalizationsDB.txMemberToEntry);
            const processedEntries = record.txMembers
              .filter(
                (member) =>
                  member[
                    PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE
                  ] === ProcessedMempoolDB.tableName,
              )
              .map(PendingBlockFinalizationsDB.txMemberToEntry);
            const status = record[J.STATUS];
            const allowed =
              kind === "removed" ? REMOVED_STATUSES : UNLANDED_STATUSES;
            const removedAwaitingAck =
              kind === "removed" &&
              status === Status.PendingSubmission &&
              record[J.INTENDED_TX_HASH] != null &&
              record[J.SIGNED_TX_CBOR] != null;
            if (!allowed.includes(status) && !removedAwaitingAck)
              return yield* Effect.fail(
                new DatabaseError({
                  table: PendingBlockFinalizationsDB.tableName,
                  message: `Cannot reopen a ${kind} block from journal status ${status}`,
                  cause: headerHashHex,
                }),
              );
            // Local finalization deleted each valid withdrawal's output from
            // the speculative ledger. Capture them before the withdrawals are
            // reopened, which clears their classification.
            if (LOCALLY_FINALIZED_STATUSES.includes(status)) {
              const validIds = new Set(
                record.withdrawalMembers
                  .filter(
                    (member) =>
                      member[
                        PendingBlockFinalizationsDB.WithdrawalMemberColumns
                          .VALIDITY
                      ] === WithdrawalsDB.Validity.WithdrawalIsValid,
                  )
                  .map((member) =>
                    member[
                      PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID
                    ].toString("hex"),
                  ),
              );
              const withdrawals = yield* WithdrawalsDB.retrieveByEventIds(
                record.withdrawalEventIds.filter((id) =>
                  validIds.has(id.toString("hex")),
                ),
              );
              if (withdrawals.length !== validIds.size)
                return yield* Effect.fail(
                  new DatabaseError({
                    table: WithdrawalsDB.tableName,
                    message:
                      "A reopened block's valid withdrawal row is missing",
                    cause: headerHashHex,
                  }),
                );
              for (const withdrawal of withdrawals)
                withdrawalRestores.push({
                  outRef: yield* WithdrawalsDB.toLedgerOutRef(withdrawal),
                  l2OutRefData: Buffer.from(
                    withdrawal[WithdrawalsDB.Columns.L2_OUTREF],
                  ),
                  baseTailHeaderHash: record[J.BASE_TAIL_HEADER_HASH],
                });
            }
            reopenedDepositEventIds.push(...record.depositEventIds);

            yield* DepositsDB.reopenAfterStateQueueCorrectionByEventIds(
              record.depositEventIds,
              headerHash,
            );
            yield* ForcedTransactionsDB.reopenAfterStateQueueCorrectionByEventIds(
              record.forcedTransactionEventIds,
              headerHash,
            );
            yield* WithdrawalsDB.reopenAfterStateQueueCorrectionByEventIds(
              record.withdrawalEventIds,
              headerHash,
            );
            yield* MempoolDB.restoreJournalEntries(mempoolEntries);
            yield* ProcessedMempoolDB.insertTxs([...processedEntries]);
            yield* BlocksDB.clearBlock(headerHash);
            // The block's transactions are pending again. A transaction left
            // in ImmutableDB would be filtered from its next block as already
            // committed; only one still referenced by a live block stays.
            const txIds = record.txMembers.map((member) =>
              Buffer.from(
                member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              ),
            );
            if (txIds.length > 0)
              yield* sql`DELETE FROM ${sql(ImmutableDB.tableName)} i
                WHERE i.tx_id IN ${sql.in(txIds)}
                  AND NOT EXISTS (
                    SELECT 1 FROM ${sql(BlocksDB.tableName)} b
                    WHERE b.tx_id = i.tx_id)`;
            if (kind === "removed" && !removedAwaitingAck)
              yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
                headerHash,
                transitionDigest,
              );
            else
              yield* abandonReopenedJournal(
                headerHash,
                transitionDigest,
                removedAwaitingAck ? [Status.PendingSubmission] : allowed,
                removedAwaitingAck,
              );
            return {
              headerHash: headerHashHex,
              journalFound: true,
              restoredMempoolTransactions: mempoolEntries.length,
              restoredProcessedTransactions: processedEntries.length,
              reopenedEvents:
                record.depositEventIds.length +
                record.forcedTransactionEventIds.length +
                record.withdrawalEventIds.length,
              abandonedFromStatus: status,
            } satisfies CorrectedBlockReinclusionResult;
          }),
        { concurrency: 1 },
      ).pipe(
        Effect.flatMap((results) =>
          Effect.gen(function* () {
            const ledger = yield* restoreSpeculativeLedgerAfterCorrection({
              withdrawals: withdrawalRestores,
              reopenedDepositEventIds,
            });
            if (results.length === 0) return results;
            return [
              ...results.slice(0, -1),
              {
                ...results.at(-1)!,
                rejectedDependentTransactions: ledger.rejectedTransactions,
                restoredWithdrawalOutputs: ledger.restoredWithdrawalOutputs,
              },
            ];
          }),
        ),
      ),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      "state_queue_correction_recovery",
      "Failed to reinclude state-queue-corrected payloads",
    ),
  );

export const reincludeFinalizedStateQueueCorrectionTransition = (
  transitionInput: unknown,
  authority: StateQueueCorrectionReinclusionAuthority,
): Effect.Effect<
  readonly CorrectedBlockReinclusionResult[],
  DatabaseError,
  Database
> => {
  const transition = authorizeStateQueueCorrectionReinclusion(
    transitionInput,
    authority,
  );
  return reincludeStateQueueCorrectedBlocks(
    transition.removedHeaderHashes.map((headerHash) => ({
      headerHash,
      transitionDigest: transition.transitionDigest,
      kind: "removed" as const,
    })),
  );
};

/**
 * Architecture G's answer to a post-finality rollback of a correction. The
 * native rewind has no inverse: once a rewind moved the native root off a
 * removed block (its plan names the block, or it abandoned the journal under
 * this correction), the rollback is an integrity failure and is refused with
 * an explicit error. A removal whose rewind never ran left no local effect, so
 * its rollback needs nothing.
 */
export const refuseRewoundStateQueueCorrectionRollback = (
  transitionInput: unknown,
  authority: StateQueueCorrectionReinclusionAuthority,
): Effect.Effect<
  void,
  DatabaseError | StateQueueCorrectionRewindIntegrityError,
  Database
> =>
  Effect.gen(function* () {
    const transition = authorizeStateQueueCorrectionReinclusion(
      transitionInput,
      authority,
    );
    const rewound = yield* correctionRewindRemovedHeaders(
      authority.expectedDeploymentIdentityDigest,
    );
    for (const headerHash of transition.removedHeaderHashes) {
      if (rewound.has(headerHash))
        return yield* Effect.fail(
          new StateQueueCorrectionRewindIntegrityError(
            headerHash,
            `correction ${transition.transitionDigest} that removed it rolled back after its rewind plan was recorded`,
          ),
        );
      const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(headerHash, "hex"),
      );
      if (
        Option.isSome(journal) &&
        journal.value[J.STATUS] === Status.Abandoned &&
        journal.value[J.CORRECTION_TRANSITION_DIGEST] != null
      )
        return yield* Effect.fail(
          new StateQueueCorrectionRewindIntegrityError(
            headerHash,
            `correction ${transition.transitionDigest} that removed it rolled back after its journal was abandoned under correction ${journal.value[J.CORRECTION_TRANSITION_DIGEST]}`,
          ),
        );
    }
  });

/**
 * Inverse of correction reinclusion for a post-finality L1 rollback which puts
 * the removed header back on the authenticated queue. The retained local block
 * journal is the only payload authority; every mutation is atomic and repeated
 * rollback reconciliation is idempotent in the Finalized state.
 */
export const restoreRetractedStateQueueCorrectionTransition = (
  transitionInput: unknown,
  authority: StateQueueCorrectionReinclusionAuthority,
): Effect.Effect<
  readonly CorrectedBlockRollbackRestoreResult[],
  DatabaseError,
  Database
> => {
  const transition = authorizeStateQueueCorrectionReinclusion(
    transitionInput,
    authority,
  );
  return Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.forEach(
        transition.removedHeaderHashes,
        (headerHashHex) =>
          Effect.gen(function* () {
            const headerHash = Buffer.from(headerHashHex, "hex");
            const journal =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                headerHash,
                true,
              );
            if (Option.isNone(journal)) {
              return {
                headerHash: headerHashHex,
                journalFound: false,
                restoredCanonicalBlock: false,
              } satisfies CorrectedBlockRollbackRestoreResult;
            }
            const record = journal.value;
            yield* PendingBlockFinalizationsDB.assertCanonicalEventMembers(
              record,
            );
            if (
              record[
                PendingBlockFinalizationsDB.Columns.CORRECTION_TRANSITION_DIGEST
              ] !== transition.transitionDigest
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: PendingBlockFinalizationsDB.tableName,
                  message: "Journal does not identify the retracted correction",
                  cause: headerHashHex,
                }),
              );
            }
            const status = record[PendingBlockFinalizationsDB.Columns.STATUS];
            if (status === PendingBlockFinalizationsDB.Status.Finalized) {
              return {
                headerHash: headerHashHex,
                journalFound: true,
                restoredCanonicalBlock: false,
              } satisfies CorrectedBlockRollbackRestoreResult;
            }
            if (status !== PendingBlockFinalizationsDB.Status.Abandoned) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: PendingBlockFinalizationsDB.tableName,
                  message:
                    "Cannot restore retracted correction from a non-abandoned journal",
                  cause: `header_hash=${headerHashHex},status=${status}`,
                }),
              );
            }
            const mempoolTxIds = record.txMembers
              .filter(
                (member) =>
                  member[
                    PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE
                  ] === MempoolDB.tableName,
              )
              .map((member) =>
                Buffer.from(
                  member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
                ),
              );
            const processedTxIds = record.txMembers
              .filter(
                (member) =>
                  member[
                    PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE
                  ] === ProcessedMempoolDB.tableName,
              )
              .map((member) =>
                Buffer.from(
                  member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
                ),
              );
            const allTxIds = record.txMembers.map((member) =>
              Buffer.from(
                member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              ),
            );
            yield* DepositsDB.markProjectedByEventIds(
              record.depositEventIds,
              headerHash,
            );
            yield* ForcedTransactionsDB.markProjectedByEventIds(
              record.forcedTransactionEventIds,
              headerHash,
            );
            yield* WithdrawalsDB.restoreCorrectedClassification(
              record.withdrawalMembers.map((member) => ({
                eventId:
                  member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
                settlementEventInfo:
                  member[
                    PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR
                  ],
                validity:
                  member[
                    PendingBlockFinalizationsDB.WithdrawalMemberColumns.VALIDITY
                  ],
                validityDetail:
                  member[
                    PendingBlockFinalizationsDB.WithdrawalMemberColumns
                      .VALIDITY_DETAIL
                  ],
              })),
              headerHash,
            );
            yield* DepositsDB.markConsumedByEventIds(record.depositEventIds);
            yield* WithdrawalsDB.markFinalizedByEventIds(
              record.withdrawalEventIds,
              headerHash,
            );
            yield* ForcedTransactionsDB.markFinalizedByEventIds(
              record.forcedTransactionEventIds,
              headerHash,
            );
            if (mempoolTxIds.length > 0)
              yield* MempoolDB.clearTxs(mempoolTxIds);
            if (processedTxIds.length > 0)
              yield* ProcessedMempoolDB.clearTxs(processedTxIds);
            // Inverse of reinclusion: the block's transactions are committed
            // again and its valid withdrawals consume their outputs again.
            yield* ImmutableDB.insertTxsValidatedNative(
              record.txMembers.map(PendingBlockFinalizationsDB.txMemberToEntry),
            );
            const validWithdrawalIds = record.withdrawalMembers
              .filter(
                (member) =>
                  member[
                    PendingBlockFinalizationsDB.WithdrawalMemberColumns.VALIDITY
                  ] === WithdrawalsDB.Validity.WithdrawalIsValid,
              )
              .map((member) =>
                Buffer.from(
                  member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
                ),
              );
            const validWithdrawals =
              yield* WithdrawalsDB.retrieveByEventIds(validWithdrawalIds);
            if (validWithdrawals.length !== validWithdrawalIds.length)
              return yield* Effect.fail(
                new DatabaseError({
                  table: WithdrawalsDB.tableName,
                  message: "A restored block's valid withdrawal row is missing",
                  cause: headerHashHex,
                }),
              );
            const withdrawnOutRefs = yield* Effect.forEach(
              validWithdrawals,
              WithdrawalsDB.toLedgerOutRef,
            );
            yield* DepositsDB.markConsumedByEventIds(
              yield* MempoolLedgerDB.clearUTxOs(withdrawnOutRefs),
            );
            yield* BlocksDB.insert(headerHash, allTxIds);
            yield* PendingBlockFinalizationsDB.reviveAbandonedCanonical(
              headerHash,
              BigInt(Date.now()),
            );
            yield* PendingBlockFinalizationsDB.markFinalized(headerHash);
            return {
              headerHash: headerHashHex,
              journalFound: true,
              restoredCanonicalBlock: true,
            } satisfies CorrectedBlockRollbackRestoreResult;
          }),
        { concurrency: 1 },
      ),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      "state_queue_correction_recovery",
      "Failed to restore a post-finality rolled-back state-queue correction",
    ),
  );
};
