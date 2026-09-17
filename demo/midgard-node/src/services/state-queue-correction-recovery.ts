import {
  parseStateQueueAuthenticatedTransition,
  type StateQueueAuthenticatedTransition,
} from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  BlocksDB,
  DepositsDB,
  ForcedTransactionsDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  WithdrawalsDB,
} from "../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import { Database } from "./database.js";

export type CorrectedBlockReinclusionResult = {
  readonly headerHash: string;
  readonly journalFound: boolean;
  readonly restoredMempoolTransactions: number;
  readonly restoredProcessedTransactions: number;
  readonly reopenedEvents: number;
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

/**
 * Reopens every locally journaled payload only after its exact L1 header has
 * been confirmed removed. All database mutations are one transaction, so a
 * crash cannot expose half-reincluded events or transactions.
 */
const reincludeStateQueueCorrectedBlockPayloadHashes = (
  removedHeaderHashes: readonly string[],
): Effect.Effect<
  readonly CorrectedBlockReinclusionResult[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.forEach(
        removedHeaderHashes,
        (headerHashHex) =>
          Effect.gen(function* () {
            const headerHash = Buffer.from(headerHashHex, "hex");
            const journal =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                headerHash,
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

            yield* MempoolDB.restoreJournalEntries(mempoolEntries);
            yield* ProcessedMempoolDB.insertTxs([...processedEntries]);
            yield* BlocksDB.clearBlock(headerHash);
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
            yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
              headerHash,
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
            } satisfies CorrectedBlockReinclusionResult;
          }),
        { concurrency: 1 },
      ),
    );
  }).pipe(
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
  return reincludeStateQueueCorrectedBlockPayloadHashes(
    transition.removedHeaderHashes,
  );
};

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
              );
            if (Option.isNone(journal)) {
              return {
                headerHash: headerHashHex,
                journalFound: false,
                restoredCanonicalBlock: false,
              } satisfies CorrectedBlockRollbackRestoreResult;
            }
            const record = journal.value;
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
            yield* MempoolDB.clearTxs(mempoolTxIds);
            yield* ProcessedMempoolDB.clearTxs(processedTxIds);
            yield* BlocksDB.insert(headerHash, allTxIds);
            yield* DepositsDB.markProjectedByEventIds(
              record.depositEventIds,
              headerHash,
            );
            yield* ForcedTransactionsDB.markProjectedByEventIds(
              record.forcedTransactionEventIds,
              headerHash,
            );
            yield* WithdrawalsDB.markProjectedByEventIds(
              record.withdrawalEventIds,
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
    sqlErrorToDatabaseError(
      "state_queue_correction_recovery",
      "Failed to restore a post-finality rolled-back state-queue correction",
    ),
  );
};
