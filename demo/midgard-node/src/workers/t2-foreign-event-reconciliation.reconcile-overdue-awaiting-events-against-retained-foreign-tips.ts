import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  WithdrawalsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { ContractDeploymentIdentity, Database } from "../services/index.js";
import { reconcileRetainedForeignTipEntry } from "./t2-foreign-event-reconciliation.reconcile-retained-foreign-tip-entry.js";
import {
  emptyIds,
  normalizedIds,
  type T2ForeignEventResolution,
} from "./t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";

export const reconcileOverdueAwaitingEventsAgainstForeignTip = ({
  foreignHeaderHash,
  header,
}: {
  readonly foreignHeaderHash: string;
  readonly header: SDK.Header;
}): Effect.Effect<
  T2ForeignEventResolution,
  DatabaseError,
  Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const suppliedHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: ForeignTipReconciliationsDB.tableName,
            message: "Failed to authenticate supplied foreign header",
            cause,
          }),
      ),
    );
    if (suppliedHash !== foreignHeaderHash) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Supplied foreign header does not match reconciliation key",
          cause: `provided=${foreignHeaderHash},recomputed=${suppliedHash}`,
        }),
      );
    }
    const reconciliation =
      yield* ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
        foreignHeaderHash,
      );
    return Option.isNone(reconciliation)
      ? ({ type: "Ready", absent: emptyIds() } as const)
      : yield* reconcileRetainedForeignTipEntry(reconciliation.value);
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof DatabaseError
        ? cause
        : new DatabaseError({
            table: "t2_foreign_event_reconciliation",
            message: "Failed to reconcile overdue events against foreign tip",
            cause,
          }),
    ),
  );

const resolvedWindowHasAwaitingEvents = (
  entry: ForeignTipReconciliationsDB.Entry,
): Effect.Effect<boolean, unknown, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const startTime =
      entry[ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME];
    const endTime = entry[ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME];
    const rows = yield* sql<{ readonly actionable: boolean }>`
      SELECT (
        EXISTS (
          SELECT 1 FROM ${sql(DepositsDB.tableName)}
          WHERE ${sql(DepositsDB.Columns.STATUS)} = ${DepositsDB.Status.Awaiting}
            AND ${sql(DepositsDB.Columns.PROJECTED_HEADER_HASH)} IS NULL
            AND ${sql(DepositsDB.Columns.INCLUSION_TIME)} > ${startTime}
            AND ${sql(DepositsDB.Columns.INCLUSION_TIME)} <= ${endTime}
        )
        OR EXISTS (
          SELECT 1 FROM ${sql(ForcedTransactionsDB.tableName)}
          WHERE ${sql(ForcedTransactionsDB.Columns.STATUS)} = ${ForcedTransactionsDB.Status.Awaiting}
            AND ${sql(ForcedTransactionsDB.Columns.PROJECTED_HEADER_HASH)} IS NULL
            AND ${sql(ForcedTransactionsDB.Columns.INCLUSION_TIME)} > ${startTime}
            AND ${sql(ForcedTransactionsDB.Columns.INCLUSION_TIME)} <= ${endTime}
        )
        OR EXISTS (
          SELECT 1 FROM ${sql(WithdrawalsDB.tableName)}
          WHERE ${sql(WithdrawalsDB.Columns.STATUS)} = ${WithdrawalsDB.Status.Awaiting}
            AND ${sql(WithdrawalsDB.Columns.PROJECTED_HEADER_HASH)} IS NULL
            AND ${sql(WithdrawalsDB.Columns.INCLUSION_TIME)} > ${startTime}
            AND ${sql(WithdrawalsDB.Columns.INCLUSION_TIME)} <= ${endTime}
        )
      ) AS actionable
    `;
    return rows[0]?.actionable === true;
  });

/**
 * Replays every retained foreign window, including resolved evidence, so
 * indexer-late Awaiting rows remain recoverable after restart or after the
 * live state-queue tip advances beyond the foreign header.
 */
export const reconcileOverdueAwaitingEventsAgainstRetainedForeignTips =
  (): Effect.Effect<
    T2ForeignEventResolution,
    DatabaseError,
    Database | ContractDeploymentIdentity
  > =>
    Effect.gen(function* () {
      const history =
        yield* ForeignTipReconciliationsDB.retrieveEvidenceHistory;
      const combinedAbsent = {
        deposits: [] as string[],
        forcedTransactions: [] as string[],
        withdrawals: [] as string[],
      };
      let firstAwaiting: T2ForeignEventResolution | undefined;
      for (const entry of history) {
        if (
          entry[ForeignTipReconciliationsDB.Columns.STATUS] ===
            ForeignTipReconciliationsDB.Status.Resolved &&
          !(yield* resolvedWindowHasAwaitingEvents(entry))
        ) {
          continue;
        }
        const resolution = yield* reconcileRetainedForeignTipEntry(entry);
        if (resolution.type === "AwaitingForeignDa") {
          firstAwaiting ??= resolution;
        } else {
          combinedAbsent.deposits.push(...resolution.absent.deposits);
          combinedAbsent.forcedTransactions.push(
            ...resolution.absent.forcedTransactions,
          );
          combinedAbsent.withdrawals.push(...resolution.absent.withdrawals);
        }
      }
      return (
        firstAwaiting ??
        ({
          type: "Ready",
          absent: {
            deposits: normalizedIds(combinedAbsent.deposits),
            forcedTransactions: normalizedIds(
              combinedAbsent.forcedTransactions,
            ),
            withdrawals: normalizedIds(combinedAbsent.withdrawals),
          },
        } as const)
      );
    }).pipe(
      Effect.mapError((cause) =>
        cause instanceof DatabaseError
          ? cause
          : new DatabaseError({
              table: "t2_foreign_event_reconciliation",
              message:
                "Failed to reconcile overdue events against retained foreign tips",
              cause,
            }),
      ),
    );
