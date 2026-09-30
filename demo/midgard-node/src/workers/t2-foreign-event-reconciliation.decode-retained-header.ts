import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ForeignTipReconciliationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";

export const decodeRetainedHeader = (
  entry: ForeignTipReconciliationsDB.Entry,
): Effect.Effect<SDK.Header, DatabaseError> =>
  Effect.gen(function* () {
    const header = yield* Effect.try({
      try: () =>
        LucidData.from(
          entry[
            ForeignTipReconciliationsDB.Columns.FOREIGN_HEADER_CBOR
          ].toString("hex"),
          SDK.Header as never,
        ) as SDK.Header,
      catch: (cause) =>
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message: "Retained foreign header CBOR is invalid",
          cause,
        }),
    });
    const foreignHeaderHash =
      entry[ForeignTipReconciliationsDB.Columns.FOREIGN_HEADER_HASH].toString(
        "hex",
      );
    const recomputedHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: ForeignTipReconciliationsDB.tableName,
            message: "Failed to hash retained foreign header",
            cause,
          }),
      ),
    );
    const explicitEvidenceMatches =
      recomputedHash === foreignHeaderHash &&
      Number(header.startTime) ===
        entry[ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME].getTime() &&
      Number(header.endTime) ===
        entry[ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME].getTime() &&
      header.depositsRoot ===
        entry[ForeignTipReconciliationsDB.Columns.DEPOSITS_ROOT] &&
      header.forcedTransactionsRoot ===
        entry[ForeignTipReconciliationsDB.Columns.FORCED_TRANSACTIONS_ROOT] &&
      header.withdrawalsRoot ===
        entry[ForeignTipReconciliationsDB.Columns.WITHDRAWALS_ROOT] &&
      header.depositCount ===
        BigInt(entry[ForeignTipReconciliationsDB.Columns.DEPOSIT_COUNT]) &&
      header.forcedTransactionCount ===
        BigInt(
          entry[ForeignTipReconciliationsDB.Columns.FORCED_TRANSACTION_COUNT],
        ) &&
      header.withdrawalCount ===
        BigInt(entry[ForeignTipReconciliationsDB.Columns.WITHDRAWAL_COUNT]);
    if (!explicitEvidenceMatches) {
      return yield* Effect.fail(
        new DatabaseError({
          table: ForeignTipReconciliationsDB.tableName,
          message:
            "Retained foreign header does not match its immutable evidence columns",
          cause: foreignHeaderHash,
        }),
      );
    }
    return header;
  });
