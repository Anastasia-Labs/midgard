import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ConfirmedLedgerDB,
  PendingBlockFinalizationsDB,
} from "@/database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import { Database } from "@/services/index.js";
import { computeLedgerMpfRootFromLedgerEntries } from "@/workers/utils/mpf.js";

export type ConfirmedLedgerSnapshot = {
  readonly entries: readonly Ledger.Entry[];
  readonly root: string;
};

export const pendingUtxoMemberToConfirmedLedgerEntry = (
  member: PendingBlockFinalizationsDB.UtxoRecord,
): Effect.Effect<Ledger.Entry, DatabaseError> =>
  Effect.try({
    try: () => {
      const input = CML.TransactionInput.from_cbor_bytes(
        member[PendingBlockFinalizationsDB.UtxoColumns.OUTREF],
      );
      const output = decodeMidgardTxOutput(
        member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT],
      );
      return {
        [Ledger.Columns.TX_ID]: Buffer.from(
          input.transaction_id().to_hex(),
          "hex",
        ),
        [Ledger.Columns.OUTREF]: Buffer.from(
          member[PendingBlockFinalizationsDB.UtxoColumns.OUTREF],
        ),
        [Ledger.Columns.OUTPUT]: Buffer.from(
          member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT],
        ),
        [Ledger.Columns.ADDRESS]: encodeMidgardAddressText(output.address),
      };
    },
    catch: (error) =>
      new DatabaseError({
        table: PendingBlockFinalizationsDB.tableName,
        message:
          "Failed to decode pending-finalization UTxO snapshot member into a confirmed ledger entry",
        cause: formatUnknownError(error),
      }),
  });

export const materializeConfirmedLedgerSnapshot = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<ConfirmedLedgerSnapshot, DatabaseError> =>
  Effect.gen(function* () {
    const entries = yield* Effect.forEach(
      record.utxoMembers,
      pendingUtxoMemberToConfirmedLedgerEntry,
      { concurrency: "unbounded" },
    );
    const root = yield* computeLedgerMpfRootFromLedgerEntries(entries).pipe(
      Effect.mapError(
        (error) =>
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Failed to compute root for pending-finalization UTxO snapshot",
            cause: formatUnknownError(error),
          }),
      ),
    );
    return { entries, root };
  });

export const replaceConfirmedLedgerWithEntries = (
  entries: readonly Ledger.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const batchSize = 100;
    yield* ConfirmedLedgerDB.clear;
    for (let i = 0; i < entries.length; i += batchSize) {
      yield* ConfirmedLedgerDB.insertMultiple(
        entries.slice(i, i + batchSize),
      ).pipe(Effect.withSpan(`confirmed-ledger-snapshot-insert-${i}`));
    }
  }).pipe(
    sqlErrorToDatabaseError(
      ConfirmedLedgerDB.tableName,
      "Failed to replace confirmed ledger with finalized UTxO snapshot",
    ),
  );

export const replaceConfirmedLedgerWithEntriesTransaction = (
  entries: readonly Ledger.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(replaceConfirmedLedgerWithEntries(entries));
  }).pipe(
    sqlErrorToDatabaseError(
      ConfirmedLedgerDB.tableName,
      "Failed to transactionally replace confirmed ledger with finalized UTxO snapshot",
    ),
  );
