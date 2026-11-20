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
  readonly delta?: {
    readonly spent: readonly Buffer[];
    readonly produced: readonly Ledger.Entry[];
  };
};

export type DecodedConfirmedLedgerDelta = NonNullable<
  ConfirmedLedgerSnapshot["delta"]
>;

export const pendingUtxoMemberToConfirmedLedgerEntry = (
  member:
    | PendingBlockFinalizationsDB.UtxoRecord
    | PendingBlockFinalizationsDB.UtxoInput,
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

export const decodeConfirmedLedgerDelta = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<DecodedConfirmedLedgerDelta | undefined, DatabaseError> =>
  record.ledgerDelta === undefined
    ? Effect.succeed(undefined)
    : Effect.forEach(
        record.ledgerDelta.produced,
        pendingUtxoMemberToConfirmedLedgerEntry,
        { concurrency: "unbounded" },
      ).pipe(
        Effect.map((produced) => ({
          spent: record.ledgerDelta!.spent,
          produced,
        })),
      );

const computeRecoveredRoot = (entries: readonly Ledger.Entry[]) =>
  computeLedgerMpfRootFromLedgerEntries(entries).pipe(
    Effect.mapError(
      (error) =>
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: "Failed to compute recovered pending-finalization root",
          cause: formatUnknownError(error),
        }),
    ),
  );

const applyDeltaToEntries = (
  entries: readonly Ledger.Entry[],
  delta: DecodedConfirmedLedgerDelta,
): readonly Ledger.Entry[] => {
  const byOutref = new Map(
    entries.map((entry) => [
      entry[Ledger.Columns.OUTREF].toString("hex"),
      entry,
    ]),
  );
  for (const outref of delta.spent) byOutref.delete(outref.toString("hex"));
  for (const entry of delta.produced) {
    byOutref.set(entry[Ledger.Columns.OUTREF].toString("hex"), entry);
  }
  return [...byOutref.values()];
};

const materializeFromBase = ({
  record,
  confirmedEntries,
  confirmedRoot,
  seen,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly confirmedEntries: readonly Ledger.Entry[];
  readonly confirmedRoot: string;
  readonly seen: ReadonlySet<string>;
}): Effect.Effect<ConfirmedLedgerSnapshot, DatabaseError, Database> =>
  Effect.gen(function* () {
    const headerHashHex = record[
      PendingBlockFinalizationsDB.Columns.HEADER_HASH
    ].toString("hex");
    if (seen.has(headerHashHex)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message: "Pending-finalization ledger delta chain contains a cycle",
          cause: `header_hash=${headerHashHex}`,
        }),
      );
    }
    const nextSeen = new Set(seen).add(headerHashHex);
    const delta = yield* decodeConfirmedLedgerDelta(record);
    if (delta === undefined) {
      const entries = yield* Effect.forEach(
        record.utxoMembers,
        pendingUtxoMemberToConfirmedLedgerEntry,
        { concurrency: "unbounded" },
      );
      const root = yield* computeRecoveredRoot(entries);
      return { entries, root };
    }

    const baseRoot =
      record[PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT];
    let baseEntries: readonly Ledger.Entry[];
    if (confirmedRoot === baseRoot) {
      baseEntries = confirmedEntries;
    } else {
      const previousHeaderHash =
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH];
      const previous =
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
          previousHeaderHash,
        );
      if (previous._tag === "None") {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Pending-finalization ledger delta base is not confirmed and its parent journal is missing",
            cause: `header_hash=${headerHashHex},base_root=${baseRoot},base_header_hash=${previousHeaderHash.toString("hex")}`,
          }),
        );
      }
      const baseSnapshot = yield* materializeFromBase({
        record: previous.value,
        confirmedEntries,
        confirmedRoot,
        seen: nextSeen,
      });
      if (baseSnapshot.root !== baseRoot) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Pending-finalization ledger delta parent root does not match the child base root",
            cause: `header_hash=${headerHashHex},parent_root=${baseSnapshot.root},base_root=${baseRoot}`,
          }),
        );
      }
      baseEntries = baseSnapshot.entries;
    }

    const entries = applyDeltaToEntries(baseEntries, delta);
    const root = yield* computeRecoveredRoot(entries);
    const expectedRoot =
      record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT];
    if (root !== expectedRoot) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Recovered pending-finalization ledger delta does not match its expected root",
          cause: `header_hash=${headerHashHex},recovered_root=${root},expected_root=${expectedRoot}`,
        }),
      );
    }
    return { entries, root, delta };
  });

export const materializeConfirmedLedgerSnapshot = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<ConfirmedLedgerSnapshot, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (record.ledgerDelta === undefined) {
      const entries = yield* Effect.forEach(
        record.utxoMembers,
        pendingUtxoMemberToConfirmedLedgerEntry,
        { concurrency: "unbounded" },
      );
      return { entries, root: yield* computeRecoveredRoot(entries) };
    }
    const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
    const confirmedRoot = yield* computeRecoveredRoot(confirmedEntries);
    return yield* materializeFromBase({
      record,
      confirmedEntries,
      confirmedRoot,
      seen: new Set(),
    });
  });

export const applyConfirmedLedgerDelta = ({
  spent,
  produced,
}: {
  readonly spent: readonly Buffer[];
  readonly produced: readonly Ledger.Entry[];
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    if (spent.length > 0) {
      yield* sql`DELETE FROM ${sql(ConfirmedLedgerDB.tableName)}
        WHERE ${sql(Ledger.Columns.OUTREF)} IN ${sql.in(spent)}`;
    }
    if (produced.length > 0) {
      yield* sql`INSERT INTO ${sql(ConfirmedLedgerDB.tableName)} ${sql.insert(
        produced,
      )}
      ON CONFLICT (${sql(Ledger.Columns.OUTREF)}) DO UPDATE SET
        ${sql(Ledger.Columns.TX_ID)} = EXCLUDED.${sql(Ledger.Columns.TX_ID)},
        ${sql(Ledger.Columns.OUTPUT)} = EXCLUDED.${sql(Ledger.Columns.OUTPUT)},
        ${sql(Ledger.Columns.ADDRESS)} = EXCLUDED.${sql(Ledger.Columns.ADDRESS)}`;
    }
  }).pipe(
    sqlErrorToDatabaseError(
      ConfirmedLedgerDB.tableName,
      "Failed to apply finalized confirmed-ledger delta",
    ),
  );

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
