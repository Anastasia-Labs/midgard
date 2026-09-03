import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  ConfirmedLedgerDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../../database/utils/common.js";
import * as Ledger from "../../database/utils/ledger.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../../mpf/index.js";
import { Database } from "../../services/index.js";

export type ConfirmedLedgerSnapshot = {
  readonly entries: readonly Ledger.Entry[];
  readonly baseRoot: string;
  readonly root: string;
  readonly deltaChain: readonly DecodedConfirmedLedgerDelta[];
  readonly delta: {
    readonly spent: readonly Buffer[];
    readonly produced: readonly Ledger.Entry[];
  };
};

export type DecodedConfirmedLedgerDelta = NonNullable<
  ConfirmedLedgerSnapshot["delta"]
>;

export const pendingUtxoMemberToConfirmedLedgerEntry = (
  member: PendingBlockFinalizationsDB.UtxoInput,
): Effect.Effect<Ledger.Entry, DatabaseError> =>
  Effect.try({
    try: () => {
      // Snapshot `outref` bytes are the §5.3 field-0/1 item form — 38 bytes,
      // `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16` — matching on-chain
      // `ledger_outref_key`, not CML's minimal-index `TransactionInput` CBOR.
      const input = decodeMidgardSpendInputItem(
        member[PendingBlockFinalizationsDB.UtxoColumns.OUTREF],
      );
      const output = decodeMidgardTxOutput(
        member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT],
      );
      return {
        [Ledger.Columns.TX_ID]: Buffer.from(input.txId),
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
): Effect.Effect<DecodedConfirmedLedgerDelta, DatabaseError> =>
  Effect.forEach(
    record.ledgerDelta.produced,
    pendingUtxoMemberToConfirmedLedgerEntry,
    { concurrency: 1 },
  ).pipe(
    Effect.map((produced) => ({
      spent: record.ledgerDelta.spent,
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
): Effect.Effect<readonly Ledger.Entry[], DatabaseError> =>
  Effect.try({
    try: () => {
      const byOutref = new Map(
        entries.map((entry) => [
          entry[Ledger.Columns.OUTREF].toString("hex"),
          entry,
        ]),
      );
      for (const outref of delta.spent) {
        const outrefHex = outref.toString("hex");
        if (!byOutref.delete(outrefHex)) {
          throw new Error(
            `ledger delta spends an outref absent from its authenticated base: ${outrefHex}`,
          );
        }
      }
      for (const entry of delta.produced) {
        const outrefHex = entry[Ledger.Columns.OUTREF].toString("hex");
        if (byOutref.has(outrefHex)) {
          throw new Error(
            `ledger delta substitutes an existing unspent outref: ${outrefHex}`,
          );
        }
        byOutref.set(outrefHex, entry);
      }
      return [...byOutref.values()];
    },
    catch: (cause) =>
      new DatabaseError({
        table: PendingBlockFinalizationsDB.tableName,
        message:
          "Pending-finalization ledger delta is invalid for its authenticated base",
        cause,
      }),
  });

const materializeFromBase = <R>({
  record,
  confirmedEntries,
  confirmedRoot,
  retrieveParent,
  seen,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly confirmedEntries: readonly Ledger.Entry[];
  readonly confirmedRoot: string;
  readonly retrieveParent: (
    headerHash: Buffer,
  ) => Effect.Effect<
    Option.Option<PendingBlockFinalizationsDB.Record>,
    DatabaseError,
    R
  >;
  readonly seen: ReadonlySet<string>;
}): Effect.Effect<ConfirmedLedgerSnapshot, DatabaseError, R> =>
  Effect.gen(function* () {
    const headerHashHex =
      record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
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

    const baseRoot =
      record[PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT];
    let baseEntries: readonly Ledger.Entry[];
    let recoveredBaseRoot: string;
    let baseDeltaChain: readonly DecodedConfirmedLedgerDelta[];
    if (confirmedRoot === baseRoot) {
      baseEntries = confirmedEntries;
      recoveredBaseRoot = confirmedRoot;
      baseDeltaChain = [];
    } else {
      const previousHeaderHash =
        record[PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH];
      const previous = yield* retrieveParent(previousHeaderHash);
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
      if (
        !previous.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH].equals(
          previousHeaderHash,
        )
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Pending-finalization ledger delta parent lookup returned a substituted journal",
            cause: `header_hash=${headerHashHex},requested_parent=${previousHeaderHash.toString("hex")},returned_parent=${previous.value[
              PendingBlockFinalizationsDB.Columns.HEADER_HASH
            ].toString("hex")}`,
          }),
        );
      }
      const baseSnapshot = yield* materializeFromBase({
        record: previous.value,
        confirmedEntries,
        confirmedRoot,
        retrieveParent,
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
      recoveredBaseRoot = baseSnapshot.baseRoot;
      baseDeltaChain = baseSnapshot.deltaChain;
    }

    const entries = yield* applyDeltaToEntries(baseEntries, delta);
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
    return {
      entries,
      baseRoot: recoveredBaseRoot,
      root,
      deltaChain: [...baseDeltaChain, delta],
      delta,
    };
  });

export const materializeConfirmedLedgerDeltaChain = <R>({
  record,
  confirmedEntries,
  retrieveParent,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly confirmedEntries: readonly Ledger.Entry[];
  readonly retrieveParent: (
    headerHash: Buffer,
  ) => Effect.Effect<
    Option.Option<PendingBlockFinalizationsDB.Record>,
    DatabaseError,
    R
  >;
}): Effect.Effect<ConfirmedLedgerSnapshot, DatabaseError, R> =>
  Effect.gen(function* () {
    const confirmedRoot = yield* computeRecoveredRoot(confirmedEntries);
    return yield* materializeFromBase({
      record,
      confirmedEntries,
      confirmedRoot,
      retrieveParent,
      seen: new Set(),
    });
  });

export const materializeConfirmedLedgerSnapshot = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<ConfirmedLedgerSnapshot, DatabaseError, Database> =>
  Effect.gen(function* () {
    const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
    return yield* materializeConfirmedLedgerDeltaChain({
      record,
      confirmedEntries,
      retrieveParent: PendingBlockFinalizationsDB.retrieveByHeaderHash,
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

export const applyConfirmedLedgerDeltaChainTransaction = (
  snapshot: ConfirmedLedgerSnapshot,
): Effect.Effect<readonly Ledger.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`LOCK TABLE ${sql(
          ConfirmedLedgerDB.tableName,
        )} IN EXCLUSIVE MODE`;
        const currentEntries = yield* ConfirmedLedgerDB.retrieve;
        const currentRoot = yield* computeRecoveredRoot(currentEntries);
        if (currentRoot !== snapshot.baseRoot) {
          return yield* Effect.fail(
            new DatabaseError({
              table: ConfirmedLedgerDB.tableName,
              message:
                "Confirmed-ledger delta-chain base no longer matches the persisted ledger",
              cause: `persisted_root=${currentRoot},authenticated_base_root=${snapshot.baseRoot}`,
            }),
          );
        }
        for (const delta of snapshot.deltaChain) {
          yield* applyConfirmedLedgerDelta(delta);
        }
        const recoveredEntries = yield* ConfirmedLedgerDB.retrieve;
        const recoveredRoot = yield* computeRecoveredRoot(recoveredEntries);
        if (recoveredRoot !== snapshot.root) {
          return yield* Effect.fail(
            new DatabaseError({
              table: ConfirmedLedgerDB.tableName,
              message:
                "Applied confirmed-ledger delta chain does not match its authenticated final root",
              cause: `recovered_root=${recoveredRoot},authenticated_root=${snapshot.root}`,
            }),
          );
        }
        return recoveredEntries;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      ConfirmedLedgerDB.tableName,
      "Failed to transactionally apply finalized confirmed-ledger delta chain",
    ),
  );
