import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as ProjectedEvents from "@/database/utils/projected-events.js";
import { Database } from "@/services/database.js";

export const tableName = "forced_transaction_utxos";

export enum Columns {
  TX_ORDER_ID = "tx_order_id",
  TX_ORDER_L1_TX_HASH = "tx_order_l1_tx_hash",
  TX_ORDER_L1_OUTPUT_INDEX = "tx_order_l1_output_index",
  ASSET_NAME = "asset_name",
  RAW_DATUM = "raw_datum",
  TX_ID = "tx_id",
  TX_COMPACT = "tx_compact",
  FORCED_INCLUSION_VALUE = "forced_inclusion_value",
  OPERATOR_VALIDITY = "operator_validity",
  INCLUSION_TIME = "inclusion_time",
  PROJECTED_HEADER_HASH = "projected_header_hash",
  STATUS = "status",
}

export const Status = {
  Awaiting: "awaiting",
  Projected: "projected",
  Finalized: "finalized",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export type Entry = {
  [Columns.TX_ORDER_ID]: Buffer;
  [Columns.TX_ORDER_L1_TX_HASH]: Buffer;
  [Columns.TX_ORDER_L1_OUTPUT_INDEX]: number;
  [Columns.ASSET_NAME]: Buffer;
  [Columns.RAW_DATUM]: Buffer;
  [Columns.TX_ID]: Buffer;
  [Columns.TX_COMPACT]: Buffer;
  [Columns.FORCED_INCLUSION_VALUE]: Buffer;
  [Columns.OPERATOR_VALIDITY]: SDK.MidgardTxValidity;
  [Columns.INCLUSION_TIME]: Date;
  [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
  [Columns.STATUS]: Status;
};

export type ForcedInclusionValueInput = {
  readonly txCompact: SDK.MidgardTxCompact;
  readonly operatorValidity: SDK.MidgardTxValidity;
};

const projectedEventsTable = {
  tableName,
  idColumn: Columns.TX_ORDER_ID,
  inclusionTimeColumn: Columns.INCLUSION_TIME,
  projectedHeaderHashColumn: Columns.PROJECTED_HEADER_HASH,
  statusColumn: Columns.STATUS,
  awaitingStatus: Status.Awaiting,
  projectedStatus: Status.Projected,
  terminalStatus: Status.Finalized,
  entitySingular: "forced transaction",
  entityPlural: "forced transactions",
  idLabel: "tx_order_id",
  touchUpdatedAt: true,
} as const satisfies ProjectedEvents.ProjectedEventTable;

const sameImmutablePayload = (left: Entry, right: Entry): boolean =>
  left[Columns.TX_ORDER_ID].equals(right[Columns.TX_ORDER_ID]) &&
  left[Columns.TX_ORDER_L1_TX_HASH].equals(
    right[Columns.TX_ORDER_L1_TX_HASH],
  ) &&
  left[Columns.TX_ORDER_L1_OUTPUT_INDEX] ===
    right[Columns.TX_ORDER_L1_OUTPUT_INDEX] &&
  left[Columns.ASSET_NAME].equals(right[Columns.ASSET_NAME]) &&
  left[Columns.RAW_DATUM].equals(right[Columns.RAW_DATUM]) &&
  left[Columns.TX_ID].equals(right[Columns.TX_ID]) &&
  left[Columns.TX_COMPACT].equals(right[Columns.TX_COMPACT]) &&
  left[Columns.FORCED_INCLUSION_VALUE].equals(
    right[Columns.FORCED_INCLUSION_VALUE],
  ) &&
  left[Columns.OPERATOR_VALIDITY] === right[Columns.OPERATOR_VALIDITY] &&
  left[Columns.INCLUSION_TIME].getTime() ===
    right[Columns.INCLUSION_TIME].getTime();

export const txIdFromTxCompact = (
  txCompact: SDK.MidgardTxCompact,
): Effect.Effect<Buffer, DatabaseError> =>
  Effect.gen(function* () {
    const bodyCbor = yield* Effect.try({
      try: () =>
        aikenSerialisedPlutusDataCbor(
          LucidData.to(txCompact.body, SDK.MidgardTxBodyCompact),
        ),
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Failed to encode forced transaction compact body",
          cause,
        }),
    });
    const txIdHex = yield* SDK.hashHexWithBlake2b(bodyCbor, 32).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: tableName,
            message: "Failed to hash forced transaction compact body",
            cause,
          }),
      ),
    );
    return Buffer.from(txIdHex, "hex");
  });

export const encodeForcedInclusionValue = ({
  txCompact,
  operatorValidity,
}: ForcedInclusionValueInput): Effect.Effect<
  { readonly txId: Buffer; readonly value: Buffer },
  DatabaseError
> =>
  Effect.gen(function* () {
    const txId = yield* txIdFromTxCompact(txCompact);
    const forcedInclusionTx: SDK.ForcedInclusionTx = {
      tx_compact: {
        body: txCompact.body,
        wits: txCompact.wits,
      },
      operator_validity: operatorValidity,
    };
    const value = yield* Effect.try({
      try: () =>
        Buffer.from(
          aikenSerialisedPlutusDataCbor(
            LucidData.to(forcedInclusionTx, SDK.ForcedInclusionTx),
          ),
          "hex",
        ),
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Failed to encode forced transaction source value",
          cause,
        }),
    });
    return { txId, value };
  });

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.TX_ORDER_ID)} BYTEA PRIMARY KEY,
          ${sql(Columns.TX_ORDER_L1_TX_HASH)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.TX_ORDER_L1_TX_HASH)}) = 32),
          ${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)} INTEGER NOT NULL CHECK (${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)} >= 0),
          ${sql(Columns.ASSET_NAME)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.ASSET_NAME)}) BETWEEN 1 AND 32),
          ${sql(Columns.RAW_DATUM)} BYTEA NOT NULL,
          ${sql(Columns.TX_ID)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.TX_ID)}) = 32),
          ${sql(Columns.TX_COMPACT)} BYTEA NOT NULL,
          ${sql(Columns.FORCED_INCLUSION_VALUE)} BYTEA NOT NULL,
          ${sql(Columns.OPERATOR_VALIDITY)} TEXT NOT NULL CHECK (${sql(Columns.OPERATOR_VALIDITY)} IN (
            'TxIsValid',
            'NonExistentInputUtxo',
            'InvalidSignature',
            'FailedScript',
            'FeeTooLow',
            'UnbalancedTx'
          )),
          ${sql(Columns.INCLUSION_TIME)} TIMESTAMPTZ NOT NULL,
          ${sql(Columns.PROJECTED_HEADER_HASH)} BYTEA,
          ${sql(Columns.STATUS)} TEXT NOT NULL CHECK (${sql(Columns.STATUS)} IN ('awaiting', 'projected', 'finalized')),
          created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          UNIQUE (${sql(Columns.TX_ORDER_L1_TX_HASH)}, ${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)}),
          CHECK (${sql(Columns.STATUS)} <> 'awaiting' OR ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL)
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.STATUS}_${Columns.INCLUSION_TIME}_${Columns.TX_ORDER_ID}`,
        )} ON ${sql(tableName)} (
          ${sql(Columns.STATUS)},
          ${sql(Columns.INCLUSION_TIME)},
          ${sql(Columns.TX_ORDER_ID)}
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.PROJECTED_HEADER_HASH}`,
        )} ON ${sql(tableName)} (${sql(Columns.PROJECTED_HEADER_HASH)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.TX_ID}`,
        )} ON ${sql(tableName)} (${sql(Columns.TX_ID)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to create forced transactions table",
    ),
  );

export const insertEntries = (
  entries: readonly Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return;
    }
    const incomingById = new Map<string, Entry>();
    for (const incoming of entries) {
      const key = incoming[Columns.TX_ORDER_ID].toString("hex");
      const existingIncoming = incomingById.get(key);
      if (
        existingIncoming !== undefined &&
        !sameImmutablePayload(existingIncoming, incoming)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Refusing to insert forced transactions because the same tx_order_id appears with conflicting payloads in one batch",
            cause: `tx_order_id=${key}`,
          }),
        );
      }
      incomingById.set(key, incoming);
    }

    const sql = yield* SqlClient.SqlClient;
    const normalizedEntries = [...incomingById.values()];
    const rows = yield* sql<{ [Columns.TX_ORDER_ID]: Buffer }>`
      INSERT INTO ${sql(tableName)} ${sql.insert(normalizedEntries)}
      ON CONFLICT (${sql(Columns.TX_ORDER_ID)}) DO UPDATE SET
        ${sql(Columns.TX_ORDER_ID)} = ${sql(tableName)}.${sql(Columns.TX_ORDER_ID)}
      WHERE ${sql(tableName)}.${sql(Columns.TX_ORDER_L1_TX_HASH)} = EXCLUDED.${sql(Columns.TX_ORDER_L1_TX_HASH)}
        AND ${sql(tableName)}.${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)} = EXCLUDED.${sql(Columns.TX_ORDER_L1_OUTPUT_INDEX)}
        AND ${sql(tableName)}.${sql(Columns.ASSET_NAME)} = EXCLUDED.${sql(Columns.ASSET_NAME)}
        AND ${sql(tableName)}.${sql(Columns.RAW_DATUM)} = EXCLUDED.${sql(Columns.RAW_DATUM)}
        AND ${sql(tableName)}.${sql(Columns.TX_ID)} = EXCLUDED.${sql(Columns.TX_ID)}
        AND ${sql(tableName)}.${sql(Columns.TX_COMPACT)} = EXCLUDED.${sql(Columns.TX_COMPACT)}
        AND ${sql(tableName)}.${sql(Columns.FORCED_INCLUSION_VALUE)} = EXCLUDED.${sql(Columns.FORCED_INCLUSION_VALUE)}
        AND ${sql(tableName)}.${sql(Columns.OPERATOR_VALIDITY)} = EXCLUDED.${sql(Columns.OPERATOR_VALIDITY)}
        AND ${sql(tableName)}.${sql(Columns.INCLUSION_TIME)} = EXCLUDED.${sql(Columns.INCLUSION_TIME)}
      RETURNING ${sql(Columns.TX_ORDER_ID)}
    `;
    if (rows.length !== normalizedEntries.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to upsert forced transaction because the same tx_order_id has conflicting persisted payload",
          cause: `requested=${normalizedEntries.length},upserted=${rows.length}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert forced transaction UTxOs",
    ),
  );

export const retrieveAllEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.TX_ORDER_ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveAllEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve forced transaction UTxOs",
    ),
  );

export const retrieveByTxOrderId = (
  txOrderId: Buffer,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.TX_ORDER_ID)} = ${txOrderId}
      LIMIT 1`;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    Effect.withLogSpan(`retrieveByTxOrderId ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve forced transaction by tx_order_id",
    ),
  );

export const retrievePendingHeaderEntriesUpTo = (
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  ProjectedEvents.retrievePendingHeaderEntriesUpTo<Entry>(
    projectedEventsTable,
    endTime,
    [Status.Awaiting, Status.Projected],
  ).pipe(
    Effect.withLogSpan(`retrievePendingHeaderEntriesUpTo ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve forced transactions pending header assignment",
    ),
  );

export const retrieveByProjectedHeaderHash = (
  projectedHeaderHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  ProjectedEvents.retrieveByProjectedHeaderHash<Entry>(
    projectedEventsTable,
    projectedHeaderHash,
  ).pipe(
    Effect.withLogSpan(`retrieveByProjectedHeaderHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve forced transactions by projected header hash",
    ),
  );

export const markAwaitingAsProjected = (
  ids: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  ProjectedEvents.markAwaitingAsProjected(projectedEventsTable, ids).pipe(
    Effect.withLogSpan(`markAwaitingAsProjected ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark awaiting forced transactions as projected",
    ),
  );

export const markProjectedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  ProjectedEvents.markProjectedByEventIds(
    projectedEventsTable,
    ids,
    projectedHeaderHash,
  ).pipe(
    Effect.withLogSpan(`markProjectedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark forced transactions as assigned to the given header",
    ),
  );

export const clearProjectedHeaderAssignmentByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  ProjectedEvents.clearProjectedHeaderAssignmentByEventIds(
    projectedEventsTable,
    ids,
    projectedHeaderHash,
  ).pipe(
    Effect.withLogSpan(`clearProjectedHeaderAssignmentByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to clear projected header assignments for forced transactions",
    ),
  );

export const markFinalizedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      [Columns.TX_ORDER_ID]: Buffer;
    }>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash},
          updated_at = NOW()
      WHERE ${sql(Columns.TX_ORDER_ID)} IN ${sql.in(ids)}
        AND ${sql(Columns.STATUS)} IN (${Status.Projected}, ${Status.Finalized})
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}
      RETURNING ${sql(Columns.TX_ORDER_ID)}`;
    if (rows.length !== ids.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to finalize forced transactions because at least one row is missing, unprojected, or assigned to a different header",
          cause: `requested=${ids.length},finalized=${rows.length},header_hash=${projectedHeaderHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markFinalizedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark forced transactions finalized",
    ),
  );

export const toRootKeyValue = (
  entry: Entry,
): { readonly key: Buffer; readonly value: Buffer } => ({
  key: Buffer.from(entry[Columns.TX_ORDER_ID]),
  value: Buffer.from(entry[Columns.FORCED_INCLUSION_VALUE]),
});

export const clear = clearTable(tableName);
