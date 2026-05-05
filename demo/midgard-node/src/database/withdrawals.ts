import { Database } from "@/services/database.js";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { SqlClient } from "@effect/sql";
import * as UserEvents from "@/database/utils/user-events.js";

export const tableName = "withdrawal_utxos";

export enum Columns {
  ID = UserEvents.Columns.ID,
  RAW_EVENT_INFO = "raw_event_info",
  SETTLEMENT_EVENT_INFO = "settlement_event_info",
  INCLUSION_TIME = UserEvents.Columns.INCLUSION_TIME,
  WITHDRAWAL_L1_TX_HASH = "withdrawal_l1_tx_hash",
  WITHDRAWAL_L1_OUTPUT_INDEX = "withdrawal_l1_output_index",
  ASSET_NAME = "asset_name",
  L2_OUTREF = "l2_outref",
  L2_OWNER = "l2_owner",
  L2_VALUE = "l2_value",
  L1_ADDRESS = "l1_address",
  L1_DATUM = "l1_datum",
  REFUND_ADDRESS = "refund_address",
  REFUND_DATUM = "refund_datum",
  VALIDITY = "validity",
  VALIDITY_DETAIL = "validity_detail",
  PROJECTED_HEADER_HASH = "projected_header_hash",
  STATUS = "status",
}

export const Status = {
  Awaiting: "awaiting",
  Projected: "projected",
  Finalized: "finalized",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export const Validity = {
  WithdrawalIsValid: "WithdrawalIsValid",
  NonExistentWithdrawalUtxo: "NonExistentWithdrawalUtxo",
  SpentWithdrawalUtxo: "SpentWithdrawalUtxo",
  IncorrectWithdrawalOwner: "IncorrectWithdrawalOwner",
  IncorrectWithdrawalValue: "IncorrectWithdrawalValue",
  IncorrectWithdrawalSignature: "IncorrectWithdrawalSignature",
  TooManyTokensInWithdrawal: "TooManyTokensInWithdrawal",
  UnpayableWithdrawalValue: "UnpayableWithdrawalValue",
} as const;

export type Validity = (typeof Validity)[keyof typeof Validity];

export type Entry = {
  [Columns.ID]: Buffer;
  [Columns.RAW_EVENT_INFO]: Buffer;
  [Columns.SETTLEMENT_EVENT_INFO]: Buffer | null;
  [Columns.INCLUSION_TIME]: Date;
  [Columns.WITHDRAWAL_L1_TX_HASH]: Buffer;
  [Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: number;
  [Columns.ASSET_NAME]: Buffer;
  [Columns.L2_OUTREF]: Buffer;
  [Columns.L2_OWNER]: Buffer;
  [Columns.L2_VALUE]: Buffer;
  [Columns.L1_ADDRESS]: Buffer;
  [Columns.L1_DATUM]: Buffer;
  [Columns.REFUND_ADDRESS]: Buffer;
  [Columns.REFUND_DATUM]: Buffer;
  [Columns.VALIDITY]: Validity | null;
  [Columns.VALIDITY_DETAIL]: unknown;
  [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
  [Columns.STATUS]: Status;
};

export type SettlementInfoAssignment = {
  readonly eventId: Buffer;
  readonly settlementEventInfo: Buffer;
  readonly validity: Validity;
  readonly validityDetail?: unknown;
};

const validityDetailEquals = (left: unknown, right: unknown): boolean =>
  JSON.stringify(left ?? {}) === JSON.stringify(right ?? {});

const sameImmutablePayload = (left: Entry, right: Entry): boolean =>
  left[Columns.ID].equals(right[Columns.ID]) &&
  left[Columns.RAW_EVENT_INFO].equals(right[Columns.RAW_EVENT_INFO]) &&
  left[Columns.INCLUSION_TIME].getTime() ===
    right[Columns.INCLUSION_TIME].getTime() &&
  left[Columns.WITHDRAWAL_L1_TX_HASH].equals(
    right[Columns.WITHDRAWAL_L1_TX_HASH],
  ) &&
  left[Columns.WITHDRAWAL_L1_OUTPUT_INDEX] ===
    right[Columns.WITHDRAWAL_L1_OUTPUT_INDEX] &&
  left[Columns.ASSET_NAME].equals(right[Columns.ASSET_NAME]) &&
  left[Columns.L2_OUTREF].equals(right[Columns.L2_OUTREF]) &&
  left[Columns.L2_OWNER].equals(right[Columns.L2_OWNER]) &&
  left[Columns.L2_VALUE].equals(right[Columns.L2_VALUE]) &&
  left[Columns.L1_ADDRESS].equals(right[Columns.L1_ADDRESS]) &&
  left[Columns.L1_DATUM].equals(right[Columns.L1_DATUM]) &&
  left[Columns.REFUND_ADDRESS].equals(right[Columns.REFUND_ADDRESS]) &&
  left[Columns.REFUND_DATUM].equals(right[Columns.REFUND_DATUM]);

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.ID)} BYTEA PRIMARY KEY,
          ${sql(Columns.RAW_EVENT_INFO)} BYTEA NOT NULL,
          ${sql(Columns.SETTLEMENT_EVENT_INFO)} BYTEA,
          ${sql(Columns.INCLUSION_TIME)} TIMESTAMPTZ NOT NULL,
          ${sql(Columns.WITHDRAWAL_L1_TX_HASH)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.WITHDRAWAL_L1_TX_HASH)}) = 32),
          ${sql(Columns.WITHDRAWAL_L1_OUTPUT_INDEX)} INTEGER NOT NULL CHECK (${sql(Columns.WITHDRAWAL_L1_OUTPUT_INDEX)} >= 0),
          ${sql(Columns.ASSET_NAME)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.ASSET_NAME)}) BETWEEN 1 AND 32),
          ${sql(Columns.L2_OUTREF)} BYTEA NOT NULL,
          ${sql(Columns.L2_OWNER)} BYTEA NOT NULL CHECK (octet_length(${sql(Columns.L2_OWNER)}) = 28),
          ${sql(Columns.L2_VALUE)} BYTEA NOT NULL,
          ${sql(Columns.L1_ADDRESS)} BYTEA NOT NULL,
          ${sql(Columns.L1_DATUM)} BYTEA NOT NULL,
          ${sql(Columns.REFUND_ADDRESS)} BYTEA NOT NULL,
          ${sql(Columns.REFUND_DATUM)} BYTEA NOT NULL,
          ${sql(Columns.VALIDITY)} TEXT,
          ${sql(Columns.VALIDITY_DETAIL)} JSONB NOT NULL DEFAULT '{}'::jsonb,
          ${sql(Columns.PROJECTED_HEADER_HASH)} BYTEA,
          ${sql(Columns.STATUS)} TEXT NOT NULL,
          created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          UNIQUE (${sql(Columns.WITHDRAWAL_L1_TX_HASH)}, ${sql(Columns.WITHDRAWAL_L1_OUTPUT_INDEX)}),
          CHECK (${sql(Columns.STATUS)} IN ('awaiting', 'projected', 'finalized')),
          CHECK (${sql(Columns.VALIDITY)} IS NULL OR ${sql(Columns.VALIDITY)} IN (
            'WithdrawalIsValid',
            'NonExistentWithdrawalUtxo',
            'SpentWithdrawalUtxo',
            'IncorrectWithdrawalOwner',
            'IncorrectWithdrawalValue',
            'IncorrectWithdrawalSignature',
            'TooManyTokensInWithdrawal',
            'UnpayableWithdrawalValue'
          )),
          CHECK (${sql(Columns.STATUS)} = 'awaiting' OR ${sql(Columns.SETTLEMENT_EVENT_INFO)} IS NOT NULL),
          CHECK (${sql(Columns.STATUS)} = 'awaiting' OR ${sql(Columns.VALIDITY)} IS NOT NULL),
          CHECK (${sql(Columns.STATUS)} <> 'awaiting' OR ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL)
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.STATUS}_${Columns.INCLUSION_TIME}_${Columns.ID}`,
        )} ON ${sql(tableName)} (
          ${sql(Columns.STATUS)},
          ${sql(Columns.INCLUSION_TIME)},
          ${sql(Columns.ID)}
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.PROJECTED_HEADER_HASH}`,
        )} ON ${sql(tableName)} (${sql(Columns.PROJECTED_HEADER_HASH)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.WITHDRAWAL_L1_TX_HASH}`,
        )} ON ${sql(tableName)} (${sql(Columns.WITHDRAWAL_L1_TX_HASH)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.L2_OUTREF}`,
        )} ON ${sql(tableName)} (${sql(Columns.L2_OUTREF)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create withdrawal table"),
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
      const key = incoming[Columns.ID].toString("hex");
      const existingIncoming = incomingById.get(key);
      if (
        existingIncoming !== undefined &&
        !sameImmutablePayload(existingIncoming, incoming)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Refusing to insert withdrawals because the same event_id appears with conflicting payloads in one batch",
            cause: `event_id=${key}`,
          }),
        );
      }
      incomingById.set(key, incoming);
    }

    const normalizedEntries = [...incomingById.values()].map((entry) => ({
      ...entry,
      [Columns.VALIDITY_DETAIL]: JSON.stringify(
        entry[Columns.VALIDITY_DETAIL] ?? {},
      ),
    }));

    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ [Columns.ID]: Buffer }>`
      INSERT INTO ${sql(tableName)} ${sql.insert(normalizedEntries)}
      ON CONFLICT (${sql(Columns.ID)}) DO UPDATE SET
        ${sql(Columns.ID)} = ${sql(tableName)}.${sql(Columns.ID)}
      WHERE ${sql(tableName)}.${sql(Columns.RAW_EVENT_INFO)} = EXCLUDED.${sql(
        Columns.RAW_EVENT_INFO,
      )}
        AND ${sql(tableName)}.${sql(Columns.INCLUSION_TIME)} = EXCLUDED.${sql(
          Columns.INCLUSION_TIME,
        )}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWAL_L1_TX_HASH)} = EXCLUDED.${sql(
          Columns.WITHDRAWAL_L1_TX_HASH,
        )}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWAL_L1_OUTPUT_INDEX)} = EXCLUDED.${sql(
          Columns.WITHDRAWAL_L1_OUTPUT_INDEX,
        )}
        AND ${sql(tableName)}.${sql(Columns.ASSET_NAME)} = EXCLUDED.${sql(
          Columns.ASSET_NAME,
        )}
        AND ${sql(tableName)}.${sql(Columns.L2_OUTREF)} = EXCLUDED.${sql(
          Columns.L2_OUTREF,
        )}
        AND ${sql(tableName)}.${sql(Columns.L2_OWNER)} = EXCLUDED.${sql(
          Columns.L2_OWNER,
        )}
        AND ${sql(tableName)}.${sql(Columns.L2_VALUE)} = EXCLUDED.${sql(
          Columns.L2_VALUE,
        )}
        AND ${sql(tableName)}.${sql(Columns.L1_ADDRESS)} = EXCLUDED.${sql(
          Columns.L1_ADDRESS,
        )}
        AND ${sql(tableName)}.${sql(Columns.L1_DATUM)} = EXCLUDED.${sql(
          Columns.L1_DATUM,
        )}
        AND ${sql(tableName)}.${sql(Columns.REFUND_ADDRESS)} = EXCLUDED.${sql(
          Columns.REFUND_ADDRESS,
        )}
        AND ${sql(tableName)}.${sql(Columns.REFUND_DATUM)} = EXCLUDED.${sql(
          Columns.REFUND_DATUM,
        )}
      RETURNING ${sql(Columns.ID)}
    `;
    if (rows.length !== normalizedEntries.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to upsert withdrawal because the same event_id has conflicting persisted payload",
          cause: `requested=${normalizedEntries.length},upserted=${rows.length}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to insert withdrawal UTxOs"),
  );

export const insertEntry = (
  entry: Entry,
): Effect.Effect<void, DatabaseError, Database> => insertEntries([entry]);

export const retrieveAllEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve withdrawals"),
  );

export const retrieveByEventId = (
  eventId: Buffer,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} = ${eventId}
      LIMIT 1`;
    if (rows.length <= 0) {
      return Option.none();
    }
    return Option.some(rows[0]!);
  }).pipe(
    Effect.withLogSpan(`retrieveByEventId ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawal by event id",
    ),
  );

export const retrieveByEventIds = (
  eventIds: readonly Buffer[],
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (eventIds.length <= 0) {
      return [];
    }
    const uniqueIds = Array.from(
      new Set(eventIds.map((eventId) => eventId.toString("hex"))),
    ).map((hex) => Buffer.from(hex, "hex"));
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawals by event ids",
    ),
  );

export const retrieveByCardanoTxHash = (
  cardanoTxHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.WITHDRAWAL_L1_TX_HASH)} = ${cardanoTxHash}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveByCardanoTxHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawals by Cardano tx hash",
    ),
  );

export const retrieveByProjectedHeaderHash = (
  projectedHeaderHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveByProjectedHeaderHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawals by projected header hash",
    ),
  );

export const retrieveAwaitingEntriesDueBy = (
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${Status.Awaiting}
        AND ${sql(Columns.INCLUSION_TIME)} <= ${endTime}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveAwaitingEntriesDueBy ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve awaiting withdrawals due by the requested time",
    ),
  );

export const retrievePendingHeaderEntriesUpTo = (
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} IN (${Status.Awaiting}, ${Status.Projected})
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL
        AND ${sql(Columns.INCLUSION_TIME)} <= ${endTime}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrievePendingHeaderEntriesUpTo ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawals pending header assignment",
    ),
  );

export const retrieveProjectedPendingHeaderEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${Status.Projected}
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveProjectedPendingHeaderEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve projected withdrawals awaiting header assignment",
    ),
  );

export const markAwaitingAsProjected = (
  ids: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const uniqueIds = Array.from(
      new Set(ids.map((id) => id.toString("hex"))),
    ).map((hex) => Buffer.from(hex, "hex"));
    const rows = yield* sql<Entry>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Projected},
          updated_at = NOW()
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}
        AND ${sql(Columns.STATUS)} = ${Status.Awaiting}
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL
      RETURNING *`;
    if (rows.length === uniqueIds.length) {
      return;
    }

    const currentRows = yield* sql<{
      [Columns.ID]: Buffer;
      [Columns.STATUS]: Status;
      [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
    }>`SELECT ${sql(Columns.ID)}, ${sql(Columns.STATUS)}, ${sql(
      Columns.PROJECTED_HEADER_HASH,
    )} FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}`;

    if (currentRows.length !== uniqueIds.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to mark awaiting withdrawals as projected because at least one row no longer exists",
          cause: `requested=${uniqueIds.length},found=${currentRows.length}`,
        }),
      );
    }

    for (const row of currentRows) {
      const status = row[Columns.STATUS];
      const projectedHeaderHash = row[Columns.PROJECTED_HEADER_HASH];
      if (status !== Status.Projected && status !== Status.Finalized) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to mark awaiting withdrawals as projected because at least one row is still not in a projected lifecycle state",
            cause: `event_id=${row[Columns.ID].toString("hex")},status=${status}`,
          }),
        );
      }
      if (projectedHeaderHash !== null) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to mark awaiting withdrawals as projected because at least one row is already assigned to a header",
            cause: `event_id=${row[Columns.ID].toString("hex")},projected_header_hash=${projectedHeaderHash.toString(
              "hex",
            )}`,
          }),
        );
      }
    }
  }).pipe(
    Effect.withLogSpan(`markAwaitingAsProjected ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark awaiting withdrawals as projected",
    ),
  );

export const setSettlementInfoForEventIds = (
  assignments: readonly SettlementInfoAssignment[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (assignments.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.forEach(
        assignments,
        (assignment) =>
          Effect.gen(function* () {
            const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
              WHERE ${sql(Columns.ID)} = ${assignment.eventId}
              LIMIT 1`;
            const current = rows[0];
            if (current === undefined) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row does not exist",
                  cause: `event_id=${assignment.eventId.toString("hex")}`,
                }),
              );
            }
            const existingInfo = current[Columns.SETTLEMENT_EVENT_INFO];
            if (
              existingInfo !== null &&
              !existingInfo.equals(assignment.settlementEventInfo)
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row already has conflicting settlement info",
                  cause: `event_id=${assignment.eventId.toString("hex")}`,
                }),
              );
            }
            const existingValidity = current[Columns.VALIDITY];
            if (
              existingValidity !== null &&
              existingValidity !== assignment.validity
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row already has conflicting validity",
                  cause: `event_id=${assignment.eventId.toString(
                    "hex",
                  )},existing=${existingValidity},requested=${assignment.validity}`,
                }),
              );
            }
            if (
              existingInfo !== null &&
              existingValidity !== null &&
              !validityDetailEquals(
                current[Columns.VALIDITY_DETAIL],
                assignment.validityDetail ?? {},
              )
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row already has conflicting validity detail",
                  cause: `event_id=${assignment.eventId.toString("hex")}`,
                }),
              );
            }

            yield* sql`UPDATE ${sql(tableName)}
              SET ${sql(Columns.SETTLEMENT_EVENT_INFO)} = ${assignment.settlementEventInfo},
                  ${sql(Columns.VALIDITY)} = ${assignment.validity},
                  ${sql(Columns.VALIDITY_DETAIL)} = CAST(${JSON.stringify(
                    assignment.validityDetail ?? {},
                  )} AS JSONB),
                  updated_at = NOW()
              WHERE ${sql(Columns.ID)} = ${assignment.eventId}`;
          }),
        { discard: true },
      ),
    );
  }).pipe(
    Effect.withLogSpan(`setSettlementInfoForEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to set withdrawal settlement event info",
    ),
  );

export const markProjectedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }

    const sql = yield* SqlClient.SqlClient;
    const uniqueIds = Array.from(
      new Set(ids.map((id) => id.toString("hex"))),
    ).map((hex) => Buffer.from(hex, "hex"));

    const existing = yield* sql<{
      [Columns.ID]: Buffer;
      [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
      [Columns.STATUS]: Status;
      [Columns.SETTLEMENT_EVENT_INFO]: Buffer | null;
      [Columns.VALIDITY]: Validity | null;
    }>`SELECT ${sql(Columns.ID)}, ${sql(
      Columns.PROJECTED_HEADER_HASH,
    )}, ${sql(Columns.STATUS)}, ${sql(Columns.SETTLEMENT_EVENT_INFO)}, ${sql(
      Columns.VALIDITY,
    )} FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}`;

    const existingById = new Map(
      existing.map((row) => [row[Columns.ID].toString("hex"), row] as const),
    );
    for (const id of uniqueIds) {
      const key = id.toString("hex");
      const row = existingById.get(key);
      if (row === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to mark withdrawal header assignment because it does not exist",
            cause: `event_id=${key}`,
          }),
        );
      }
      if (
        row[Columns.STATUS] !== Status.Projected &&
        row[Columns.STATUS] !== Status.Finalized
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to assign projected header because the withdrawal is not in a projected lifecycle state",
            cause: `event_id=${key},status=${row[Columns.STATUS]}`,
          }),
        );
      }
      if (
        row[Columns.SETTLEMENT_EVENT_INFO] === null ||
        row[Columns.VALIDITY] === null
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to assign projected header because the withdrawal has not been classified",
            cause: `event_id=${key}`,
          }),
        );
      }
      const currentProjectedHeaderHash = row[Columns.PROJECTED_HEADER_HASH];
      if (
        currentProjectedHeaderHash !== null &&
        !currentProjectedHeaderHash.equals(projectedHeaderHash)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to assign projected header because the withdrawal is already assigned to a different header",
            cause: `event_id=${key},existing_header_hash=${currentProjectedHeaderHash.toString(
              "hex",
            )},requested_header_hash=${projectedHeaderHash.toString("hex")}`,
          }),
        );
      }
    }

    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash},
          updated_at = NOW()
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL`;
  }).pipe(
    Effect.withLogSpan(`markProjectedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark withdrawals as assigned to the given header",
    ),
  );

export const clearProjectedHeaderAssignmentByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }

    const sql = yield* SqlClient.SqlClient;
    const uniqueIds = Array.from(
      new Set(ids.map((id) => id.toString("hex"))),
    ).map((hex) => Buffer.from(hex, "hex"));

    const existing = yield* sql<{
      [Columns.ID]: Buffer;
      [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
    }>`SELECT ${sql(Columns.ID)}, ${sql(Columns.PROJECTED_HEADER_HASH)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}`;

    const existingById = new Map(
      existing.map((row) => [row[Columns.ID].toString("hex"), row] as const),
    );
    for (const id of uniqueIds) {
      const key = id.toString("hex");
      const row = existingById.get(key);
      if (row === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to clear projected header assignment because the withdrawal does not exist",
            cause: `event_id=${key}`,
          }),
        );
      }
      const currentProjectedHeaderHash = row[Columns.PROJECTED_HEADER_HASH];
      if (
        currentProjectedHeaderHash !== null &&
        !currentProjectedHeaderHash.equals(projectedHeaderHash)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to clear projected header assignment because the withdrawal is assigned to a different header",
            cause: `event_id=${key},existing_header_hash=${currentProjectedHeaderHash.toString(
              "hex",
            )},requested_header_hash=${projectedHeaderHash.toString("hex")}`,
          }),
        );
      }
    }

    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.PROJECTED_HEADER_HASH)} = NULL,
          updated_at = NOW()
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}`;
  }).pipe(
    Effect.withLogSpan(`clearProjectedHeaderAssignmentByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to clear projected header assignments for withdrawals",
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
    const uniqueIds = Array.from(
      new Set(ids.map((id) => id.toString("hex"))),
    ).map((hex) => Buffer.from(hex, "hex"));
    const rows = yield* sql<{ [Columns.ID]: Buffer }>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash},
          updated_at = NOW()
      WHERE ${sql(Columns.ID)} IN ${sql.in(uniqueIds)}
        AND ${sql(Columns.STATUS)} IN (${Status.Projected}, ${Status.Finalized})
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}
      RETURNING ${sql(Columns.ID)}`;
    if (rows.length !== uniqueIds.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to finalize withdrawals because at least one row is missing, unprojected, or assigned to a different header",
          cause: `requested=${uniqueIds.length},finalized=${rows.length},header_hash=${projectedHeaderHash.toString(
            "hex",
          )}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markFinalizedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to mark withdrawals finalized"),
  );

export const toRootKeyValue = (
  entry: Entry,
): Effect.Effect<
  { readonly key: Buffer; readonly value: Buffer },
  DatabaseError,
  never
> =>
  Effect.gen(function* () {
    const settlementEventInfo = entry[Columns.SETTLEMENT_EVENT_INFO];
    if (settlementEventInfo === null) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to convert withdrawal to root key/value because it has not been classified",
          cause: `event_id=${entry[Columns.ID].toString("hex")}`,
        }),
      );
    }
    return {
      key: Buffer.from(entry[Columns.ID]),
      value: Buffer.from(settlementEventInfo),
    };
  });

export const toLedgerOutRef = (
  entry: Entry,
): Effect.Effect<Buffer, DatabaseError, never> =>
  Effect.try({
    try: () => {
      const outRef = LucidData.from(
        entry[Columns.L2_OUTREF].toString("hex"),
        SDK.OutputReference,
      );
      return Buffer.from(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(outRef.transactionId),
          outRef.outputIndex,
        ).to_cbor_bytes(),
      );
    },
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "Failed to convert withdrawal l2_outref into ledger outref",
        cause,
      }),
  });

export const pruneOlderThan = (
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql<{ [Columns.ID]: Buffer }>`DELETE FROM ${sql(
      tableName,
    )}
      WHERE ${sql(Columns.INCLUSION_TIME)} < ${cutoff}
        AND ${sql(Columns.STATUS)} = ${Status.Finalized}
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} IS NOT NULL
        AND NOT EXISTS (
          SELECT 1 FROM ${sql("pending_block_finalization_withdrawals")} pending
          WHERE pending.${sql("member_id")} = ${sql(tableName)}.${sql(
            Columns.ID,
          )}
        )
      RETURNING ${sql(Columns.ID)}`;
    return deleted.length;
  }).pipe(
    Effect.withLogSpan(`pruneOlderThan ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to prune old withdrawals"),
  );

export const clear = clearTable(tableName);
