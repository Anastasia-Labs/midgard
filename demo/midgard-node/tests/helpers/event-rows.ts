/**
 * Test-only seeding of deposit and withdrawal rows. Production writes event
 * rows only through follower ingestion (`reconcileFollowerEvents`); these
 * insert prepared entries directly, under the same history ingestion gate,
 * refusing a batch whose event id repeats with another payload or whose
 * persisted row differs in an immutable field.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { DepositsDB, WithdrawalsDB } from "../../src/database/index.js";
import {
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "../../src/database/utils/common.js";
import type { Database } from "../../src/services/database.js";
import { withHistoryIngestion } from "../../src/services/event-history-producer.js";

const sameDepositPayload = (
  left: DepositsDB.Entry,
  right: DepositsDB.Entry,
): boolean =>
  left[DepositsDB.Columns.ID].equals(right[DepositsDB.Columns.ID]) &&
  left[DepositsDB.Columns.INFO].equals(right[DepositsDB.Columns.INFO]) &&
  left[DepositsDB.Columns.INCLUSION_TIME].getTime() ===
    right[DepositsDB.Columns.INCLUSION_TIME].getTime() &&
  left[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].equals(
    right[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
  ) &&
  left[DepositsDB.Columns.LEDGER_TX_ID].equals(
    right[DepositsDB.Columns.LEDGER_TX_ID],
  ) &&
  left[DepositsDB.Columns.LEDGER_OUTPUT].equals(
    right[DepositsDB.Columns.LEDGER_OUTPUT],
  ) &&
  left[DepositsDB.Columns.LEDGER_ADDRESS] ===
    right[DepositsDB.Columns.LEDGER_ADDRESS];

export const insertDeposits = (
  entries: readonly DepositsDB.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const incomingById = new Map<string, DepositsDB.Entry>();
    for (const incoming of entries) {
      const key = incoming[DepositsDB.Columns.ID].toString("hex");
      const existingIncoming = incomingById.get(key);
      if (
        existingIncoming !== undefined &&
        !sameDepositPayload(existingIncoming, incoming)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: DepositsDB.tableName,
            message:
              "Refusing to insert deposits because the same event_id appears with conflicting payloads in one batch",
            cause: `event_id=${key}`,
          }),
        );
      }
      incomingById.set(key, incoming);
    }
    const normalizedEntries = [...incomingById.values()];
    // The whole payload is immutable, the L1 tx hash included (ruling 2: it
    // is the admission tx, not the Order's current output). A known event is
    // kept as it is; reject the entire batch if any of its payload differs.
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const rows = yield* sql<{ [DepositsDB.Columns.ID]: Buffer }>`
          INSERT INTO ${sql(DepositsDB.tableName)} ${sql.insert(normalizedEntries)}
          ON CONFLICT (${sql(DepositsDB.Columns.ID)}) DO UPDATE SET
            ${sql(DepositsDB.Columns.DEPOSIT_L1_TX_HASH)} = ${sql(DepositsDB.tableName)}.${sql(DepositsDB.Columns.DEPOSIT_L1_TX_HASH)}
          WHERE ${sql(DepositsDB.tableName)}.${sql(DepositsDB.Columns.DEPOSIT_L1_TX_HASH)} = EXCLUDED.${sql(
            DepositsDB.Columns.DEPOSIT_L1_TX_HASH,
          )}
            AND ${sql(DepositsDB.tableName)}.${sql(DepositsDB.Columns.INFO)} = EXCLUDED.${sql(
              DepositsDB.Columns.INFO,
            )}
            AND ${sql(DepositsDB.tableName)}.${sql(DepositsDB.Columns.INCLUSION_TIME)} = EXCLUDED.${sql(
              DepositsDB.Columns.INCLUSION_TIME,
            )}
            AND ${sql(DepositsDB.tableName)}.${sql(DepositsDB.Columns.LEDGER_TX_ID)} = EXCLUDED.${sql(
              DepositsDB.Columns.LEDGER_TX_ID,
            )}
            AND ${sql(DepositsDB.tableName)}.${sql(DepositsDB.Columns.LEDGER_OUTPUT)} = EXCLUDED.${sql(
              DepositsDB.Columns.LEDGER_OUTPUT,
            )}
            AND ${sql(DepositsDB.tableName)}.${sql(DepositsDB.Columns.LEDGER_ADDRESS)} = EXCLUDED.${sql(
              DepositsDB.Columns.LEDGER_ADDRESS,
            )}
          RETURNING ${sql(DepositsDB.Columns.ID)}
        `;
        if (rows.length !== normalizedEntries.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: DepositsDB.tableName,
              message:
                "Refusing to upsert deposit because the same event_id has conflicting persisted payload",
              cause: `requested=${normalizedEntries.length},upserted=${rows.length}`,
            }),
          );
        }
      }),
    );
  }).pipe(
    withHistoryIngestion,
    Effect.withLogSpan(`insertEntries ${DepositsDB.tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(DepositsDB.tableName, "insertEntries", e),
    ),
    sqlErrorToDatabaseError(
      DepositsDB.tableName,
      "Failed to insert given deposit UTxOs",
    ),
  );

const sameWithdrawalPayload = (
  left: WithdrawalsDB.Entry,
  right: WithdrawalsDB.Entry,
): boolean =>
  left[WithdrawalsDB.Columns.ID].equals(right[WithdrawalsDB.Columns.ID]) &&
  left[WithdrawalsDB.Columns.RAW_EVENT_INFO].equals(
    right[WithdrawalsDB.Columns.RAW_EVENT_INFO],
  ) &&
  left[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() ===
    right[WithdrawalsDB.Columns.INCLUSION_TIME].getTime() &&
  left[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH].equals(
    right[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH],
  ) &&
  left[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX] ===
    right[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX] &&
  left[WithdrawalsDB.Columns.ASSET_NAME].equals(
    right[WithdrawalsDB.Columns.ASSET_NAME],
  ) &&
  left[WithdrawalsDB.Columns.L2_OUTREF].equals(
    right[WithdrawalsDB.Columns.L2_OUTREF],
  ) &&
  left[WithdrawalsDB.Columns.L2_OWNER].equals(
    right[WithdrawalsDB.Columns.L2_OWNER],
  ) &&
  left[WithdrawalsDB.Columns.L2_VALUE].equals(
    right[WithdrawalsDB.Columns.L2_VALUE],
  ) &&
  left[WithdrawalsDB.Columns.L1_ADDRESS].equals(
    right[WithdrawalsDB.Columns.L1_ADDRESS],
  ) &&
  left[WithdrawalsDB.Columns.L1_DATUM].equals(
    right[WithdrawalsDB.Columns.L1_DATUM],
  ) &&
  left[WithdrawalsDB.Columns.REFUND_ADDRESS].equals(
    right[WithdrawalsDB.Columns.REFUND_ADDRESS],
  ) &&
  left[WithdrawalsDB.Columns.REFUND_DATUM].equals(
    right[WithdrawalsDB.Columns.REFUND_DATUM],
  );

export const insertWithdrawals = (
  entries: readonly WithdrawalsDB.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return;
    }
    const incomingById = new Map<string, WithdrawalsDB.Entry>();
    for (const incoming of entries) {
      const key = incoming[WithdrawalsDB.Columns.ID].toString("hex");
      const existingIncoming = incomingById.get(key);
      if (
        existingIncoming !== undefined &&
        !sameWithdrawalPayload(existingIncoming, incoming)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: WithdrawalsDB.tableName,
            message:
              "Refusing to insert withdrawals because the same event_id appears with conflicting payloads in one batch",
            cause: `event_id=${key}`,
          }),
        );
      }
      incomingById.set(key, incoming);
    }

    const sql = yield* SqlClient.SqlClient;
    const insertColumns = [
      WithdrawalsDB.Columns.ID,
      WithdrawalsDB.Columns.RAW_EVENT_INFO,
      WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO,
      WithdrawalsDB.Columns.INCLUSION_TIME,
      WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH,
      WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX,
      WithdrawalsDB.Columns.ASSET_NAME,
      WithdrawalsDB.Columns.L2_OUTREF,
      WithdrawalsDB.Columns.L2_OWNER,
      WithdrawalsDB.Columns.L2_VALUE,
      WithdrawalsDB.Columns.L1_ADDRESS,
      WithdrawalsDB.Columns.L1_DATUM,
      WithdrawalsDB.Columns.REFUND_ADDRESS,
      WithdrawalsDB.Columns.REFUND_DATUM,
      WithdrawalsDB.Columns.VALIDITY,
      WithdrawalsDB.Columns.VALIDITY_DETAIL,
      WithdrawalsDB.Columns.PROJECTED_HEADER_HASH,
      WithdrawalsDB.Columns.STATUS,
    ];
    // Bind serialized JSON as text before casting; a JSONB-typed parameter
    // makes postgres.js encode the string a second time. Render rows explicitly
    // because sql.insert only preserves parameter/custom fragments, not casts.
    const normalizedEntries = [...incomingById.values()].map((entry) => ({
      ...entry,
      [WithdrawalsDB.Columns.VALIDITY_DETAIL]: sql`CAST(${JSON.stringify(
        entry[WithdrawalsDB.Columns.VALIDITY_DETAIL] ?? {},
      )} AS TEXT)::JSONB`,
    }));
    // Ingestion may observe a continuation at a new output. Refresh only that
    // location: projection, settlement classification and event content survive.
    // Reject the entire batch if any immutable payload differs.
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const rows = yield* sql<{ [WithdrawalsDB.Columns.ID]: Buffer }>`
          INSERT INTO ${sql(WithdrawalsDB.tableName)} (${sql.csv(insertColumns.map((column) => sql`${sql(column)}`))})
          VALUES ${sql.csv(normalizedEntries.map((entry) => sql`(${sql.csv(insertColumns.map((column) => sql`${entry[column]}`))})`))}
          ON CONFLICT (${sql(WithdrawalsDB.Columns.ID)}) DO UPDATE SET
            ${sql(WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH)} = EXCLUDED.${sql(WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH)},
            ${sql(WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX)} = EXCLUDED.${sql(WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX)},
            updated_at = NOW()
          WHERE ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.RAW_EVENT_INFO)} = EXCLUDED.${sql(
            WithdrawalsDB.Columns.RAW_EVENT_INFO,
          )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.INCLUSION_TIME)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.INCLUSION_TIME,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.ASSET_NAME)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.ASSET_NAME,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.L2_OUTREF)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.L2_OUTREF,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.L2_OWNER)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.L2_OWNER,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.L2_VALUE)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.L2_VALUE,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.L1_ADDRESS)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.L1_ADDRESS,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.L1_DATUM)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.L1_DATUM,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.REFUND_ADDRESS)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.REFUND_ADDRESS,
            )}
            AND ${sql(WithdrawalsDB.tableName)}.${sql(WithdrawalsDB.Columns.REFUND_DATUM)} = EXCLUDED.${sql(
              WithdrawalsDB.Columns.REFUND_DATUM,
            )}
          RETURNING ${sql(WithdrawalsDB.Columns.ID)}
        `;
        if (rows.length !== normalizedEntries.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: WithdrawalsDB.tableName,
              message:
                "Refusing to upsert withdrawal because the same event_id has conflicting persisted payload",
              cause: `requested=${normalizedEntries.length},upserted=${rows.length}`,
            }),
          );
        }
      }),
    );
  }).pipe(
    withHistoryIngestion,
    Effect.withLogSpan(`insertEntries ${WithdrawalsDB.tableName}`),
    sqlErrorToDatabaseError(
      WithdrawalsDB.tableName,
      "Failed to insert withdrawal UTxOs",
    ),
  );
