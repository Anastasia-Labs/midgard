import { SqlClient } from "@effect/sql";
import { Duration, Effect, Fiber } from "effect";

import { Database } from "../services/database.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "mpf_engine_state";

export type UtxoPayloadSizeAggregate = {
  readonly entryCount: number;
  readonly encodedTupleBytes: number;
};

export const assertLedgerAuditHealthy: Effect.Effect<
  void,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly audit_diverged: boolean }>`SELECT
    audit_diverged FROM ${sql(tableName)} WHERE store_name = 'ledger'`;
  if (rows[0]?.audit_diverged === true) {
    return yield* Effect.fail(
      new DatabaseError({
        table: tableName,
        message:
          "Commit halted because the independent ledger MPF audit diverged",
        cause: undefined,
      }),
    );
  }
}).pipe(
  sqlErrorToDatabaseError(tableName, "Failed to read ledger MPF audit state"),
);

export const recordLedgerAudit = ({
  rootHex,
  diverged,
  utxoPayloadAggregate,
}: {
  readonly rootHex: string;
  readonly diverged: boolean;
  readonly utxoPayloadAggregate?: UtxoPayloadSizeAggregate;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)}
      (store_name, migration_version, root_hex, audit_diverged,
       last_audit_diverged, last_audit_at, utxo_payload_entry_count,
       utxo_payload_encoded_tuple_bytes)
      VALUES ('ledger', 1, ${rootHex}, ${diverged}, ${diverged}, CURRENT_TIMESTAMP,
        ${utxoPayloadAggregate?.entryCount ?? null},
        ${utxoPayloadAggregate?.encodedTupleBytes ?? null})
      ON CONFLICT (store_name) DO UPDATE SET
        migration_version = 1,
        root_hex = EXCLUDED.root_hex,
        audit_diverged = ${sql(tableName)}.audit_diverged OR EXCLUDED.audit_diverged,
        last_audit_diverged = EXCLUDED.last_audit_diverged,
        utxo_payload_entry_count = COALESCE(
          EXCLUDED.utxo_payload_entry_count,
          ${sql(tableName)}.utxo_payload_entry_count
        ),
        utxo_payload_encoded_tuple_bytes = COALESCE(
          EXCLUDED.utxo_payload_encoded_tuple_bytes,
          ${sql(tableName)}.utxo_payload_encoded_tuple_bytes
        ),
        last_audit_at = CURRENT_TIMESTAMP,
        updated_at = CURRENT_TIMESTAMP`;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to record ledger MPF audit"),
  );

export const acquireLedgerStoreLease = ({
  owner,
  ttlMs,
}: {
  readonly owner: string;
  readonly ttlMs: number;
}): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly lease_owner: string }>`
      INSERT INTO ${sql(tableName)}
        (store_name, migration_version, root_hex, lease_owner, lease_expires_at)
      VALUES ('ledger', 1, NULL, ${owner}, NOW() + (${ttlMs} * INTERVAL '1 millisecond'))
      ON CONFLICT (store_name) DO UPDATE SET
        lease_owner = EXCLUDED.lease_owner,
        lease_expires_at = EXCLUDED.lease_expires_at,
        updated_at = CURRENT_TIMESTAMP
      WHERE ${sql(tableName)}.lease_expires_at IS NULL
         OR ${sql(tableName)}.lease_expires_at <= NOW()
         OR ${sql(tableName)}.lease_owner = ${owner}
      RETURNING lease_owner`;
    return rows[0]?.lease_owner === owner;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to acquire ledger MPF lease"),
  );

export const releaseLedgerStoreLease = (
  owner: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)} SET
      lease_owner = NULL, lease_expires_at = NULL, updated_at = CURRENT_TIMESTAMP
      WHERE store_name = 'ledger' AND lease_owner = ${owner}`;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to release ledger MPF lease"),
  );

export const renewLedgerStoreLease = ({
  owner,
  ttlMs,
}: {
  readonly owner: string;
  readonly ttlMs: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly lease_owner: string }>`UPDATE ${sql(
      tableName,
    )} SET
      lease_expires_at = NOW() + (${Math.max(1, Math.floor(ttlMs))} * INTERVAL '1 millisecond'),
      updated_at = CURRENT_TIMESTAMP
      WHERE store_name = 'ledger'
        AND lease_owner = ${owner}
        AND lease_expires_at > NOW()
      RETURNING lease_owner`;
    if (rows[0]?.lease_owner !== owner) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Ledger MPF store lease is no longer active",
          cause: `owner=${owner}`,
        }),
      );
    }
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to renew ledger MPF lease"),
  );

export const revalidateLedgerStoreLease = (
  owner: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly lease_owner: string }>`SELECT lease_owner
      FROM ${sql(tableName)}
      WHERE store_name = 'ledger'
        AND lease_owner = ${owner}
        AND lease_expires_at > NOW()`;
    if (rows[0]?.lease_owner !== owner) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Ledger MPF store lease is no longer active",
          cause: `owner=${owner}`,
        }),
      );
    }
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to revalidate ledger MPF lease"),
  );

export type LedgerStoreLeaseRunResult<A> =
  | { readonly _tag: "Ran"; readonly value: A }
  | { readonly _tag: "Busy" };

export const tryWithLedgerStoreLease = <A, E, R>(
  owner: string,
  program: (owner: string) => Effect.Effect<A, E, R>,
  {
    ttlMs = 10 * 60 * 1000,
    renewIntervalMs = Math.min(60_000, ttlMs / 3),
  }: {
    readonly ttlMs?: number;
    readonly renewIntervalMs?: number;
  } = {},
): Effect.Effect<
  LedgerStoreLeaseRunResult<A>,
  E | DatabaseError,
  R | Database
> =>
  Effect.gen(function* () {
    const normalizedTtlMs = Math.max(1, Math.floor(ttlMs));
    const acquired = yield* acquireLedgerStoreLease({
      owner,
      ttlMs: normalizedTtlMs,
    });
    if (!acquired) return { _tag: "Busy" as const };

    const keepAlive = yield* Effect.fork(
      Effect.forever(
        Effect.sleep(
          Duration.millis(Math.max(1, Math.floor(renewIntervalMs))),
        ).pipe(
          Effect.andThen(
            renewLedgerStoreLease({ owner, ttlMs: normalizedTtlMs }),
          ),
          // The protected program revalidates at its mutation boundary. A
          // failed renewal must not extend the lease under a stale owner.
          Effect.catchAll(() => Effect.void),
        ),
      ),
    );
    return yield* program(owner).pipe(
      Effect.map((value) => ({ _tag: "Ran" as const, value })),
      Effect.ensuring(
        Fiber.interrupt(keepAlive).pipe(Effect.catchAll(() => Effect.void)),
      ),
      Effect.ensuring(
        releaseLedgerStoreLease(owner).pipe(Effect.catchAll(() => Effect.void)),
      ),
    );
  });

export const acknowledgeCleanLedgerAudit = (
  expectedRootHex: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly root_hex: string }>`UPDATE ${sql(
      tableName,
    )} SET
      audit_diverged = FALSE,
      updated_at = CURRENT_TIMESTAMP
      WHERE store_name = 'ledger'
        AND root_hex = ${expectedRootHex}
        AND last_audit_diverged = FALSE
      RETURNING root_hex`;
    if (rows[0]?.root_hex !== expectedRootHex) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to acknowledge MPF divergence without a matching clean audit",
          cause: `expected_root=${expectedRootHex}`,
        }),
      );
    }
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to acknowledge clean ledger MPF audit",
    ),
  );

export const stampLedgerMigration = (
  rootHex: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)}
      (store_name, migration_version, root_hex)
      VALUES ('ledger', 1, ${rootHex})
      ON CONFLICT (store_name) DO UPDATE SET
        migration_version = GREATEST(${sql(tableName)}.migration_version, 1),
        utxo_payload_entry_count = CASE
          WHEN ${sql(tableName)}.root_hex = EXCLUDED.root_hex
            THEN ${sql(tableName)}.utxo_payload_entry_count
          ELSE NULL
        END,
        utxo_payload_encoded_tuple_bytes = CASE
          WHEN ${sql(tableName)}.root_hex = EXCLUDED.root_hex
            THEN ${sql(tableName)}.utxo_payload_encoded_tuple_bytes
          ELSE NULL
        END,
        root_hex = EXCLUDED.root_hex,
        updated_at = CURRENT_TIMESTAMP`;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to stamp ledger MPF migration"),
  );

export const stampLedgerPayloadAggregate = ({
  rootHex,
  aggregate,
}: {
  readonly rootHex: string;
  readonly aggregate: UtxoPayloadSizeAggregate;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)}
      (store_name, migration_version, root_hex, utxo_payload_entry_count,
       utxo_payload_encoded_tuple_bytes)
      VALUES ('ledger', 1, ${rootHex}, ${aggregate.entryCount},
        ${aggregate.encodedTupleBytes})
      ON CONFLICT (store_name) DO UPDATE SET
        migration_version = GREATEST(${sql(tableName)}.migration_version, 1),
        root_hex = EXCLUDED.root_hex,
        utxo_payload_entry_count = EXCLUDED.utxo_payload_entry_count,
        utxo_payload_encoded_tuple_bytes =
          EXCLUDED.utxo_payload_encoded_tuple_bytes,
        updated_at = CURRENT_TIMESTAMP`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to persist ledger DA UTxO size aggregate",
    ),
  );

export const retrieveLedgerPayloadAggregate = (
  rootHex: string,
): Effect.Effect<
  UtxoPayloadSizeAggregate | undefined,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      readonly entry_count: bigint | number | string;
      readonly encoded_tuple_bytes: bigint | number | string;
    }>`SELECT
      utxo_payload_entry_count AS entry_count,
      utxo_payload_encoded_tuple_bytes AS encoded_tuple_bytes
      FROM ${sql(tableName)}
      WHERE store_name = 'ledger'
        AND root_hex = ${rootHex}
        AND utxo_payload_entry_count IS NOT NULL
        AND utxo_payload_encoded_tuple_bytes IS NOT NULL`;
    const row = rows[0];
    if (row === undefined) return undefined;
    const entryCount = Number(row.entry_count);
    const encodedTupleBytes = Number(row.encoded_tuple_bytes);
    if (
      !Number.isSafeInteger(entryCount) ||
      entryCount < 0 ||
      !Number.isSafeInteger(encodedTupleBytes) ||
      encodedTupleBytes < 0
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Stored ledger DA UTxO size aggregate is invalid",
          cause: `entry_count=${String(row.entry_count)},encoded_tuple_bytes=${String(row.encoded_tuple_bytes)}`,
        }),
      );
    }
    return { entryCount, encodedTupleBytes };
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve ledger DA UTxO size aggregate",
    ),
  );

export const ledgerAuditIsDue = ({
  intervalBlocks,
  intervalMs,
  now = new Date(),
}: {
  readonly intervalBlocks: number;
  readonly intervalMs: number;
  readonly now?: Date;
}): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [state] = yield* sql<{ readonly last_audit_at: Date | null }>`
      SELECT last_audit_at FROM ${sql(tableName)} WHERE store_name = 'ledger'`;
    const lastAuditAt = state?.last_audit_at ?? null;
    if (
      lastAuditAt === null ||
      now.getTime() - lastAuditAt.getTime() >= intervalMs
    ) {
      return true;
    }
    const [row] = yield* sql<{
      readonly finalized_count: bigint | string;
    }>`SELECT COUNT(*) AS finalized_count
       FROM pending_block_finalizations
       WHERE status = 'finalized' AND updated_at > ${lastAuditAt}`;
    return BigInt(row?.finalized_count ?? 0) >= BigInt(intervalBlocks);
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to determine MPF audit cadence"),
  );
