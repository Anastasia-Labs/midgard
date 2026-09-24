import { SqlClient } from "@effect/sql";
import { Context, Effect, Option } from "effect";

import type { LedgerSnapshotPoint } from "../l1-ledger-snapshot.js";
import type { Database } from "../services/database.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const tableName = "event_history_authority";
export type Token = Readonly<{
  deploymentIdentity: string;
  ownerToken: string;
  generation: string;
}>;
export type State = "recovering" | "ready" | "suspended";
export type Row = {
  readonly deployment_identity: Buffer;
  readonly owner_token: string;
  readonly generation: string;
  readonly state: State;
  readonly reason: string;
  readonly lease_until: Date;
  readonly point_slot: string | null;
  readonly point_hash: Buffer | null;
  readonly snapshot_digest: Buffer | null;
};
type LockedRow = Row & { readonly lease_live: boolean };

const failure = (message: string) =>
  new DatabaseError({ table: tableName, message, cause: undefined });
const recoveryTransaction = Context.GenericTag<Token>(
  "midgard/EventHistoryRecoveryTransaction",
);

const ownedTransaction = Context.GenericTag<
  Readonly<{ token: Token; state: "ready" | "recovering" }>
>("midgard/HistoryOwnedTransaction");
/** Read-only capability installed after the authority lock; nested bounded
 * database operations may reuse it, but a generic SQL transaction cannot. */
export const currentOwnedTransaction = Effect.serviceOption(ownedTransaction);

/** Journal mutations compose with recovery's existing outer transaction. A
 * generic SQL transaction is insufficient: this capability is installed only
 * after the authority row has been locked and its live token checked. */
export const requireRecoveryTransaction = Effect.gen(function* () {
  const token = yield* Effect.serviceOption(recoveryTransaction);
  const transaction = yield* Effect.serviceOption(
    SqlClient.TransactionConnection,
  );
  if (Option.isNone(token) || Option.isNone(transaction))
    return yield* Effect.fail(
      failure("History journal requires an owned recovery transaction"),
    );
  return token.value;
});
const digest = (
  value: string,
  label: string,
): Effect.Effect<Buffer, DatabaseError> =>
  /^[0-9a-f]{64}$/u.test(value)
    ? Effect.succeed(Buffer.from(value, "hex"))
    : Effect.fail(
        failure(`${label} must be a complete lowercase SHA256 digest`),
      );
const duration = (value: number): Effect.Effect<number, DatabaseError> =>
  Number.isSafeInteger(value) && value > 0
    ? Effect.succeed(value)
    : Effect.fail(
        failure(
          "History authority lease duration must be a positive safe integer",
        ),
      );
export const tokenFromRow = (row: Row): Token => ({
  deploymentIdentity: row.deployment_identity.toString("hex"),
  ownerToken: row.owner_token,
  generation: row.generation,
});

export const retrieve: Effect.Effect<
  Option.Option<Row>,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows =
    yield* sql<Row>`SELECT * FROM event_history_authority WHERE singleton = true`;
  return Option.fromNullable(rows[0]);
}).pipe(sqlErrorToDatabaseError(tableName, "Failed to read history authority"));

const lockedRow = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  // Evaluate the clock after the row lock has actually been acquired. A plain
  // SELECT projection can otherwise evaluate it before waiting for FOR UPDATE.
  const rows = yield* sql<LockedRow>`WITH locked AS MATERIALIZED (
    SELECT * FROM event_history_authority WHERE singleton = true FOR UPDATE
  ) SELECT *, lease_until > clock_timestamp() AS lease_live FROM locked`;
  if (rows[0] === undefined)
    return yield* Effect.fail(
      failure("History authority has not been initialized"),
    );
  return rows[0];
});

const requireToken = (row: LockedRow, token: Token, requireLive = true) => {
  if (
    row.deployment_identity.toString("hex") !== token.deploymentIdentity ||
    row.owner_token !== token.ownerToken ||
    row.generation !== token.generation
  )
    return Effect.fail(
      failure("History authority generation or owner changed"),
    );
  if (requireLive && !row.lease_live)
    return Effect.fail(failure("History authority lease expired"));
  return Effect.void;
};

/** First lock in every protected SQL mutation. Never wait for in-memory cache
 * locks/tails, network I/O, or post-commit publication inside this transaction.
 * Recovery commits suspension, releases this lock, then drains the pipeline. */
const withState = <A, E, R>(
  token: Token,
  state: "ready" | "recovering",
  mutation: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | DatabaseError, R | Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    if (
      Option.isSome(
        yield* Effect.serviceOption(SqlClient.TransactionConnection),
      )
    )
      return yield* Effect.fail(
        failure("History authority must own the outermost transaction"),
      );
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const row = yield* lockedRow;
        yield* requireToken(row, token);
        if (row.state !== state)
          return yield* Effect.fail(
            failure(`History authority is not ${state}`),
          );
        const ownedMutation = Effect.provideService(
          mutation,
          ownedTransaction,
          { token, state },
        );
        const result = yield* state === "recovering"
          ? Effect.provideService(ownedMutation, recoveryTransaction, token)
          : ownedMutation;
        // clock_timestamp(), not transaction-start NOW(), fences a long write.
        const final = yield* lockedRow;
        yield* requireToken(final, token);
        if (final.state !== state)
          return yield* Effect.fail(
            failure(`History authority is not ${state}`),
          );
        return result;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed history authority transaction"),
  );

export const withReady = <A, E, R>(
  token: Token,
  mutation: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | DatabaseError, R | Database> =>
  withState(token, "ready", mutation);

/** Recovery-only SQL repair. This is not an override: owner, generation, live
 * lease and recovering state are checked under the same first row lock. */
export const withRecovery = <A, E, R>(
  token: Token,
  mutation: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | DatabaseError, R | Database> =>
  withState(token, "recovering", mutation);

/** Claim an absent/expired owner. A foreign deployment is never overwritten.
 * Even the same owner must advance generation and revalidate after restart. */
export const acquire = (input: {
  readonly deploymentIdentity: string;
  readonly ownerToken: string;
  readonly leaseDurationMs: number;
}): Effect.Effect<Token, DatabaseError, Database> =>
  Effect.gen(function* () {
    const deployment = yield* digest(
      input.deploymentIdentity,
      "Deployment identity",
    );
    const ttl = yield* duration(input.leaseDurationMs);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const inserted = yield* sql<Row>`INSERT INTO event_history_authority
      (deployment_identity, owner_token, generation, state, reason, lease_until)
      VALUES (${deployment}, ${input.ownerToken}::uuid, 0, 'recovering', 'startup revalidation',
        clock_timestamp() + (${ttl} * interval '1 millisecond'))
      ON CONFLICT (singleton) DO NOTHING RETURNING *`;
        if (inserted[0] !== undefined) return tokenFromRow(inserted[0]);
        const row = yield* lockedRow;
        if (!row.deployment_identity.equals(deployment))
          return yield* Effect.fail(
            failure("History authority belongs to another deployment"),
          );
        if (row.lease_live && row.owner_token !== input.ownerToken)
          return yield* Effect.fail(
            failure("History authority still has a live owner"),
          );
        const rows = yield* sql<Row>`UPDATE event_history_authority
      SET owner_token = ${input.ownerToken}::uuid, generation = generation + 1,
          state = 'recovering', reason = 'startup revalidation',
          lease_until = clock_timestamp() + (${ttl} * interval '1 millisecond'), updated_at = clock_timestamp()
      WHERE singleton = true RETURNING *`;
        return tokenFromRow(rows[0]!);
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to acquire history authority"),
  );

/** Revoke the old generation before slow replay. Retained point/digest are
 * diagnostic recovery checkpoints; they confer no readiness in this state. */
export const beginRecovery = (
  token: Token,
  reason: string,
): Effect.Effect<Token, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const row = yield* lockedRow;
        yield* requireToken(row, token);
        const rows =
          yield* sql<Row>`UPDATE event_history_authority SET generation = generation + 1,
        state = 'recovering', reason = ${reason}, updated_at = clock_timestamp()
        WHERE singleton = true RETURNING *`;
        return tokenFromRow(rows[0]!);
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to revoke history generation"),
  );

export const renew = (
  token: Token,
  leaseDurationMs: number,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const ttl = yield* duration(leaseDurationMs);
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const row = yield* lockedRow;
        yield* requireToken(row, token);
        if (row.state === "suspended")
          return yield* Effect.fail(
            failure("Suspended authority requires revalidation"),
          );
        yield* sql`UPDATE event_history_authority SET lease_until = clock_timestamp() + (${ttl} * interval '1 millisecond'),
        updated_at = clock_timestamp() WHERE singleton = true`;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to renew history authority"),
  );

export const suspend = (
  token: Token,
  reason: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* requireToken(yield* lockedRow, token, false);
        yield* sql`UPDATE event_history_authority SET generation = generation + 1,
        state = 'suspended', reason = ${reason}, lease_until = clock_timestamp(), updated_at = clock_timestamp()
        WHERE singleton = true`;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to suspend history authority"),
  );

/** Teardown CAS: an expired/replaced owner must not revoke its successor.
 * False means ownership was already lost; storage failures still propagate. */
export const release = (
  token: Token,
): Effect.Effect<boolean, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`UPDATE event_history_authority
      SET generation = generation + 1, state = 'suspended', reason = 'history owner closed',
          lease_until = clock_timestamp(), updated_at = clock_timestamp()
      WHERE singleton = true AND deployment_identity = ${Buffer.from(token.deploymentIdentity, "hex")}
        AND owner_token = ${token.ownerToken}::uuid AND generation = ${token.generation}::bigint
      RETURNING singleton`;
    return rows.length === 1;
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to release history authority"),
  );

/** The source owner must establish branch/provenance and finish durable repair
 * before calling this CAS. SQL does not authenticate caller-supplied snapshots. */
export const publishReady = (
  token: Token,
  capture: {
    readonly point: LedgerSnapshotPoint;
    readonly snapshotDigest: string;
  },
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const hash = yield* digest(capture.point.id, "Point hash");
    const snapshotDigest = yield* digest(
      capture.snapshotDigest,
      "Snapshot digest",
    );
    if (!Number.isSafeInteger(capture.point.slot) || capture.point.slot < 0)
      return yield* Effect.fail(
        failure("History point slot must be a safe natural number"),
      );
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const row = yield* lockedRow;
        yield* requireToken(row, token);
        if (row.state === "suspended")
          return yield* Effect.fail(
            failure("Suspended authority requires revalidation"),
          );
        if (
          row.state === "ready" &&
          row.point_slot !== null &&
          (BigInt(row.point_slot) > BigInt(capture.point.slot) ||
            (BigInt(row.point_slot) === BigInt(capture.point.slot) &&
              !row.point_hash!.equals(hash)))
        )
          return yield* Effect.fail(
            failure("History branch replacement requires a new generation"),
          );
        yield* sql`UPDATE event_history_authority SET state = 'ready', reason = 'canonical snapshot verified',
      point_slot = ${capture.point.slot}, point_hash = ${hash}, snapshot_digest = ${snapshotDigest}, updated_at = clock_timestamp()
      WHERE singleton = true`;
      }),
    );
  }).pipe(
    sqlErrorToDatabaseError(tableName, "Failed to publish history readiness"),
  );
