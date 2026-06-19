import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Duration, Effect, Fiber } from "effect";

import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as PendingBlockFinalizationsDB from "@/database/pendingBlockFinalizations.js";
import { Database } from "@/services/database.js";

export const tableName = "state_queue_mutation_leases";

const SCOPE = "state_queue";
const DEFAULT_TTL_MS = 10 * 60 * 1000;
const DEFAULT_RENEW_INTERVAL_MS = 60 * 1000;

export const Status = {
  Active: "active",
  Released: "released",
  Failed: "failed",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export enum Columns {
  TOKEN = "token",
  SCOPE = "scope",
  HOLDER = "holder",
  STATUS = "status",
  ACQUIRED_AT = "acquired_at",
  EXPIRES_AT = "expires_at",
  RELEASED_AT = "released_at",
  LAST_ERROR = "last_error",
}

export type Entry = {
  [Columns.TOKEN]: string;
  [Columns.SCOPE]: string;
  [Columns.HOLDER]: string;
  [Columns.STATUS]: Status;
  [Columns.ACQUIRED_AT]: Date;
  [Columns.EXPIRES_AT]: Date;
  [Columns.RELEASED_AT]: Date | null;
  [Columns.LAST_ERROR]: string | null;
};

export type LeaseAcquireResult =
  | {
      readonly _tag: "Acquired";
      readonly token: string;
    }
  | {
      readonly _tag: "Busy";
      readonly activeLease: Entry | undefined;
    };

export type LeaseRunResult<A> =
  | {
      readonly _tag: "Ran";
      readonly value: A;
    }
  | {
      readonly _tag: "Busy";
      readonly activeLease: Entry | undefined;
    };

type LeaseOptions = {
  readonly ttlMs?: number;
  readonly renewIntervalMs?: number;
};

export type PendingFinalizationLeaseInspection = {
  readonly headerHash: string;
  readonly submittedTxHash: string | null;
  readonly status: PendingBlockFinalizationsDB.Status;
  readonly createdAt: Date;
  readonly updatedAt: Date;
};

export type LeaseInspection = {
  readonly dbNow: Date;
  readonly activeLease: Entry | undefined;
  readonly recentLeases: readonly Entry[];
  readonly pendingFinalizations: readonly PendingFinalizationLeaseInspection[];
};

const normalizeTtlMs = (ttlMs: number | undefined): number =>
  Math.max(1, Math.floor(ttlMs ?? DEFAULT_TTL_MS));

const normalizeRenewIntervalMs = (
  ttlMs: number,
  renewIntervalMs: number | undefined,
): number =>
  Math.max(
    1,
    Math.floor(
      renewIntervalMs ?? Math.min(DEFAULT_RENEW_INTERVAL_MS, ttlMs / 3),
    ),
  );

const expireTimedOutLeases = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`UPDATE ${sql(tableName)}
    SET ${sql(Columns.STATUS)} = ${Status.Failed},
        ${sql(Columns.RELEASED_AT)} = NOW(),
        ${sql(Columns.LAST_ERROR)} = 'lease expired before release'
    WHERE ${sql(Columns.SCOPE)} = ${SCOPE}
      AND ${sql(Columns.STATUS)} = ${Status.Active}
      AND ${sql(Columns.EXPIRES_AT)} < NOW()`;
}).pipe(
  sqlErrorToDatabaseError(
    tableName,
    "Failed to expire timed-out state-queue mutation leases",
  ),
);

export const retrieveActive = (): Effect.Effect<
  Entry | undefined,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* expireTimedOutLeases;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.SCOPE)} = ${SCOPE}
        AND ${sql(Columns.STATUS)} = ${Status.Active}
        AND ${sql(Columns.EXPIRES_AT)} >= NOW()
      ORDER BY ${sql(Columns.ACQUIRED_AT)} DESC
      LIMIT 1`;
    return rows[0];
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve active state-queue mutation lease",
    ),
  );

export const describeActiveLease = (activeLease: Entry | undefined): string =>
  activeLease === undefined
    ? "active_lease=none"
    : `active_holder=${activeLease[Columns.HOLDER]},active_token=${
        activeLease[Columns.TOKEN]
      },active_expires_at=${activeLease[Columns.EXPIRES_AT].toISOString()}`;

const encodePendingFinalizationLeaseInspection = (
  row: PendingBlockFinalizationsDB.Row,
): PendingFinalizationLeaseInspection => ({
  headerHash:
    row[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
  submittedTxHash:
    row[PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]?.toString(
      "hex",
    ) ?? null,
  status: row[PendingBlockFinalizationsDB.Columns.STATUS],
  createdAt: row[PendingBlockFinalizationsDB.Columns.CREATED_AT],
  updatedAt: row[PendingBlockFinalizationsDB.Columns.UPDATED_AT],
});

export const inspect = ({
  recentLimit = 10,
}: {
  readonly recentLimit?: number;
} = {}): Effect.Effect<LeaseInspection, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* expireTimedOutLeases;
    const [{ now: dbNow }] = yield* sql<{ now: Date }>`SELECT NOW() AS now`;
    const activeLease = yield* retrieveActive();
    const limit = Math.max(1, Math.min(100, Math.floor(recentLimit)));
    const recentLeases = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.SCOPE)} = ${SCOPE}
      ORDER BY ${sql(Columns.ACQUIRED_AT)} DESC
      LIMIT ${limit}`;
    const pendingFinalizations =
      activeLease === undefined
        ? []
        : (yield* PendingBlockFinalizationsDB.retrieveActiveByStateQueueLeaseToken(
            activeLease[Columns.TOKEN],
          )).map(encodePendingFinalizationLeaseInspection);
    return {
      dbNow,
      activeLease,
      recentLeases,
      pendingFinalizations,
    };
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to inspect state-queue mutation leases",
    ),
  );

export const tryAcquire = ({
  holder,
  ttlMs = DEFAULT_TTL_MS,
}: {
  readonly holder: string;
  readonly ttlMs?: number;
}): Effect.Effect<LeaseAcquireResult, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const token = `${holder}:${randomUUID()}`;
    yield* expireTimedOutLeases;
    const expiresAt = new Date(Date.now() + Math.max(1, ttlMs));
    const rows = yield* sql<{ [Columns.TOKEN]: string }>`INSERT INTO ${sql(
      tableName,
    )} ${sql.insert({
      [Columns.TOKEN]: token,
      [Columns.SCOPE]: SCOPE,
      [Columns.HOLDER]: holder,
      [Columns.STATUS]: Status.Active,
      [Columns.EXPIRES_AT]: expiresAt,
    })} ON CONFLICT DO NOTHING RETURNING ${sql(Columns.TOKEN)}`;
    if (rows.length !== 1) {
      const activeLease = yield* retrieveActive();
      return {
        _tag: "Busy" as const,
        activeLease,
      };
    }
    yield* Effect.logInfo(
      `Acquired state-queue mutation lease token=${token},holder=${holder},expires_at=${expiresAt.toISOString()}`,
    );
    return {
      _tag: "Acquired" as const,
      token,
    };
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to acquire state-queue mutation lease",
    ),
  );

export const acquire = ({
  holder,
  ttlMs = DEFAULT_TTL_MS,
}: {
  readonly holder: string;
  readonly ttlMs?: number;
}): Effect.Effect<string, DatabaseError, Database> =>
  Effect.gen(function* () {
    const result = yield* tryAcquire({ holder, ttlMs });
    if (result._tag === "Busy") {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to acquire state-queue mutation lease",
          cause: `holder=${holder},${describeActiveLease(result.activeLease)}`,
        }),
      );
    }
    return result.token;
  });

export const revalidate = (
  token: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* expireTimedOutLeases;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.TOKEN)} = ${token}
        AND ${sql(Columns.SCOPE)} = ${SCOPE}
        AND ${sql(Columns.STATUS)} = ${Status.Active}
        AND ${sql(Columns.EXPIRES_AT)} >= NOW()
      LIMIT 1`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "State-queue mutation lease is no longer active",
          cause: `token=${token}`,
        }),
      );
    }
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to revalidate state-queue mutation lease",
    ),
  );

export const renew = ({
  token,
  ttlMs = DEFAULT_TTL_MS,
}: {
  readonly token: string;
  readonly ttlMs?: number;
}): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const normalizedTtlMs = normalizeTtlMs(ttlMs);
    yield* expireTimedOutLeases;
    const rows = yield* sql<Entry>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.EXPIRES_AT)} =
            NOW() + (${normalizedTtlMs} * INTERVAL '1 millisecond')
      WHERE ${sql(Columns.TOKEN)} = ${token}
        AND ${sql(Columns.SCOPE)} = ${SCOPE}
        AND ${sql(Columns.STATUS)} = ${Status.Active}
        AND ${sql(Columns.EXPIRES_AT)} >= NOW()
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "State-queue mutation lease is no longer active",
          cause: `token=${token}`,
        }),
      );
    }
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to renew state-queue mutation lease",
    ),
  );

const keepLeaseAlive = ({
  token,
  holder,
  ttlMs,
  renewIntervalMs,
}: {
  readonly token: string;
  readonly holder: string;
  readonly ttlMs: number;
  readonly renewIntervalMs: number;
}): Effect.Effect<never, never, Database> =>
  Effect.forever(
    Effect.sleep(Duration.millis(renewIntervalMs)).pipe(
      Effect.andThen(renew({ token, ttlMs })),
      Effect.tapError((error) =>
        Effect.logWarning(
          `State-queue mutation lease renewal failed; holder=${holder},token=${token},error=${formatUnknownError(error)}`,
        ),
      ),
      Effect.catchAll(() => Effect.void),
    ),
  );

export const release = (
  token: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Released},
          ${sql(Columns.RELEASED_AT)} = NOW()
      WHERE ${sql(Columns.TOKEN)} = ${token}
        AND ${sql(Columns.STATUS)} = ${Status.Active}`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to release state-queue mutation lease",
    ),
  );

export const markFailed = (
  token: string,
  error: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Failed},
          ${sql(Columns.RELEASED_AT)} = NOW(),
          ${sql(Columns.LAST_ERROR)} = ${error.slice(0, 4000)}
      WHERE ${sql(Columns.TOKEN)} = ${token}
        AND ${sql(Columns.STATUS)} = ${Status.Active}`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark state-queue mutation lease failed",
    ),
  );

export const tryWithLease = <A, E, R>(
  holder: string,
  program: (token: string) => Effect.Effect<A, E, R>,
  options: LeaseOptions = {},
): Effect.Effect<LeaseRunResult<A>, E | DatabaseError, R | Database> =>
  Effect.gen(function* () {
    const ttlMs = normalizeTtlMs(options.ttlMs);
    const renewIntervalMs = normalizeRenewIntervalMs(
      ttlMs,
      options.renewIntervalMs,
    );
    const acquisition = yield* tryAcquire({ holder, ttlMs });
    if (acquisition._tag === "Busy") {
      return {
        _tag: "Busy",
        activeLease: acquisition.activeLease,
      };
    }

    const token = acquisition.token;
    let completed: "running" | "succeeded" | "failed" = "running";
    const keepAliveFiber = yield* Effect.fork(
      keepLeaseAlive({ token, holder, ttlMs, renewIntervalMs }),
    );
    return yield* Effect.gen(function* () {
      const result = yield* Effect.either(program(token));
      if (result._tag === "Left") {
        completed = "failed";
        yield* markFailed(token, formatUnknownError(result.left)).pipe(
          Effect.catchAll(() => Effect.void),
        );
        return yield* Effect.fail(result.left);
      }

      completed = "succeeded";
      return {
        _tag: "Ran" as const,
        value: result.right,
      };
    }).pipe(
      Effect.ensuring(
        Effect.gen(function* () {
          if (completed === "succeeded") {
            yield* release(token).pipe(Effect.catchAll(() => Effect.void));
          } else {
            const reason =
              completed === "failed"
                ? "lease program failed before normal release"
                : "lease program interrupted before normal release";
            yield* markFailed(token, reason).pipe(
              Effect.catchAll(() => Effect.void),
            );
          }
        }),
      ),
      Effect.ensuring(
        Fiber.interrupt(keepAliveFiber).pipe(
          Effect.catchAll(() => Effect.void),
        ),
      ),
    );
  });
