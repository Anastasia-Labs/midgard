import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { SqlClient } from "@effect/sql";
import { Duration, Effect, Fiber } from "effect";

import { Database } from "../services/database.js";
import * as PendingBlockFinalizationsDB from "./pendingBlockFinalizations.js";
import {
  Columns,
  DEFAULT_RENEW_INTERVAL_MS,
  DEFAULT_TTL_MS,
  type Entry,
  SCOPE,
  Status,
  tableName,
} from "./stateQueueMutationLeases.columns.js";
import { INSPECTABLE_LEASE_ROWS } from "./stateQueueMutationLeases.prune.js";
import {
  settleLeaseDurably,
  settleUnsettledOwnLeases,
} from "./stateQueueMutationLeases.settle.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export {
  Columns,
  type Entry,
  Status,
  tableName,
} from "./stateQueueMutationLeases.columns.js";
export {
  INSPECTABLE_LEASE_ROWS,
  pruneSettledLeases,
} from "./stateQueueMutationLeases.prune.js";
export {
  markFailed,
  release,
  unsettledOwnLeaseTokens,
} from "./stateQueueMutationLeases.settle.js";

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

const encodeLeaseJson = (lease: Entry | undefined, now: Date = new Date()) =>
  lease === undefined
    ? null
    : (() => {
        const expiresAt = lease[Columns.EXPIRES_AT];
        const remainingMs = expiresAt.getTime() - now.getTime();
        return {
          token: lease[Columns.TOKEN],
          holder: lease[Columns.HOLDER],
          status: lease[Columns.STATUS],
          acquiredAt: lease[Columns.ACQUIRED_AT].toISOString(),
          expiresAt: expiresAt.toISOString(),
          releasedAt: lease[Columns.RELEASED_AT]?.toISOString() ?? null,
          lastError: lease[Columns.LAST_ERROR] ?? null,
          remainingMs,
          expired: remainingMs < 0,
          blockedUntil: expiresAt.toISOString(),
        };
      })();

/** JSON-safe rendering of a lease inspection, for operator surfaces. */
export const encodeInspectionJson = (inspection: LeaseInspection) => ({
  status: inspection.activeLease === undefined ? "idle" : "busy",
  dbNow: inspection.dbNow.toISOString(),
  activeLease: encodeLeaseJson(inspection.activeLease, inspection.dbNow),
  pendingFinalizations: inspection.pendingFinalizations.map((entry) => ({
    headerHash: entry.headerHash,
    submittedTxHash: entry.submittedTxHash,
    status: entry.status,
    createdAt: entry.createdAt.toISOString(),
    updatedAt: entry.updatedAt.toISOString(),
  })),
  recentLeases: inspection.recentLeases.map((lease) =>
    encodeLeaseJson(lease, inspection.dbNow),
  ),
});

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
    const limit = Math.max(
      1,
      Math.min(INSPECTABLE_LEASE_ROWS, Math.floor(recentLimit)),
    );
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
    // A lease this process could not release blocks this acquisition until
    // its expiry unless released first.
    yield* settleUnsettledOwnLeases;
    yield* expireTimedOutLeases;
    const normalizedTtlMs = normalizeTtlMs(ttlMs);
    // Acquisition, renewal and expiry share the database clock.
    const rows = yield* sql<Entry>`INSERT INTO ${sql(tableName)} (
      ${sql(Columns.TOKEN)}, ${sql(Columns.SCOPE)}, ${sql(Columns.HOLDER)},
      ${sql(Columns.STATUS)}, ${sql(Columns.EXPIRES_AT)}
    ) VALUES (
      ${token}, ${SCOPE}, ${holder}, ${Status.Active},
      NOW() + (${normalizedTtlMs} * INTERVAL '1 millisecond')
    ) ON CONFLICT DO NOTHING RETURNING *`;
    if (rows.length !== 1) {
      const activeLease = yield* retrieveActive();
      return {
        _tag: "Busy" as const,
        activeLease,
      };
    }
    yield* Effect.logInfo(
      `Acquired state-queue mutation lease token=${token},holder=${holder},expires_at=${rows[0]![Columns.EXPIRES_AT].toISOString()}`,
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

/** Marks failed every active lease taken under one of `holders`, whoever took
 * it. Only a caller that proved no live process holds such a lease may run
 * it: see releaseStateQueueLeasesOfPreviousNodeProcess. */
export const retireActiveLeasesOfHolders = (
  holders: readonly string[],
  reason: string,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (holders.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Failed},
          ${sql(Columns.RELEASED_AT)} = NOW(),
          ${sql(Columns.LAST_ERROR)} = ${reason.slice(0, 4000)}
      WHERE ${sql(Columns.SCOPE)} = ${SCOPE}
        AND ${sql(Columns.STATUS)} = ${Status.Active}
        AND ${sql(Columns.HOLDER)} IN ${sql.in(holders)}
      RETURNING *`;
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to retire leases"));

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
    let failure = "";
    const keepAliveFiber = yield* Effect.fork(
      keepLeaseAlive({ token, holder, ttlMs, renewIntervalMs }),
    );
    return yield* Effect.gen(function* () {
      const result = yield* Effect.either(program(token));
      if (result._tag === "Left") {
        completed = "failed";
        failure = formatUnknownError(result.left);
        return yield* Effect.fail(result.left);
      }

      completed = "succeeded";
      return {
        _tag: "Ran" as const,
        value: result.right,
      };
    }).pipe(
      // Renewal stops before the release is recorded, so a lease whose
      // release is still being retried is not extended meanwhile.
      Effect.ensuring(
        Fiber.interrupt(keepAliveFiber).pipe(
          Effect.catchAll(() => Effect.void),
        ),
      ),
      Effect.ensuring(
        Effect.suspend(() =>
          settleLeaseDurably(
            token,
            holder,
            completed === "succeeded"
              ? { kind: "released" }
              : {
                  kind: "failed",
                  error:
                    completed === "failed"
                      ? failure
                      : "lease program interrupted before normal release",
                },
          ),
        ),
      ),
    );
  });
