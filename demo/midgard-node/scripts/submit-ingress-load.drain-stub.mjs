// The drain stub and the Postgres observer for the submit-ingress harness.
//
// The stub stands in for validation when only ingress is measured: it marks
// queued admissions terminal at a fixed rate, oldest first, as rejected under
// its own code, so tx status reads show they never reached validation. It
// takes rows with FOR UPDATE SKIP LOCKED, so it never blocks the node's own
// tx-queue processor; rows the node claims first are not the stub's, and the
// report counts both.
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, ManagedRuntime, Redacted } from "effect";

export const DRAIN_STUB_REJECT_CODE = "E_LOAD_DRAIN_STUB";

/** A Postgres runtime for the harness, from a `postgres://` URL. */
export const makeHarnessPg = (url, applicationName) =>
  ManagedRuntime.make(
    PgClient.layer({
      url: Redacted.make(url),
      maxConnections: 2,
      connectTimeout: 10_000,
      applicationName,
    }),
  );

/** Marks at most `limit` queued admissions terminal; returns how many. */
export const drainQueuedAdmissions = (limit) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql`
      WITH picked AS (
        SELECT tx_id FROM tx_admissions
        WHERE status = 'queued'
        ORDER BY arrival_seq, tx_id
        LIMIT ${limit}
        FOR UPDATE SKIP LOCKED
      )
      UPDATE tx_admissions AS admissions
      SET
        status = 'rejected',
        reject_code = ${DRAIN_STUB_REJECT_CODE},
        reject_detail = NULL,
        terminal_at = GREATEST(NOW(), admissions.first_seen_at,
          admissions.last_seen_at, admissions.updated_at),
        updated_at = GREATEST(NOW(), admissions.first_seen_at,
          admissions.last_seen_at, admissions.updated_at)
      FROM picked
      WHERE admissions.tx_id = picked.tx_id
      RETURNING admissions.tx_id`;
    return rows.length;
  });

/**
 * Runs the stub until `stop()`: every `tickMs` it marks up to
 * `ratePerSecond * tickMs / 1000` rows (fractions carry over). `stop()`
 * resolves to what it did.
 */
export const startDrainStub = ({ runtime, ratePerSecond, tickMs = 100 }) => {
  if (!(ratePerSecond > 0)) throw new Error("drain rate must be positive");
  const stats = { marked: 0, ticks: 0, errors: 0, lastError: null };
  let stopped = false;
  let credit = 0;
  const loop = (async () => {
    while (!stopped) {
      const tickStartedAt = performance.now();
      credit += (ratePerSecond * tickMs) / 1_000;
      const limit = Math.floor(credit);
      if (limit > 0) {
        try {
          const marked = await runtime.runPromise(drainQueuedAdmissions(limit));
          stats.marked += marked;
          // An empty queue does not bank credit for a later burst.
          credit = marked < limit ? 0 : credit - limit;
        } catch (error) {
          stats.errors += 1;
          stats.lastError = String(error?.message ?? error);
        }
      }
      stats.ticks += 1;
      const wait = tickMs - (performance.now() - tickStartedAt);
      if (wait > 0) await new Promise((resolve) => setTimeout(resolve, wait));
    }
  })();
  return {
    stats,
    stop: async () => {
      stopped = true;
      await loop;
      return { ...stats };
    },
  };
};

/**
 * One reading of the counters the report turns into rates: admission rows by
 * status (exact) and the database's tuple and commit counters. Postgres
 * backends publish their tuple and commit counts about once a second, so
 * those rates are only good over a run of many seconds.
 */
export const samplePostgres = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`SELECT pg_stat_clear_snapshot()`;
  const [database] = yield* sql`
    SELECT xact_commit, tup_inserted, tup_updated, tup_deleted
    FROM pg_stat_database WHERE datname = current_database()`;
  const statuses = yield* sql`
    SELECT status::text AS status, reject_code = ${DRAIN_STUB_REJECT_CODE}
      AS by_stub, count(*)::bigint AS rows
    FROM tx_admissions GROUP BY 1, 2`;
  const admissions = {};
  let drainedByStub = 0;
  for (const row of statuses) {
    admissions[row.status] = (admissions[row.status] ?? 0) + Number(row.rows);
    if (row.by_stub === true) drainedByStub += Number(row.rows);
  }
  return {
    atMs: Date.now(),
    xactCommit: Number(database.xact_commit),
    tupInserted: Number(database.tup_inserted),
    tupUpdated: Number(database.tup_updated),
    tupDeleted: Number(database.tup_deleted),
    admissions,
    drainedByStub,
  };
});

/** Per-second rates between two `samplePostgres` readings. */
export const postgresRates = (before, after) => {
  const seconds = (after.atMs - before.atMs) / 1_000;
  const rate = (key) => (after[key] - before[key]) / seconds;
  const total = (sample) =>
    Object.values(sample.admissions).reduce((sum, value) => sum + value, 0);
  return {
    seconds,
    admissionRowsPerSecond: (total(after) - total(before)) / seconds,
    commitsPerSecond: rate("xactCommit"),
    tuplesInsertedPerSecond: rate("tupInserted"),
    tuplesUpdatedPerSecond: rate("tupUpdated"),
    tuplesDeletedPerSecond: rate("tupDeleted"),
    drainedByStub: after.drainedByStub - before.drainedByStub,
    admissionsAtEnd: after.admissions,
  };
};
