# P0 Blocker 5: Versioned Schema Migrations and Startup Compatibility Checks

## Problem Statement

`demo/midgard-node` currently initializes PostgreSQL by executing the latest
table definitions during normal node startup. This makes the database shape a
side effect of starting the process instead of an explicit, auditable protocol
state transition.

For a production L2, schema drift can become state drift. The database stores
ledger projections, pending block finalization state, deposit ingestion cursors,
mempool state, immutable transactions, and rejection history. The node must be
able to prove which schema it is running against, refuse incompatible schemas,
and apply schema changes only through an ordered migration ledger.

The production fix is to replace ad hoc startup table creation with:

- versioned migration files committed with the code;
- a durable migration ledger in PostgreSQL;
- a migration runner that applies unapplied migrations explicitly;
- startup gates that fail closed unless the database schema is exactly
  compatible with the running binary;
- backup, rollback, and test workflows that treat schema changes as operational
  events.

This plan does not implement code. It defines the implementation strategy and
acceptance criteria for the migration work.

## Scope

In scope:

- Replace `InitDB.program` startup schema creation with a migration compatibility
  assertion.
- Add an explicit migration runner and operator-facing commands.
- Convert the current schema into the first canonical migration.
- Define the exact table, index, constraint, and dependency inventory that
  `0001_initial_schema.sql` must create.
- Define checksum, ordering, and compatibility policy.
- Define tests, observability, packaging, and rollout steps.

Out of scope:

- Changing application table semantics except where needed to represent the
  current schema exactly in the initial migration.
- Adding backward-compatibility shims for old `demo/midgard-node` databases.
- Automatically repairing drifted production databases at startup.
- Weakening validation or using schema rewrites to hide integrity issues.

## Current Behavior

The readiness report identifies this blocker in
[`PRODUCTION_READINESS_REPORT.md:397`](../PRODUCTION_READINESS_REPORT.md#L397).
It states that database initialization creates current tables directly and that
most table creation uses `CREATE TABLE IF NOT EXISTS`.

The long-running node calls database initialization before serving traffic:

- [`src/commands/listen.ts:69`](../src/commands/listen.ts#L69) runs
  `InitDB.program.pipe(Effect.provide(Database.layer))`.
- [`src/commands/listen.ts:70`](../src/commands/listen.ts#L70) then proceeds to
  protocol startup checks.
- [`src/commands/listen.ts:115`](../src/commands/listen.ts#L115) launches the
  HTTP server only after those startup steps.

`InitDB.program` sets session options and then delegates to table modules:

- [`src/database/init.ts:21`](../src/database/init.ts#L21) defines the startup
  initialization effect.
- [`src/database/init.ts:28`](../src/database/init.ts#L28) sets
  `client_min_messages`.
- [`src/database/init.ts:29`](../src/database/init.ts#L29) sets the default
  isolation level to `serializable`.
- [`src/database/init.ts:31`](../src/database/init.ts#L31) through
  [`src/database/init.ts:43`](../src/database/init.ts#L43) create every table or
  table group directly.
- [`src/database/init.ts:45`](../src/database/init.ts#L45) logs successful
  initialization without recording a schema version.

The table modules contain schema-changing DDL that runs from startup:

- [`src/database/addressHistory.ts:27`](../src/database/addressHistory.ts#L27)
  defines `createTable`; [`src/database/addressHistory.ts:32`](../src/database/addressHistory.ts#L32)
  creates `address_history` if missing; [`src/database/addressHistory.ts:38`](../src/database/addressHistory.ts#L38)
  adds `created_at` if missing.
- [`src/database/blocks.ts:68`](../src/database/blocks.ts#L68) defines
  `createTable`; [`src/database/blocks.ts:73`](../src/database/blocks.ts#L73)
  creates `blocks` if missing.
- [`src/database/utils/ledger.ts:46`](../src/database/utils/ledger.ts#L46)
  defines a reusable ledger table creator; [`src/database/utils/ledger.ts:53`](../src/database/utils/ledger.ts#L53)
  creates the caller-provided ledger table if missing.
- [`src/database/deposits.ts:55`](../src/database/deposits.ts#L55) defines
  `createTable`; [`src/database/deposits.ts:60`](../src/database/deposits.ts#L60)
  creates `deposits_utxos` if missing.
- [`src/database/utils/tx.ts:36`](../src/database/utils/tx.ts#L36) defines a
  reusable transaction table creator; [`src/database/utils/tx.ts:43`](../src/database/utils/tx.ts#L43)
  creates the caller-provided transaction table if missing.
- [`src/database/mempoolLedger.ts:54`](../src/database/mempoolLedger.ts#L54)
  defines `createTable`; [`src/database/mempoolLedger.ts:59`](../src/database/mempoolLedger.ts#L59)
  creates `mempool_ledger` if missing.
- [`src/database/mempoolLedger.ts:77`](../src/database/mempoolLedger.ts#L77)
  drops `uniq_mempool_ledger_source_event_id` if it exists, then
  [`src/database/mempoolLedger.ts:80`](../src/database/mempoolLedger.ts#L80)
  recreates it.
- [`src/database/mempoolTxDeltas.ts:119`](../src/database/mempoolTxDeltas.ts#L119)
  defines `createTable`; [`src/database/mempoolTxDeltas.ts:122`](../src/database/mempoolTxDeltas.ts#L122)
  creates `mempool_tx_deltas` if missing.
- [`src/database/txRejections.ts:28`](../src/database/txRejections.ts#L28)
  defines `createTable`; [`src/database/txRejections.ts:33`](../src/database/txRejections.ts#L33)
  creates `tx_rejections` if missing.
- [`src/database/depositIngestionCursor.ts:41`](../src/database/depositIngestionCursor.ts#L41)
  defines `createTable`; [`src/database/depositIngestionCursor.ts:44`](../src/database/depositIngestionCursor.ts#L44)
  creates `deposit_ingestion_cursor` if missing.
- [`src/database/pendingBlockFinalizations.ts:108`](../src/database/pendingBlockFinalizations.ts#L108)
  defines `createTables`; [`src/database/pendingBlockFinalizations.ts:113`](../src/database/pendingBlockFinalizations.ts#L113),
  [`src/database/pendingBlockFinalizations.ts:130`](../src/database/pendingBlockFinalizations.ts#L130),
  and [`src/database/pendingBlockFinalizations.ts:145`](../src/database/pendingBlockFinalizations.ts#L145)
  create the pending-finalization tables if missing.
- [`src/database/pendingBlockFinalizations.ts:158`](../src/database/pendingBlockFinalizations.ts#L158)
  drops `uniq_pending_block_finalizations_single_active` if it exists, then
  [`src/database/pendingBlockFinalizations.ts:161`](../src/database/pendingBlockFinalizations.ts#L161)
  recreates it.

The database service opens a PostgreSQL client but has no schema compatibility
gate:

- [`src/services/database.ts:20`](../src/services/database.ts#L20) constructs
  the `PgClient` layer.
- [`src/services/database.ts:64`](../src/services/database.ts#L64) exposes the
  database layer used by startup and runtime code.

There is no durable table that records applied schema versions, migration
checksums, app version, migration attempts, or the expected schema version for
the running binary.

## Target Migration Invariants

The migration system must satisfy these invariants:

1. Startup must never silently create, alter, drop, or repair application schema.
2. Every schema change must be represented by exactly one ordered migration file.
3. Applied migrations must be recorded in PostgreSQL with immutable version and
   checksum metadata.
4. Migration files that have been applied must never be edited in place.
5. The running binary must declare the exact latest schema version it supports.
6. Startup must fail closed when the database is unversioned, behind, ahead,
   checksum-mismatched, non-contiguous, or currently being migrated.
7. Migration execution must be serialized across all node processes and operator
   commands.
8. Each migration must be transactional unless the migration is explicitly
   marked as non-transactional and has a bespoke, reviewed recovery plan.
9. A failed transactional migration must leave no partial application in the
   successful migration ledger.
10. Attempts, failures, and successful migrations must be auditable.
11. Existing table modules must become data-access modules, not startup schema
    mutators.
12. Fresh database creation must use migration `0001`, not ad hoc latest table
    creation.
13. Existing unversioned databases must not be auto-baselined by startup.
14. Schema compatibility checks must run before HTTP serving and background
    fibers start.
15. Schema drift must be treated as an integrity incident. The implementation
    must not repair, normalize, or ignore unexpected application schema objects
    during startup.

## Migration Ledger Design

Create migration metadata tables in the same PostgreSQL database and `public`
schema as the current node tables. A future move to a dedicated metadata schema
must itself be an explicit migration. The ledger table is the only startup
bootstrap exception: the migration runner and startup checker may create or
verify the metadata table shape before checking application migrations.
Application schema remains owned by migrations.

### `schema_migrations`

`schema_migrations` records successful migrations only.

Required columns:

- `version INTEGER PRIMARY KEY CHECK (version > 0)`
- `name TEXT NOT NULL`
- `checksum_sha256 TEXT NOT NULL CHECK (checksum_sha256 ~ '^[0-9a-f]{64}$')`
- `manifest_hash_sha256 TEXT NOT NULL CHECK (manifest_hash_sha256 ~ '^[0-9a-f]{64}$')`
- `applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `app_version TEXT NOT NULL`
- `execution_ms INTEGER NOT NULL CHECK (execution_ms >= 0)`
- `applied_by TEXT NOT NULL`

Required indexes and constraints:

- Primary key on `version`.
- Unique constraint on `(version, checksum_sha256)`.
- Check constraint that `name` matches the migration manifest entry for the
  version at application validation time.

Rows are append-only. Application code must never update or delete rows from
this table outside explicit operator recovery procedures. The implementation
must enforce this as much as PostgreSQL allows for the selected deployment
role, for example by using a trigger that rejects `UPDATE` and `DELETE` unless a
documented recovery-only session setting is present, or by using privileges that
only let the migration command insert rows.

### `schema_migration_events`

`schema_migration_events` records attempts and failures for auditability.

Required columns:

- `id BIGSERIAL PRIMARY KEY`
- `version INTEGER`
- `name TEXT`
- `checksum_sha256 TEXT`
- `event_type TEXT NOT NULL CHECK (event_type IN ('started', 'succeeded', 'failed', 'verification_failed'))`
- `created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()`
- `app_version TEXT NOT NULL`
- `actor TEXT NOT NULL`
- `details JSONB NOT NULL DEFAULT '{}'::jsonb`

This table is append-only. A failed migration event does not mean the schema
changed, but it gives operators a durable trail for incident response.

`details` must include structured, non-secret evidence such as lock wait
duration, statement timeout, failure class, PostgreSQL error code, verification
query name, and row-count summaries for data migrations. It must not include
database credentials, seed phrases, API keys, or full transaction payloads.

### Manifest Shape

Add a code-owned manifest, for example:

```ts
type Migration = {
  readonly version: number;
  readonly name: string;
  readonly checksumSha256: string;
  readonly sqlPath: string;
  readonly transactional: true;
};
```

The manifest must be generated or verified at build/test time from committed SQL
files, not manually duplicated without checks. The application must export:

- `EXPECTED_SCHEMA_VERSION`, equal to the highest migration version.
- `MIGRATION_MANIFEST_HASH`, a SHA-256 hash of ordered `(version, name,
checksum)` entries.
- `MIGRATIONS`, the ordered manifest.

## Migration File Layout

Use a deterministic directory layout:

```text
src/database/migrations/
  index.ts
  runner.ts
  status.ts
  sql/
    0001_initial_schema.sql
    0002_add_example_column.sql
```

Naming policy:

- File names must be zero-padded and start with the numeric version.
- File names must be immutable once merged.
- `0001_initial_schema.sql` must create the complete current schema represented
  by the existing startup table modules.
- Future migrations must use the next integer version.
- No migration may skip a version.

`0001_initial_schema.sql` must represent the canonical schema directly. It must
not encode the current startup's ad hoc evolution, such as
`ALTER TABLE ... ADD COLUMN IF NOT EXISTS` for a table that is created in the
same migration. It must create the final table shape and indexes explicitly.

### Initial Schema Inventory

Before writing `0001_initial_schema.sql`, produce a checked-in inventory of the
current effective schema from a fresh database created by the existing
`InitDB.program`. The implementation may store this as a test fixture or a
schema-introspection snapshot, but it must be deterministic and reviewed with
the initial migration.

`0001_initial_schema.sql` must create, at minimum, the current node-owned
application tables:

- `address_history`
- `blocks`
- `confirmed_ledger`
- `latest_ledger`
- `deposits_utxos`
- `immutable`
- `mempool`
- `processed_mempool`
- `mempool_ledger`
- `mempool_tx_deltas`
- `tx_rejections`
- `deposit_ingestion_cursor`
- `pending_block_finalizations`
- `pending_block_finalization_deposits`
- `pending_block_finalization_txs`

The migration must also create the current required indexes and constraints,
including:

- primary keys for all keyed tables;
- `blocks.tx_id` uniqueness and indexes on `blocks.header_hash` and
  `blocks.tx_id`;
- address lookup indexes for ledger-style tables and `mempool_ledger`;
- timestamp indexes used by retention on transaction tables, address history,
  and rejection history;
- deposit status check constraints and deposit lookup indexes;
- `mempool_ledger.source_event_id` foreign key to
  `deposits_utxos.event_id`, `ON DELETE RESTRICT`;
- unique `mempool_ledger.source_event_id` semantics that match PostgreSQL's
  handling of nullable values;
- pending-finalization status check constraints, member ordinal uniqueness, and
  the partial unique index that permits only one active pending finalization.

Foreign-key dependency order must be explicit: create `deposits_utxos` before
`mempool_ledger` and before pending-finalization deposit members; create
`pending_block_finalizations` before its member tables. Do not add new foreign
keys, cascading behavior, enum types, generated columns, or stricter
constraints in `0001` unless the current runtime code already relies on them and
the change is called out as a separate reviewed schema hardening decision.

## Migration Runner Strategy

Add a migration runner that can be invoked by an operator command and by tests.
Normal node startup must call compatibility assertion, not migration apply.

Runner phases:

1. Open the PostgreSQL client through the existing `Database.layer`.
2. Set strict session options:
   - `SET client_min_messages = 'error'`
   - `SET default_transaction_isolation TO 'serializable'`
   - `SET lock_timeout = '30s'` for `db:migrate`
   - `SET statement_timeout = '15min'` for `db:migrate`
   - a shorter startup compatibility lock wait, defaulting to 5 seconds
3. Acquire a PostgreSQL advisory lock dedicated to Midgard schema migration.
4. Create or verify migration metadata tables.
5. Load the migration manifest from code.
6. Read `schema_migrations` ordered by version.
7. Validate existing ledger rows before applying anything.
8. Apply pending migrations in order.
9. Record successful rows and audit events.
10. Re-read and verify final database version and checksums.
11. Release the advisory lock.

The advisory lock key must be a deterministic Midgard-specific value documented
in code, not an arbitrary magic number. The runner must use session-level
`pg_advisory_lock` or `pg_try_advisory_lock` consistently and must guarantee
release in success, failure, and interruption paths. Startup compatibility must
use the same lock namespace to detect a moving schema.

Validation before applying:

- If `schema_migrations` is empty and application tables do not exist, apply
  `0001` onward.
- If `schema_migrations` is empty and application tables exist, fail. This is an
  unversioned database and must not be silently baselined.
- If the database contains a version not present in the binary manifest, fail.
- If any database checksum differs from the manifest checksum, fail.
- If versions are non-contiguous, fail.
- If any lower version is missing while a higher version exists, fail.
- If a previous failed event exists for a version that appears partially applied
  according to migration-specific verification, fail with a recovery message.
- If node-owned application tables, indexes, or constraints differ from the
  expected pre-migration state for the next migration, fail before applying.
  Migrations must not use `IF EXISTS` or `IF NOT EXISTS` to hide unexpected
  drift in production paths.

Application of a transactional migration:

1. Insert a `schema_migration_events` row with `event_type='started'`.
2. Start a transaction.
3. Execute the migration SQL exactly as committed.
4. Run migration-specific verification queries when defined.
5. Insert the successful `schema_migrations` row and a
   `schema_migration_events` row with `event_type='succeeded'` inside the same
   transaction.
6. Commit.
7. Re-read and verify the committed migration ledger.

For migrations that rewrite data, the SQL or companion verification module must
define explicit preconditions, deterministic ordering, expected row-count
relationships, and postconditions. Unexpected data is a failed migration, not an
implicit cleanup opportunity.

If execution fails:

- Roll back the transaction.
- Insert a `schema_migration_events` row with `event_type='failed'` outside the
  failed transaction.
- Return a typed migration error that includes version, name, checksum, and
  recovery guidance.

Non-transactional migrations:

- Do not support them in the first implementation.
- If a future migration requires `CREATE INDEX CONCURRENTLY` or another
  non-transactional operation, require a separate design review and migration
  type with explicit resume and verification semantics.

Concurrency:

- `db:migrate` must hold the advisory lock while validating and applying.
- startup compatibility checks must attempt the same advisory lock with a short
  timeout or use a dedicated "migration in progress" detection path.
- If startup observes an active migration, it must fail closed or wait only for a
  bounded operator-configured period. It must not serve traffic against a moving
  schema.

## Startup Gate

Replace startup schema creation with a compatibility assertion.

Desired startup sequence:

1. Create database connection layer.
2. Run `MigrationRunner.assertCompatible(EXPECTED_SCHEMA_VERSION)`.
3. Run protocol startup checks.
4. Seed or verify runtime protocol dependencies as already designed.
5. Start HTTP server and background fibers.

`assertCompatible` must:

- Create or verify only the migration metadata table if it is missing.
- Refuse to create application tables.
- Refuse unversioned databases that already contain application tables.
- Refuse an empty database with only missing or empty metadata as
  `schema_not_migrated`; startup must never apply `0001`.
- Read `schema_migrations`.
- Verify contiguous versions from `1` through `EXPECTED_SCHEMA_VERSION`.
- Verify every applied checksum matches the binary manifest.
- Verify the database has no versions greater than `EXPECTED_SCHEMA_VERSION`.
- Verify the final applied version equals `EXPECTED_SCHEMA_VERSION`.
- Verify that the runtime can introspect the expected node-owned tables,
  indexes, and constraints for the applied version. A matching migration ledger
  is necessary but not sufficient if the live schema has drifted.
- Emit structured logs for `expected_version`, `actual_version`,
  `manifest_hash`, and failure reason.
- Fail with a typed error before `listen.ts` starts serving.

Startup failure examples:

```text
startup_failed: schema_unversioned_database
startup_failed: schema_version_behind expected=7 actual=6
startup_failed: schema_version_ahead expected=7 actual=8
startup_failed: schema_checksum_mismatch version=4
startup_failed: schema_migration_in_progress
startup_failed: schema_drift_detected object=mempool_ledger
```

Readiness and metrics must include schema version metadata after startup
succeeds. If startup fails, readiness must never report healthy because the
server must not be running.

## Checksum and Version Policy

Version policy:

- Versions are monotonically increasing positive integers.
- The latest version supported by a binary is fixed at build time.
- A binary supports exactly the versions present in its manifest.
- Running against a database ahead of the binary is forbidden.
- Running against a database behind the binary is forbidden at node startup; the
  operator must run `db:migrate` first.
- Editing or deleting an applied migration file is forbidden.

Checksum policy:

- Compute SHA-256 over exact migration file bytes.
- Store lowercase hex checksums.
- Store the checksum in `schema_migrations`.
- Validate all applied checksums on every startup.
- CI must fail if a migration file's computed checksum differs from the
  manifest.
- CI must fail if an older migration file changes after it has been merged.

Manifest policy:

- The manifest must be sorted by version.
- The manifest must be contiguous.
- The manifest hash must be logged on startup and migration apply.
- Migration status commands must show both database and binary manifest hashes.
- Build output must include the exact SQL files whose bytes were checksummed.
  Add a packaged-artifact test that runs `db:checksum` and at least
  `db:verify` from `dist/index.js`, so `tsup` packaging cannot silently omit or
  rewrite migration files.

## Observability Requirements

Migration and startup schema checks must emit structured logs and metrics
suitable for operator dashboards and incident response.

Required structured log fields:

- `component=schema_migration` or `component=schema_compatibility`
- `event`
- `app_version`
- `expected_schema_version`
- `actual_schema_version`
- `manifest_hash`
- `migration_version` when applicable
- `checksum_sha256` when applicable
- `duration_ms`
- `failure_code` and non-secret `failure_detail` on failure

Required metrics:

- gauge for binary expected schema version;
- gauge for database applied schema version after successful compatibility
  verification;
- counter for migration attempts by version and result;
- histogram or summary for migration duration;
- counter for startup schema compatibility failures by failure code.

`/readyz` or its underlying readiness model must expose the verified schema
version and manifest hash once the server is running. The endpoint must not
perform schema repair or migration work.

## Rollback and Backup Stance

Rollback policy must prioritize correctness over convenience.

Default stance:

- No automatic down migrations.
- No startup rollback.
- No destructive repair during startup.
- Failed transactional migrations rely on PostgreSQL transaction rollback.
- Successful migrations are rolled back operationally by restoring a verified
  database backup or by applying a new forward recovery migration.

Operator requirements before running migrations:

- Take a PostgreSQL physical snapshot or PITR-capable backup.
- Record current `schema_migrations` state.
- Record current application version and Git commit.
- Record critical L2 state observability points, including confirmed ledger
  root, latest local boundary, pending finalization state, and deposit cursor.
- Ensure only one node instance can apply migrations.

Recovery expectations:

- If a migration fails before commit, inspect `schema_migration_events`, fix the
  cause, and rerun.
- If a migration succeeds but the new binary cannot run, restore backup or apply
  an explicit forward fix. Do not mutate `schema_migrations` by hand except under
  a documented incident runbook.
- If schema drift is detected, stop the node. Drift is an integrity incident, not
  a warning.

## Dev and Test Workflow

Developer workflow:

1. Change TypeScript data-access code and domain behavior.
2. Add a new migration file for any schema change.
3. Regenerate or update the migration manifest/checksums.
4. Add or update tests proving the migration and runtime code agree.
5. Run migration tests against a clean PostgreSQL database.
6. Run migration tests against the previous schema version when applicable.
7. Run startup compatibility tests.

Commands to add:

- `midgard-node db:migrate`
  - Applies pending migrations explicitly.
- `midgard-node db:status`
  - Prints database version, expected version, pending migrations, unknown
    migrations, and checksum status.
  - Supports `--json` for automation and deployment gates.
- `midgard-node db:verify`
  - Performs the same compatibility check as startup without starting the node.
- `midgard-node db:checksum`
  - Recomputes checksums and verifies the manifest.

Local development convenience:

- A reset command may drop and recreate a local development database only when
  explicitly named as destructive.
- Test helpers must run migrations for fresh test databases instead of calling
  table `createTable` functions.
- No development path may reintroduce normal startup table creation.

## Integration With Future Schema Changes

Future PRs that change schema must include:

- A new migration file with the next version.
- Updated manifest/checksum.
- Updated TypeScript table access code.
- Tests that prove the migration creates or transforms the expected schema.
- Tests that prove startup rejects pre-migration and post-future schemas.
- Operator notes describing expected lock duration, data rewrite size, and
  backup requirements.

Schema review checklist for future PRs:

- Does this migration preserve protocol correctness and state integrity?
- Is it deterministic?
- Is every data rewrite auditable and reproducible?
- Does it fail closed on unexpected existing data?
- Does it avoid default shortcuts and compatibility shims?
- Does it have a rollback-by-backup or forward-fix story?
- Does it avoid serving traffic while the schema is incompatible?

Table modules must keep constants, types, and DML helpers. They must not export
startup DDL helpers after the migration system is adopted. If tests need
tables, they must use migrations.

## Tests

Add tests at the runner, startup, and integration levels.

Runner tests:

- Empty database applies `0001_initial_schema.sql` and records version `1`.
- Empty database applies all migrations in order.
- Re-running `db:migrate` with no pending migrations is a no-op except for
  status logging.
- Checksum mismatch for an applied version fails.
- Unknown database version greater than the manifest fails.
- Non-contiguous ledger rows fail.
- Missing lower version with higher version present fails.
- Existing application tables with an empty migration ledger fail as
  `schema_unversioned_database`.
- Failed migration rolls back application DDL and does not insert a successful
  `schema_migrations` row.
- Failed migration records a `schema_migration_events` failure.
- Concurrent migration runners serialize through the advisory lock.

Startup tests:

- Startup succeeds when database version equals `EXPECTED_SCHEMA_VERSION`.
- Startup fails when database is behind.
- Startup fails when database is ahead.
- Startup fails when checksum differs.
- Startup fails when migration lock is held.
- Startup does not call any application table creation code.

Schema shape tests:

- `0001_initial_schema.sql` produces the tables currently created by
  `InitDB.program`.
- Required primary keys, foreign keys, unique constraints, check constraints,
  and indexes exist.
- `mempool_ledger.source_event_id` foreign key and uniqueness match the current
  intended schema.
- Pending block finalization member tables preserve ordinal uniqueness.
- Deposit status check constraints match the application status values.

Operational tests:

- `db:status` reports expected, actual, pending, and checksum status.
- `db:status --json` emits machine-readable expected version, actual version,
  manifest hash, pending migrations, unknown migrations, checksum status, and
  drift status.
- `db:verify` returns a non-zero exit code on incompatibility.
- Migration logs include version, checksum, manifest hash, and duration.
- Migration metrics and readiness schema metadata are emitted after successful
  startup compatibility checks.
- Built `dist/index.js` can run `db:checksum` and `db:verify` against packaged
  migration SQL.
- A test database created only through migrations can pass the existing database
  and startup test suites.

## Rollout Steps

1. Inventory the current schema from table modules and a live fresh database
   created by the current code.
2. Write `0001_initial_schema.sql` that creates the canonical current schema.
3. Add the migration metadata tables and runner.
4. Add `db:migrate`, `db:status`, `db:verify`, and checksum tooling.
5. Replace `InitDB.program` startup table creation with
   `assertCompatible(EXPECTED_SCHEMA_VERSION)`.
6. Remove or stop exporting application `createTable` helpers from runtime paths.
7. Update tests and test helpers to create schema through migrations.
8. Add CI checks for manifest ordering, checksum correctness, and immutable old
   migrations.
9. Add packaged-artifact checks for migration SQL availability in `dist`.
10. Document operator migration procedure in the node README or operations guide.
11. For local demo databases without a migration ledger, require explicit reset
    and migration. Do not auto-baseline.
12. For any persistent deployment, take backup, run `db:status`, run
    `db:migrate`, run `db:verify`, then start the node.
13. Monitor startup logs, metrics, readiness schema fields, and migration audit
    rows after rollout.

## Risks

Risks:

- The first canonical migration may accidentally differ from the current
  effective schema. Mitigation: compare against PostgreSQL introspection from a
  current fresh database.
- Long-running future migrations can block startup or writers. Mitigation:
  require migration-specific lock and duration notes before merge.
- Data-rewrite migrations can expose historical data inconsistencies.
  Mitigation: fail closed and require explicit remediation migration or incident
  runbook.
- Packaging SQL files for ESM/runtime execution can be brittle. Mitigation:
  add a packaged-artifact test that runs migrations from the built output.
- Multiple node instances may race operator migration. Mitigation: advisory
  locks and startup lock detection.

## Concrete Checklist

- [ ] Add migration SQL directory and manifest.
- [ ] Encode the current schema as `0001_initial_schema.sql`.
- [ ] Add checksum generation or verification tooling.
- [ ] Add `schema_migrations` ledger table.
- [ ] Add `schema_migration_events` audit table.
- [ ] Implement advisory-lock protected migration runner.
- [ ] Implement startup `assertCompatible`.
- [ ] Replace `InitDB.program` schema creation in `runNode`.
- [ ] Add CLI commands for migrate, status, verify, and checksum.
- [ ] Update tests to use migrations for database setup.
- [ ] Add runner unit and integration tests.
- [ ] Add startup compatibility tests.
- [ ] Add schema introspection tests for `0001`.
- [ ] Add CI checks for manifest ordering and checksums.
- [ ] Add packaged-artifact checks for migration SQL availability in `dist`.
- [ ] Add structured logs, metrics, and readiness schema metadata.
- [ ] Document operator backup and migration procedure.
- [ ] Remove runtime dependencies on table `createTable` helpers.
- [ ] Confirm startup fails closed for unversioned, behind, ahead, checksum
      mismatch, migration-in-progress, empty-unmigrated, and drifted databases.
- [ ] Confirm no normal startup path mutates application schema.

## Acceptance Criteria

This blocker is complete when:

- A fresh database can be initialized only by running versioned migrations.
- Node startup refuses to serve unless the database is at the binary's expected
  schema version with matching checksums.
- The migration ledger records every successful migration.
- Migration attempts and failures are auditable.
- Existing ad hoc application table creation is removed from normal startup.
- Tests cover migration ordering, checksum enforcement, startup failure modes,
  current schema shape, drift detection, packaged SQL availability, and
  observability outputs.
- Operator documentation explains backup, migration, verification, and recovery
  steps.
