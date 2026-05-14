# P0 Blocker 5: Versioned Schema Migrations and Startup Compatibility Checks

## Problem Statement

`demo/midgard-node` previously initialized PostgreSQL by executing the latest
table definitions during normal node startup. That made database shape a side
effect of starting the process instead of an explicit, auditable protocol state
transition.

For a production L2, schema drift can become state drift. The database stores
ledger projections, pending block finalization state, deposit ingestion cursors,
mempool state, immutable transactions, and rejection history. The node must be
able to prove which schema it is running against, refuse incompatible schemas,
and apply schema changes only through an ordered migration ledger.

The core production fix has now landed: ad hoc startup table creation has been
replaced with:

- versioned migration files committed with the code;
- a durable migration ledger in PostgreSQL;
- a migration runner that applies unapplied migrations explicitly;
- startup gates that fail closed unless the database schema is exactly
  compatible with the running binary;
- backup, rollback, and test workflows that treat schema changes as operational
  events.

This document now tracks the landed migration implementation and the remaining
hardening needed before this blocker can be considered fully complete.

## Scope

In scope:

- Landed: replace `InitDB.program` startup schema creation with a migration compatibility
  assertion.
- Landed: add an explicit migration runner and operator-facing commands.
- Landed: convert the schema into explicit migrations `0001` through `0006`.
- Landed: define checksum, ordering, and exact-version compatibility policy.
- Remaining: deepen schema drift detection from table/index presence to columns,
  constraints, foreign keys, check expressions, and index definitions.
- Remaining: complete tests, observability, packaging checks, and operator
  rollout documentation.

Out of scope:

- Changing application table semantics except where needed to represent the
  current schema exactly in the initial migration.
- Adding backward-compatibility shims for old `demo/midgard-node` databases.
- Automatically repairing drifted production databases at startup.
- Weakening validation or using schema rewrites to hide integrity issues.

## Current Behavior

The long-running node still calls `InitDB.program` before serving traffic:

- [`src/commands/listen.ts:72`](../src/commands/listen.ts#L72) runs
  `InitDB.program.pipe(Effect.provide(Database.layer))`.
- [`src/commands/listen.ts:73`](../src/commands/listen.ts#L73) then proceeds to
  protocol startup checks.
- [`src/commands/listen.ts:160`](../src/commands/listen.ts#L160) constructs the
  HTTP server only after startup checks and reconciliation complete.

`InitDB.program` is now a schema compatibility gate, not startup schema DDL:

- [`src/database/init.ts:13`](../src/database/init.ts#L13) defines the effect as
  `MigrationRunner.assertCompatible`.
- [`src/database/init.ts:15`](../src/database/init.ts#L15) maps migration
  incompatibility into `DatabaseError` for startup.
- [`src/database/migrations/runner.ts:767`](../src/database/migrations/runner.ts#L767)
  implements `assertCompatible`.

Explicit migrations are committed under `src/database/migrations`:

- [`src/database/migrations/index.ts:20`](../src/database/migrations/index.ts#L20)
  declares manifest entries for versions `0001` through `0006`.
- [`src/database/migrations/index.ts:65`](../src/database/migrations/index.ts#L65)
  derives `EXPECTED_SCHEMA_VERSION` from the manifest.
- [`src/database/migrations/index.ts:68`](../src/database/migrations/index.ts#L68)
  computes the manifest hash from ordered version/name/checksum entries.
- [`src/database/migrations/sql/0001_initial_schema.sql`](../src/database/migrations/sql/0001_initial_schema.sql)
  through
  [`src/database/migrations/sql/0006_state_queue_mutation_leases.sql`](../src/database/migrations/sql/0006_state_queue_mutation_leases.sql)
  are the current SQL migration files.

The migration runner now creates and uses durable metadata tables:

- [`src/database/migrations/runner.ts:59`](../src/database/migrations/runner.ts#L59)
  creates or verifies `schema_migrations` and `schema_migration_events`.
- [`src/database/migrations/runner.ts:120`](../src/database/migrations/runner.ts#L120)
  acquires a Midgard-specific advisory lock.
- [`src/database/migrations/runner.ts:650`](../src/database/migrations/runner.ts#L650)
  applies pending migrations through `migrate`.
- [`src/database/migrations/runner.ts:758`](../src/database/migrations/runner.ts#L758)
  reports status.
- [`src/database/migrations/runner.ts:801`](../src/database/migrations/runner.ts#L801)
  exposes `verify` as the startup-compatible assertion.

Operator-facing CLI commands exist:

- [`src/index.ts:300`](../src/index.ts#L300) defines `db:migrate`.
- [`src/index.ts:320`](../src/index.ts#L320) defines `db:status`.
- [`src/index.ts:338`](../src/index.ts#L338) defines `db:verify`.
- [`src/index.ts:357`](../src/index.ts#L357) defines `db:checksum`.

Remaining gaps:

- `verifyApplicationShape` checks table and index presence only, not full column,
  constraint, foreign-key, check-expression, or trigger definitions:
  [`src/database/migrations/runner.ts:310`](../src/database/migrations/runner.ts#L310).
- Table modules still export legacy `createTable`/`createTables` helpers, but
  normal startup no longer calls them.
- Migration and compatibility observability is mostly logs/status output; metrics,
  readiness schema metadata, CI immutability checks, and packaged-artifact checks
  remain to be completed.

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
only let the migration command insert rows. Current code records rows by append
only convention; database-level anti-update/delete enforcement remains
outstanding.

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
  sql/
    0001_initial_schema.sql
    0002_durable_tx_admissions.sql
    0003_local_mutation_jobs.sql
    0004_withdrawal_events.sql
    0005_pending_finalization_journal_payloads.sql
    0006_state_queue_mutation_leases.sql
```

Naming policy:

- File names must be zero-padded and start with the numeric version.
- File names must be immutable once merged.
- `0001_initial_schema.sql` creates the first canonical baseline schema.
- Later schema changes are represented by later migrations; the current schema is
  version `0006`.
- Future migrations must use the next integer version.
- No migration may skip a version.

`0001_initial_schema.sql` must represent its canonical baseline directly. It
must not encode the old startup's ad hoc evolution, such as
`ALTER TABLE ... ADD COLUMN IF NOT EXISTS` for a table that is created in the
same migration. It must create the final table shape and indexes explicitly.

### Initial Schema Inventory

The current schema inventory is the version `0006` manifest, not only
`0001_initial_schema.sql`. The effective node-owned application tables are:

- `address_history`
- `blocks`
- `confirmed_ledger`
- `latest_ledger`
- `deposits_utxos`
- `withdrawal_utxos`
- `immutable`
- `mempool`
- `processed_mempool`
- `mempool_ledger`
- `mempool_tx_deltas`
- `tx_rejections`
- `deposit_ingestion_cursor`
- `pending_block_finalizations`
- `pending_block_finalization_deposits`
- `pending_block_finalization_withdrawals`
- `pending_block_finalization_txs`
- `tx_admissions`
- `local_mutation_jobs`
- `state_queue_mutation_leases`

The full migration set must also create the current required indexes and
constraints, including:

- primary keys for all keyed tables;
- `blocks.tx_id` uniqueness and indexes on `blocks.header_hash` and
  `blocks.tx_id`;
- address lookup indexes for ledger-style tables and `mempool_ledger`;
- timestamp indexes used by retention on transaction tables, address history,
  and rejection history;
- deposit status check constraints and deposit lookup indexes;
- withdrawal status/validity check constraints and withdrawal lookup indexes;
- `mempool_ledger.source_event_id` foreign key to
  `deposits_utxos.event_id`, `ON DELETE RESTRICT`;
- unique `mempool_ledger.source_event_id` semantics that match PostgreSQL's
  handling of nullable values;
- pending-finalization status check constraints, member ordinal uniqueness, and
  the partial unique index that permits only one active pending finalization.
- durable admission indexes for dequeue/status/lease lookup;
- state-queue mutation lease uniqueness for active scope.

Foreign-key dependency order must be explicit: create `deposits_utxos` before
`mempool_ledger` and before pending-finalization deposit members; create
`withdrawal_utxos` before pending-finalization withdrawal members; create
`pending_block_finalizations` before its member tables. Do not add new foreign
keys, cascading behavior, enum types, generated columns, or stricter
constraints in any migration unless the runtime code already relies on them and
the change is called out as a separate reviewed schema hardening decision.

## Migration Runner Strategy

The migration runner exists and can be invoked by operator commands and tests.
Normal node startup calls compatibility assertion, not migration apply.

Current runner phases:

1. Open the PostgreSQL client through the existing `Database.layer`.
2. Set strict session options:
   - `SET client_min_messages = 'error'`
   - `SET default_transaction_isolation TO 'serializable'`
   - `SET lock_timeout = '30s'` for `db:migrate`
   - `SET statement_timeout = '15min'` for `db:migrate`
   - a shorter startup compatibility lock wait, defaulting to 5 seconds
3. Acquire a PostgreSQL advisory lock dedicated to Midgard schema migration.
4. Create or verify migration metadata tables. This is the only startup-time DDL
   path and it is limited to migration metadata.
5. Load the migration manifest from code.
6. Read `schema_migrations` ordered by version.
7. Validate existing ledger rows before applying anything.
8. Apply pending migrations in order.
9. Record successful rows and audit events.
10. Re-read and verify final database version and checksums.
11. Release the advisory lock.

Remaining runner hardening: precondition and postcondition verification is not
yet migration-specific for every future data migration, and full schema drift
verification is currently table/index presence rather than complete object
fingerprinting.

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

Startup schema creation has been replaced with a compatibility assertion.

Current startup sequence:

1. Create database connection layer.
2. Run `MigrationRunner.assertCompatible` through `InitDB.program`.
3. Run protocol startup checks.
4. Seed or verify runtime protocol dependencies as already designed.
5. Start HTTP server and background fibers.

`assertCompatible` currently:

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
- Verify that the runtime can introspect the expected node-owned tables and
  indexes for the applied version. A matching migration ledger is necessary but
  not sufficient if the live schema has drifted.
- Still needs deeper introspection of columns, constraints, foreign keys, check
  expressions, and index definitions.
- Logs a success summary with `expected_version`, `actual_version`, and
  `manifest_hash`.
- Fails with a typed migration error before `listen.ts` starts serving.
- Still needs richer structured failure logs and metrics.

Migration compatibility failure codes:

```text
schema_unversioned_database
schema_version_behind expected=6 actual=5
schema_version_ahead expected=6 actual=7
schema_checksum_mismatch version=4
schema_migration_in_progress
schema_drift_detected object=mempool_ledger
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

Commands now available:

- `midgard-node db:migrate`
  - Applies pending migrations explicitly.
- `midgard-node db:status`
  - Prints database version, expected version, pending migrations, unknown
    migrations, and checksum status.
  - Current output is JSON-formatted status suitable for automation.
- `midgard-node db:verify`
  - Performs the same compatibility check as startup without starting the node.
- `midgard-node db:checksum`
  - Prints the compiled manifest hash and per-migration checksums.

Local development convenience:

- A reset command may drop and recreate a local development database only when
  explicitly named as destructive.
- Test helpers must run migrations for fresh test databases instead of calling
  table `createTable` functions. The main database suite and deposit-flow
  emulator setup now call `MigrationRunner.migrate`.
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

Current coverage includes SQL-statement splitter tests and test-suite setup that
creates schema through `MigrationRunner.migrate`. The comprehensive runner,
startup, shape, CLI, and packaging tests below still need to be completed.

Runner tests:

- Empty database applies migrations `0001` through `0006` and records version
  `6`.
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

- Migrations `0001` through `0006` produce the current version `6` application
  schema.
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

1. Landed: inventory and encode schema in explicit migrations `0001` through
   `0006`.
2. Landed: add migration metadata tables and runner.
3. Landed: add `db:migrate`, `db:status`, `db:verify`, and checksum tooling.
4. Landed: replace `InitDB.program` startup table creation with
   `assertCompatible`.
5. Landed: update primary test setup paths to create schema through migrations.
6. Remove or stop exporting application `createTable` helpers from runtime paths,
   or clearly fence them as non-startup legacy helpers until removed.
7. Deepen startup drift detection beyond table/index presence.
8. Add CI checks for manifest ordering, checksum correctness, and immutable old
   migrations.
9. Add packaged-artifact checks for migration SQL availability in `dist`.
10. Add migration metrics and readiness schema metadata.
11. Document operator migration procedure in the node README or operations guide.
12. For local demo databases without a migration ledger, require explicit reset
    and migration. Do not auto-baseline.
13. For any persistent deployment, take backup, run `db:status`, run
    `db:migrate`, run `db:verify`, then start the node.
14. Monitor startup logs, metrics, readiness schema fields, and migration audit
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

- [x] Add migration SQL directory and manifest.
- [x] Encode schema migrations `0001` through `0006`.
- [x] Add checksum generation or verification tooling.
- [x] Add `schema_migrations` ledger table.
- [x] Add `schema_migration_events` audit table.
- [x] Implement advisory-lock protected migration runner.
- [x] Implement startup `assertCompatible`.
- [x] Replace `InitDB.program` schema creation in `runNode`.
- [x] Add CLI commands for migrate, status, verify, and checksum.
- [x] Update primary database test setup paths to use migrations.
- [ ] Add comprehensive runner unit and integration tests.
- [ ] Add startup compatibility tests.
- [ ] Add schema introspection tests for the effective version `0006` schema.
- [ ] Add CI checks for manifest ordering and checksums.
- [ ] Add packaged-artifact checks for migration SQL availability in `dist`.
- [ ] Add metrics and readiness schema metadata. Structured success logs and JSON
      status/checksum output are present, but dashboard-grade observability is
      not complete.
- [ ] Document operator backup and migration procedure.
- [ ] Remove or fence remaining table `createTable` helpers from runtime paths.
- [ ] Confirm startup fails closed for unversioned, behind, ahead, checksum
      mismatch, migration-in-progress, empty-unmigrated, and drifted databases.
- [x] Confirm no normal startup path mutates application schema.

## Acceptance Criteria

This blocker is complete when:

- A fresh database can be initialized only by running versioned migrations.
- Node startup refuses to serve unless the database is at the binary's expected
  schema version with matching checksums and a fully verified schema shape.
- The migration ledger records every successful migration.
- Migration attempts and failures are auditable.
- Existing ad hoc application table creation is removed from normal startup.
- Tests cover migration ordering, checksum enforcement, startup failure modes,
  current schema shape, drift detection, packaged SQL availability, and
  observability outputs.
- Operator documentation explains backup, migration, verification, and recovery
  steps.
