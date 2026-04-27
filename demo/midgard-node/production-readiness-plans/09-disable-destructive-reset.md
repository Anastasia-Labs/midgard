# P0 Blocker 9: Remove Or Hard-Disable Destructive Reset From Production

## Problem Statement

The node still contains an online reset workflow reachable from the production
HTTP router. The on-chain reset branch currently fails closed, but the same
workflow still includes local state deletion for every core SQL table and both
persistent MPT stores. A production L2 must not expose a live endpoint whose
purpose is to wipe local ledger state, pause protocol workers, and reset
process-global recovery variables.

The production fix is to remove `/reset` from the production route graph and
delete the destructive reset workflow from runtime services. Recovery must be
performed through narrow, offline, auditable commands that verify L1 evidence,
SQL state, MPT roots, schema version, and operator intent before applying any
mutation. Development-only state reset, if retained at all, must be outside the
production server, explicitly named unsafe, impossible to invoke accidentally,
and unavailable on persistent or production networks.

This plan implements the readiness report finding:
[`PRODUCTION_READINESS_REPORT.md:674`](../PRODUCTION_READINESS_REPORT.md#L674)
to
[`PRODUCTION_READINESS_REPORT.md:755`](../PRODUCTION_READINESS_REPORT.md#L755).

## Current Behavior

The readiness report identifies the issue directly:

- The on-chain UTxO reset branch is disabled:
  [`PRODUCTION_READINESS_REPORT.md:678`](../PRODUCTION_READINESS_REPORT.md#L678)
  and
  [`src/reset.ts:218`](../src/reset.ts#L218).
- The local DB/MPT wipe still exists:
  [`PRODUCTION_READINESS_REPORT.md:693`](../PRODUCTION_READINESS_REPORT.md#L693)
  and
  [`src/reset.ts:233`](../src/reset.ts#L233).
- The route is exposed as `GET /reset`:
  [`PRODUCTION_READINESS_REPORT.md:708`](../PRODUCTION_READINESS_REPORT.md#L708)
  and
  [`src/commands/listen-router.ts:655`](../src/commands/listen-router.ts#L655).
- The recommended production direction is route removal plus offline recovery
  tools:
  [`PRODUCTION_READINESS_REPORT.md:738`](../PRODUCTION_READINESS_REPORT.md#L738).

Relevant code paths:

- `src/reset.ts` imports every destructive table helper and MPT deletion helper:
  [`src/reset.ts:25`](../src/reset.ts#L25) to
  [`src/reset.ts:40`](../src/reset.ts#L40).
- `resetUTxOs` always fails with `cause: "reset-disabled"` because deinit is not
  implemented for deployed contracts:
  [`src/reset.ts:218`](../src/reset.ts#L218) to
  [`src/reset.ts:228`](../src/reset.ts#L228).
- `resetDatabases` still clears `mempool`, `mempool_ledger`,
  `processed_mempool`, `blocks`, `immutable`, `latest_ledger`,
  `confirmed_ledger`, `address_history`, `deposits_utxos`,
  `deposit_ingestion_cursor`, pending block finalization tables,
  `tx_rejections`, and deletes the mempool and ledger MPT stores:
  [`src/reset.ts:233`](../src/reset.ts#L233) to
  [`src/reset.ts:255`](../src/reset.ts#L255).
- `Reset.program` sets `RESET_IN_PROGRESS`, runs `resetUTxOs` with retries in
  parallel with `resetDatabases`, then resets in-memory state and heartbeat refs:
  [`src/reset.ts:261`](../src/reset.ts#L261) to
  [`src/reset.ts:303`](../src/reset.ts#L303).
- `/reset` is declared as a router endpoint constant:
  [`src/commands/listen-router.ts:72`](../src/commands/listen-router.ts#L72).
- `getResetHandler` logs a reset request, runs `Reset.program`, and returns a
  success message if the effect completes:
  [`src/commands/listen-router.ts:659`](../src/commands/listen-router.ts#L659)
  to
  [`src/commands/listen-router.ts:678`](../src/commands/listen-router.ts#L678).
- `buildListenRouter` registers `GET /reset` under admin-key authorization:
  [`src/commands/listen-router.ts:1101`](../src/commands/listen-router.ts#L1101)
  to
  [`src/commands/listen-router.ts:1104`](../src/commands/listen-router.ts#L1104).
- Admin authorization lists `/reset` as an admin route:
  [`src/commands/listen-utils.ts:14`](../src/commands/listen-utils.ts#L14) to
  [`src/commands/listen-utils.ts:22`](../src/commands/listen-utils.ts#L22).
- Admin authorization can protect access only after the route exists; it does
  not make an online destructive state wipe production-safe:
  [`src/commands/listen-router.ts:106`](../src/commands/listen-router.ts#L106)
  to
  [`src/commands/listen-router.ts:130`](../src/commands/listen-router.ts#L130).

Destructive helper behavior:

- `clearTable` runs `TRUNCATE TABLE ... CASCADE` for any logical table adapter:
  [`src/database/utils/common.ts:38`](../src/database/utils/common.ts#L38) to
  [`src/database/utils/common.ts:54`](../src/database/utils/common.ts#L54).
- `MempoolDB.clear` truncates `mempool` and clears mempool tx deltas:
  [`src/database/mempool.ts:134`](../src/database/mempool.ts#L134) to
  [`src/database/mempool.ts:138`](../src/database/mempool.ts#L138).
- Pending block finalization `clear` truncates the pending journal parent and
  member tables:
  [`src/database/pendingBlockFinalizations.ts:458`](../src/database/pendingBlockFinalizations.ts#L458)
  to
  [`src/database/pendingBlockFinalizations.ts:465`](../src/database/pendingBlockFinalizations.ts#L465).
- `DepositsDB.clear` exposes whole-table deposit deletion:
  [`src/database/deposits.ts:656`](../src/database/deposits.ts#L656).
- `deleteMempoolMpt` and `deleteLedgerMpt` read paths from config and call
  `deleteMpt`:
  [`src/workers/utils/mpt.ts:146`](../src/workers/utils/mpt.ts#L146) to
  [`src/workers/utils/mpt.ts:154`](../src/workers/utils/mpt.ts#L154).
- `deleteMpt` removes the LevelDB path with `FS.rmSync(path, { recursive: true,
force: true })`:
  [`src/workers/utils/mpt.ts:156`](../src/workers/utils/mpt.ts#L156) to
  [`src/workers/utils/mpt.ts:167`](../src/workers/utils/mpt.ts#L167).
- `MidgardMpt.delete()` closes a backing database and then deletes its backing
  store:
  [`src/workers/utils/mpt.ts:706`](../src/workers/utils/mpt.ts#L706) to
  [`src/workers/utils/mpt.ts:721`](../src/workers/utils/mpt.ts#L721).

Reset-related process state and worker behavior:

- `Globals` includes `RESET_IN_PROGRESS`, described as development-only
  coordination around spending all state-queue UTxOs:
  [`src/services/globals.ts:24`](../src/services/globals.ts#L24) to
  [`src/services/globals.ts:26`](../src/services/globals.ts#L26).
- Block commitment skips work while `RESET_IN_PROGRESS` is true:
  [`src/fibers/block-commitment.ts:257`](../src/fibers/block-commitment.ts#L257)
  to
  [`src/fibers/block-commitment.ts:263`](../src/fibers/block-commitment.ts#L263).
- Block confirmation returns early while `RESET_IN_PROGRESS` is true:
  [`src/fibers/block-confirmation.ts:184`](../src/fibers/block-confirmation.ts#L184)
  to
  [`src/fibers/block-confirmation.ts:190`](../src/fibers/block-confirmation.ts#L190).
- Merge returns early if `RESET_IN_PROGRESS` is true:
  [`src/transactions/state-queue/merge-to-confirmed-state.ts:489`](../src/transactions/state-queue/merge-to-confirmed-state.ts#L489)
  to
  [`src/transactions/state-queue/merge-to-confirmed-state.ts:499`](../src/transactions/state-queue/merge-to-confirmed-state.ts#L499).
- `getLogGlobalsHandler` exposes the reset flag for debug inspection:
  [`src/commands/listen-router.ts:827`](../src/commands/listen-router.ts#L827)
  and
  [`src/commands/listen-router.ts:867`](../src/commands/listen-router.ts#L867).

Startup and recovery context:

- Startup initializes database tables, performs protocol checks, hydrates pending
  finalization state, and then starts the HTTP server:
  [`src/commands/listen.ts:69`](../src/commands/listen.ts#L69) to
  [`src/commands/listen.ts:119`](../src/commands/listen.ts#L119).
- Startup hydrates active pending block finalization journals into global refs:
  [`src/commands/listen-startup.ts:211`](../src/commands/listen-startup.ts#L211)
  to
  [`src/commands/listen-startup.ts:259`](../src/commands/listen-startup.ts#L259).
- The CLI is built with `commander`, with `listen` registered as a normal
  command:
  [`src/index.ts:35`](../src/index.ts#L35) and
  [`src/index.ts:238`](../src/index.ts#L238) to
  [`src/index.ts:251`](../src/index.ts#L251).
- Existing operator-facing audit command wiring already follows this CLI shape:
  [`src/index.ts:640`](../src/index.ts#L640) to
  [`src/index.ts:668`](../src/index.ts#L668).
- Existing mutating CLI commands are not a substitute for the replacement
  recovery surface. `project-deposits-once` currently mutates local deposit and
  mempool-ledger state through normal node services:
  [`src/index.ts:622`](../src/index.ts#L622) to
  [`src/index.ts:637`](../src/index.ts#L637). `audit-blocks-immutable --repair`
  can also mutate database state:
  [`src/index.ts:640`](../src/index.ts#L640) to
  [`src/index.ts:668`](../src/index.ts#L668). Any retained production mutating
  operator command must be brought under the same offline, advisory-lock,
  schema-check, precondition, postcondition, and audit requirements below.

Related readiness plans:

- P0 blocker 2 requires destructive recovery operations and MPT rebuilds to be
  explicit operator commands with durable audit records:
  [`02-atomic-recoverable-ledger-mutations.md:217`](./02-atomic-recoverable-ledger-mutations.md#L217).
- P0 blocker 2 explicitly warns that whole-table reset remains dangerous and
  should be isolated:
  [`02-atomic-recoverable-ledger-mutations.md:950`](./02-atomic-recoverable-ledger-mutations.md#L950)
  to
  [`02-atomic-recoverable-ledger-mutations.md:952`](./02-atomic-recoverable-ledger-mutations.md#L952).
- P0 blocker 4 already defines MPT rebuild as an explicit dry-run/apply recovery
  command with audit manifests:
  [`04-startup-fail-closed-integrity.md:493`](./04-startup-fail-closed-integrity.md#L493)
  to
  [`04-startup-fail-closed-integrity.md:507`](./04-startup-fail-closed-integrity.md#L507).
- P0 blocker 5 allows local development reset only when explicitly named as
  destructive:
  [`05-versioned-schema-migrations.md:458`](./05-versioned-schema-migrations.md#L458)
  to
  [`05-versioned-schema-migrations.md:464`](./05-versioned-schema-migrations.md#L464).

## Target Invariants

1. Production startup must not register any HTTP route that can delete SQL rows,
   truncate tables, remove MPT backing stores, reset recovery journals, or reset
   process-global protocol state.
2. `GET /reset` must not exist in the production route graph. A request to
   `/reset` must return the same unknown-route behavior as any unsupported path,
   without invoking reset code or authorization-specific side effects.
3. Admin-key authorization must not be treated as sufficient protection for
   destructive operations. Online destructive recovery is prohibited even for
   authenticated administrators.
4. Normal recovery must never depend on whole-table truncation, MPT directory
   deletion, or process-global zeroing.
5. Any state-mutating recovery must be offline: the HTTP listener and workers are
   stopped, the command obtains the node-wide PostgreSQL advisory lock, and the
   command proves it is the sole writer before mutation.
6. Recovery tools must be narrow. Each tool mutates one explicit domain, verifies
   preconditions, supports dry-run where meaningful, emits durable audit records,
   and is idempotent or fail-closed on replay.
7. MPT rebuild must be an auditable backup/swap operation. It must never call
   `rm -rf` on the active store as a first step.
8. Persistent SQL history, finalization journals, deposit records, admission
   records, and audit records must not be deleted as a convenience recovery path.
9. Development-only destructive reset, if retained, must be unavailable in
   production builds and persistent-network configs, must require explicit
   command-line acknowledgements, and must never be reachable through `listen`.
10. No compatibility shim, alias route, hidden toggle, legacy reset path, or
    fallback reset behavior may be added for `demo/midgard-node`.

## Route Removal And Production Gating

### Remove The Production HTTP Route

Implementation should remove the reset route completely from the server:

1. Delete `RESET_ENDPOINT` from `listen-router.ts` if it has no remaining
   production use.
2. Delete `getResetHandler`.
3. Remove the `RESET_ENDPOINT` route registration from `buildListenRouter`.
4. Remove the `Reset` import from `listen-router.ts`.
5. Remove `/reset` from `ADMIN_ROUTE_PATHS` in `listen-utils.ts`.
6. Add a router test proving `GET /reset` is not registered and does not call
   `Reset.program`.

Do not replace the route with a 403, 404 handler, compatibility route, or
feature-flagged handler. A named denial endpoint still advertises a production
reset surface and invites future reactivation.

### Remove Runtime Reset Coupling

Implementation should remove reset-specific runtime coordination from production
workers:

1. Remove `RESET_IN_PROGRESS` from `Globals` once no production path can set it.
2. Remove reset skip branches from block commitment, block confirmation, and
   merge. These workers should be controlled by durable recovery/readiness state,
   startup fail-closed gates, and advisory locks, not a reset-specific in-memory
   flag.
3. Remove `RESET_IN_PROGRESS` from `getLogGlobalsHandler`.
4. Keep any necessary operational pause semantics under the durable recovery
   model from P0 blockers 2 and 4, using explicit recovery states rather than a
   reset flag.

If other in-progress work still references `RESET_IN_PROGRESS`, do not preserve
it as a compatibility shim. Replace each use with the production recovery state
that actually describes the condition being guarded.

### Delete Or Quarantine `src/reset.ts`

The preferred production implementation is to delete `src/reset.ts` after route
removal and after tests prove no runtime import remains.

If development reset must be retained temporarily, move it out of runtime code
into a clearly named dev-only module, for example
`src/commands/unsafe-dev-reset.ts`, and enforce all of these constraints:

- The command is registered only when `NODE_ENV === "development"` or
  `NODE_ENV === "test"`.
- The production CLI entrypoint must not statically import the unsafe module.
  Prefer a separate development entrypoint or a dynamic import guarded before
  module loading, so production bundles and import-graph tests cannot reach the
  destructive code.
- The command refuses `NETWORK=mainnet`, `NETWORK=preprod`, and any config that
  marks the database or MPT paths as persistent.
- The command name includes `unsafe-dev-reset`.
- It requires an exact acknowledgement flag such as
  `--i-understand-this-deletes-local-state`.
- It requires a second selector proving the target is disposable, such as
  `--allow-disposable-database-name midgard_dev_*` or an explicit ephemeral
  data-root marker file.
- It is excluded from production package scripts, production docs, and recovery
  runbooks.
- It writes a local audit log even in development.

This dev command must not be used to satisfy production recovery requirements.
Production recovery must be built from the replacement tools below.

## Replacement Offline Recovery Tooling

The replacement surface should be command-line only, explicit, auditable, and
composable with the startup integrity work. Commands should live under
`src/commands/` and be wired in `src/index.ts` using the existing `commander`
style.

### Read-Only Inspection Commands

Add read-only commands first so operators can diagnose without mutating state:

- `midgard-node verify-state --json`
  - Runs schema compatibility checks, SQL cross-table invariants, pending
    finalization inspection, MPT root checks, deposit projection checks, and L1
    state-queue root comparison.
  - Exits non-zero on any invariant failure.
  - Emits stable machine-readable evidence and recommended recovery commands.
- `midgard-node mpt status --json`
  - Prints configured MPT paths, active roots, root manifest rows, filesystem
    presence, root persistence status, and SQL-derived expected roots.
- `midgard-node pending-finalization status --json`
  - Prints active pending journal state, submitted tx hash if known, header hash,
    observed L1 evidence, and whether local finalization can be retried.
- `midgard-node deposits verify-projection --json`
  - Verifies deposit ingestion cursor, deposit statuses, projected header
    assignments, and mempool-ledger projection consistency.
- `midgard-node audit-blocks-immutable --json`
  - Extend the existing command so it can run in read-only JSON mode.

Read-only commands may run while the HTTP node is stopped or started, but their
output must clearly state whether they observed a moving system. State-mutating
commands below must require the node to be offline.

### Mutating Recovery Commands

Add narrow mutating commands that replace reset use cases:

- `midgard-node mpt rebuild --trie <mempool|ledger> --from-sql --dry-run --write-audit <path>`
  - Recomputes the trie from SQL, writes an audit manifest, and applies no
    mutation.
- `midgard-node mpt rebuild --trie <mempool|ledger> --from-sql --apply --audit <path>`
  - Requires a prior dry-run audit manifest, verifies it still matches current
    SQL/L1 evidence, builds a new store under a staging path, fsyncs, atomically
    swaps the active pointer or directory, and retains the previous store under a
    timestamped quarantine path.
- `midgard-node pending-finalization recover-local --header-hash <hex>`
  - Completes local finalization only when L1 proves the submitted block is
    canonical and the journal payload matches the confirmed header.
- `midgard-node pending-finalization abandon --header-hash <hex> --evidence <path>`
  - Marks a pending journal abandoned only when the evidence proves the header is
    not canonical or cannot be recovered safely.
- `midgard-node deposits project-once --strict --verify`
  - Applies deterministic deposit projection for due deposits, verifies the
    result, and records the exact cursor/header boundary.
- `midgard-node tx-admission replay --from-durable-log --limit <n>`
  - Requeues or advances durable admitted transactions without deleting
    processed history. This depends on P0 blocker 1.
- `midgard-node local-ledger recover-jobs --job-id <id|all> --apply`
  - Runs the P0 blocker 2 recovery state machine for unfinished local mutation
    jobs.

Every mutating command must:

1. Refuse to run while the HTTP listener is active for the same database/MPT
   paths. Prefer an advisory lock plus process identity marker over best-effort
   port checks.
2. Acquire the same node-wide PostgreSQL advisory lock used by startup recovery
   and migrations.
3. Verify schema version and migration manifest before reading application
   tables.
4. Verify configured network, contract identifiers, and MPT paths match the
   audit manifest or command evidence.
5. Write a durable `started` audit event before mutation.
6. Apply mutation in a transaction or journaled state machine.
7. Verify postconditions before writing `succeeded`.
8. Write `failed` or `quarantined` audit events on error with enough evidence to
   resume or escalate.
9. Exit non-zero unless postconditions are proven.

Existing mutating commands must be handled explicitly:

- Replace or wrap `project-deposits-once` with `deposits project-once --strict
  --verify`; the old top-level command must either be removed from production
  help or made an alias to the audited command without changing semantics.
- Split `audit-blocks-immutable --repair` into read-only audit and audited
  offline repair behavior, or make `--repair` acquire the recovery lock, emit
  operator audit events, and prove postconditions before it can mutate.
- Add CLI tests proving no production mutating command bypasses the shared
  recovery guard.

## Audit Records

Add a general append-only operator audit table through the versioned migration
system from P0 blocker 5. If blocker 5 is not merged yet, this implementation
must include an explicit migration file and must not create the table implicitly
at normal startup.

Suggested table: `operator_recovery_events`.

Required columns:

- `event_id UUID PRIMARY KEY`
- `operation_id UUID NOT NULL`
- `event_type TEXT NOT NULL`
- `command_name TEXT NOT NULL`
- `command_args_hash TEXT NOT NULL CHECK (command_args_hash ~ '^[0-9a-f]{64}$')`
- `operator_identity TEXT NOT NULL`
- `network TEXT NOT NULL`
- `database_fingerprint TEXT NOT NULL`
- `schema_version BIGINT NOT NULL`
- `schema_manifest_hash TEXT NOT NULL`
- `contract_bundle_hash TEXT NOT NULL`
- `mpt_manifest_before JSONB NOT NULL`
- `mpt_manifest_after JSONB`
- `l1_evidence JSONB NOT NULL DEFAULT '{}'::jsonb`
- `precondition_report JSONB NOT NULL`
- `postcondition_report JSONB`
- `status TEXT NOT NULL`
- `failure_code TEXT`
- `failure_detail TEXT`
- `created_at TIMESTAMPTZ NOT NULL DEFAULT now()`

Required indexes:

- `(operation_id, created_at)`
- `(command_name, created_at)`
- `(status, created_at)`
- `(failure_code, created_at)` where `failure_code IS NOT NULL`

Required event types:

- `recovery_started`
- `recovery_preconditions_verified`
- `recovery_mutation_applied`
- `recovery_postconditions_verified`
- `recovery_succeeded`
- `recovery_failed`
- `recovery_quarantined`
- `unsafe_dev_reset_started` and `unsafe_dev_reset_finished` only if a dev-only
  command is retained.

The audit stream must be append-only. Do not update old audit rows except through
an explicit forward migration that preserves the old data.

Route absence is a code and deployment property, not a database recovery event.
Prove it with static import/route tests, CI guards, and deployment smoke checks.
If a deployment marker is recorded in the audit table, it is informational only
and must not delay removing the HTTP route.

## Operator Workflow

### Normal Production Recovery

1. Stop the node process and verify no other node instance is serving the same
   database and MPT paths.
2. Take a database backup and a filesystem snapshot or copy of both MPT stores.
3. Run `midgard-node db:status` and `midgard-node db:verify` once P0 blocker 5 is
   available.
4. Run `midgard-node verify-state --json > verify-state.before.json`.
5. If the failure is an MPT drift:
   - run `midgard-node mpt rebuild --trie <name> --from-sql --dry-run --write-audit <path>`;
   - review old root, computed root, SQL fingerprint, L1 root evidence, and
     affected row counts;
   - run the matching `--apply --audit <path>` command;
   - run `verify-state` again.
6. If the failure is pending local finalization:
   - run `pending-finalization status --json`;
   - recover only when L1 proves the block is canonical;
   - abandon only with explicit non-canonical evidence.
7. If the failure is deposit projection:
   - run read-only projection verification;
   - run strict projection once;
   - verify the cursor and resulting ledger rows.
8. Restart the node only after `verify-state` exits successfully.
9. Check `/readyz`, startup logs, and recovery audit rows.

### Development-Only Disposable Reset

If retained, the dev reset workflow is not a production runbook:

```text
NODE_ENV=development midgard-node unsafe-dev-reset \
  --i-understand-this-deletes-local-state \
  --allow-disposable-data-root <path>
```

The command must refuse production-like networks and persistent data roots. It
must tell the operator to use migrations and recovery commands for any persistent
deployment.

## Tests

### Static And Unit Tests

- Import graph test: production modules must not import `src/reset.ts` or any
  unsafe dev reset module.
- Production CLI import test: `src/index.ts` and production package entrypoints
  must not statically import `unsafe-dev-reset`.
- Router test: `buildListenRouter` must not register `/reset`.
- Admin-route test: `ADMIN_ROUTE_PATHS` must not contain `/reset`.
- CLI help test: production `midgard-node --help` must not list `reset` or
  `unsafe-dev-reset` unless explicitly running in a development/test mode.
- Globals test: `Globals` must not expose `RESET_IN_PROGRESS` after production
  removal.
- Worker tests: block commitment, block confirmation, and merge behavior must be
  controlled by durable recovery state, not reset flags.
- Destructive helper scan: production code must not call `clearTable` for whole
  application reset or `deleteMpt` against active stores outside audited recovery
  modules.

### Integration Tests

- `GET /reset` returns the generic unknown-route response and leaves SQL tables,
  MPT directories, and globals unchanged.
- A request to `/reset` with a valid admin key still cannot invoke any reset
  behavior.
- Starting the production listener and then running a mutating recovery command
  against the same database fails before mutation because the node is online or
  the advisory lock is unavailable.
- Existing mutating CLI commands, including deposit projection and immutable
  block repair if retained, fail before mutation unless the shared offline
  recovery guard, schema checks, and audit writer are active.
- `mpt rebuild --dry-run` writes an audit manifest and does not mutate active MPT
  stores.
- `mpt rebuild --apply` refuses if the audit manifest no longer matches current
  SQL/L1 evidence.
- `mpt rebuild --apply` preserves the old store in quarantine and updates only
  after postcondition verification.
- Pending-finalization recovery succeeds only with matching L1 header evidence.
- Pending-finalization abandon refuses without explicit non-canonical evidence.
- Deposit projection recovery is idempotent across repeated runs.
- Every mutating recovery command writes `started`, terminal, and postcondition
  audit rows.

### Failure-Injection Tests

- Crash after writing `recovery_started` but before mutation: rerun resumes or
  fails closed with an existing in-progress audit event.
- Crash after staging an MPT rebuild but before promotion: startup ignores the
  staging path and the recovery command can clean or resume it.
- Crash after promotion but before `recovery_succeeded`: startup detects root
  manifest/audit mismatch and requires recovery verification, not reset.
- Database lock contention: a second recovery command fails without mutation.
- Corrupt audit manifest: `--apply` fails before touching active state.

### CI Guards

- Add a grep-style CI guard that fails on reset route registration or `"/reset"`
  in production router/admin route code.
- Add a guard that flags `resetDatabases`, `resetUTxOs`, and `Reset.program`
  exports if `src/reset.ts` is retained.
- Add a guard that flags `.complete({ localUPLCEval: false })` as already
  required by repository policy.
- Add targeted tests to the default `pnpm test` path and keep longer recovery
  crash tests in a named integration suite if runtime is high.

## Rollout Steps

1. Remove `/reset` from `listen-router.ts` and `/reset` from admin route paths.
   This is the immediate blocker fix and must not wait for replacement recovery
   commands.
2. Delete `src/reset.ts` or move any retained disposable cleanup code to a
   dev-only command with all hard gates and no production static import.
3. Remove `RESET_IN_PROGRESS` from `Globals`, logs, and worker branches.
4. Add unit, integration, static, and CI guard tests proving reset cannot be
   reached or reintroduced.
5. Add the audit migration for operator recovery events, or sequence after P0
   blocker 5 if migrations land first.
6. Add read-only state verification commands.
7. Add MPT status and dry-run rebuild command.
8. Add MPT apply rebuild with staging, backup/quarantine, advisory lock, and
   audit records.
9. Add pending-finalization and deposit projection recovery commands, or link to
   the implementations from P0 blockers 2 and 4 if they land first.
10. Replace or wrap existing mutating operator commands so none bypass the
    shared offline recovery guard and audit stream.
11. Update operator docs to replace reset instructions with verification and
    recovery workflows.
12. Run a deployment smoke test proving `/reset` is absent and normal startup,
    `/readyz`, `/submit`, commit, confirmation, and merge still work.
13. During rollout, monitor startup integrity failures, recovery audit events,
    MPT root metrics, and unknown-route hits for `/reset`.

Rollback must not reintroduce `/reset`, `src/reset.ts` production imports, or
`RESET_IN_PROGRESS`. If the first deployment has an unrelated regression, roll
forward with a targeted fix or revert only unrelated changes while preserving
the reset removal.

## Risks And Mitigations

- Risk: operators used `/reset` as a broad recovery escape hatch.
  Mitigation: ship read-only diagnostics before removing any documented recovery
  path, and provide narrow commands for the actual failure classes.
- Risk: delaying route removal behind replacement tooling leaves the P0 online
  destructive surface exposed.
  Mitigation: remove the HTTP route, production imports, and reset flag first;
  deliver replacement recovery tooling as separate audited CLI work.
- Risk: deleting the reset flag changes worker scheduling behavior.
  Mitigation: replace reset gating with durable recovery/readiness gates from P0
  blockers 2 and 4, and add worker tests for pending recovery states.
- Risk: retained development reset code leaks into the production binary through
  static imports or package scripts.
  Mitigation: keep unsafe reset in a separate dev-only entrypoint or guarded
  dynamic import and add import-graph/package help tests.
- Risk: existing mutating CLI commands become unofficial reset replacements.
  Mitigation: remove, wrap, or split them so production mutation always uses the
  shared offline recovery guard and append-only audit stream.
- Risk: dev/test environments still need disposable cleanup.
  Mitigation: provide a dev-only command with explicit acknowledgement and
  disposable-data-root checks, never an HTTP route.
- Risk: MPT rebuild apply can lose the previous store if implemented as direct
  deletion.
  Mitigation: require staging, atomic promotion, quarantine retention, and
  postcondition verification before marking success.
- Risk: audit records become incomplete if the process crashes mid-command.
  Mitigation: write `started` before mutation, use operation ids, and make rerun
  detect incomplete operations.
- Risk: multiple node instances or an online node race recovery commands.
  Mitigation: use PostgreSQL advisory locks and an explicit online-node marker;
  do not rely only on ports or process names.

## Open Questions

- What exact advisory lock key should be shared by startup integrity, migrations,
  local ledger recovery, and these recovery commands?
- Where should immutable operator audit manifests be retained outside the
  database: local filesystem, object storage, or both?
- How should operator identity be supplied in non-interactive deployments:
  environment variable, signed manifest, OS user, or deployment identity?
- Should dev-only `unsafe-dev-reset` be compiled into the published binary but
  hidden behind runtime gates, or excluded from production packaging entirely?
  Excluding it from production packaging is safer.
- What retention period should apply to quarantined MPT stores after successful
  rebuild?
- Which P0 blocker owns the final shared `verify-state` command if blockers 2,
  4, 5, and 9 are implemented in parallel?

## Concrete Checklist

- [ ] Remove `GET /reset` from `buildListenRouter`.
- [ ] Remove `/reset` from `ADMIN_ROUTE_PATHS`.
- [ ] Remove `getResetHandler` and reset endpoint constants/imports from
      production router code.
- [ ] Delete `src/reset.ts` or move any retained cleanup code to a dev-only
      `unsafe-dev-reset` command with hard gates.
- [ ] Prove production CLI entrypoints do not statically import any unsafe dev
      reset module.
- [ ] Remove `RESET_IN_PROGRESS` from `Globals`.
- [ ] Remove reset gating from block commitment, block confirmation, and merge.
- [ ] Remove reset state from debug/global logging.
- [ ] Add `operator_recovery_events` migration and repository helpers.
- [ ] Add read-only `verify-state`, `mpt status`, pending-finalization status,
      and deposit projection verification commands.
- [ ] Add audited MPT rebuild dry-run and apply commands.
- [ ] Add pending-finalization recovery/abandon commands if not already provided
      by P0 blocker 2 or 4.
- [ ] Add strict deposit projection recovery command if not already provided by
      P0 blocker 4.
- [ ] Remove, wrap, or split existing mutating operator commands so deposit
      projection and immutable block repair cannot bypass recovery guards.
- [ ] Add advisory-lock enforcement for every mutating recovery command.
- [ ] Add static tests proving no production route or import exposes reset.
- [ ] Add integration tests proving `/reset` cannot mutate state, even with an
      admin key.
- [ ] Add recovery command tests for dry-run, apply, audit records, lock
      contention, stale manifests, and crash recovery.
- [ ] Update README/operator runbooks to remove reset instructions and document
      offline recovery workflows.
- [ ] Add rollout smoke checks for route absence, startup integrity, readiness,
      submit, commit, confirmation, merge, and recovery audit visibility.

## Definition Of Done

This blocker is complete when:

- No production HTTP route, admin route, handler, or router constant exposes
  reset.
- No production runtime import can call a whole-node reset workflow.
- Production workers no longer depend on `RESET_IN_PROGRESS`.
- Any retained disposable reset is dev/test-only, explicitly unsafe, and
  impossible to reach through `listen`.
- Operators have offline recovery commands that cover MPT rebuild, pending local
  finalization, deposit projection, and durable admission replay without
  deleting unrelated state.
- Every mutating recovery command is guarded by advisory locks, schema checks,
  precondition verification, postcondition verification, and durable audit
  records.
- Tests and CI guards fail if `/reset` or production reset exports are
  reintroduced.
