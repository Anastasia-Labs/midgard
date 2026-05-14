# Empty Ledger MPF Authority Revision Plan

Status: refreshed on 2026-05-13 against the current `demo/midgard-node`
implementation. The historical file name still contains `mpt`, but the runtime
code now uses MPF naming and the `src/workers/utils/mpf.ts` module.

Landed since the earlier draft:

- MPF naming and paths are canonical in code: `LEDGER_MPF_DB_PATH` and
  `TRANSACTIONS_MPF_DB_PATH`.
- Deprecated root-store environment names are rejected at config load:
  `LEDGER_MPT_DB_PATH`, `MEMPOOL_MPT_DB_PATH`, and `MEMPOOL_MPF_DB_PATH` are not
  compatibility aliases.
- The MPF wrapper normalizes the local empty root to the canonical Midgard empty
  root, `SDK.EMPTY_MERKLE_TREE_ROOT`.
- Unit tests cover canonical MPF empty-root initialization, delete-to-empty
  normalization, persistence, and corrupt persisted root markers.
- The old HTTP `/reset` route and `src/reset.ts` path are gone from the current
  route graph.

Still open:

- `makeMpfs` still treats an empty ledger MPF as "insert configured
  `GENESIS_UTXOS`".
- Startup does not yet classify an empty ledger MPF against L1 authority, SQL
  state, local journals, or durable local-state metadata.
- `local_state_metadata`, explicit adoption, MPF rebuild, and MPF integrity
  readiness state are not implemented.

## Goal

Revise `makeMpfs` and startup sequencing so the node does not silently mutate a
persistent ledger MPF merely because `ledgerMpf.rootIsEmpty()` is true. Empty is a
valid Midgard ledger root when authoritative state proves the current L2
pre-state is empty. Empty is also a corruption signal when SQL, local journals,
or L1 state prove prior execution.

Normal startup must never seed, rebuild, delete, or rewrite durable state as a
side effect of opening MPF handles. `GENESIS_UTXOS` may be used only by an
explicit non-mainnet bootstrap path with durable metadata and root verification.

## Current Code Snapshot

`makeMpfs` opens the transactions and ledger MPF stores from configured paths:
[`src/workers/utils/mpf.ts` lines 155-168](../src/workers/utils/mpf.ts#L155-L168).
It then checks whether the ledger root is empty and, if so, inserts
`nodeConfig.GENESIS_UTXOS`:
[`src/workers/utils/mpf.ts` lines 169-185](../src/workers/utils/mpf.ts#L169-L185).
That branch is not gated by SQL state, L1 state, startup policy,
`local_state_metadata`, or an operator recovery command.

The block commitment worker opens MPFs when it starts building a block:
[`src/workers/commit-block-header.ts` lines 1430-1444](../src/workers/commit-block-header.ts#L1430-L1444).
The mandatory startup sequence verifies the migrated SQL schema, verifies
protocol deployment, hydrates state-queue/local-finalization globals, and checks
unfinished mutation jobs, but it does not open or verify the ledger MPF before
deposit catch-up, HTTP serving, or background fibers:
[`src/commands/listen.ts` lines 72-91](../src/commands/listen.ts#L72-L91).

The config service exposes MPF paths as `LEDGER_MPF_DB_PATH` and
`TRANSACTIONS_MPF_DB_PATH`:
[`src/services/config.ts` lines 72-74](../src/services/config.ts#L72-L74).
The defaults are `midgard-ledger-mpf-db` and
`midgard-transactions-mpf-db`:
[`src/services/config.ts` lines 276-283](../src/services/config.ts#L276-L283).
Deprecated names are rejected, not aliased:
[`src/services/config.ts` lines 77-96](../src/services/config.ts#L77-L96).

`Genesis.program` remains a separate non-mainnet convenience path. It inserts
configured UTxOs into `mempool_ledger`, not canonical confirmed state, and it
submits a genesis deposit when configured genesis UTxOs exist:
[`src/genesis.ts` lines 24-126](../src/genesis.ts#L24-L126).
Startup only schedules this program when `shouldRunGenesisOnStartup` allows it:
[`src/commands/startup-policy.ts` lines 3-12](../src/commands/startup-policy.ts#L3-L12)
and [`src/commands/listen.ts` lines 135-158](../src/commands/listen.ts#L135-L158).
The MPF empty-root reseed branch does not call this policy.

## Empty Root Handling Already Landed

The MPF wrapper now treats the canonical Midgard empty root as the MPF empty
root:
[`src/workers/utils/mpf.ts` lines 40-42](../src/workers/utils/mpf.ts#L40-L42).
When the persisted root marker is absent, the store opens at that canonical root;
when the marker is the library internal null root (`00...00`), persisted parsing
fails closed:
[`src/workers/utils/mpf.ts` lines 63-78](../src/workers/utils/mpf.ts#L63-L78).
Writes normalize any internal null-root value back to the canonical Midgard empty
root:
[`src/workers/utils/mpf.ts` lines 1865-1874](../src/workers/utils/mpf.ts#L1865-L1874).

The tests verify:

- a new `MidgardMpf` initializes to `SDK.EMPTY_MERKLE_TREE_ROOT` and
  `rootIsEmpty()` is true:
  [`tests/mpf.test.ts` lines 71-85](../tests/mpf.test.ts#L71-L85);
- deleting the last item persists the canonical empty root after reopen:
  [`tests/mpf.test.ts` lines 120-142](../tests/mpf.test.ts#L120-L142);
- corrupt persisted root markers fail closed:
  [`tests/mpf.test.ts` lines 247-264](../tests/mpf.test.ts#L247-L264).

This means the old draft's root-domain warning about an EthereumJS empty trie
root is no longer current for the MPF wrapper. The remaining issue is authority:
the node must prove that an empty MPF root is expected for this local state
instance before it allows mutating workers to proceed.

## Authority Model

The authoritative expected ledger root must come from this precedence order:

1. An active pending local-finalization or recovery journal, but only when it can
   be tied to explicit root evidence. The current pending-finalization rows do
   not store complete expected pre/post MPF roots, so the first implementation
   should either fetch and verify the target header from L1 or fail closed until
   metadata is added.
2. The latest committed state-queue tail on L1:
   - if the latest datum is a block node, expected root is the block header's
     `utxosRoot`;
   - if the latest datum is the confirmed-state root node, expected root is the
     confirmed state's `utxoRoot`.
3. Canonical initialized genesis state, where the expected UTxO root is
   `SDK.EMPTY_MERKLE_TREE_ROOT`.

Local SQL tables and metadata are not higher authority than L1, but they are
evidence that an empty MPF may be inconsistent and must not be ignored.

## Target Behavior

`makeMpfs` should open stores and return handles only. It should not seed
genesis, rebuild, delete, or otherwise repair durable state.

Startup should classify local state before HTTP serving, deposit projection, and
mutating workers:

```text
empty ledger MPF root
  -> expected authoritative root is empty and SQL/journal state is compatible
       => healthy empty pre-state; allow startup
  -> authoritative state is non-empty, or SQL/journal state implies prior work
       => integrity failure; stop before serving or mutating
```

For the first production-grade revision, ambiguous state must fail closed. Do not
add compatibility modes, fallback roots, or automatic adoption of existing local
state.

## Verifier Modes

### `pre_l1_initialization_empty_local_state`

This mode runs before any startup path may submit L1 protocol initialization. It
has no deployed L1 state to compare against yet, so it must prove local emptiness
strictly:

- ledger MPF root is empty or the configured ledger MPF path is absent;
- transactions MPF root is empty or the configured transactions MPF path is
  absent;
- no `local_state_metadata` row exists;
- all application tables that imply prior local activity are empty:
  `address_history`, `blocks`, `confirmed_ledger`, `latest_ledger`,
  `deposits_utxos`, `immutable`, `mempool`, `processed_mempool`,
  `mempool_ledger`, `mempool_tx_deltas`, `tx_rejections`,
  `deposit_ingestion_cursor`, `pending_block_finalizations`,
  pending-finalization member tables, `tx_admissions`, and
  `local_mutation_jobs`;
- the explicit startup/init policy permits bootstrap for the configured network.

If any predicate fails, startup must not run protocol initialization or
non-mainnet genesis bootstrap. The operator must use explicit adoption/recovery
or perform a full local plus on-chain redeploy/reset.

### `l1_authorized_mpf_state`

This mode runs after L1 state-queue or confirmed-state authority is available. It
classifies local MPF roots against L1 authority, SQL state, durable journals, and
metadata.

An empty ledger MPF root is compatible only when every local durable source that
would imply an advanced ledger is also compatible with the authoritative empty
root. For the first release, the following counts must be zero when classifying
empty as healthy:

```sql
SELECT COUNT(*) FROM local_mutation_jobs;
SELECT COUNT(*) FROM address_history;
SELECT COUNT(*) FROM blocks;
SELECT COUNT(*) FROM immutable;
SELECT COUNT(*) FROM latest_ledger;
SELECT COUNT(*) FROM mempool;
SELECT COUNT(*) FROM mempool_ledger;
SELECT COUNT(*) FROM processed_mempool;
SELECT COUNT(*) FROM confirmed_ledger;
SELECT COUNT(*) FROM tx_admissions;
SELECT COUNT(*) FROM tx_rejections;
SELECT COUNT(*) FROM deposit_ingestion_cursor;
SELECT COUNT(*) FROM deposits_utxos;

SELECT COUNT(*) FROM pending_block_finalizations
WHERE status IN (
  'pending_submission',
  'submitted_local_finalization_pending',
  'submitted_unconfirmed',
  'observed_waiting_stability'
);

SELECT COUNT(*) FROM pending_block_finalization_deposits;
SELECT COUNT(*) FROM pending_block_finalization_txs;

SELECT COUNT(*) FROM mempool_tx_deltas d
WHERE NOT EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = d.tx_id)
  AND NOT EXISTS (SELECT 1 FROM processed_mempool p WHERE p.tx_id = d.tx_id)
  AND NOT EXISTS (SELECT 1 FROM immutable i WHERE i.tx_id = d.tx_id);
```

The verifier must also inspect the persistent transactions MPF root. If that root
is non-empty while the ledger is being classified as protocol-empty and there is
no durable pending/deferred payload journal explaining it, startup must fail with
a typed integrity reason.

## Required Startup Order

1. Verify/apply explicit SQL migrations through the migration gate.
2. Run `pre_l1_initialization_empty_local_state` before any path that may submit
   L1 protocol initialization.
3. Perform read-only protocol deployment/topology checks.
4. Initialize L1 only when the existing explicit initialization policy allows it
   and the local empty-state preflight has passed.
5. Fetch authoritative L1 state-queue/confirmed-state evidence.
6. Run `l1_authorized_mpf_state` before any local durable mutation.
7. Only after verification passes, run deposit catch-up, deposit projection,
   explicit non-mainnet genesis bootstrap, HTTP serving, and workers.

The current `runNode` sequence has no MPF verifier in this order, so this is
remaining work.

## Root Rules

- Empty MPF roots must be represented as `SDK.EMPTY_MERKLE_TREE_ROOT`.
- The library internal null root must never be accepted from persisted state.
- Non-empty ledger MPF roots must be explained by metadata, a verified block
  header, or deterministic replay from verified durable sources.
- Do not treat `GENESIS_UTXOS` as authority for a persistent ledger MPF root.
- `latest_ledger` is a derived cache, not root authority.
- `confirmed_ledger` is a rebuild base only after verification against L1
  confirmed-state evidence and relevant pending merge/finalization journals.

## Implementation Checklist

- [x] Rename runtime MPF module and config paths to MPF names.
- [x] Reject deprecated `*_MPT_DB_PATH`/`MEMPOOL_MPF_DB_PATH` config names.
- [x] Normalize empty MPF roots to `SDK.EMPTY_MERKLE_TREE_ROOT`.
- [x] Add tests for canonical empty-root initialization and delete-to-empty
      persistence.
- [x] Remove the old HTTP `/reset` route from the current router.
- [ ] Remove empty-root genesis seeding from `makeMpfs`.
- [ ] Make `makeMpfs` a pure open operation that returns unmutated handles.
- [ ] Add `pre_l1_initialization_empty_local_state`.
- [ ] Add `l1_authorized_mpf_state`.
- [ ] Add `local_state_metadata` and adoption/recovery audit metadata.
- [ ] Add mandatory startup MPF authority verification before HTTP serving and
      mutating workers.
- [ ] Add transactions MPF integrity verification for deferred payload state.
- [ ] Add readiness reasons and metrics for MPF integrity/rebuild state.
- [ ] Add operator diagnostics that include configured MPF paths, observed roots,
      expected roots, SQL predicate counts, L1 evidence, and recovery suggestions.
- [ ] Add a read-only `verify-ledger-mpf-authority --json` command using the same
      verifier as startup.
- [ ] Add tests proving `GENESIS_UTXOS` are never inserted by `makeMpfs` and that
      empty MPF roots are accepted only with authoritative empty state.

## Deferred Questions

- Should the first implementation include the read-only verifier command, or
  should it land after recovery commands exist?
- Should explicit non-mainnet genesis MPF seeding be a dedicated command, or part
  of a stricter bootstrap flow shared by startup and `GET /init`?
- Which metadata field should record the source of a local root that is ahead of
  confirmed state: pending-finalization id, state-queue header hash, mutation job
  id, or a dedicated root-transition journal?
