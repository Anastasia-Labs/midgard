# Empty Ledger MPT Authority Revision Plan

Status: draft plan complete after five independent review passes.

Owner scope: `demo/midgard-node` ledger MPT startup/opening behavior,
startup integrity checks, and the boundary between canonical empty L2 state,
explicit non-mainnet/dev bootstrap, and corruption recovery.

## Goal

Revise the earlier `makeMpts` implementation so the node does not fail merely
because `ledgerRootIsEmpty` is true. Empty is a valid Midgard ledger root when
authoritative state says the current L2 pre-state is empty. The node should fail
only when an empty persistent ledger MPT is inconsistent with L1 state, SQL
state, pending recovery/finalization journals, or explicit bootstrap metadata.

The plan must preserve the production rule that normal startup never silently
seeds, rebuilds, deletes, or rewrites durable state. In particular,
`GENESIS_UTXOS` must not be inserted into the persistent ledger MPT as a recovery
shortcut.

## Current Issue

The old implementation treated an empty ledger trie as "insert configured
genesis UTxOs":

```ts
const ledgerRootIsEmpty = yield * ledgerTrie.rootIsEmpty();
if (ledgerRootIsEmpty) {
  // old behavior: insert nodeConfig.GENESIS_UTXOS into ledger trie
}
```

That was unsafe because an empty LevelDB can mean a missing/corrupt MPT store,
not a fresh chain. It also conflated two different states:

- canonical Midgard confirmed genesis, whose UTxO root is the empty Merkle root;
- test/dev `GENESIS_UTXOS`, which are local funding conveniences.

The first correction changed the behavior to fail whenever the ledger MPT root
is empty. That is too strict for Midgard because the smart contracts and SDK
define the genesis confirmed-state `utxoRoot` as the empty Merkle root.

## Relevant Evidence

On-chain Midgard confirmed genesis is empty:

- `onchain/aiken/lib/midgard/ledger-state.ak` defines
  `genesis_utxo_root = empty_merkle_tree_root`.
- `onchain/aiken/validators/state-queue.ak` initialization expects the produced
  confirmed-state datum to contain an empty ledger root.
- `demo/midgard-sdk/src/state-queue.ts` sets `GENESIS_UTXO_ROOT` to
  `EMPTY_MERKLE_TREE_ROOT`.

Node-side configured `GENESIS_UTXOS` are not the same authority:

- `demo/midgard-node/src/genesis.ts` inserts configured genesis UTxOs into
  `mempool_ledger`, not into canonical confirmed state.
- `shouldRunGenesisOnStartup` excludes mainnet and treats genesis execution as a
  non-production convenience.
- Therefore, writing `GENESIS_UTXOS` into the persistent ledger MPT during
  normal `makeMpts` would make the local committed ledger root disagree with
  the on-chain genesis root.

## Target Behavior

`makeMpts` should return open trie handles and should not mutate state.

When the ledger MPT root is empty, startup should classify it before allowing
mutating workers to build blocks:

```text
empty ledger MPT root
  -> expected authoritative root is empty and SQL/recovery state is compatible
       => healthy empty pre-state; allow startup
  -> authoritative state is non-empty, or SQL/recovery state implies prior work
       => integrity failure; stop before serving or mutating
```

The implementation should fail closed only when empty is inconsistent with
authoritative state. It must also account for root-domain differences: a local
EthereumJS empty trie root is not byte-equal to Midgard's protocol empty Merkle
root.

## Root Domain Rule

The plan must not compare every local MPT root byte-for-byte with every L1 root
without first proving they are in the same root domain.

Current evidence shows two distinct empty-root constants:

- local EthereumJS MPT empty root:
  `56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421`;
- Midgard protocol empty Merkle root:
  `0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8`.

First implementation rule:

- `ledgerTrie.rootIsEmpty()` may be classified as matching the protocol empty
  root only when the authoritative expected root is exactly
  `SDK.GENESIS_UTXO_ROOT` / `SDK.EMPTY_MERKLE_TREE_ROOT` and SQL/recovery state
  is compatible with empty state.
- Non-empty local ledger MPT roots must not be translated or normalized into
  protocol roots unless a separate audited proof establishes that the local MPT
  algorithm, key encoding, value encoding, and root hashing are exactly the
  protocol root domain used on L1.
- Until that proof exists, a non-empty local MPT root may be used internally for
  the current node implementation, but startup verification must explicitly
  label root domains in diagnostics and must not claim a raw non-empty local MPT
  root is an L1 protocol root.
- `emptyRootHexProgram` and block-construction paths must be audited because
  they currently return/use the EthereumJS empty root. The plan's implementation
  must avoid producing a block header whose empty `utxosRoot` disagrees with the
  protocol empty root.

Root-domain labels must be explicit in code, diagnostics, and tests. Use names
such as:

- `protocol_midgard_merkle`;
- `local_ethereumjs_mpt`.

### Non-Empty Root Rule

The first release must not accidentally accept a non-empty local MPT root as an
L1 protocol root unless the equivalence is proven.

For `expected root is non-empty && local ledger MPT is non-empty`, one of these
must be true:

- an audited implementation note and tests prove that the non-empty local
  EthereumJS MPT root domain is the exact protocol Midgard Merkle root domain
  for the key/value encoding used by every on-chain header root the node emits:
  `utxosRoot`, `transactionsRoot`, `depositsRoot`, and `withdrawalsRoot`; or
- durable metadata links the exact local root to a previously produced,
  submitted, and/or confirmed block header whose root source is understood; or
- startup fails closed with `startup_failed:ledger_mpt_root_domain_unproven`.

For this revision, prefer fail-closed unless the implementation can prove the
root-domain equivalence in a tight test suite. The empty-root exception is not a
general translation rule for non-empty roots.

If this revision is implemented without non-empty root-domain proof or durable
root metadata, it must be documented as sufficient only for empty/pre-genesis
startup and not sufficient for production restart after block production. A
production-ready restart path for non-empty ledgers requires either the proof or
metadata path above.

### Protocol Empty Root In Block Headers

Any on-chain block/header/root field that represents an empty Midgard Merkle
root must use the protocol empty root (`SDK.EMPTY_MERKLE_TREE_ROOT` /
`SDK.GENESIS_UTXO_ROOT`), not the EthereumJS empty trie root returned by
`emptyRootHexProgram`.

The implementation checklist must cover every header root field:

- `utxosRoot`;
- `transactionsRoot`;
- `depositsRoot`;
- `withdrawalsRoot`.

This is a required implementation item, not a later audit. The block commitment
path and commit planner must not emit the local EthereumJS empty root into an
on-chain Midgard header field when the protocol root should be empty.

The non-empty case is subject to the same root-domain standard. If a local MPT
helper produces a non-empty root for any on-chain header field, the
implementation must prove that the produced root is in the protocol root domain
for that field or clearly mark the node as not production-ready after block
production.

## Authority Model

The authoritative expected ledger root comes from this precedence order:

1. Active pending block/local-finalization recovery journal only when it can be
   tied to concrete root evidence. The current pending-finalization table does
   not store expected pre/post roots, so the first implementation must either:
   - fetch and verify the target header from L1 before treating the journal as
     root authority; or
   - fail closed for active pending finalization until a later migration adds
     explicit expected-root and plan-hash fields.
2. Latest committed state-queue tail on L1:
   - if the latest datum is a block node, expected root is the header's
     `utxosRoot`;
   - if the latest datum is the confirmed-state root node, expected root is the
     confirmed state's `utxoRoot`.
3. Canonical initialized genesis state, which is empty only when L1 confirms the
   initialized state queue is at genesis/empty state.

Local SQL tables and metadata are not higher authority than L1, but they are
evidence that an empty MPT may be inconsistent and must not be ignored.

## First-Release Verifier Modes

The implementation should expose two explicit verifier modes. They have
different authorities and must not share a predicate set accidentally.

### `pre_l1_initialization_empty_local_state`

This mode runs before any startup path may submit L1 protocol initialization. It
has no L1 deployment to compare against yet, so it must be stricter than the
post-L1 authority check.

It passes only when:

- ledger MPT root is local-empty;
- mempool MPT root is local-empty;
- every application table that can represent prior local activity is empty:
  `address_history`, `blocks`, `confirmed_ledger`, `latest_ledger`,
  `deposits_utxos`, `immutable`, `mempool`, `processed_mempool`,
  `mempool_ledger`, `mempool_tx_deltas`, `tx_rejections`,
  `deposit_ingestion_cursor`, `pending_block_finalizations`,
  `pending_block_finalization_deposits`, `pending_block_finalization_txs`,
  `tx_admissions`, and `local_mutation_jobs`.

If any count is non-zero, L1 protocol auto-initialization must not run. The
operator must either use an explicit recovery/adoption path or start from a
clean local state.

### `l1_authorized_mpt_state`

This mode runs after L1 state-queue/confirmed-state authority is available. It
classifies local MPT roots against L1 authority, SQL state, and recovery
journals.

## First-Release SQL And Recovery Compatibility Checks

An empty ledger MPT root is compatible only if every local durable state source
that would imply a non-empty or advanced ledger is also compatible with the
authoritative empty root.

The first implementation should not leave SQL compatibility as an open-ended
heuristic. It should implement a strict initial rule.

If the authoritative L1 expected root is empty and the local ledger MPT is empty,
startup is allowed only when all of these are true:

- no `local_mutation_jobs` rows;
- no active pending block finalization;
- no pending-finalization member rows;
- no `address_history` rows;
- no `blocks` rows;
- no `immutable` rows;
- no `latest_ledger` rows;
- no `mempool` rows;
- no `mempool_ledger` rows;
- no `processed_mempool` rows;
- no `mempool_tx_deltas` rows that cannot be tied to a current `mempool`,
  `processed_mempool`, or immutable lifecycle state;
- no `tx_admissions` rows in any status;
- no `tx_rejections` rows;
- no `deposit_ingestion_cursor` rows;
- no `deposits_utxos` rows;
- no `confirmed_ledger` rows unless a root rebuild proves the table is empty-root
  equivalent;
- no pending/deferred mempool transaction root and no non-empty persistent
  mempool MPT state that would be required for a pending block;
- `latest_ledger` is treated as a derived cache only, not as authority;
- deposit state does not imply an unfinalized or committed block root that
  should already be reflected in the ledger MPT.

Ambiguous non-empty SQL state fails closed in the first release. Later
allow-listing must be backed by read-only audit commands or schema metadata that
proves a table is merely a derived cache.

If the authoritative expected root is non-empty and local ledger MPT is empty,
startup fails with
`startup_failed:ledger_mpt_empty_but_expected_non_empty`.

If the authoritative expected root is protocol-empty and local ledger MPT is
non-empty, startup fails with
`startup_failed:ledger_mpt_non_empty_but_expected_empty` unless a later audited
root-domain proof establishes equivalence.

### First-Release SQL Predicates

The first implementation should evaluate SQL compatibility in one read-only
transaction so the verifier does not mix snapshots. The initial predicates are:

```sql
SELECT COUNT(*) FROM local_mutation_jobs;

SELECT COUNT(*) FROM address_history;

SELECT COUNT(*) FROM pending_block_finalizations
WHERE status IN (
  'pending_submission',
  'submitted_local_finalization_pending',
  'submitted_unconfirmed',
  'observed_waiting_stability'
);

SELECT COUNT(*) FROM pending_block_finalization_deposits;
SELECT COUNT(*) FROM pending_block_finalization_txs;

SELECT COUNT(*) FROM blocks;
SELECT COUNT(*) FROM immutable;
SELECT COUNT(*) FROM latest_ledger;
SELECT COUNT(*) FROM mempool;
SELECT COUNT(*) FROM mempool_ledger;
SELECT COUNT(*) FROM processed_mempool;
SELECT COUNT(*) FROM confirmed_ledger;

SELECT COUNT(*) FROM mempool_tx_deltas d
WHERE NOT EXISTS (SELECT 1 FROM mempool m WHERE m.tx_id = d.tx_id)
  AND NOT EXISTS (SELECT 1 FROM processed_mempool p WHERE p.tx_id = d.tx_id)
  AND NOT EXISTS (SELECT 1 FROM immutable i WHERE i.tx_id = d.tx_id);

SELECT COUNT(*) FROM tx_admissions;

SELECT COUNT(*) FROM tx_rejections;

SELECT COUNT(*) FROM deposit_ingestion_cursor;

SELECT COUNT(*) FROM deposits_utxos;
```

For the first release, every count above must be zero when classifying a local
empty ledger MPT as compatible with protocol-empty L1 state. If a later
implementation proves a table is harmless derived state, it must add a narrower
predicate and tests for that case.

The verifier must also inspect the persistent mempool MPT root:

- if `mempoolTrie.rootIsEmpty()` is false while the ledger is being classified as
  empty/protocol-genesis, fail with
  `startup_failed:mempool_mpt_non_empty_for_empty_ledger`;
- if a later implementation supports deferred transaction-root recovery, it must
  tie the mempool MPT root to an explicit pending-finalization or mutation
  journal before startup can pass.

### SQL Predicate Invariant Map

| Predicate                                                     | Protected invariant                                                                                           |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| Any `local_mutation_jobs` row                                 | Local mutation history must be adopted or explicitly cleared by recovery before clean-empty classification.   |
| Non-empty `address_history`                                   | Address index history implies prior tx or ledger activity.                                                    |
| Active `pending_block_finalizations`                          | Pending local block state cannot be inferred without root evidence.                                           |
| Non-empty pending-finalization member tables                  | Member evidence implies prior block finalization state even if parent status is no longer active.             |
| Non-empty `blocks` or `immutable`                             | Committed block history implies prior non-empty or advanced ledger state.                                     |
| Non-empty `latest_ledger`                                     | Derived ledger cache must not be trusted as clean genesis.                                                    |
| Non-empty `mempool`, `mempool_ledger`, or `processed_mempool` | Admission/pre-state or deferred block state exists and must not be treated as clean genesis.                  |
| Orphan `mempool_tx_deltas`                                    | Delta evidence without a lifecycle owner is corruption/ambiguity.                                             |
| Any `tx_admissions` row                                       | Durable admission history means prior submission state must be explicitly adopted or cleared by recovery.     |
| Any `tx_rejections` row                                       | Rejection history is durable tx lifecycle evidence, not clean genesis.                                        |
| Non-empty `deposit_ingestion_cursor`                          | L1 deposit scanning state implies prior synchronization work.                                                 |
| Non-empty `confirmed_ledger`                                  | Local confirmed SQL state must not contradict protocol-empty L1.                                              |
| Any `deposits_utxos` row                                      | Deposit evidence must be reconciled with L1 and cannot be assumed harmless during empty-state classification. |

## Implementation Steps

1. Change `makeMpts` back into a pure open operation:
   - create `ledgerTrie` and `mempoolTrie`;
   - do not insert `GENESIS_UTXOS`;
   - do not fail solely because `ledgerTrie.rootIsEmpty()` is true;
   - expose enough helper APIs to read roots for startup verification.

2. Add a startup MPT authority check before HTTP serving and before all durable
   startup mutations:
   - fetch latest committed state-queue datum from L1 using the same
     `SDK.fetchLatestCommittedBlockProgram` path used by startup boundary
     seeding;
   - derive expected ledger root from confirmed-state or latest block header;
   - open/read ledger MPT root;
   - classify root domains before comparison;
   - allow local empty only when expected protocol root is empty and SQL/recovery
     state is compatible;
   - if roots/domains/state are inconsistent, fail startup with a typed integrity
     error.

   Required startup order:
   1. verify/apply explicit SQL migrations through the migration gate;
   2. run `pre_l1_initialization_empty_local_state` before any startup path that may submit
      L1 protocol initialization; this preflight must verify that local SQL,
      ledger MPT, and mempool MPT do not contain prior state;
   3. perform read-only protocol deployment/topology checks, and run L1 protocol
      initialization only when the existing explicit initialization policy allows
      it and the local empty-state preflight has passed;
   4. fetch the authoritative L1 state-queue/confirmed-state datum;
   5. run the MPT authority verifier before any local durable mutation;
   6. only after the verifier passes, run deposit catch-up, deposit projection,
      non-mainnet genesis bootstrap, HTTP serving, and background workers.

3. Add compatibility checks for empty roots:
   - if expected protocol root is empty and `ledgerTrie.rootIsEmpty()` is true,
     allow only when SQL/recovery checks do not imply prior local mutation;
   - if local ledger MPT root is empty and expected root is non-empty, fail with
     `startup_failed:ledger_mpt_empty_but_expected_non_empty`;
   - if local ledger MPT root is non-empty and expected root is empty, fail with
     `startup_failed:ledger_mpt_non_empty_but_expected_empty`.

4. Keep genesis/test funding out of `makeMpts`:
   - `GENESIS_UTXOS` may be used only in explicit non-mainnet/dev bootstrap
     paths;
   - if `DEBUG_MODE` is introduced, it must not silently change production
     startup behavior and must not run on mainnet;
   - any debug seeding should be an explicit command or isolated bootstrap path,
     not a side effect of opening MPTs.

5. Add operator-visible diagnostics:
   - expected protocol root and source;
   - local ledger MPT root and local root domain;
   - whether the local root is the local EthereumJS empty root;
   - source of expected root (`l1_confirmed_state`, `l1_state_queue_tail`,
     `pending_finalization_recovery`);
   - relevant SQL counts and active journal status;
   - recommended next step: verify, rebuild dry-run, or explicit dev bootstrap.

   MPT handles opened only for startup verification must be closed before the
   verifier returns or fails.

6. Add a read-only diagnostic command after the verifier exists:
   - `verify-ledger-mpt-authority --json`;
   - use exactly the same verifier as startup;
   - perform no repair or mutation;
   - return structured roots, root domains, SQL predicate counts, active journal
     state, and recommended operator action.

7. Add tests:
   - empty local MPT plus L1 genesis empty root plus empty SQL is accepted;
   - local EthereumJS empty root is classified separately from protocol empty
     root and accepted only for the protocol-empty case;
   - empty local MPT plus L1 non-empty tail root fails;
   - empty local MPT plus active pending finalization fails unless the target
     header/root is retrieved from L1 or explicit expected-root fields exist;
   - non-empty local MPT plus L1 empty root fails;
   - non-empty local MPT plus non-empty expected root fails unless root-domain
     equivalence or durable local root metadata is implemented and tested;
   - block/header construction uses `SDK.EMPTY_MERKLE_TREE_ROOT` for protocol
     empty on-chain `utxosRoot`, `transactionsRoot`, `depositsRoot`, and
     `withdrawalsRoot`, not the local EthereumJS empty trie root or zero hash;
   - `GENESIS_UTXOS` are never inserted by `makeMpts`;
   - non-empty `mempool`, `mempool_ledger`, `processed_mempool`, or mempool MPT
     blocks empty-ledger startup classification;
   - L1 protocol initialization cannot run when local empty-state preflight
     detects prior SQL/MPT state;
   - startup verification runs before deposit catch-up, deposit projection, and
     `Genesis.program`;
   - `pre_l1_initialization_empty_local_state` rejects any non-empty application
     table listed above before L1 initialization can submit;
   - verifier output returns root-domain labels as data, not only logs;
   - debug/dev bootstrap, if added, is disabled on mainnet and isolated from
     normal startup.

## Initial Implementation Preference

Do the smallest production-correct revision first:

- remove the unconditional empty-root failure from `makeMpts`;
- add a separate read-only startup verification function that derives the
  authoritative expected root, classifies root domains, and checks SQL/recovery
  compatibility;
- run compatibility reads in one read-only SQL transaction;
- run that verifier before deposit catch-up, deposit projection, genesis
  startup, HTTP serving, and all mutating worker fibers;
- fail closed on any mismatch or ambiguity;
- do not add `DEBUG_MODE` genesis seeding until there is an explicit bootstrap
  command or test-only harness that cannot run in production.
- prefer a dedicated explicit dev bootstrap command over a broad `DEBUG_MODE`
  flag.

This keeps normal node operation correct while preserving a clear path for
fresh empty Midgard state.

## Deferred Questions

- Should a diagnostic read-only mode be added now, or deferred until recovery
  commands exist?
- Is `DEBUG_MODE` already a repo-wide convention, or should a more specific
  flag such as `MIDGARD_DEV_BOOTSTRAP_GENESIS_MPT` be used for any non-prod
  bootstrap helper?
