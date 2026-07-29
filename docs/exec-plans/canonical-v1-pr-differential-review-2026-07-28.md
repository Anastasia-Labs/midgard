# Canonical V1 checkpoint differential review

Date: 2026-07-28

## Scope

- Target branch: `tx-validation`
- Target revision reviewed during integration:
  `8bae9403a13124f647f215999848ff5c82784e37`
- Head branch: `colll78/canonical-v1-watcher-l1-source-checkpoint`
- Review boundary: the complete target-to-head Git diff after merging the
  target revision, including conflict resolutions and final-tree fixes.
- Protected pre-Goal Aiken checkpoint paths and the authoritative
  `GOAL_SPEC.md` were not edited or imported from the dirty source checkout.

This is a checkpoint review, not §15 completion evidence. Live target-testnet,
release, and aggregate Goal acceptance remain open in `GOAL_PROGRESS.md`.

## Review method

The review split non-overlapping surfaces across watcher/L1 consistency,
runtime and durable recovery, and Aiken/fault-proof correctness. The parent
reviewed shared catalogue/deployment identity, resolved integration conflicts,
regenerated the Aiken blueprint with the pinned compiler, and replayed focused
final-tree gates. Reviewers traced attacker-controlled inputs through protocol
decisions and checked rollback/restart behavior, exact deployment identity,
proof routing, deadlines, and correction authority.

## Findings and disposition

### Fixed: external L1 evidence was not bound to configured authorities

External-provider observations could previously supply self-asserted distinct
identities. W01/W11 now bind each observation to the configured
provider/operator/HTTPS endpoint policy and reject substitutions, duplicate
authority, authentication downgrade, network mismatch, or incompatible chain
points. Local-node mode retains one node authority and aligned query surfaces;
it does not manufacture a provider quorum.

### Fixed: L1 rollback/disagreement did not invalidate durable DA decisions

The DA watcher now persists the selected source mode and canonical observation
cursor. A disappeared, forked, stale, or no-longer-final decided observation
quarantines the source and atomically invalidates affected headers, payloads,
signatures, submissions, and pending peer broadcasts. Quarantine survives
restart. Production rejects fixture/file L1 sources unless explicit test mode
is enabled, and the legacy single-provider construction bypass was removed.

### Fixed: watcher catalogue omitted the live zero-input proof family

The exact deployed catalogue is now:

1. `doubleSpend` (`00000000`)
2. `nonExistentInput` (`00000001`)
3. `nonExistentInputNoIndex` (`00000002`)
4. `invalidRange` (`00000003`)
5. `transitionTrace` (`00000004`)
6. `zeroInput` (`00000005`)
7. `validationTraceDispute` (`00000006`)

Deployment identity, W17 indexing, dependency evidence, CLI/runtime dispatch,
and fixtures use the same order and exact deployed contract binding.

### Fixed: active startup journal omitted committed validation traces

Node startup recovery now requires the validation-trace member collection and
its exact committed count before hydrating an active finalization journal.
Hostile coverage rejects an active journal with committed trace data omitted.

### Fixed: forced omission ignored authenticated inclusion time

The transition proof now requires the authenticated transaction-order
`inclusion_time` to lie in the block window in addition to the required
validity overlap. A transaction created after an honest historical block can
no longer fabricate an omission proof for that block.

### Fixed: delayed source verification could create an expired operator turn

The validation-dispute source hop now authenticates its transaction time,
rejects time travel and stale setup, and derives the operator response deadline
from the authenticated source-hop validity upper bound. The challenger cannot
wait out the old deadline and immediately win by timeout.

### Fixed: transition replay and zero-input integration drift

The merged transition proof authenticates the transaction ID, bounded native
spend/output commitments, and exact UTxO delete/insert replay. Category routing
keeps transition trace at ID 4, zero input at ID 5, and validation trace at ID 6. The zero-input empty-spend commitment uses the canonical native V1 bounded
collection derivation. Formatting-only changes to live zero-input validators
were dropped.

### Fixed: retained DA corpus producers used the pre-canonical field order

Five validation-boundary producers still assigned script witnesses and address
witnesses according to the previous field order. They now use canonical fields
6 and 7 respectively, and the committed retained-corpus transaction
commitments were regenerated from those exact production boundaries. The
normal verifier regenerates the private corpus before running both consumers,
so stale committed bytes fail closed.

### Fixed: semantic resolver cardinalities were not the deployed V1 counts

The validation resolver routed two script-source variants and twenty-eight
phase-A script preconditions through stale cardinalities. The on-chain
resolver now requires exactly `2` and `28`, rejects adjacent and cross-family
routes, and has exact hostile selectors.

### Fixed: committed header time was not bound to Cardano validity

The state-queue mint policy now requires every committed header end to equal
the normalized inclusive upper bound of the committing transaction. SDK and
node production builders require a closed interval no longer than eight
minutes and encode `header.endTime = validTo - 1`. Scheduler selection compares
that inclusive end to its cap.

The final block end is also the user-event completeness boundary. Before
journal preparation, both commit paths run the canonical deposit, withdrawal,
and transaction-order ingestion barriers through that exact end. Inside the
journal transaction they lock all three source tables and require exact event
ID sets. Speculative commits perform the same final-end exact-set check while
holding their state-queue and MPF leases, so a late due event invalidates the
candidate rather than being omitted.

### Fixed: genesis and ordinary header protocol identities were conflated

The state-queue genesis root is now the sole authenticated protocol-`0`
sentinel: all-zero header identities, empty UTxO root, and equal non-negative
times. The first and every later committed header use protocol `1`.
Initialization, merge, SDK production builders, fault tooling, and emulator
fixtures share the same constructor and hostile identity checks.

### Fixed: speculative withdrawals could classify against mutable state

Withdrawal classification now uses the selected immutable commit-base entries,
not the mutable mempool ledger. Base entries are hydrated whenever a
withdrawal is due, even when the normal payload/corpus options are disabled.
Hostile tests cover roots that exist only in the selected base and reject
mutable-state substitution.

### Fixed: catalogue fixtures used placeholder roots and membership proofs

All watcher deployment fixtures now use one seven-entry helper containing the
deployed script hashes, canonical catalogue root, and exact membership-proof
CBOR. The helper verifies itself through the production manifest verifier, so
script or proof drift fails before a fixture can authorize indexing.

### Fixed: local-node observation was not a real rollback-capable source

Local-node mode now consumes an Ogmios WebSocket chain-sync stream and Kupo or
Kupmios query surfaces aligned to that node's network and canonical point.
Handshake rollback is not misreported as a chain rollback, an intersection
outside the submitted bounded history is rejected, real roll-forwards and
rollbacks propagate through W10-W13, deep rollbacks fail closed, and stale
Kupo JSON tips quarantine decisions. HTTP Ogmios endpoints are normalized to
WebSocket transport without changing authority identity.

External-provider mode independently rejects aliased endpoint/operator
identities and retains the two-authority agreement rule. Same-node Ogmios,
Kupo, and db-sync surfaces are not added to `independentProviderCount`; the
watcher-operated full node is the sole local chain authority.

### Fixed: authority persistence and signature serving were fail-open

The DA watcher persists the selected chain-authority observation digest and
revalidates it on restart. A mismatch quarantines the source. Signatures for a
quarantined or invalidated header are no longer returned to peers.

### Fixed: CI and normative documentation did not guard the checkpoint

Midgard Node CI now builds the SDK before validation, checks watcher formatting
and the hash-bound dependency map, and triggers on both evidence surfaces.
The LaTeX workflow runs on pull requests with read-only contents permission.
The technical specification and public documentation now agree on protocol
version `1` for ordinary headers, the 16 MiB native transaction bound, enabled
mint/script/reference-input semantics, Aiken `v1.1.22`, the seven catalogue
families, and the explicit `local_node | external_providers` trust model.

### Fixed: dependency-tree binding followed a checked-out Git link

The dependency-map verifier previously read every tracked path as a regular
file. A checked-out `technical-spec/Lean4Midgard` Git link therefore became a
directory and made otherwise identical content trees unverifiable. The binder
now reads tracked mode and object identity from the Git index, hashes Git-link
identities directly, and continues to hash ordinary working-tree bytes and
symbolic-link targets deterministically.

## Final-tree evidence

- Aiken `v1.1.22+39d6b04` build and blueprint generation: PASS.
- Generated `onchain/aiken/plutus.json` SHA-256:
  `32dd6f052b5fb9e2da1f81efb5c2bdc816c6136f09a20f0e1ea5c526b20b3466`;
  355 validators; phase-A router 5,302 bytes, script-sources router 5,305
  bytes, and state-queue mint policy 10,762 bytes.
- New on-chain resolver, header-time, and genesis hostile selectors: 13/13
  PASS. Previously reviewed transition replay, forced timing, and source
  lifecycle selectors remain green on the same source.
- Watcher build, typecheck, lint, and format check: PASS.
- Watcher suite: 194/194 PASS.
- Canonical watcher dependency-map verifier: 8/8 dependency classes PASS.
- Core package: 36 files and 271/271 tests PASS; production build PASS.
- Node runtime/source-completeness focused suite: 74/74 PASS; typecheck PASS.
- SDK production build/typecheck and full suite: 16 files and 80/80 tests
  PASS under Node `22.22.2`.
- Fault-proof package: 14/14 files and 110/110 tests PASS; rebuilt-blueprint
  zero-input emulator: 1/1 PASS.
- Retained-DA verifier: 13 producer files/14 tests, 20 DA consumer tests, and
  3 fault-proof consumer tests PASS after private-corpus regeneration.
- DA package: 189 executed tests PASS; the one
  PostgreSQL-environment test is explicitly skipped here and its hostile
  startup-journal regression passed separately against the configured local
  PostgreSQL instance.
- DA source-mode typecheck, production build, and no-HTTP transport guard:
  PASS.
- Fault-proof typecheck, ESLint, and scoped Prettier: PASS.
- Documentation facts (10 groups), links (190 Markdown/MDX files), and voice
  (83 pages): PASS under pinned Node 22.

The checkpoint commit and publication/PR review result are recorded in
`GOAL_PROGRESS.md` and the PR metadata once created.

## Residual boundaries

- PostgreSQL-backed tests require the configured local test database and are
  reported separately from in-memory/JSON-store results.
- Local listener tests require execution outside the filesystem/network
  sandbox; sandbox `listen EPERM` is not treated as product evidence.
- Formal §4.4 journey, target-testnet acceptance, and all still-open §12
  criteria remain explicitly unpromoted.
