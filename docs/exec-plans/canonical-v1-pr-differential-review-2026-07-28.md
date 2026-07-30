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
it does not manufacture a provider quorum. The provider state-machine
constructor requires the discriminator, rejects omitted or unknown runtime
values, and has no compatibility default.

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
now reads every path, mode, and blob identity from the Git index, hashes
Git-link identities directly, reads ordinary and symbolic-link bytes from
their staged blobs, sorts paths by explicit UTF-8 byte order, and includes mode
in the digest. A dirty worktree, executable-bit change, or symlink conversion
can no longer masquerade as the reviewed commit tree.

### Fixed: local-node query services were rejected by surface kind

The local-node consistency evaluator incorrectly treated two distinct,
configured query services of the same kind as one duplicated surface. Surface
kind is not an authority identity: separate Ogmios or indexer services may
legitimately query the same watcher-operated node. The evaluator now rejects
duplicate provider IDs while allowing distinct same-kind services, continues
to count the local node as exactly one independent chain authority, and still
requires every query result to match its network and canonical chain point.

### Fixed: W11 evidence was order-sensitive at hostile boundaries

External-provider bindings now use a total tuple order, so conflicting
identities that reuse one provider ID cannot change the consistency digest by
changing input order. Canonical chain points are deduplicated before bounded
lag checks while same-point content mismatches still quarantine decisions.
Malformed or adversarial configured-source objects are caught at the public
boundary and produce deterministic, secret-safe quarantine output.

### Fixed: native transaction field order and compact fixture ownership drifted

The canonical native V1 wire order now agrees across Aiken, the core proof
layer, validation, node ingestion, and the Lucid SDK. The cross-language
fixture generator owns the compact JSON and Aiken goldens, checks their
transaction IDs and body hashes, and is byte-idempotent. Exact Aiken vectors
cover high-cardinality, size-balanced, ordinary full/compact, TypeScript
one-step, canonical decode, and all fifteen script-discovery slots.

### Fixed: generated terminal proof chunk had no executable regression

The proof-fragment generator emitted the maximum-profile terminal
`chunk_proof_8_15_1` case without selecting it in any test scenario. The
generated fixture now executes that terminal chunk alongside the other
representative scenarios, so regeneration cannot silently leave the final
field/chunk boundary dead.

### Fixed: Lucid tests preserved pre-domain-separated identities

Several Lucid fixtures and tests still asserted pre-canonical field
commitments, a stale provider transaction ID, and idempotent repeated signing.
They now derive the exact domain-separated field commitments, pin the corrected
static transaction identity, and require repeated signing to fail with the
documented duplicate-witness `SigningError`.

### Fixed: Phase-4 verifier pinned a stale PHAS blueprint identity

The full node suite exposed a Phase-4 process-summary fixture whose PHAS reward
address, script hash, registration CBOR, transaction hash, and artifact digest
still described the prior blueprint. The fixture has been regenerated from
the current testnet blueprint and its focused five-case verifier passes.

### Fixed: validation-trace lookup confused step keys with event keys

The complete node replay exposed a production lookup that built its
validation-trace source index from transition-trace member keys. Those keys
encode step indexes, while forced and normal validation inputs query by
canonical event-key CBOR. The index now derives keys from each authenticated
transition step's `event_key`, rejects duplicates, and requires exact set
equality with the forced/L2 validation inputs. A custom builder cannot replace
a required transaction descriptor with an otherwise valid deposit or
withdrawal event key. The hostile substitution regression passes.

### Fixed: realistic commit fixtures bypassed strict DA and CEK inputs

The emulator commit fixture previously relied on an absent DA runtime manifest
and queued normal transactions without their now-mandatory CEK
program-material sidecars. Its setup now supplies a syntactically and
semantically valid producer manifest, persists canonical empty sidecars for
script-free transfers, and decodes the retained DA envelope before asserting
its payload. Synthetic backlog times are monotonic after the initialized
state-queue tip and use the production `(timestamp, tx_id)` tie-break.
Production continues to fail closed when any of those inputs is missing or
malformed.

The fixture's operational DA identity is no longer merely syntactic. Its
producer peer ID and announced multiaddress are derived and checked through
the production libp2p identity loader from the same deterministic private-key
source used at runtime. The manifest network now matches the Preprod emulator
configuration.

### Fixed: maximum validation CI guards used stale contention budgets

The published PR check showed five timeouts rather than assertion failures:
three validation-machine cases, the maximum retained redeemer traversal, and
the maximum inline-data breadth case. The deterministic workloads and
assertions are unchanged; their explicit budgets now cover the observed
bounded runner-contended duration. The validation-machine cases use 60 seconds,
the redeemer traversal 120 seconds, and both maximum data-breadth cases 600
seconds.

### Fixed: speculative proof validation crossed the provider boundary before readiness

The complete Node replay caught canonical V1 proof setup acquiring the Lucid
service before a speculative candidate emitted `CandidateReady`. That broke
the provider-free build invariant and made invalidation paths depend on L1
access. The parent now copies Lucid's already-validated, immutable per-instance
slot mapping into plain worker data and performs the exact enclosing-slot
conversion locally. `Custom` mappings remain sourced from aligned local-node
Shelley genesis at Lucid initialization; standard networks retain Lucid's
selected mapping. Missing, malformed, unsafe, or pre-genesis mappings fail
closed.

The Architecture-G candidate artifact language now binds the same three-field
slot mapping to a SHA-256-identified source artifact. Standard networks must
match Lucid `0.6.0`'s immutable table; `Custom` derives the mapping from the
canonical Shelley-genesis response returned by the exact configured Ogmios
endpoint. The production probe rejects a document network different from
`NodeConfig.NETWORK`; for `Custom`, it re-queries that endpoint and rejects any
endpoint or canonical genesis digest mismatch before the provider-free build.
The capture command bounds both response time and streamed response bytes, so
a stalled or oversized configured Ogmios cannot hang or exhaust the evidence
workflow. Focused pure-conversion, source-boundary, artifact-decoder, and
candidate-gate tests pass, as do both emulator regressions that assert zero
Lucid acquisitions before readiness.

### Fixed: native integration derived a pre-canonical script-integrity hash

The complete Node replay exposed two test-only transaction builders that
derived the script-integrity hash from a raw redeemer preimage. Production
uses the canonical, domain-separated redeemer collection. Both builders now
consume `redeemerTxWitsHash` from the production compact-witness derivation,
and the empty top-level mint-map regression fails closed at the same canonical
construction boundary. The full native integration file passes 79/79.

### Fixed: transition-trace ABI golden described the retired combined proof

The transition-trace ABI fixture still described the old combined validator
after the production blueprint split route selection from finalization. The
golden now binds the current route and final argument records, and an explicit
package command regenerates it deterministically from the SDK schemas. The
complete SDK ABI fixture file passes 8/8.

### Fixed: settlement redeemer boundary depended on compiler field reordering

Aiken `v1.1.22` reordered the typed settlement mint-redeemer record at the
validator boundary. The mint handler now decodes the constructor tag, exact
arity, primitive field types, and canonical field order into the existing
typed `Spawn | Remove` model before executing unchanged validator semantics.
Dedicated Spawn, Remove, unknown-tag, wrong-arity, and wrong-type tests close
the boundary; the broader settlement selector passes 13/13. The rebuilt
settlement minting validator is 3,660 raw bytes with script hash
`7480e0d91c418bb3e3ab96d0e7eb174325d298396128646f3c735546`, and the real
settlement-merge emulator journey passes against that blueprint.

### Fixed: authenticated L1 collections had only per-array bounds

W10 previously limited each transaction sub-array independently, allowing an
attacker to multiply many maximum-sized arrays before rejection. The adapter
now preflights the outer transaction array and every nested UTxO, script,
datum, and redeemer count against one 65,536-member aggregate budget before
parsing or sorting members. The hostile multiplicative case fails at the
public `$.transactions` boundary.

### Fixed: direct providers could self-attest a false language subset

The Lucid builder compared two language lists supplied by the same provider,
so mutually consistent empty or false subsets could pass. Both provider lists
must now independently equal the compiled canonical
`PlutusV3:2, MidgardV1:128` set. Empty, one-sided, false-subset, and malformed
tag cases fail before builder creation.

### Fixed: native-script capability exceeded executable codec depth

The consensus profile advertised depth 16,384 while the recursive decoder
stopped near 4,096 and other recursive traversals remained host-stack-bound.
Encoding, decoding, verification, and consensus complexity measurement are now
iterative and share exact 16,384 depth and node-count constants. Canonical
depth/node maxima round-trip and verify; their deep and wide adjacent cases
reject or return false without shrinking the Cardano capability floor.

### Fixed: CEK execution ignored declared redeemer ex-units

The structural executor could run until the global trace-step ceiling even
after a redeemer exhausted its declared CPU or memory. It now retains the
first authenticated over-budget transition and stops deterministically.
Phase B passes each redeemer budget to both the default and injected proof
evaluator, and validation-machine initial evaluation and exact trace
regeneration use the same limits. The existing explicit
`enforceScriptBudget: false` diagnostic seam remains the only unlimited mode.

### Fixed: missing run state could silently replace deployment authority

When the run-state file was absent, deployment could generate a new
reference-script authorization policy even though a finalized manifest already
bound the deployment. Resume now strictly parses that manifest, binds its
network, one-shot out-ref, and path to the current identity, restores its exact
policy, and persists it before publication. Malformed manifests and
manifest/run-state policy conflicts fail closed without overwriting state.

### Fixed: Architecture G merge finalization reopened its LevelDB

Confirmed-state merge finalization used the legacy persistent synchronizer
after SQL and on-chain success, reopening the path owned by the live
Architecture G native service. Engine-aware routing now observes and validates
the live owner's durable tail without evaluating the persistent synchronizer.
Missing owners and malformed roots fail closed; a defensive lower-level guard
also refuses any Architecture G persistent-store synchronization. Legacy,
overlay, and event-flat engines retain their existing behavior.

### Fixed: evidence claims and CI exceeded their proof

The checkpoint ledger claimed strict 132/132 format-registry success although
the integrated registry contained 10 `PASS` and 122 `UNVERIFIED` rows. The
claim is downgraded to structural incomplete-mode evidence, and strict
verification continues to fail closed. A repository-wide evidence workflow
now runs that structural gate and the staged-tree dependency verifier on every
push and pull request. The dependency verifier additionally enforces the exact
dependency set/order, trust classifications, allowed/prohibited input sets,
and rejected operator-private surfaces. W23's completed rule-bundle state is
consistent across the map, and the final tree digest is regenerated only
after staging.

## Fresh PR-head review and remediation cycle

The published checkpoint at
`da65efebf556ed9998604fd5b03c458c11111b4f` was reviewed again against
`8bae9403a13124f647f215999848ff5c82784e37`. Four non-overlapping lanes
covered W13 recovery authority, W14-W17 downstream projections, evidence/CI
and Aiken/SDK equivalence, and cross-cutting public admission/resource
boundaries. The parent re-read every finding, reviewed the remediations,
integrated shared surfaces, and owns the final evidence binding.

The review found four high-severity and four medium-severity issues. Every
finding below is fixed in the reviewed result tree:

- **High — sparse projection recovery required a domain event at the W13
  common ancestor.** W14-W17 now cut history at the latest domain transition
  whose block number is at or before W13's authenticated common ancestor.
  Every later projection entry must be covered by W13's exact removed sets.
  A projection with no owned change can still append its recovery audit.
  W15 derives its target internally rather than accepting a caller-selected
  post-finality entry.
- **High — W16/W17 advanced directly to the replacement tip and could skip a
  relevant non-tip transaction.** Their recovery cursor remains at the common
  ancestor. Replacement blocks replay in canonical order before an eventual
  tip/successor advance. Three-block replacement-path tests place relevant
  settlement and proof activity in a non-tip block, restart, replay, and then
  accept the next successor.
- **High — W13 exhausted its 128-entry journal and rejected transition 129.**
  W13 now rotates a bounded epoch checkpoint at the limit. The checkpoint
  binds the root bootstrap, prior terminal state and lineage, store, W12
  finality state, incident, and any recovered-incident lifecycle. Rotation
  and recovery return the active bootstrap and checkpoint digest for atomic
  persistence.
- **High — public submission durably retained unclaimed CEK material.** HTTP
  admission and Phase A are strict when no reference inputs exist. When
  reference-script outputs are not yet resolvable, attached programs must be
  complete and only residual exactness is deferred. Phase B unions attached,
  newly created, and resolved historical reference-script envelopes and
  verifies exact material coverage. The DA committee is strict. The CEK store
  persists only the verified reachable union and strict-rechecks retrieval.
  Rejected admissions replace the retained sidecar with canonical empty bytes
  while preserving its original digest for duplicate identity. A PostgreSQL
  transaction advisory lock serializes an aggregate queued/validating sidecar
  byte quota across direct and microbatch writers; the default is one full
  64 MiB V1 DA envelope, invalid limits fail configuration, and duplicates do
  not consume new capacity.
- **Medium — a pull-request branch filter could bypass Midgard Node CI.** The
  verifier rejects `branches` and `branches-ignore` filters, including quoted
  and spaced YAML keys, under `pull_request`. Both push and pull-request path
  scopes include every verifier surface.
- **Medium — required class capabilities could become private, protected,
  static, or optional.** AST verification now accepts only a concrete public
  instance method with a body. Mutation coverage exercises every rejected
  shape.
- **Medium — focused test counts were duplicated literals rather than runner
  evidence.** The map now declares expected collection counts. A dedicated
  verifier runs all 13 watcher files serially with Vitest JSON, requires
  nonzero exact collection and all-pass status per file, and matches the
  declared total. Midgard Node CI and Evidence Integrity CI both execute it;
  Evidence Integrity also executes the mutation suite.
- **Medium — the resolver applied-hash fixture used dummy parameters without
  crossing production construction.** The SDK check now invokes
  `buildFaultProofContracts` over the committed blueprint, compares the exact
  75 production-builder semantic identities to the Aiken fixture groups, and
  independently reapplies the Phase-A and ScriptSources prepare validators.
  The evidence is accurately labeled a deterministic production-builder
  fixture, not a live-deployment identity.

During the W13 fix review, a residual authentication flaw was found before
integration: an unkeyed prior-checkpoint digest was not an authority when the
caller controlled both the compacted state and replacement bootstrap. Epochs
above zero now require the separately persisted trusted checkpoint-state
digest. A hostile test self-hashes a forged epoch-one bootstrap and supplies
it as both state inputs; parsing returns `null` and evaluation rejects it.
Genesis remains the explicit installation trust anchor.

These fixes preserve the authoritative L1-source discriminator. `local_node`
uses one watcher-operated chain-sync authority plus aligned same-node query
surfaces without provider quorum. `external_providers` requires at least two
independent authorities. W14 consumes the canonical W10-W13 observation and
rollback pipeline and indexes node-accepted transaction/output/datum bytes; it
does not reimplement Cardano validator semantics.

## Final-tree evidence

- Aiken `v1.1.22+39d6b04` build and blueprint generation: PASS.
- Generated `onchain/aiken/plutus.json` SHA-256:
  `d49f3ced61d967e0043aabcd37cb3fe8c4ceea03553a6cfbca90013ba79f7e4d`;
  355 validators.
- Canonical native V1 cross-language selectors: 7/7 PASS. The newly covered
  maximum-profile terminal proof chunk selector passes 1/1.
- Watcher build, typecheck, lint, and format check: PASS.
- Watcher suite: all 13 files and 267/267 tests PASS in one serial,
  machine-counted run.
- Canonical watcher dependency-map verifier: 8/8 dependency classes PASS.
- Core package: 36 files and 273/273 tests PASS; production build PASS.
- Validation package: 37 files and 175/175 tests PASS; build/typecheck PASS.
- Lucid SDK: 148/148 tests PASS; typecheck PASS under Node `22.22.2`.
- Node material-chain focused suite: 3/3 PASS; Phase-4 isolation/verifier:
  27/27 PASS; native transaction integration: 79/79 PASS; SDK ABI fixtures:
  8/8 PASS; deployment/merge review regressions: 29/29 PASS; typecheck and lint
  PASS. The earlier complete package replay is diagnostic only because final
  review fixes changed source after it started; `GOAL_PROGRESS.md` does not
  award it final-tree PASS credit.
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
- Fresh CC-01 focused evidence: Phase A/B 50/50, retained DA corpus 20/20,
  Node writer/pipeline 15/15, database HTTP/material/quota 4/4 with 93
  unrelated tests skipped by exact selection, terminal scrub/duplicate 1/1,
  and quota configuration 5/5 PASS. The concurrency case admits exactly four
  empty-sidecar submissions at a four-sidecar cap, rejects 28/32 with the
  byte-quota response, proves the SQL byte/count cap, and still accepts an
  exact duplicate while full.
- Fresh W13 evidence: 25/25 PASS, including automatic 128-to-129 rotation,
  serialized restart, incident recovery, external checkpoint anchoring, and
  forged compacted-bootstrap rejection.
- Fresh W14-W17 evidence: 93/93 PASS (20 W14, 22 W15, 28 W16, 23 W17);
  watcher build, typecheck, ESLint, and Prettier PASS.
- SDK production-builder/Aiken applied-hash equivalence: 1/1 PASS; SDK
  typecheck, scoped ESLint, and scoped Prettier PASS. All eight affected Aiken
  guards independently collected one test and passed one test; Aiken formatter
  check PASS.
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
