# Canonical V1 pull-request review findings

## Status and scope

- **Review verdict:** request changes; do not merge the reviewed revision as-is.
- **Pull request:** [#471 — Canonical V1 watcher source modes and zero-input
  checkpoint](https://github.com/Anastasia-Labs/midgard/pull/471).
- **Base revision:** `8bae9403a13124f647f215999848ff5c82784e37`.
- **Reviewed head revision:** `d145d19d14eb8e8835f07d7e3e7647dc65423a14`.
- **Reviewed range:** `8bae9403a13124f647f215999848ff5c82784e37..d145d19d14eb8e8835f07d7e3e7647dc65423a14`.
- **Diff size:** 1,134 files, 380,743 insertions, and 36,106 deletions.
- **Method:** the complete changed-path list was divided among 32 independent
  GPT-5.6 Luna reviews at maximum reasoning, followed by cross-language,
  architecture, KISS/YAGNI, test-quality, and evidence-integrity passes.
- **Dirty-worktree rule:** pre-existing and concurrent uncommitted changes were
  excluded from the review range and preserved.

This document records review findings only. `GOAL_SPEC.md` remains authoritative
for Goal scope and acceptance. `GOAL_PROGRESS.md` is historical human context,
not an execution-state authority. Line numbers below refer to the reviewed head
revision and may move as findings are repaired.

Priority meanings:

- **P1:** merge blocker: fund safety, protocol soundness, liveness, build, or
  deterministic required-gate failure.
- **P2:** significant correctness, operability, maintainability, or coverage
  defect that should be resolved before declaring the Goal complete.
- **P3:** cleanup or simplification with lower immediate risk.

## P1 merge blockers

### RF-001 — Transaction-order receipt verification is discarded

`onchain/aiken/validators/user-events/tx-order-v1.ak:149-157` binds the Boolean
returned by `verify_order_receipts` instead of asserting that the result is
`True`. An invalid or unreceipted transaction order can therefore mint and later
break ingestion and commit barriers.

**Smallest repair:** use `expect True = verify_order_receipts(...)` and add
negative tests proving missing, malformed, and mismatched receipts cannot mint.

**Resolved (2026-08-10, #587):** the subject was retired rather than repaired.
`verify_order_receipts` and the counted publication receipt chain it walked are
deleted — the receipt mint gated on
`verify_midgard_transaction_field_chunk_v1`, which `docs/spec/midgard-tx.md` §4
makes unsatisfiable, so no receipt could be minted for any field carrying an
item. The mint now binds `verify_order_material` under `expect`
(`onchain/aiken/validators/user-events/tx-order-v1.ak`), which is the assertion
this row asked for, applied to the surviving check. The availability gap that
remained after that — the mint admitting only the canonically-empty transaction —
was closed by #594's owner ruling, which re-expressed `verify_order_material` on
the §8.8 field-access door against carriage supplied in the mint redeemer, with
positive and negative selectors per tier. #589 closed as superseded. Row kept for
history.

### RF-002 — False MPF non-membership proofs are accepted

`onchain/aiken/lib/midgard/mpf-proof-v1.ak:120-142` and `:255-267` validate a
terminal Fork or Leaf proof's shape and length but do not prove divergence from
the queried key. A singleton proof containing the queried key can reproduce the
root while returning `does_not_have = True`; this result is trusted by the
`reject_input_not_found` path.

**Smallest repair:** require the terminal nibble at `next_cursor - 1` to differ
from the query and add singleton-presence regressions.

### RF-003 — Confirmed-state merging applies only the child delta

`demo/midgard-node/src/transactions/state-queue/merge-to-confirmed-state.ts:1244`
calls the single-delta merge even though
`demo/midgard-node/src/transactions/state-queue/confirmed-ledger-snapshot.ts:246-250`
constructs a parent-plus-child `deltaChain`. In a pipelined journal, inputs
created by the parent can remain unspent while the pending state is cleared.

**Smallest repair:** apply the complete chain through
`applyConfirmedLedgerDeltaChainTransaction` and assert the chain roots before
clearing journal state.

### RF-004 — Reference-script replay cannot reach completion

`onchain/aiken/lib/midgard/validation-machine-v1.ak:8708-8864` increments the
processed source counter but leaves the successor's total source count
unchanged. The encoder's completion condition at `:1059` therefore cannot be
satisfied for the replay path.

**Smallest repair:** set the successor total to `next_sources.count` and add a
multi-source completion trace.

### RF-005 — Admitted stage-four outputs can be too large to challenge on L1

`onchain/aiken/lib/midgard/validation-machine-v1.ak:8933-9017` admits a complete
item up to 16,384 bytes, while the recorded one-step Cardano transaction limit
is 14,774 bytes. There is no chunked or reference-input fallback. A validly
admitted block can therefore produce an output that cannot be challenged.

**Smallest repair:** lower the consensus admission bound to the demonstrated L1
limit or implement and verify a bounded chunk/reference fallback.

### RF-006 — Header validation ignores committed validation-context scalars

`onchain/aiken/lib/midgard/ledger-state.ak:282-291` ignores the newly committed
`block_slot`, `expected_network_id`, `min_fee_a`, and `min_fee_b` fields declared
at `:79-82`. Commit and merge validators trust this predicate, while later claim
logic requires exact values. An invalid header can be committed and made
undisputable.

**Smallest repair:** enforce nonnegative fee values, the expected network, and
the exact block-slot invariants at header admission.

### RF-007 — Positive validation-trace roots can have malformed byte lengths

`onchain/aiken/lib/midgard/ledger-state.ak:216-221` and `:275-278` distinguish
only empty and nonempty roots. A one-byte root can pass header validation, but
claim construction later requires the canonical 32-byte value.

**Smallest repair:** require every positive-count validation-trace root to be
exactly 32 bytes.

### RF-008 — On-chain non-membership evidence has no off-chain submission route

`onchain/aiken/validators/fraud-proofs/validation-trace/resolve-inputs-non-membership-semantic-v1.ak:49-64`
emits auxiliary constructor 6. The accepted shapes in
`demo/midgard-fault-proofs/src/validation-dispute/submit.ts:577-585`, `:664-680`,
and `:2749-2783` do not recognize constructor 6, making the proof family
unsubmitable.

**Smallest repair:** define and route the tag-6 four-field shape through the
off-chain decoder and add a submit-to-validator integration test.

### RF-009 — Valid direct CEK constants above 4,095 bytes abort

`onchain/aiken/lib/midgard/cek-constant-v1.ak:8-11` limits
`hash_blob_chunk_v1` to 4,095 bytes. Direct witnesses are admitted up to 9,215
bytes by `cek-proof-v1.ak:484-489` and consumed by
`cek-machine-v1.ak:718-729`, which invokes the smaller helper.

**Smallest repair:** centralize the bounded blob-root implementation used by
both direct and streamed witnesses.

### RF-010 — Deep valid Plutus Data can crash the TypeScript verifier

`demo/midgard-validation/src/cek-data-tree.ts:235-342` recursively traverses
data. The protocol test at
`demo/midgard-validation/tests/plutus-data-unary-depth-boundary.test.ts:36-64`
admits depth 4,043, which can exhaust the JavaScript stack. The implementation
also recalculates subtree summaries repeatedly.

**Smallest repair:** replace recursion with one explicit post-order stack and
cache each subtree result once.

### RF-011 — The TypeScript CEK semantic constant decoder accepts noncanonical CBOR

`demo/midgard-core/src/cek-proof.ts:2141-2146` accepts definite encoding `8100`
where the L1 Aiken decoder requires canonical indefinite encoding `9f00ff`.

**Smallest repair:** decode and require byte-for-byte equality with the
canonical serialization used by L1.

### RF-012 — TypeScript semantic constants omit L1 payload/type validation

`demo/midgard-core/src/cek-proof.ts:2484-2527` accepts arbitrary Data for Unit
and Bool, does not establish UTF-8 for strings, and omits BLS payload-length
constraints. This result is used at `:3425-3427`, while Aiken enforces
`payload_matches_type`.

**Smallest repair:** mirror the exact Aiken payload/type predicate and share
cross-language fixtures for every semantic constant kind.

### RF-013 — TypeScript CEK builtins omit the aggregate direct-witness cap

`demo/midgard-validation/src/cek-builtin.ts:305-347` and `:829-924` enforce
per-item bounds but not the Aiken sum of all arguments and result, capped at
9,215 bytes. Material accepted by TypeScript can consequently be rejected on
L1.

**Smallest repair:** compute the same aggregate bound before accepting direct
witnesses.

### RF-014 — Large semantic builtin results are rejected off-chain but valid on-chain

`demo/midgard-validation/src/cek-builtin.ts:702-710` and `:807-818` throw for
large direct results, such as a 10,000-byte `replicate_byte`, even though Aiken
accepts a semantic constant representation.

**Smallest repair:** reuse the semantic result construction already present in
`demo/midgard-validation/src/cek-executor.ts:841-879`.

### RF-015 — Non-`every_block` modes construct incomplete spending payloads

`demo/midgard-node/src/workers/commit-block-header.ts:229-240` and `:2041-2050`
can require normal or forced validation while transaction entries are absent at
`:964-974`, `:1001-1011`, and `:1095-1106`. Alignment then receives an empty
list, and `demo/midgard-node/src/workers/utils/mpf.ts:3597-3960` constructs
empty MPF prestates.

**Smallest repair:** hydrate entries and prestates whenever either normal or
forced validation is required, independent of the source mode.

### RF-016 — DA committee and node packages do not typecheck

`demo/da-committee-node/src/l1/provider.ts:936`, `:994`, and `:1066-1069`
reference undeclared `WatcherConfig`, while `:973-974` references undeclared
`urls`. Tests at
`demo/da-committee-node/tests/libp2p-payload-protocols.test.ts:280` and `:321`
also provide stale mocks without `getL1SourceState`. The node package imports
the broken provider and fails as well.

**Smallest repair:** use one loaded configuration type, derive the canonical
provider URL list once, and update typed mocks.

### RF-017 — Canonical double-spend preparation compares different root domains

`demo/midgard-fault-proofs/src/prepare-double-spend.ts:567-582` compares a raw
PHAS root with the header's counted transactions root. Correct evidence is
therefore rejected.

**Smallest repair:** compute
`commitCountedRootProgram(TransactionsV1RootDomain, nativeRoot, count)` before
comparison.

### RF-018 — Metadata-only reference scripts remain executable

`demo/lucid-midgard/src/builder/script-materialization.ts:472-481` and
`:582-590` let metadata-only reference scripts reach Phase B without an
envelope or canonical material, where L1 rejects them.

**Smallest repair:** reject metadata-only Path V1 scripts or require canonical
material before the transaction can be completed.

### RF-019 — Same-point watcher replacement bypasses rollback

`demo/midgard-watcher/src/user-event-indexer.ts:2957-2969`, `:3128-3136`, and
`:3194-3244` accept a same-point replacement through the normal apply path.
The durable store replaces pending state without constructing rollback
history, despite the finality engine emitting an explicit rewind operation.

**Smallest repair:** accept normal indexing only for held or finality-granted
events and route every replacement through rollback and replay.

### RF-020 — Watcher storage accepts impossible spend chronology

`demo/midgard-watcher/src/durable-store.ts:990-1000` proves only that the
referenced output exists. It does not require the creation point to precede the
spend point, so rollback can restore an output before it was created.

**Smallest repair:** compare canonical `(blockNo, slot)` order and reject
non-monotonic spends.

### RF-021 — New Aiken validators reference missing or private helpers

The following validators call unavailable helpers and are build blockers:

- `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-finalize-frame-executor-v1.ak:78,101,105-108`;
- `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-fold-map-executor-v1.ak:83,104-107`;
- `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-outer-normalizer-v1.ak:81-83,124-132`.

These validators are also unwired and untested.

**Smallest repair:** remove them until an integration route exists; otherwise
implement/export the helpers and add exact validator tests.

### RF-022 — Goal closure can claim release readiness with open nested evidence

`demo/scripts/canonical-v1-goal-closure-v1.mjs:438-454` requires a top-level
status and nonempty evidence lists but does not require every nested artifact
to be bound. `demo/scripts/verify-canonical-v1-goal-closure.mjs:136-177` skips
the necessary binding semantics, while the decoder's “PASS requires bound
evidence” rule checks only list length.

**Smallest repair:** require `isBoundFile` for every required nested artifact
before release-ready status is accepted.

**Retired (2026-08-29):** the Goal closure manifest, decoder, verifier,
self-test, verification plan, and aggregate harness were deleted. Progress
notes no longer participate in release readiness, so this failure mode no
longer exists.

### RF-023 — Reconciliation and closure evidence is stale

The following files carry hashes that do not match the reviewed tree while
their surrounding ledger claims PASS:

- `docs/exec-plans/evidence/canonical-v1-capability-reconciliation-v1.json:5-8,29-32`;
- `docs/exec-plans/evidence/canonical-v1-fault-proof-reconciliation-v1.json:4-7`;
- the now-retired Goal task and closure manifests.

**Smallest repair:** regenerate and bind evidence from the final source tree, or
truthfully mark each unavailable artifact OPEN.

### RF-024 — The format registry reports 132 PASS rows without final-tree evidence

`docs/exec-plans/evidence/canonical-v1-format-registry-v1.json:1-6` and
`:493-503` contain a 16,699-line registry with no source-revision binding and
all rows marked PASS. `GOAL_PROGRESS.md:1149`, `:2236`, `:2618-2620`, and
`:2752-2755` say 122 rows remain open or unverified.

**Smallest repair:** generate one revision-bound registry from canonical source
and preserve truthful OPEN/PASS states.

### RF-025 — Documentation facts deterministically fail

`docs-site/content/docs/fault-proofs/overview.mdx:40` and `:52-57` state that
there are 30 commands and omit `scaffold-family`; production source exposes 31.
The required docs-facts verifier fails.

### RF-026 — The watcher dependency map deterministically fails

`docs/exec-plans/evidence/canonical-v1-watcher-dependency-map-v1.json:25-60`
contains 17 stale source hashes and omits the public-DA client and test. The
exact verifier at
`demo/scripts/verify-canonical-v1-watcher-dependency-map.mjs:718-725` and
`:842-860` is invoked by CI and rejects the current tree.

### RF-027 — Focused watcher test manifests omit a gated test

The watcher Vitest glob includes
`demo/midgard-watcher/tests/public-da-client.test.ts`, but the exact focused
manifest in
`demo/scripts/verify-canonical-v1-watcher-dependency-map-focused-tests.mjs:16-78`
hardcodes 13 tests and excludes it. CI therefore rejects the newly gated test.

**Smallest repair for RF-026/RF-027:** either remove the dormant public-DA
source and test or regenerate both exact manifests from the final intended
surface.

### RF-028 — The data-breadth gate rejects expanded coverage

`demo/midgard-validation/scripts/verify-goal-cardano-capability-p2-data-breadth.mjs:7-8`
and `:75-91` require two cases. The test suite has four at
`demo/midgard-validation/tests/data-breadth-boundary.test.ts:623-626` and
`:803`; the retained-DA verifier independently hardcodes two.

**Smallest repair:** derive the exact expected set from one manifest or update
both gates to four.

### RF-029 — Constructor-30 ABI fixtures disagree across languages

`onchain/aiken/lib/midgard/validation-controls-v1-abi.test.ak:166-168` expects
arity one. The TypeScript encoder at
`demo/midgard-validation/src/validation-machine-data.ts:1356-1360`, SDK schema
at `demo/midgard-sdk/src/validation-auxiliary-witness.ts:1140-1143`, and
actual Aiken type at
`onchain/aiken/lib/midgard/validation-machine-v1.ak:272-275` use arity two.

**Smallest repair:** generate one shared fixture from the canonical schema and
consume it from all three suites.

### RF-030 — The formal Header schema is incompatible with canonical HeaderV1

`technical-spec/1-ledger-state/1-block.tex:33-37` and `:138-159` define eight
roots and six counts. `onchain/aiken/lib/midgard/ledger-state.ak:60-85` defines
nine roots and seven counts plus slot, network, and fee fields. Architecture
documentation presents yet another projection.

**Smallest repair:** establish one exact normative HeaderV1 schema and generate
or mechanically verify all implementation and documentation projections.

### RF-031 — Transaction and forced root value types disagree across sources

`technical-spec/6-offchain-data-architecture/1-da-layer.tex:41-56`, legacy
ledger equations, `docs/consensus-profile-v1.md:98-109`, and
`demo/midgard-sdk/src/ledger-state.ts:681-700` disagree about the values
committed by the transactions and forced root fields.

**Smallest repair:** define exact domain-separated byte encodings in the
normative specification and bind cross-language fixtures to them.

### RF-032 — The published testnet acceptance command targets an absent script

`demo/scripts/canonical-v1-goal-testnet-acceptance.mjs:59-66` delegates to
`demo/midgard-node/scripts/canonical-v1-goal-testnet-acceptance.mjs`, which does
not exist, while `demo/package.json:29` publishes the command.

**Smallest repair:** implement the guarded orchestrator or remove the command
and keep the acceptance criterion OPEN.

### RF-033 — Conflict-evidence test leaves its exclusive store locked

`demo/da-committee-node/tests/conflict-evidence.test.ts:31-48` opens an
exclusive `JsonFileWatcherStore`, writes through it, and opens a second
exclusive store at line 44 without closing the first. The focused test fails
deterministically.

**Smallest repair:** close the first store before reopening it and retain a
separate test proving concurrent exclusive opens fail.

## P2 correctness, safety, and operability findings

### RF-034 — Fresh PostgreSQL deployments reject early DA signatures

`demo/da-committee-node/src/store/postgres.ts:525-536` and `:956-966` reject a
signature when source state is absent, but source state is created only at the
end of the first watcher tick.

**Repair direction:** treat absent state as nonquarantined under a strict row
lock, or atomically initialize source state before accepting traffic.

### RF-035 — DA proof artifacts do not bind the new validation-trace commitment

`demo/midgard-node/src/da/proof-artifacts.ts:415-435` and `:894-1001` compute
the new validation-trace root/count but compare only legacy commitments. A
mismatched artifact can be served as valid.

### RF-036 — SDK public header validation checks only the forced root

`demo/midgard-sdk/src/ledger-state.ts:199-203` validates the forced root/count
but omits transaction, deposit, withdrawal, and other committed roots. The
state queue consumes this partial validation.

### RF-037 — Invalid confirmation depths fail open

`demo/midgard-sdk/src/evidence-source.ts:262-301` does not require
`minimumConfirmationDepth` to be a nonnegative safe integer. `-1`, `NaN`, and
other invalid values can bypass the intended delay.

### RF-038 — SDK validation proof items are not bound to collection proofs

`demo/midgard-sdk/src/validation-proof-item.ts:81-109` does not verify item
length or commitment against the supplied collection proof. A test intentionally
combines a 14,396-byte item with proof length two without resolving the datum.

### RF-039 — Fault-proof scaffolding drops `outputState`

`demo/midgard-fault-proofs/src/family-scaffold/spec.ts:204-229` carries an
output state, but `emit.ts:120-255` and `:544-609` do not enforce it against
the next input or emitted witness.

### RF-040 — One-step scaffold specifications are accepted but cannot be emitted

`demo/midgard-fault-proofs/src/family-scaffold/spec.ts:318-324` accepts
fewer than two states, while `emit.ts:446-506` requires a transition.

**Repair direction for RF-039/RF-040:** reject specifications with fewer than
two states and validate every emitted output state against the following input.

### RF-041 — Transition traces do not require dense canonical indexes

`demo/midgard-fault-proofs/src/transition-trace/reconstruct.ts:265-290` and
`:887-912` accept sparse or duplicate decoded keys. Detection can consequently
operate over a trace that is not `0..N-1`.

### RF-042 — Transition proof submission lacks a transaction-size guard

The submission path around
`demo/midgard-fault-proofs/src/validation-dispute/submit.ts:417-465` places the
full proof in datum and redeemer without proving it fits the canonical 16 KiB
profile.

### RF-043 — Q03 diagnostic provenance remains submit-capable

The provenance checks in `demo/midgard-fault-proofs/src/bin.ts:676-855` are
advisory, but preparation and submission still permit legacy diagnostic
evidence to reach L1.

**Repair direction:** encode and enforce exact provenance or remove the legacy
submission route.

### RF-044 — CEK persistence failures are converted into empty awaiting sidecars

`demo/midgard-node/src/workers/fetch-and-insert-tx-order-utxos.ts:588-656`
catches infrastructure and verification failures and commits an `Awaiting`
record with an empty sidecar.

**Smallest repair:** propagate all errors except one explicit typed
material-not-yet-published condition.

### RF-045 — Validation claims do not enforce event family and phase

`onchain/aiken/lib/midgard/validation-claim-v1.ak:306-348` does not derive the
phase from the event key. A forced-family key can target an L2 phase when the
roots happen to contain compatible data.

### RF-046 — Builder completion does not require complete script material

`demo/lucid-midgard/src/builder.ts:3134-3153` and `:782-785` allow transaction
completion without proving that every referenced script has canonical material.

### RF-047 — Empty policy maps receive an ambiguous preimage

`onchain/aiken/lib/midgard/fraud-proofs/native-tx/preimages.ak:553-557` accepts
empty CBOR map `a0` with the same commitment treatment as canonical empty list
`80`; the off-chain parser rejects it.

### RF-048 — Protocol-info rejects configurations accepted by configuration parsing

`demo/midgard-node/src/commands/protocol-info.ts:96-104` requires the submit
bound to equal the maximum, while configuration accepts any value from one to
the maximum. A valid configuration can yield an HTTP 500 endpoint.

### RF-049 — Watchdog journal sequencing can become unverifiable

`demo/midgard-node/scripts/throughput-load-watchdog.mjs:433-442` increments the
sequence before validation. Cleanup later swallows oversized invalid error
records, leaving a gap in the journal.

### RF-050 — Unix-socket identity checking is vulnerable to path replacement

`demo/midgard-watcher/src/l1-adapter.ts:1653-1690` checks socket metadata and
then connects by path, allowing the target to change between validation and
use.

### RF-051 — Watcher wire commitments depend on raw JSON property order

`demo/midgard-watcher/src/user-event-indexer.ts:472-477` and
`rollback-engine.ts:464-465,604,790-791` hash or compare raw JSON rather than a
shared canonical serialization.

### RF-052 — Validation data scanning is quadratic at admitted depths

`demo/midgard-validation/src/cek-data-scan.ts:284-291` and `:447-562`
recompute summaries along deep paths.

**Smallest repair:** use the same explicit post-order traversal proposed for
RF-010 and compute each node summary once.

### RF-053 — Semantic resolver families no longer enforce family guards

Generic dispatch at
`onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-stage-one-redeemer-semantic-v1.ak:45-65`
and analogous stage-ten and stage-twelve validators accepts witnesses and
branches belonging to other families, contrary to
`onchain/aiken/lib/midgard/validation-resolver-v1.ak:131-135`.

**Smallest repair:** restore specialized verifiers or add an explicit family
and branch guard before generic dispatch.

### RF-054 — The formal custom-native rule still contains normative TODOs

`technical-spec/5-ledger-rules/2-custom-native-script-validation.tex:23-29`
leaves forbidden-field behavior unresolved in a normative protocol path.

### RF-055 — Necessity artifacts and evidence metadata are not final-tree bound

Several capability-necessity artifacts refer to stale blueprint/source state,
and their schema lacks sufficient source and evidence metadata to establish
that the measured result belongs to the reviewed release tree.

## KISS, YAGNI, and clean-code findings

### RF-056 — Delete the dormant 3,015-line public-DA client

`demo/midgard-watcher/src/public-da-client.ts` is approximately 102 KiB, its
transport interface is unimplemented at `:61-63` and `:203-216`, the package
build excludes it, and only its own deep-importing test references it.

**Recommendation:** delete the source and test until a production transport
and integration route are ready. Exporting it would turn dead code into public
API without delivering value.

### RF-057 — Delete six unwired stage-one redeemer validators

The envelope, execution-settlement, finalize-frame-executor,
fold-map-executor, outer-normalizer, and traversal-normalizer validators add
891 untested on-chain lines. They have no registry or production references;
only the semantic sibling is wired through
`demo/midgard-sdk/src/fraud-proof/contracts.ts:198-201`.

**Recommendation:** remove them until the complete proof route and exact tests
exist. This also resolves part of RF-021.

### RF-058 — Watcher packages import sibling/private dependencies

`demo/midgard-watcher/src/state-queue-indexer.ts:27-31` and
`user-event-indexer.ts:31-46` import from sibling package internals, including
`../../midgard-core/node_modules/@noble/hashes`. Behavior depends on repository
layout rather than declared package boundaries.

**Recommendation:** consume an exported core API or add one direct dependency.

### RF-059 — Four watcher indexers duplicate ancestor and transition logic

The proof, state-queue, settlement, and user-event indexers each contain large,
near-identical ancestor lookup, same-point handling, canonicalization, and
transition loops. They have already drifted in replacement behavior.

**Recommendation:** extract only small bounded primitives for point comparison,
ancestor resolution, and transition validation; retain domain-specific apply
logic in each indexer.

### RF-060 — The prelaunch “migration” layer is only a V1 initializer

`demo/midgard-watcher/src/durable-store.ts:12-32`, `:1297-1310`, and
`:1589-1627` carry migration/versioning machinery that only reads or creates
V1. Prelaunch policy explicitly favors replacing schemas in place.

**Recommendation:** rename it to initialization and remove migration branches
until a schema has actually shipped.

### RF-061 — The TypeScript validation-machine generator is monolithic and duplicates scanners

`demo/midgard-validation/src/validation-machine.ts:1237-7507` is roughly 6,300
lines. Native scanning logic at `:2738-3070` and `:5005-5259` is duplicated.

**Recommendation:** extract shared per-phase scan/result helpers with one exact
input/output contract; avoid a broad framework rewrite.

### RF-062 — BLAKE2b trace implementations are almost identical

`demo/midgard-core/src/blake2b-224-trace.ts` and
`blake2b-256-trace.ts` each contain about 468 lines of duplicated state
machine logic.

**Recommendation:** implement one private parameterized engine and retain thin
domain-specific wrappers.

### RF-063 — ABI fixtures are copied across three suites

The constructor fixture is duplicated in validation, SDK, and Aiken tests,
which directly enabled RF-029.

**Recommendation:** generate one fixture/manifest from the canonical schema and
make every language consume it.

### RF-064 — Compact-binding validation bypasses a shared combinator

`onchain/aiken/validators/fraud-proofs/validation-trace/compact-binding-semantic-v1.ak:56`
duplicates the `continue_winning` flow rather than using the common verifier.

### RF-065 — Canonical proof-item reference parsing is duplicated

The observe and semantic validation-trace validators contain parallel parsing
implementations around their respective lines 154-158.

### RF-066 — A dead transition helper remains in the Aiken validation machine

`onchain/aiken/lib/midgard/validation-machine-v1.ak:12204-12230` has no reachable
caller and should be removed rather than retained as a speculative seam.

### RF-067 — DA manifest callers retain unreachable nullable branches

`demo/midgard-node/src/da/libp2p-producer.ts:329-342` either returns a manifest
or throws, but callers at `:549-555`, `:897-945`, `:1059-1064`, and
`:1137-1177` retain null branches. The reconciler repeats the dead state.

### RF-068 — SDK compatibility branch has identical outcomes

`demo/midgard-sdk/src/state-queue-transactions.ts:573-584` selects between two
branches that perform the same operation.

### RF-069 — Reconcile CLI passes unread options

`demo/midgard-node/src/commands/reconcile.ts:971-983` passes `watcherUrl` and
`deploymentFingerprint`, but the receiving path never reads them.

### RF-070 — Historical plan and progress surfaces obscure current truth

Nineteen dated checkpoint plans add approximately 2,859 lines that duplicate
the ledger, while `GOAL_PROGRESS.md` behaves as an append-only diary with no
concise current-action view. Obsolete phase-five decision prose adds further
conflicting status.

**Recommendation:** retain a short current-state ledger and archive historical
narrative outside the active acceptance surface.

### RF-071 — Package/configuration cleanup

- The SDK declares core as both a development and runtime dependency.
- Prettier exclusions are duplicated.
- The workspace's pnpm declaration and lock/tool expectations are not pinned
  to one exact version.

These are P3 cleanup items, but resolving them reduces nondeterministic local
and CI behavior.

## Test-quality findings

### RF-072 — Throwaway C26 tests live in the default suite

`demo/midgard-validation/tests/c26-investigation-cml-depth-limits.test.ts` and
`c26-investigation-cml-depth-limits-iterative-gate.test.ts` explicitly describe
themselves as throwaway/non-evidence and normally skip behind environment
variables.

**Recommendation:** delete them or move them to an explicit investigation
command outside the default test glob.

### RF-073 — Source-text tests substitute for behavior tests

Examples include:

- `demo/midgard-node/tests/canonical-commit-profile.test.ts:24-158`;
- `demo/midgard-node/tests/commit-source-completeness.test.ts:47-69`;
- `demo/midgard-node/tests/pipeline-status-route.test.ts:31-45`;
- `demo/midgard-node/tests/reconcile-da-attested.test.ts:133-140`.

They assert regular expressions or source layout rather than invoking runtime
behavior, and can pass while semantics are broken.

### RF-074 — Worker-output test is tautological

`demo/midgard-node/tests/commit-block-header-worker-output.test.ts:88-96`
primarily restates a TypeScript type and provides little regression value.

### RF-075 — Lucid builder tests lost high-value completion coverage

`demo/lucid-midgard/tests/script-builders.test.ts:52-73` snapshots builder state
but no longer calls `.complete()` or validates pointers, hashes, reference
scripts, materialization, and rejection behavior.

**Recommendation:** restore a small end-to-end matrix covering inline scripts,
reference scripts, missing material, and canonical completion.

### RF-076 — Native compact golden generation has no freshness check

`demo/lucid-midgard/scripts/generate-native-compact-aiken-goldens.mjs:124-278`
writes fixtures, while the prior conformance test was removed.

**Recommendation:** add a non-writing `--check` mode and gate it in the focused
test suite.

### RF-077 — Lucid provider tests cover only empty script material

`demo/lucid-midgard/tests/provider.test.ts:568-612` does not round-trip a
nonempty canonical material envelope.

### RF-078 — DA operational coverage was substantially weakened

`demo/midgard-node/tests/da-multi-peer-integration.test.ts:85-205` uses one
process and three transactions, replacing child-process, 50,000-transaction,
RSS, and large-sample coverage. The larger reconciler is opt-in and therefore
not a required regression surface.

**Recommendation:** restore a bounded child-process test using the exact 50,000
case and keep broader soak testing opt-in.

### RF-079 — MPF probe tests never exercise the producer

`demo/midgard-node/tests/mpf-commit-candidate-probe-artifacts.test.ts:598-695`
and related seed tests hand-build artifacts and invoke validators directly.

**Recommendation:** add one command/worker-level test proving the real emitter
produces an artifact accepted by the validator.

### RF-080 — Canonical double-spend “valid control” fails too early

`demo/midgard-fault-proofs/tests/canonical-evidence-source.test.ts:518-532`
expects an unauthenticated-root error and never reaches double-spend detection.
Its input flattening is also incorrect.

**Recommendation:** authenticate the root and assert that the valid control
contains no double spend after flattening the actual inputs.

### RF-081 — Negative fault-proof tests publish expensive scripts before stopping

`demo/midgard-fault-proofs/tests/submit-init-emulator.test.ts:7031-7059`
publishes scripts before scenarios that stop at an earlier validation branch.

**Recommendation:** publish lazily only after the tested route requires L1
submission.

### RF-082 — DA stream-level safety coverage was removed

`demo/da-committee-node/tests/libp2p-payload-protocols.test.ts` now tests direct
admission but no longer proves handler deadlines, stalled-stream termination,
or recovery after malformed input.

**Recommendation:** restore focused handler-level tests for timeout, stalled
stream, malformed frame, and subsequent healthy request.

### RF-083 — DA deployment fixtures fabricate missing contracts

`demo/da-committee-node/tests/helpers/deployment-fixture.ts:122-170` and
`:218-305` copy `stateQueueSpend` material into missing contract slots. Tests
can pass with an incomplete or invalid deployment fixture.

**Recommendation:** fail fast unless every required contract is present.

### RF-084 — Native Lucid high-cardinality fixture freshness is no longer verified

The prior high-cardinality golden recomputation was removed, and no normal test
proves that generated fixtures still match current Aiken behavior.

### RF-085 — Duplicate deposit test

`onchain/aiken/validators/user-events/deposit.ak:376-386` duplicates an existing
case without adding a distinct boundary or failure mode.

## Verification performed at the reviewed revision

### Passed

- `git diff --check` for the committed review range.
- `demo/midgard-watcher` typecheck.
- `demo/midgard-core` typecheck.
- `demo/midgard-validation` typecheck.
- `demo/midgard-sdk` typecheck.
- `demo/midgard-fault-proofs` typecheck.
- `demo/lucid-midgard` typecheck.
- `node demo/scripts/verify-canonical-v1-format-registry.mjs` exited PASS, but
  RF-024 explains why that PASS does not establish truthful final-tree status.

### Failed

- `demo/da-committee-node` typecheck: 16 TypeScript errors centered on RF-016.
- `demo/midgard-node` typecheck because it imports the broken DA provider.
- `pnpm --dir demo/da-committee-node exec vitest run tests/conflict-evidence.test.ts`:
  one of two tests failed due to RF-033.
- `node docs-site/scripts/check-docs-facts.mjs`: RF-025.
- `node demo/scripts/verify-canonical-v1-watcher-dependency-map.mjs`: RF-026.
- `node demo/scripts/verify-canonical-v1-capability-reconciliation.mjs`: stale
  Goal-spec source hash, RF-023.
- `node demo/scripts/verify-canonical-v1-fault-proof-reconciliation.mjs`: stale
  matrix source hash, RF-023.

### Not run destructively

The testnet acceptance runner correctly required an explicit state-changing
environment guard. No acceptance variables were set and no external testnet,
deployment, wallet, or protocol state was modified during review.

## Recommended repair order

1. Fix RF-001 through RF-021: validator soundness, CEK parity, confirmed-state
   application, watcher rollback, challenge feasibility, DA build, and reachable
   proof routes.
2. Reconcile the normative schema and cross-language ABI in RF-029 through
   RF-031 before regenerating artifacts.
3. Remove dormant code identified by RF-056 and RF-057 rather than repairing
   speculative surfaces.
4. Repair the significant P2 semantics and operational paths RF-034 through
   RF-055.
5. Regenerate all evidence and exact manifests from the final source tree,
   then make RF-022 through RF-028 pass without skips or stale bindings.
6. Replace low-value tests with the focused runtime coverage in RF-072 through
   RF-085.
7. Rerun package typechecks, focused validator checks, exact verifiers, docs
   facts, and the named Goal acceptance surfaces before updating any PASS or
   release-ready claim.
