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

## Final-tree evidence

- Aiken `v1.1.22+39d6b04` build and blueprint generation: PASS.
- Generated `onchain/aiken/plutus.json` SHA-256:
  `449e7aecc51820f77866e6fe15c79ce29b7e3ea3ad9425b55f90d14abcbc3b81`.
- On-chain hostile selectors for transition replay, forced timing, and source
  lifecycle: 12/12 PASS; critical final-tree replays: 2/2 PASS.
- Watcher typecheck and lint: PASS.
- Watcher suite: 194/194 PASS.
- Canonical watcher dependency-map verifier: 8/8 dependency classes PASS.
- Core deployment identity: 6/6 PASS.
- Node deployment/contract registry: 24/24 PASS.
- SDK fault-proof catalogue/blueprint integration: 17/17 PASS.
- Fault-proof package: 14/14 files and 107/107 tests PASS; rebuilt-blueprint
  zero-input emulator: 1/1 PASS.
- Retained-DA verifier: 13 producer files/14 tests, 20 DA consumer tests, and
  3 fault-proof consumer tests PASS after private-corpus regeneration.
- DA package: 26/26 executed files and 184/184 executed tests PASS; the one
  PostgreSQL-environment test is explicitly skipped here and its hostile
  startup-journal regression passed separately against the configured local
  PostgreSQL instance.
- DA source-mode typecheck, production build, and no-HTTP transport guard:
  PASS.
- Fault-proof typecheck, ESLint, and scoped Prettier: PASS.
- Documentation facts (9 groups), links (190 Markdown/MDX files), and voice
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
