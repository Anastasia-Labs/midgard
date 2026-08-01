# Canonical V1 capability, state correction, and autonomous challenger

## 0. Status and intent

- **Status:** active authoritative execution specification.
- **Owner/approver:** the repository owner (Philip DiSarro). Amendments
  require owner approval and are delivered as ordinary reviewed commits.
- **Last reviewed:** 2026-07-29 (adversarial implementability review; this
  revision incorporates its corrections).
- **Integrity:** this file is tracked in Git at the repository root (§2.4);
  Git already content-addresses it, so amendment history and drift detection
  come from `git log -p -- GOAL_SPEC.md`. Do not record this file's SHA-256
  in the ledger or bind it inside evidence artifacts. (Owner amendment
  2026-08-01: the former rebind-cascade rule caught no defect in the
  program's history, produced six divergent recorded hashes and fifteen
  rebinding commits, and left the capability-reconciliation gate failing on
  a stale copy of its own bookkeeping.)

This document is the complete and authoritative execution specification for
one repository Goal with three inseparable outcomes:

1. **Canonical V1 is independently verifiable on Cardano L1 at the Cardano
   transaction-capability floor.**
2. **Every enabled fund-safety or state-correction rule has a sound, reachable,
   operational fault-proof path.**
3. **An independent production watcher can reconstruct, detect, prove, and
   complete correction without trusting an operator's private state.**

The Goal is not complete when code exists, an emulator test passes, a matrix
looks mostly green, or a watcher detects a fault. It is complete only when
every acceptance criterion in §12 is satisfied at the `releaseCommit` defined
in §0.2 and the verification in §13 proves it.

This is a long-running production-L2 closure program. Correctness, safety,
liveness, performance, and convenience are prioritized in that order.

### 0.1 Authoring baseline

This specification was authored at repository revision
`d5f36df25a9a1696e4df857e01aa81d2f0b6ef96`.

The external Graphify graph used for navigation indexes
`320ed869262dba7f4aac5627f1bd9efa0b5618a6` and is stale relative to that
baseline. Graph results are navigation hints only. Every consequential
relationship, absence, protocol rule, and completion claim must be verified
against source at the revision being changed.

The checkout was already dirty when this specification was authored. A future
Goal run must record its own starting revision and dirty state. Pre-existing
and in-flight work is input to the Goal: integrate and finish it wherever the
Goal's outcomes require it, recording provenance in `GOAL_PROGRESS.md`, and
never delete or descope it merely to simplify delivery (§3 invariant 14).

### 0.2 Release and evidence revisions

Goal completion binds two kinds of revision, because a commit cannot contain
its own hash:

- **`releaseCommit`** — the final integration commit containing every
  source-bearing deliverable: production source, schemas, migrations, tests,
  fixtures, generated validators, blueprint, and documentation. Deployment,
  testnet acceptance, the release-evidence digest, and every source-bearing
  claim bind exactly this commit.
- **Evidence commits** — zero or more descendants of `releaseCommit` whose
  diffs touch only the declared evidence paths below. Live acceptance
  evidence, the closure manifest, the completion report, and ledger updates
  produced after `releaseCommit` land here. The Goal pull request's final head
  is the last evidence commit, or `releaseCommit` itself when nothing needed
  recording afterward.

Declared evidence paths: `GOAL_PROGRESS.md`,
`docs/exec-plans/canonical-v1-goal-completion-report.md`, and
`docs/exec-plans/evidence/`.

Binding rules:

- The closure manifest and all other evidence identify `releaseCommit`
  exactly. No artifact is ever required to record the hash of the commit that
  contains it.
- The closure verifier resolves the current checkout itself and passes only
  when HEAD is `releaseCommit` or a descendant whose entire
  `releaseCommit..HEAD` diff is confined to the declared evidence paths.
- An evidence commit must not change source, generated validators, schemas,
  commands, or any behavior-bearing file. Such a change moves `releaseCommit`
  and invalidates every piece of evidence bound to the old one.
- A live-acceptance discovery that changes a readiness claim — a failed
  drill, a new §9.5 residual launch blocker, a weakened capability
  statement — legitimately moves `releaseCommit`. Only pure evidence
  recording stays in evidence commits.

## 1. Authority and conflict rules

The following authorities are incorporated by reference. They define protocol
semantics and evidence requirements; this file defines the execution scope,
dependency order, task granularity, and completion decision.

1. `GOAL_SPEC.md`.
2. `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`.
3. `docs/consensus-profile-v1.md` and the exact compiled canonical V1 profile.
4. `docs/exec-plans/cardano-capability-proof-completion.md`.
5. `docs/exec-plans/cardano-capability-p2-closure-matrix-2026-07-26.md`.
6. `docs/fault-proofs/execution-plan.md`,
   `docs/fault-proofs/coverage-matrix.md`, and
   `docs/fault-proofs/catalogue-status.md`.
7. `demo/midgard-watcher/midgard-watcher-architecture.md` and
   `demo/midgard-watcher/watcher-plan-adversarial-review.md`.
8. The normative technical specification: `technical-spec/`, entry point
   `technical-spec/midgard.tex`, built into `technical-spec/midgard.pdf` by
   the root `make spec` target.
9. Current source, generated validators, and tests as implementation evidence.

Precedence by claim type:

- **Protocol semantics and on-chain rules:** the normative technical
  specification and the accepted decision/plan documents (items 2–7), with
  current deployed source as implementation evidence, in that order.
- **Execution scope, task granularity, dependency order, evidence
  obligations, and the completion decision:** this file (item 1).
- **Implementation status:** current source plus executable final-tree
  evidence only; a prose claim or status table never outranks a failing or
  missing executable check.

Conflict handling:

- The accepted capability-floor decision and canonical V1 semantics must not
  be weakened by an older plan, stale status table, emulator convenience, or
  current implementation limitation.
- Current source and final-tree tests may prove that an older status entry is
  already complete, but source cannot silently narrow required protocol
  capability.
- The complete-item-first policy and necessity gate in §3.2 supersede any
  older plan, matrix, source comment, or test assumption that treats absence
  of complete proof items or mandatory bounded traversal as closure. Reconcile
  those sources to this file before promoting their status.
- A matrix or plan status is not evidence by itself. Update stale status only
  after source and executable evidence agree.
- If two normative sources genuinely require incompatible behavior, record the
  exact conflict in `GOAL_PROGRESS.md`, exhaust source/history/spec evidence,
  and treat it as a blocker. Do not select the easier behavior.

## 2. Scope

### 2.1 In scope

#### G1 — Canonical V1 Cardano-capability activation

Complete P0 through P6 of
`docs/exec-plans/cardano-capability-proof-completion.md`:

- preserve the sole pre-launch canonical V1 format and fail-closed gate;
- retain and authenticate maximum Cardano-capable dynamic content;
- make authenticated complete proof items the default challenged-transition
  input, carried directly or published once in an inline-datum output and then
  consumed or referenced by the proof transaction;
- add multi-output publication, chunked reveals, or incremental traversal only
  for a proof family whose concrete final-validator path proves that complete
  direct and single-datum-reference carriage cannot fit;
- complete narrow L1 resolvers and all enabled ledger semantics;
- complete normal, forced, invalid, no-op, and misclassification paths;
- derive target-network capability parity and adjacent-boundary fixtures;
- measure actual applied/parameterized L1 proof transactions;
- bind the final parameter snapshot, profile, DA framing, program/rule
  commitments, validator hashes, and measurements into release evidence; and
- pass a fresh, resource-bounded testnet deployment and acceptance run.

#### G2 — Complete state-correction coverage

For every enabled canonical V1 rule that can admit, preserve, or fail to remove
an invalid state:

- name the rule and violation;
- define canonical evidence;
- implement a sound L1 verifier;
- make the proof reachable through the deployed catalogue;
- build evidence from retained public data;
- provide resumable prepare/submit/remove tooling;
- prove valid blocks cannot be challenged;
- complete emulator and target-testnet acceptance; and
- restore canonical state, slash the correct operator, and pay the prover
  exactly as specified.

This includes single-party proof families, interactive validation disputes,
transition-trace faults, event inclusion/classification faults, data
commitment faults, and correction lifecycle faults.

#### G4 — Independent autonomous watcher/challenger

Implement `demo/midgard-watcher` as an actual workspace package and production
service, distinct from the DA committee and operator node. It must:

- verify deployment identity;
- follow Cardano through exactly one declared §3.1.8 L1-source mode: a
  watcher-operated `local_node` with aligned query/index services, or
  `external_providers` with at least two operationally independent providers;
- apply explicit finality and rollback rules;
- retrieve authenticated DA and proof data without operator-private storage;
- reconstruct and replay every queued block deterministically;
- produce a canonical watcher decision;
- classify a violation into an enabled proof family;
- durably build and submit the complete proof workflow;
- recover across process and L1 rollback boundaries;
- track removal/slashing to completion; and
- expose reproducible replay, status, metrics, and alerts.

### 2.2 Necessary cross-cutting DA scope

The standalone broad DA/governance program is not independently added as a
fourth Goal. The following DA work is nevertheless mandatory because G2 and
G4 cannot be truthful without it:

- authenticated public or permissionless retrieval of every proof-critical
  retained byte and witness;
- canonical `DaPayloadV1` and proof-bundle reconstruction;
- retention for at least canonical maturity plus worst-case proof time and
  safety margin;
- hash/preimage accountability;
- fail-closed handling of missing, malformed, mismatched, stale, or
  deployment-mismatched material; and
- an enforceable unavailable-data outcome that prevents an unverifiable block
  from silently maturing.

Do not expand this Goal into unrelated DA throughput, committee product
features, or governance redesign except where a listed acceptance criterion
cannot otherwise be satisfied.

### 2.3 Explicitly out of scope

- Mainnet deployment or mainnet value submission.
- General open-public-testnet ingress, marketing, SDK publication, support,
  or product-launch work beyond the exact P6 and watcher acceptance required
  here.
- Throughput Architecture G default flips, 2,500 TPS work, unrelated
  benchmarks, or MPF optimization.
- Escape-hatch implementation and broader operator-liveness work unless an
  exact G2 correction path or G4 acceptance test cannot complete without the
  missing behavior. If that happens, stop and report the atomic dependency
  rather than absorbing the whole liveness program. The attestation-timeout
  removal of a non-faulty but unattested head-of-line block (Q61) is inside
  necessary DA/liveness scope; the wider escape hatch stays out of scope and,
  if still absent at closure, is recorded as a named residual launch blocker
  per §9.5 rather than left implicit.
- Compatibility shims, migrations, aliases, or legacy decoders for undeployed
  pre-launch formats.
- Unrelated cleanup, dependency upgrades, formatting sweeps, or refactors.

### 2.4 Required final deliverables

The final tree must contain:

- `GOAL_SPEC.md` itself, tracked in Git at the repository root. The root path
  is bound by existing machine-readable evidence artifacts
  (`docs/exec-plans/evidence/*.json` reference `GOAL_SPEC.md`) and must not
  move during this Goal. Notwithstanding §0.1, staging and committing this one
  previously untracked file is explicitly authorized and required;
- all production source, schemas, migrations, tests, fixtures, commands, and
  generated validators required by G1, G2, and G4;
- a fully implemented `demo/midgard-watcher` workspace package;
- updated canonical P2, fault-coverage, catalogue, and public-readiness status
  documents that agree with executable evidence (the public-readiness document
  is root `public_testnet_readiness.md`);
- `GOAL_PROGRESS.md` as the durable execution/criterion ledger;
- `docs/exec-plans/canonical-v1-goal-completion-report.md` as the concise
  human-readable final evidence index;
- `docs/exec-plans/evidence/canonical-v1-goal-closure-v1.json` as the canonical
  machine-readable closure manifest;
- a repo-owned schema/decoder and verifier for that manifest;
- `docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json` (F05),
  kept current through integration;
- the exact aggregate scripts in §13.1; and
- coherent checkpoint commits plus one final integration commit, without
  rewriting pre-existing history; and
- exactly one long-lived Goal pull request targeting the `tx-validation`
  branch, containing `releaseCommit` plus its §0.2 evidence commits.

Large logs, databases, secrets, temporary corpora, and disposable deployment
state are not deliverables. The closure manifest references committed
evidence by path and records how to regenerate anything too large or
transient to commit, rather than committing unsafe or irreproducible runtime
state (§13.4).

## 3. Non-negotiable invariants

1. **Capability is never reduced for proof convenience.** Every supported
   Cardano-valid shape admitted by the target parameter snapshot remains
   representable and independently disputable.
2. **Complete proof items are preferred; decomposition requires necessity.**
   Accept the complete canonical item directly or through an authenticated
   inline-datum input/reference input whenever the complete publication and
   consuming transactions fit. More L1 transactions, multiple publication
   outputs, chunked reveals, or incremental traversal are permitted only when
   concrete final-validator measurements prove that a simpler complete-item
   path cannot fit. Every concrete transaction must fit the live byte/execution
   envelope within the §3.3 margin thresholds.
3. **Every sufficient single-party proof remains single-party.** If one prover
   can establish a violation from retained public authenticated evidence, the
   family must use a non-interactive proof path. L1 byte or execution limits may
   decompose that proof into an ordered multi-step, multi-transaction
   computation-thread chain; transaction count alone never justifies moving it
   into challenge/response. Interactive proof is permitted only when sound
   resolution intrinsically requires competing authenticated execution traces,
   adversarial responses, or a withholding deadline. Every interactive family
   must record executable necessity evidence for that claim.
4. **Interactive proofs remain necessary and narrow.** The dispute hub
   authenticates and routes; instruction-specific resolvers consume the
   simplest authenticated proof input that fits, preferring the complete
   canonical item.
5. **No unjustified bounded-only proof path.** A helper, codec,
   select/discovery witness, test fixture, or bridge may not force provers to
   provide chunks or incremental reveals when the complete item fits directly
   or through an authenticated inline-datum input/reference input. Complete
   items remain commitment-bound and exact; this rule does not permit an
   unauthenticated raw-object shortcut.
6. **No placeholder semantics.** Do not use empty scripts, zero-byte reference
   scripts, copied Cardano validity evidence, arbitrary size caps, disabled
   features, or emulator limit increases to claim closure.
7. **Normal and forced semantics agree.** Equal source transaction and prior
   state imply equal deterministic validation and ledger delta.
8. **Canonical encoding is exact.** TypeScript/Aiken encoders, decoders,
   constructor tags, array lengths, hashes, purposes, indexes, execution
   units, counts, and terminal summaries agree.
9. **Soundness is symmetric.** Each positive proof has negative controls
   showing mutation and valid-block rejection.
10. **Public evidence, not operator state, is authoritative for challengers.**
11. **Only `verified` is healthy.** `pending_da`, `unprovable_gap`,
    `fault_detected`, and recovery states keep readiness fail closed.
12. **Release evidence is final-tree evidence.** It binds the exact source
    revision (the §0.2 `releaseCommit`), both C70 parameter snapshots, the
    generated blueprint, applied validator hashes, deployment identity,
    fixtures, commands, and results.
13. **Pre-launch schema replacement is in place.** Remove obsolete branches;
    do not reserve dormant protocol surface or add compatibility layers.
14. **Necessary work is finished, not discarded.** No task may delete,
    descope, or hollow out work that the Goal's outcomes require merely to
    reach a mergeable, green, or simpler state — if required work is
    incomplete, complete it. Pre-existing and in-flight work, including
    uncommitted checkpoints from other tasks, may be edited, overwritten,
    staged, committed, and claimed whenever doing so advances it toward the
    Goal's outcomes; record the provenance of such integrations in
    `GOAL_PROGRESS.md`.

### 3.1 Required protocol decisions for this Goal

Older fault-proof documents record several decisions as open. This Goal
resolves them as follows so parallel tasks do not select incompatible designs:

1. **All canonical V1 features remain enabled.** Scripts, redeemers, reference
   inputs/scripts, script credentials, protected outputs, observers, Values,
   mint/burn, and valid effectful forced transactions may not be disabled to
   reduce proof scope.
2. **Large Values and Data are complete-item-first.** Exact signed quantities,
   policy/asset identity, map key/value identity, child order, counts, and
   terminal summaries are preserved. The complete canonical Value or Data is
   accepted directly or through an authenticated inline-datum input/reference
   input whenever the applied path fits. Authenticated incremental folds are a
   fallback only where the necessity gate in §3.2 proves they are required. No
   flattened, lossy, or round-number substitute is allowed.
3. **Duplicate vkey hashes are noncanonical and reject.** Signer and witness
   collections use the sole canonical V1 encoding and strict ordering/dedup
   semantics on-chain and off-chain.
4. **Fees and min-Ada are parameterized rules, not constants or stubs.** The
   canonical V1 minimum-fee proof implements the exact Phase A deployed
   formula from the bound target parameter snapshot; output min-Ada uses the
   applicable target Cardano ledger rule and canonical output bytes. A
   zero-return helper is forbidden.
5. **Withheld-after-attestation data uses a bond-backed availability
   challenge.** Opening a timely challenge blocks merge. The accountable DA
   signers publish the complete committed payload and proof material through
   the fewest authenticated L1 inline-datum outputs that fit. A complete
   single-output publication is preferred; ordered multi-output/chunk
   publication is used only where the complete item cannot fit in one
   publication transaction. Every publication binds offsets, lengths, item or
   chunk hashes, terminal accumulator, payload identity, header identity,
   deployment identity, and deadline. Anyone can reconstruct the complete
   response from L1; a private endpoint or a fresh signature is insufficient.
   Successful exact publication closes only the availability challenge and
   does not excuse malformed or root-mismatched content. Timeout authorizes
   deterministic header/descendant correction and slashing of the
   deployment-bound DA accountability bond. Retention is at least maturity
   plus measured worst-case proof time and margin. An undocumented honesty
   assumption is not an acceptable closure.
6. **Proof selection is deterministic.** The final rule bundle contains a
   total, versioned violation-priority table. It selects the earliest invalid
   transition/event and then the table's stable family order. No provable fault
   may be hidden by a later or less specific diagnostic.
7. **Economics are nonzero and release-bound.** Bond, slash, inactivity
   penalty, prover reward, fee/collateral requirements, and deadline margins
   come from the approved F04 decision record and are included in the
   deployment/release identity.
8. **Finality is depth- and chain-point-based.** Acceptance mode declares
   exactly one L1-source mode:

   - `local_node`: a watcher-operated Cardano full node is the chain-consensus
     authority and chain-sync supplies roll-forward and rollback events.
     Ogmios, Kupo/Kupmios, or db-sync may query or index that same node. Those
     services are one authority, need not be operationally independent, and
     require no second provider. Their network and query/index chain point must
     match the local node; stale or mismatched results fail closed.
   - `external_providers`: the watcher does not operate its own Cardano node
     and instead uses services such as Blockfrost or Koios. At least two
     operationally independent external providers with compatible network,
     chain-point, and content observations are required. Services sharing one
     backend or authority do not count twice; disagreement quarantines
     protocol decisions.

   Both modes require an explicit confirmation depth, pending-state rollback
   below that depth, and — for finalized rollback, which remains possible up
   to Cardano's security parameter k (2,160 blocks) — an incident state with
   automated W13 recovery rather than manual state surgery (F04 owner
   condition). W10–W14
   bind indexed records to the actual transaction/output/datum bytes observed
   through the selected source mode. Cardano consensus and the deployed
   validators establish L1 transition validity; the watcher deterministically
   decodes and indexes accepted L1 state rather than reimplementing those
   validators. Fault proofs remain the protocol mechanism for adjudicating
   dishonest L2 operators.

9. **Block application follows the canonical deployed V1 rule bundle.** The
   watcher imports/reuses that rule; it does not maintain a second folklore
   ordering.
10. **The capability comparison never uses a weaker testnet ceiling.** Fixture
    generation uses the accepted Cardano mainnet capability floor and any more
    permissive applicable target-network parameter. A smaller testnet value is
    useful for deployment validation, not permission to lower canonical V1.

### 3.2 Complete proof-item carriage and bounded-fallback necessity gate

The prover-facing evidence API and tooling must accept the complete canonical
proof item for every supported family. Ordinary provers must not be required
to construct chunk proofs, offsets, fold controls, or incremental state by
hand merely because an exceptional maximum shape may require them.

For each proof item, builders and validators use the simplest authenticated
representation that fits, in this order:

1. carry the complete item directly in the proof transaction;
2. publish the complete item as an inline datum in a dedicated authenticated
   output, then consume or reference that output from the proof transaction;
3. when one publication transaction cannot carry the complete item, accept the
   same complete logical item at the public API and transparently publish it
   across the minimum necessary ordered, commitment-bound inline-datum outputs
   for later input/reference-input use; and
4. introduce stateful chunk-by-chunk or incremental on-chain traversal only
   when complete logical reconstruction and verification from those referenced
   outputs still cannot fit the execution or other live protocol limits.

`maxTxSize` applies to the complete serialized publication or proof
transaction. The full inline datum of an already-created input/reference input
is resolved from the UTxO set rather than serialized again in the consuming
transaction, although its script-context representation and processing still
consume execution resources. Proof-fit decisions therefore measure the actual
publication transaction, consuming proof transaction, execution memory/CPU,
fees, min-Ada, confirmation time, and maturity-window margin; they may not
infer failure from the proof item's byte length alone.

Before adding any multi-output, chunked, or incremental representation for a
proof family, a canonical necessity artifact must:

1. use the final applied/parameterized validators and bound target-network
   parameters;
2. construct and evaluate the complete-item direct proof transaction;
3. construct and evaluate complete-item inline-datum publication followed by
   input/reference-input consumption;
4. where the item exceeds one publication transaction, test the minimum
   transparent multi-output publication and complete logical reconstruction;
5. identify the exact byte, execution, Value, datum, reference-input, timing,
   or economic limit that prevents the simpler path, with measured margin; and
6. show that no simpler authenticated representation closes that limit.

A necessity artifact binds the exact validator hashes and parameter digests
it measured. Any change to those hashes or digests invalidates the artifact —
tracked through F05 invalidation triggers — and requires re-measurement
before CG5.

If bounded support is justified, the direct or single-reference complete-item
path remains enabled for every item for which it fits. Both representations
must authenticate the same canonical commitment, apply identical validation
semantics, reach the same terminal result, and reject omission, duplication,
reorder, substitution, trailing data, and representation-dependent outcomes.
No bounded fallback may reduce canonical V1 capability or turn an exceptional
large-item constraint into mandatory complexity for ordinary proofs.

### 3.3 Exact margin thresholds

Wherever this specification requires "measured margin", "substantial margin",
"maturity-window margin", or "bounded resources", the pass/fail thresholds
are:

1. **Byte fit:** every serialized publication or proof transaction is at or
   below the deployment's measured `maxTxSize`.
2. **Execution fit:** execution memory and CPU are at or below the
   deployment's measured protocol limits with at least a 20% reserve
   (`docs/consensus-profile-v1.md` §10).
3. **Maturity fit:** the complete measured worst-case correction path — DA
   fetch, evidence construction, every proof step including 32 interactive
   rounds where applicable, settlement, L1 confirmations, retries, rollback
   allowance, and removal — completes inside half the canonical maturity
   window under the configured response deadlines.
4. **Resource fit:** live acceptance runs inside the explicit container
   memory/CPU/PID ceilings fixed by the F04 decision record and enforced by
   C80. Those ceilings are local-acceptance containment caps only; F04
   separately records the owner-set production hardware floor, and no
   document may present the acceptance caps as production sizing.

A path that fits its raw limit but not the applicable reserve is a failing
result, not a smaller margin. Bond, slash, inactivity penalty, prover reward,
finality/confirmation depths, retry budget, and availability deadlines are
never invented per task; they come from the F04 decision record.

## 4. Execution and durable progress protocol

### 4.1 Required first-turn behavior

The active parent must:

1. Read this file completely. On a later turn or resumption, a complete
   re-read is required only when `git diff` shows this file changed since the
   revision recorded as last read, or when no prior complete read exists;
   otherwise §0–§6 plus the sections owning current-phase tasks suffice.
2. Record `git rev-parse HEAD`, branch, `git status --short`, tool versions,
   and a secret-safe external-credential inventory in `GOAL_PROGRESS.md`:
   credential names, presence, source type, public addresses/identities, and
   readiness only. Never record a secret value, seed phrase, or key byte in
   any Goal artifact.
3. Reconcile current source against the live P2, fault-proof, and watcher
   matrices before assigning implementation work.
4. Create the compact plan and immediately perform a material read, edit,
   command, or test.
5. Continue until all criteria pass or a genuine external blocker remains.

### 4.2 `GOAL_PROGRESS.md` schema

Maintain these sections and no diary-style transcript:

- **Baseline:** revision, branch, dirty paths, tool versions, Graphify
  indexed revision, and external services/credentials available (secret-safe
  per §4.1). Do not record SHA-256 values of tracked repository files here;
  Git already content-addresses them.
- **Criterion ledger:** every `AC-*` from §12 with `TODO`, `IN_PROGRESS`,
  `BLOCKED`, or `PASS`, plus exact evidence.
- **Task queue:** task ID, dependencies, owner, owned paths, status, commit,
  focused verification.
- **Decisions:** only consequential decisions and why existing authority
  selected them.
- **Validation ledger:** exact command, revision/artifact identity, result,
  count, duration where material.
- **Current next action:** one concrete action.
- **Blockers:** exact external dependency, evidence, exhausted alternatives,
  and smallest unlock action.

An item may be `PASS` only when its final-tree evidence meets this file. A
historical green command against an incompatible blueprint, stale source
revision, different parameter snapshot, or changed ABI is not a pass.

### 4.3 Worktree and commit discipline

- Inventory dirty state before every assignment and integration.
- Establish explicit path ownership before concurrent work begins; every
  change stays inside the paths it owns.
- Integrate pre-existing dirty bytes deliberately: record their provenance
  when staging or building on them, and never let an integration silently
  drop content the Goal requires (§3 invariant 14).
- Stage with explicit paths only.
- Inspect staged diff and run `git diff --cached --check`.
- Commit coherent checkpoints; never amend or rewrite unrelated commits.
- Generated `onchain/aiken/plutus.json` belongs to the parent integration
  lane and is rebuilt only from the exact final Aiken source with the compiler
  pinned by `onchain/aiken/aiken.toml`.
- Goal-owned work must be committed and the final worktree must be clean
  relative to the recorded pre-Goal dirty baseline.

### 4.4 Single pull request

Deliver the entire Goal through one long-lived pull request:

- The pull request base branch is exactly `tx-validation`.
- Use one Goal-owned head branch and one pull request for the lifetime of this
  Goal. Do not open separate, stacked, replacement, proof-family, watcher, or
  release pull requests for Goal-owned work.
- Open the pull request as a draft no later than the first Goal-owned push. If
  a Goal pull request already exists, verify its base and reuse it rather than
  opening another.
- Never force-push or rewrite published Goal history. Push coherent commits to
  the same head branch.
- Push completed work promptly; unpushed work is lost work. A push is not
  final Goal completion. The pull-request description points at
  `GOAL_PROGRESS.md` rather than restating it.
- Keep the pull request draft while any mandatory `AC-*` is not `PASS`. Mark
  it ready for review only as the final §15 delivery action, after every
  technical and acceptance check succeeds and `releaseCommit`, its evidence
  commits, and the closure evidence are pushed.

Push order follows the §6 dependency graph; a push is coherent when every gate
it claims has passed at that revision.

Before any push that changed validators, schemas, migrations, manifests,
persistence, or node orchestration:

- run the focused checks for every touched component, using the §13.2 guarded
  selector runner for Aiken modules; and
- run a deterministic fresh-state local/emulator end-to-end regression of
  deposit → L2 transactions → withdrawal on that revision. Reuse the most
  recent recorded result when nothing it exercises changed; cite reuse
  explicitly.

Never describe a skipped or failed required regression as passing.

Target-testnet lifecycle checks and the complete fresh target-testnet
acceptance run under IG5 and `goal:accept:testnet` before final closure.

Pre-launch database, schema, and validator compatibility is not required:
development state may be reset and redeployed as §3 requires. That reset does
not permit loss of the supported end-user journey — deposit, block
production/finalization, L2 transaction submission and execution, withdrawal
initiation and completion — on the fresh deployment. Failure of that journey
is a regression unless this specification explicitly requires the old behavior
to be removed.

(Owner amendment 2026-08-01: this section previously enumerated seven numbered
push checkpoints — a second copy of the §6 dependency graph that could drift
from it — and required, at every one, a hand-maintained pull-request
description mirroring `GOAL_PROGRESS.md`, a duplicate evidence record already
mandated by §4.2, and a ~200-second journey rerun regardless of whether
anything it exercises had changed. The protective content is retained above;
the per-push ceremony is not. §12 is byte-identical after this change.)


## 5. Concurrent work

Independent ready tasks may proceed concurrently. This specification does not
prescribe how that work is organized, scheduled, sized, or delegated — those
are execution decisions belonging to whoever is doing the work. What it
constrains is the state of the repository that work produces.

### 5.1 Serialization-sensitive surfaces

Concurrent changes to these surfaces yield incoherent integrations rather than
honest merge conflicts, so changes to them must be serialized however the work
is organized:

- `GOAL_SPEC.md`, `GOAL_PROGRESS.md`, live closure matrices, and completion
  reports;
- root/workspace package manifests and lockfiles;
- canonical format registries, consensus profile, deployment manifest, release
  evidence, and capability parity;
- shared Aiken/TypeScript witness sum types and codecs;
- `onchain/aiken/lib/midgard/validation-machine-v1.ak` and its large test
  module;
- fault-proof catalogue ordering, deployment script maps, and generated
  reference-script manifests;
- `onchain/aiken/plutus.json`;
- global CLI switches and shared proof-family unions;
- CI workflow aggregation.

Establish explicit ownership of these paths before concurrent work begins, and
do not run two concurrent changes that import or rewrite the same ABI.

### 5.2 Task completion template

Every task below must produce:

1. A source-verified before-state.
2. The smallest scoped implementation.
3. Positive, adjacent-boundary where applicable, mutation, malformed, and
   fail-closed tests.
4. Cross-language vectors where data crosses TypeScript/Aiken.
5. Focused typecheck/lint/format/static checks.
6. An explicit path/diff audit.
7. Exact evidence recorded in `GOAL_PROGRESS.md`.

“Already implemented” is a valid outcome only if final-tree source and all
listed evidence prove the task's acceptance; then update the authoritative
matrix rather than reimplementing it.

## 6. Dependency graph and integration gates

```text
F00 current-truth freeze
 ├─ F01–F03 preflight ─────────> F04 economics record · F05 task manifest
 │                                └─ F41 evidence schema ──> F40 harness
 │                                   (F40 also needs F10 + F20 + F30)
 ├─ F10 capability inventory ──> C20-* + C21–C33 (P2) ──> CG2
 │                                └──────────────────────> C40–C53 (P3) ──> CG3
 │                                                               └──────> C60–C68 (P4)
 ├─ F20 proof inventory ───────> Q00 binding ──> Q10–Q49 local families ─> QG1
 │                                    └────────> Q50–Q56 + Q58–Q63 ─> QG2
 └─ F30 watcher inventory ─────> W00–W04 foundation

CG2 + Q00 ──> CG3 / interactive-family closure
QG1 + watcher foundation ──> W10–W29 deterministic verifier
QG2 + W10–W29 ──> W30–W44 ─> WG1 local watcher gate
CG3 + C60–C68 + QG2 + WG1 ──> C70–C76 release evidence
C70–C76 ──> C79 acceptance-orchestrator readiness ──> C80–C82 deployment
C81 + Q57 ──> QG3 live state-correction gate
C81 + QG3 + W45–W46 ──> WG2 live watcher gate
C82 + QG3 + WG2 ──> C83–C87 final acceptance
C80–C87 + QG3 + WG2 + all ACs ──> releaseCommit ──> evidence-only
commits (§0.2) ──> final closure
```

No downstream gate may credit an upstream item as complete merely because a
compatible-looking helper exists.

## 7. Foundation work packages

| ID  | Independent deliverable                         | Depends on    | Acceptance                                                                                                                                                                                                                                                                                                                                                                                       |
| --- | ----------------------------------------------- | ------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| F00 | Current-truth and dirty-state freeze            | none          | Record final starting revision, dirty ownership, toolchain, current graph staleness, and the provenance of pre-existing dirty paths. The freeze records state; it does not forbid integrating that work (§3 invariant 14).                                                                                                                                                                                                                                                 |
| F01 | Canonical feature inventory                     | F00           | One machine-readable list names every enabled canonical V1 transaction, script, event, forced, and correction feature and its current TypeScript/Aiken/DA/proof/watcher surfaces. Unknowns fail closed.                                                                                                                                                                                          |
| F02 | Final format/ABI registry audit                 | F00           | Every serialized/authenticated format has one V1 schema and exact cross-language tag/arity tests; obsolete pre-launch branches are absent.                                                                                                                                                                                                                                                       |
| F03 | Target-network authority preflight              | F00           | Select and identify the trusted Cardano L1-source mode/topology, effective/pending parameter query, testnet network, finality policy inputs, and credentials. Local-node mode records the watcher-operated node and aligned query/index services; external-provider mode records every independent provider. Missing external credentials are recorded (secret-safe per §4.1) before P5/P6 but do not block local work. F03 emits exactly one machine-readable L1-source declaration (mode, network, endpoints/identities, finality inputs) that every later task consumes. For this Goal the accepted acceptance-mode selection is `Preprod` + `local_node` + aligned local Kupmios — the only mode current repository tooling supports; selecting `external_providers` instead requires first deliberately building that acceptance path and amending this file. |
| F04 | Quantitative economics and margin decision record | F03         | One approved decision record under `docs/midgard/decisions/` fixes: bond, slashing penalty, inactivity penalty, prover reward, fee/collateral floors, confirmation/finality depths per L1-source mode, retry budget, DA availability deadlines, `da_attestation_timeout` (Q61), governed DA-governor lower bounds (Q63), local acceptance-topology container ceilings (C80) plus the separate owner-set production hardware floor (documented through W46 and the readiness document), and confirms the §3.3 thresholds. Q53, W04, W12, C74, C80, Q61, and Q63 consume these values; no later task invents its own number. Values may enter as `PROVISIONAL` to unblock local work; owner approval is required before CG5 binds them into the release identity. Testnet deadline/timeout values must keep the complete C83–C87 live sweep executable inside a bounded acceptance window (target ≤ 48 hours) without violating §3.3. |
| F05 | Machine-readable task manifest                  | F01–F03, F20–F21, F30, F41 | `docs/exec-plans/evidence/canonical-v1-goal-task-manifest-v1.json` lists every §7–§10 task with: ID, dependencies, exact source anchors, writable paths, paths it must not touch, required evidence outputs, focused verification commands, expected nonzero counts, invalidation triggers, and a size (S/M/L/XL) and risk classification. Assignments quote the manifest entry; inventory findings update it before dependent work is assigned. The manifest may decompose an oversized task into ordered sub-assignments (`Q22a`, `Q22b`, …) with their own path sets and focused commands while closure is judged for the whole ID. F05 also ships worked examples under `docs/exec-plans/templates/`: a golden manifest row, a §3.2 necessity artifact, an executable structural `N/A`, and a task assignment brief — templates, excluded from evidence aggregation. |
| F10 | P0/P1/P2 evidence reconciliation                | F01–F02       | Re-run or invalidate every claimed P0/P1/P2 pass against current source; update the P2 task queue without weakening the matrix definition.                                                                                                                                                                                                                                                       |
| F20 | Fault-proof matrix and catalogue reconciliation | F01–F02       | For every coverage row, identify rule, enabled state, proof family, current binding, catalogue reachability, tooling, tests, emulator/preprod evidence, and remaining task ID. F20 also emits the initial concrete §9.1 launch-scope family list.                                                                                                                                                                                                                   |
| F21 | Structural/N/A claim audit                      | F20           | Each “unrepresentable”, “L1-enforced”, or “reduces to another proof” row has an executable adversarial test. Unsupported prose-only N/A claims become open tasks.                                                                                                                                                                                                                                |
| F30 | Watcher dependency/source map                   | F00–F02       | Resolve current public DA/proof-bundle, validation, proof tooling, manifest, L1-source, state-queue, and removal APIs by source path. No watcher design relies on operator-private DB/admin APIs.                                                                                                                                                                                                |
| F40 | Goal verification harness                       | F10, F20, F30, F41 | Add exact `demo/package.json` entry points described in §13.1 and deterministic report verifiers consuming the F41 schema. Local verification never silently skips tests; state-changing acceptance is separate. F40 also adds the non-gating `goal:tasks:ready` helper (§13.1) and wires `goal:verify:static` into repository CI for the Goal branch so drift is caught between checkpoints.                                                                                                                                                                            |
| F41 | Evidence manifest schema                        | F00–F03       | Define the canonical machine-readable closure manifest binding revision per §0.2 (`releaseCommit`, branch, worktree state — never the hash of the commit containing the manifest), dirty baseline, compiler/tool versions, both C70 parameter snapshots, blueprint, validator hashes, deployment identity, fixtures, commands, results, tx hashes, timings, §9.5 residual launch blockers, and all `AC-*` statuses. Reconcile the existing v1 schema/verifier to §0.2: drop manifest-head equality and verify descent plus evidence-only diffs instead. |

`F01`, `F02`, `F20`, and `F30` may discover that recent source has advanced
past older plans. Preserve valid implementation; repair the evidence ledger.

`F41` deliberately precedes `F40` and all broad implementation work: the
evidence schema exists first so early tasks record evidence in its final
shape rather than retrofitting it later.

## 8. G1 work packages — canonical V1 capability

### 8.1 P0/P1 control-plane gate

| ID  | Deliverable                     | Depends on | Acceptance                                                                                                                                              |
| --- | ------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| C10 | P0 freeze/baseline revalidation | F10        | Release gate remains unset until P5; no activation/deployment path uses a monolith; current compiled size evidence is reproducible.                     |
| C11 | Dispute hub fit                 | C10        | Actual applied/parameterized hub publication transaction fits live `maxTxSize` with margin.                                                             |
| C12 | Resolver routing completeness   | C10        | Every nonterminal phase routes to exactly one deployment-bound resolver; unknown, duplicate, absent, terminal, or wrong hashes fail closed.             |
| C13 | Control lifecycle ABI           | C11–C12    | SDK, fault-proof tooling, manifest, catalogue, publication order, Aiken/TS codecs, and accepted-trace replay agree against the freshly built blueprint. |
| CG1 | P1 gate                         | C11–C13    | Every parameterized hub/control validator fits a real 16,384-byte publication transaction; evidence is bound to final validator hashes.                 |

### 8.2 P2 proof-item capability tasks

Each task must prove retained normal and forced sources, exact typed
count/length, complete-item direct or inline-datum input/reference-input
carriage wherever that path fits, exhaustive TypeScript terminal, Aiken
terminal agreement, accepted Cardano maximum, and immediately adjacent
rejection where Cardano defines a boundary. Multi-output, chunked, or
incremental evidence is required only for a family with a §3.2 necessity
artifact, and must coexist with the complete-item path for ordinary fitting
items.

#### Ordered fields

| ID    | Field/family                                                                             | Depends on |
| ----- | ---------------------------------------------------------------------------------------- | ---------- |
| C20-0 | Field 0 spend inputs, including maximum schedule/replay                                  | F10        |
| C20-1 | Field 1 reference inputs and mixed spend/reference ordering                              | F10        |
| C20-2 | Field 2 outputs, address/value/datum/reference-script descriptors                        | F10        |
| C20-3 | Field 3 observers, exact count/order/dedup and terminal summary                          | F10        |
| C20-4 | Field 4 required signers                                                                 | F10        |
| C20-5 | Field 5 mint/burn policy and asset entries                                               | F10        |
| C20-6 | Field 6 native/non-native script witnesses                                               | F10        |
| C20-7 | Field 7 vkey witnesses and exact signer identities                                       | F10        |
| C20-8 | Field 8 redeemer item descriptor, outer envelope, Data traversal, purpose/index/ex-units | F10        |

#### Other dynamic content

| ID  | Family                                      | Depends on   | Additional acceptance                                                                                                                                                                                                                                                                                                                                 |
| --- | ------------------------------------------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| C21 | Complete proof-item carriage                | F10          | Public tooling accepts the complete canonical item; direct and inline-datum input/reference-input paths are constructed and measured first. Any multi-output, chunked, or incremental fallback has a §3.2 necessity artifact, preserves the ordinary complete-item path, and rejects omission, duplication, reorder, substitution, and trailing data. |
| C22 | Nested output Value                         | F10          | Exact policy/asset identity, signed quantities, 5,000/5,001 Cardano Value boundary, terminal equality, and complete-Value input/reference-input proof-fit before any incremental fallback.                                                                                                                                                            |
| C23 | Plutus Data constructor breadth             | C20-2, C20-8 | Signed accepted `<= 16,384` and adjacent `> 16,384`; exact constructor children and terminal summary; complete-Data direct/reference proof-fit is tested first.                                                                                                                                                                                       |
| C24 | Plutus Data list breadth                    | C20-2, C20-8 | Exact child identities/count/order and adjacent boundary; complete-Data direct/reference proof-fit is tested first.                                                                                                                                                                                                                                   |
| C25 | Plutus Data map breadth                     | C20-2, C20-8 | Exact pair count and key/value identity, no key-only or count-only shortcut, and complete-Data direct/reference proof-fit before any incremental fallback.                                                                                                                                                                                            |
| C26 | Plutus Data unary depth                     | C20-2, C20-8 | Maximum admitted depth and adjacent rejection derive from Cardano transaction capacity, not an arbitrary recursion cap; any traversal fallback is justified by measured complete-item execution failure.                                                                                                                                              |
| C27 | Script envelope                             | C20-6        | Canonical script kind/language/hash/length; raw Cardano script is never relabelled as a canonical Midgard envelope; complete-script direct/reference proof-fit is attempted first.                                                                                                                                                                    |
| C28 | Content-addressed program material          | C27          | Complete material is retained and accepted directly or through authenticated input/reference-input datums wherever it fits; bounded-node traversal requires a §3.2 necessity artifact; terminal binds the canonical program hash.                                                                                                                     |
| C29 | Canonical-CBOR verification                 | C20-0–C20-8  | Maximum retained source from normal and forced paths reaches the exact terminal through the simplest proof-fit representation; malformed/trailing/noncanonical forms reject; incremental scanning requires §3.2 necessity evidence.                                                                                                                   |
| C30 | Strict normal retained-DA reconstruction    | C20-0–C29    | Maximum fixtures enter complete-item or necessity-justified fallback verification only from real production `reconstructDaPayloadV1` output.                                                                                                                                                                                                          |
| C31 | Strict forced retained-DA reconstruction    | C20-0–C29    | Forced preimage authentication returns the exact canonical source; substitution, count, root, verdict, classification, and representation mismatches reject.                                                                                                                                                                                          |
| C32 | Maximum-specific TS/Aiken agreement         | C20-0–C31    | Every maximum task has applied/terminal vectors for its complete-item path and any necessity-justified fallback; focused Aiken selectors collect and pass a nonzero exact test count.                                                                                                                                                                 |
| C33 | Genuine Cardano vs schema projection labels | C23–C28      | Genuine signed Cardano emulator acceptance remains distinct from canonical Midgard schema projection; collateralized reverse bridge stays fail closed; projected integrity is recomputed rather than emptied.                                                                                                                                         |
| CG2 | Proof-item capability gate                  | C20-0–C33    | Every required P2 matrix row and every cell is `PASS`; complete-item carriage remains available wherever it fits; every bounded fallback has a measured §3.2 necessity artifact and exact semantic-equivalence tests; production searches and ABI tests reject unjustified bounded-only paths.                                                        |

Tasks `C20-*`, `C23`–`C26`, and `C27`–`C29` are logically independent
after their named prerequisites, but any pair sharing a codec or the large
validation-machine module must run serially.

### 8.3 P3 narrow semantic resolvers

| ID  | Resolver family                         | Depends on   | Acceptance                                                                                                                                |
| --- | --------------------------------------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------------------- |
| C40 | Canonical envelope/directory            | CG2          | Exact transaction identity, field directory, profile/deployment domain, and terminal framing.                                             |
| C41 | Input/reference resolution              | CG2          | Membership/non-membership, same-block dependencies, disjointness, duplicates, and exact resolution schedule.                              |
| C42 | Output/address/datum/reference-script   | C40–C41      | Output well-formedness, credentials, inline datum, protected outputs, real reference scripts, and bounded material.                       |
| C43 | Required signers and vkey authorization | C40–C41      | Exact required-set semantics, canonical witness encoding, duplicate behavior, and signature verification.                                 |
| C44 | Native-script semantics                 | C43          | Every node kind, ordering, thresholds, timelocks, empty/invalid cases, and bounded maximum path.                                          |
| C45 | Script sources, purposes, and redeemers | C41–C44      | Every spend/mint/receive/observer purpose has exact source, purpose, retained redeemer, and unused/extraneous rejection.                  |
| C46 | Script integrity and language views     | C45          | Exact field commitments, language set, cost models/views, and final hash; no empty placeholder for a scripted transaction.                |
| C47 | Plutus context construction             | C41–C46      | Exact Cardano/Midgard context semantics and source-derived summaries.                                                                     |
| C48 | CEK execution and execution units       | C45–C47      | Deterministic canonical program, data/context, builtin/cost semantics, aggregate budget, failure result, and bounded microsteps.          |
| C49 | Value, fee, mint, and burn              | C41–C48      | Exact signed multi-asset conservation, Ada rule, fee, min-Ada, mint/burn authorization, and no arbitrary sub-cap.                         |
| C50 | Observer/protected-output semantics     | C42, C45–C48 | Required observer and protected receive behavior is fully semantic and independently disputable.                                          |
| C51 | Ledger delta                            | C41–C50      | Exact spent/produced operations, accepted/rejected behavior, roots, receipt, terminal replay, and no extra/missing operation.             |
| C52 | Aggregate script-execution floor        | C45–C51      | A Cardano-capable transaction receives at least the target snapshot's aggregate memory/steps across bounded proof transactions.           |
| C53 | Resolver proof-fit sweep                | C40–C52      | Every concrete one-step argument and actual applied resolver transaction fits target-network byte/memory/CPU limits within the §3.3 margin thresholds. |
| CG3 | P3 gate                                 | C40–C53      | No enabled accepted transition or rejection reason lacks an L1 one-step verifier; TypeScript/Aiken vectors cover every instruction.       |

### 8.4 P4 forced execution and classification

| ID  | Case                                    | Depends on   | Acceptance                                                                                                                                         |
| --- | --------------------------------------- | ------------ | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| C60 | Normal valid/effectful                  | CG3          | Exact accepted delta.                                                                                                                              |
| C61 | Forced valid/effectful                  | CG3          | Same validation and delta as equivalent normal source; cannot be skipped/rejected/no-op.                                                           |
| C62 | Forced invalid                          | CG3          | Consumes exact forced-order effect and applies no invalid ledger delta.                                                                            |
| C63 | Forced specified no-op                  | CG3          | Remains exact authenticated no-op.                                                                                                                 |
| C64 | Normal misclassified as forced          | C60–C63      | Single deterministic proof/correction path.                                                                                                        |
| C65 | Forced misclassified as normal          | C60–C63      | Single deterministic proof/correction path.                                                                                                        |
| C66 | Accepted source paired with wrong delta | C60–C63      | Detected and corrected for normal and forced sources.                                                                                              |
| C67 | Single-party proof regression           | C60–C66      | Existing self-contained families remain direct and pass soundness tests.                                                                           |
| C68 | Both interactive verdict directions     | C60–C67      | Operator-valid/challenger-invalid and operator-invalid/challenger-valid lifecycles, including withholding timeout, complete through award/removal. |
| CG4 | P4 gate                                 | C60–C68, QG1 | No missing or partial fund-safety/classification row for canonical V1.                                                                             |

### 8.5 P5 release evidence

| ID  | Deliverable                           | Depends on        | Acceptance                                                                                                                                                           |
| --- | ------------------------------------- | ----------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| C70 | Trusted parameter snapshots           | F03, CG4          | Two bound snapshots: trusted Cardano mainnet effective/pending parameters as the capability floor, and target-testnet effective/pending deployment parameters; plus configured L1-source identities and chain points, canonical encoding, and one digest per snapshot. Capability parity (C71–C72) derives from the least restrictive applicable value/rule across both (§3.1.10); deployment validation uses the target snapshot. |
| C71 | Cardano-to-Midgard constraint map     | C70               | Every applicable constraint from the §3.1.10 merged capability floor maps to exact Midgard behavior; unknown/new parameters fail closed.                             |
| C72 | Generated boundary corpus             | C70–C71           | Accepted maxima and immediately adjacent rejects for all applicable fields/shapes; deterministic byte-for-byte regeneration.                                         |
| C73 | Actual L1 path construction           | C72               | Use exact applied/parameterized final validators to construct every publication, reveal, resolution, settlement, correction, and removal transaction.                |
| C74 | Measurement report                    | C73               | Per path: bytes, memory, CPU, fee, transaction count, wall time, and maturity-window margin. Emulator-only or representative framing is insufficient.                |
| C75 | Release-evidence digest               | C70–C74, QG2, WG1 | Bind both C70 snapshot digests, profile, DA framing, rule/program commitments, blueprint, applied validator hashes, capability report, local proof/watcher closure, and measurements. |
| C76 | Generated profile/runbook consistency | C75               | Documentation is generated/checked from compiled values; stale schema/profile/manifest/hash strings fail CI semantically.                                            |
| CG5 | P5 gate                               | C70–C76           | Fail-closed activation opens only for the exact measured release; digest reproduction is deterministic.                                                              |

### 8.6 P6 bounded deployment and acceptance

| ID  | Deliverable                           | Depends on    | Acceptance                                                                                                                                                                                                                  |
| --- | ------------------------------------- | ------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| C79 | Acceptance orchestrator and skill readiness | CG5, QG2, WG1 | The `goal:accept:testnet` orchestrator and the `midgard-e2e-acceptance` skill/runbook cover the complete C82–C87, Q57, and W45–W46 scope — `demo/midgard-watcher` operation, per-family correction drills, withdrawal/reserve/payout, forced/classification drills, and the crash/rollback matrix — beyond the existing DA-committee/deposit/L2/merge baseline. The missing-orchestrator placeholder is replaced; validation is the runbook validator plus a deterministic non-state-changing local rehearsal. |
| C80 | Exclusive topology/resource preflight | CG5, C79      | Enforced lock; no other Midgard topology; explicit container memory/CPU/PID limits from F04; serialized builds/tests; resource and container-count checks. Verify every operator/prover/DA wallet holds the W31-computed worst-case funding before any state-changing step. Preserve the local Preprod node/Kupo databases across attempts; a fresh chain sync is never required for a fresh Midgard deployment.                                                                  |
| C81 | Fresh testnet deployment              | C80           | Use the C79-validated E2E acceptance skill in `fresh` mode; new on-chain identity, exact manifest, reference scripts, init, and matching clean local state. Never touch unrelated Cardano or L1-source state.               |
| C82 | Functional lifecycle                  | C81           | Operator registration/activation, deposit, L2 submit, commit, confirmation, merge, withdrawal/reserve/payout, and final DB/chain/API reconciliation.                                                                        |
| C83 | Fault-proof drills                    | C81, QG3, WG2 | At least one target-testnet drill per launch-scope proof family; complete proof token, removal, slashing, prover payment, and corrected queue. Drills may run in parallel where wallets and path ownership permit; the Q51 journal makes the sweep resumable mid-run rather than restartable. Share drill instances with Q57 and W45 per the Q57 single-execution rule.                                                                              |
| C84 | Forced/classification drills          | C81–C83       | All C60–C68 paths against final deployment artifacts. A C83 drill instance may serve as this evidence where it exercises the identical final path; never spend a live transaction twice to prove the same claim.                                                                                                                                                                       |
| C85 | Restart/rollback/recovery/withholding | C81–C84       | Injectable live drills — named crash boundaries, watcher recovery, configured L1-source inconsistency (including external-provider disagreement, induced by stopping or desyncing the aligned query/index services), missing DA, withholding, and stale manifest — fail closed without lost/duplicate state. L1 rollback and finalized-rollback incident paths are proven by the W44 local matrix plus an adapter-level rewind rehearsal against recorded live chain data; a naturally observed Preprod rollback is recorded as bonus evidence and is never a required trigger. |
| C86 | Bounded stress                        | C82–C85       | Run only after functional acceptance; respect resource gate; preserve correctness/root/proof evidence.                                                                                                                      |
| C87 | Final deployment evidence             | C81–C86       | Immutable redacted evidence contains manifest, tx hashes, chain points/depths, roots, proof steps, decisions, resource data, and final state; secrets are absent.                                                           |
| CG6 | P6 gate                               | C80–C87       | Fresh final release passes all functional and adversarial acceptance at `releaseCommit`.                                                                                                                                 |

## 9. G2 work packages — complete state correction

### 9.1 Per-family atomic closure contract

Every `Q1x`–`Q4x` task is one independently assignable proof-family closure.
Family completion is two-stage so the task graph stays acyclic:

- **`LOCAL_PASS`** — every output 1–10 below exists at the family's
  integration revision. This is the completion state a `Q1x`–`Q4x` task owns
  and the state QG1 requires.
- **`LIVE_PASS`** — the same family completes its target-testnet lifecycle
  against the fresh final release. This is owned exclusively by the Q57 sweep
  and QG3, never by the family task itself: the fresh final deployment (C81)
  requires CG5 → CG4 → QG1, so a family task must never be blocked on
  evidence that can only exist after QG1 passes.

Where §12 requires a family `PASS`, that means `LOCAL_PASS` plus, for every
launch-scope family, Q57/QG3 `LIVE_PASS`.

**Launch-scope** means every proof family reachable from an enabled canonical
V1 feature according to the Q55 violation-to-family coverage table, as
registered in the deployed Q50 catalogue. F20 emits the initial concrete
launch-scope list; Q50/Q55 keep it exact through integration.

A family task is `LOCAL_PASS` only when all applicable outputs exist:

1. Normative rule and violation identifier.
2. Canonical evidence schema and strict TypeScript/Aiken codec agreement.
3. Correct native counted-root/typed-commitment binding.
4. Aiken proof steps with positive and valid-block negative tests.
5. Maximum/adversarial proof-fit fixture.
6. Catalogue identifier, first-step hash, membership proof, deployment and
   reference-script records.
7. DA-first evidence builder using retained authenticated material.
8. One resumable prepare/submit command, with no hidden manual state.
9. Emulator lifecycle: init, every step, permanent proof token, fraudulent
   header/claim removal, correct slashing/reward.
10. Coverage and catalogue rows changed to `LOCAL_PASS` at integration.
    `LIVE_PASS` is recorded only from Q57/QG3 evidence, and only in the
    closure manifest and completion report (§0.2 evidence paths); in-repo
    matrices state local status and reference the manifest for live status,
    so live results never edit a source-bearing document after
    `releaseCommit`.

If a rule is structurally enforced elsewhere, task acceptance is an
adversarial executable proof of that fact, removal of unreachable proof
surface, and a precise matrix `N/A`; prose alone is insufficient.

### 9.2 Shared prerequisites

| ID  | Deliverable                   | Depends on | Acceptance                                                                                                                                                        |
| --- | ----------------------------- | ---------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Q00 | Native V1 proof binding       | F20, F02   | Every family binds the exact current counted/typed roots and canonical native codec; legacy PlutusData/root paths and witness-encoding splits are absent.         |
| Q01 | Common proof-state safety     | Q00        | Computation-thread init/continue/finalize/cancel, duplicate init, token coupling, catalogue immutability, reference inputs, and valid-block rejection are tested. |
| Q02 | Family scaffold generator     | Q00–Q01    | Shared generator may create boilerplate only; generated families retain explicit schemas/tests and no dynamic “accept any” dispatch.                              |
| Q03 | Canonical evidence-source API | Q00        | Builders consume verified `DaPayloadV1`/proof bundles and authenticated L1 observations, not operator-private REST/DB/files except labelled diagnostics.          |

### 9.3 Independently assignable proof-family closures

| ID  | Family                                                                                | Key dependency                                                                      |
| --- | ------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------- |
| Q10 | `double-spend`                                                                        | Q00–Q03                                                                             |
| Q11 | `no-input`                                                                            | Q00–Q03                                                                             |
| Q12 | `invalid-range`                                                                       | Q00–Q03                                                                             |
| Q13 | `input-no-idx`                                                                        | Q00–Q03                                                                             |
| Q14 | `zero-input`                                                                          | Q00–Q03                                                                             |
| Q15 | `invalid-signature`                                                                   | Q00–Q03, C43                                                                        |
| Q16 | `missing-signature`                                                                   | Q00–Q03, C43                                                                        |
| Q17 | `missing-native-script-tx`                                                            | Q00–Q03, C44                                                                        |
| Q18 | `no-reference-input`                                                                  | Q00–Q03, C41                                                                        |
| Q19 | `withdrawn-reference-input`                                                           | Q00–Q03, C41                                                                        |
| Q20 | `min-fee`                                                                             | Q00–Q03, C49; replace the zero-return stub with the canonical parameterized formula |
| Q21 | `transition-trace` all subvariants                                                    | Q00–Q03, C51                                                                        |
| Q22 | interactive `validation-trace` dispute                                                | Q00–Q03, CG3                                                                        |
| Q23 | `value-not-preserved`                                                                 | Q00–Q03, C49                                                                        |
| Q24 | `ada-minted`                                                                          | Q00–Q03, C49                                                                        |
| Q25 | `negative-output-value`                                                               | Q00–Q03, C42, C49                                                                   |
| Q26 | `mint-authorization`                                                                  | Q00–Q03, C45, C49                                                                   |
| Q27 | `min-ada`                                                                             | Q00–Q03, C42, C49                                                                   |
| Q28 | `withdrawn-input`                                                                     | Q00–Q03, C41                                                                        |
| Q29 | `double-withdraw`                                                                     | Q00–Q03, C41                                                                        |
| Q30 | `input-set-uniqueness` including intra-tx duplicates and spend/reference overlap      | Q00–Q03, C41                                                                        |
| Q31 | `reference-input-no-idx`                                                              | Q00–Q03, C41                                                                        |
| Q32 | `req-signer-set`                                                                      | Q00–Q03, C43                                                                        |
| Q33 | `missing-native-script-utxo`                                                          | Q00–Q03, C42, C44                                                                   |
| Q34 | `native-script-invalid`                                                               | Q00–Q03, C44                                                                        |
| Q35 | `network-id`                                                                          | Q00–Q03, C42                                                                        |
| Q36 | `output-well-formedness`                                                              | Q00–Q03, C42                                                                        |
| Q37 | `hash-field-consistency` including auxiliary and script-integrity commitments         | Q00–Q03, C46                                                                        |
| Q38 | `size-limits`/provability at every maximum                                            | Q00–Q03, C53                                                                        |
| Q39 | `fabricated-deposit` existence and content fidelity                                   | Q00–Q03                                                                             |
| Q40 | `fabricated-withdrawal` existence and content fidelity                                | Q00–Q03                                                                             |
| Q41 | `withdrawal-mistag` in both directions including exact payability                     | Q00–Q03                                                                             |
| Q42 | `cross-block-duplicate-event` with evidence surviving event-NFT consumption           | Q00–Q03                                                                             |
| Q43 | `l2-tx-mistag` valid transaction incorrectly made a no-op                             | Q00–Q03, C60                                                                        |
| Q44 | `da-hash-preimage`                                                                    | Q00–Q03, C30–C31                                                                    |
| Q45 | `script-failure` for enabled native/PlutusV3/MidgardV1 semantics                      | Q00–Q03, C44–C50                                                                    |
| Q46 | Forced inclusion, valid/invalid/no-op, and both classification directions             | Q00–Q03, C60–C68                                                                    |
| Q47 | Omitted/out-of-window deposit, withdrawal, and forced-event variants                  | Q00–Q03                                                                             |
| Q48 | Source-phase mismatch, trace-link/order, event-to-step, and every count-fault variant | Q00–Q03, C51                                                                        |
| Q49 | All structural/N/A rows from F21                                                      | F21, Q00–Q03                                                                        |

Multiple family tasks may run concurrently only when their complete path sets
do not overlap. Shared catalogue, CLI unions,
blueprint, and matrices are integrated later by the parent.

Q21, Q22, and Q45–Q48 are substantially larger than typical rows — the
interactive validation-trace game especially. Decompose them into ordered F05
sub-assignments with their own path sets and focused commands; §9.1 closure is
still judged for the whole family.

### 9.4 Shared correction lifecycle

| ID  | Deliverable                        | Depends on            | Acceptance                                                                                                                                                                                                                                                                                                                                                                                  |
| --- | ---------------------------------- | --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Q50 | Catalogue/deployment integration   | Q10–Q49 as applicable | Every enabled family has stable ID, exact first-step hash, derived root/membership proof, reference scripts, applied hashes, and fail-closed manifest inspection.                                                                                                                                                                                                                           |
| Q51 | Unified resumable CLI/API          | Q10–Q50               | One command per family and one programmatic API; journal every submitted/confirmed step; safe reconcile after ambiguous submission.                                                                                                                                                                                                                                                         |
| Q52 | Descendant/cross-operator removal  | Q01, Q50–Q51          | Tail/non-tail, same/different/rotated active/retired/already-slashed operators, stale UTxOs, concurrent append/merge, ordered descendant pruning, and event re-inclusion all complete without deadlock.                                                                                                                                                                                     |
| Q53 | Slashing and prover economics      | F04, Q01, Q52         | Non-placeholder target-testnet bond, penalty, inactivity penalty, prover reward, exact value routing, and duplicate-token/reward prevention, with every value and its rationale from the F04 decision record.                                                                                                                                                                                                                        |
| Q54 | Retention enforcement              | Q03, Q44              | DA and proof stores enforce maturity + worst-case proof time + margin; deployment identity binds the window; pruning cannot remove still-challengeable evidence; deadline alerts are executable.                                                                                                                                                                                            |
| Q55 | Violation-to-family coverage table | Q10–Q54, Q58          | Every enabled violation maps deterministically to exactly one primary proof route or an explicitly ordered equivalent; unknown/unprovable maps to `unprovable_gap`, never `verified`.                                                                                                                                                                                                       |
| Q56 | Emulator family sweep              | Q50–Q55, Q58–Q63      | Every launch-scope family completes its atomic lifecycle and every valid-block negative rejects.                                                                                                                                                                                                                                                                                            |
| Q57 | Target-testnet family sweep        | QG2, CG5, C81         | Every launch-scope family completes against the fresh final deployment; evidence binds tx hashes, chain points, blueprint, manifest, parameters, release digest, and final correction. One live execution per family is the intent: a single drill instance may generate the evidence for Q57, C83, and W45 simultaneously when it meets each claim's requirements (Q57 binding, C83 lifecycle completion, W45 autonomous watcher drive); each task claims that evidence once its own dependencies pass, and no family is rerun live solely because a different task ID also claims it.                                                                                                                                                                                                      |
| Q58 | Bond-backed availability challenge | Q01, Q03, Q44, Q53    | Timely challenge blocks merge; complete-item L1 inline-datum publication is preferred, with the fewest necessity-justified ordered multi-output/chunk receipts and an exact terminal accumulator when one publication cannot fit; wrong item/chunk, offset, length, hash, order, header, deployment, or deadline rejects; timeout enables deterministic correction and exact DA-bond slash. |
| Q59 | Availability tooling and lifecycle | Q51, Q54, Q58         | Public evidence builder, challenge/respond/timeout/correct commands, watcher adapter, emulator lifecycle, restart/reconcile tests, retention checks, and maturity-margin measurement; final target-testnet use is part of Q57.                                                                                                                                                              |
| QG1 | Coverage gate                      | Q10–Q50, Q55, Q58–Q63 | No `PARTIAL`, documented-missing, required-undocumented, unreachable, stub, untooled, or untested fund-safety row remains. QG1 is the local-closure gate: it requires §9.1 `LOCAL_PASS`, never live evidence.                                                                                                                                                                                                                                                                  |
| QG2 | Local state-correction gate        | Q51–Q56, Q58–Q63      | Every family and unavailable-data path completes locally/emulator from public evidence through deterministic correction, exact slash/reward, restart, and due-event preservation.                                                                                                                                                                                                           |
| QG3 | Live state-correction gate         | QG2, Q57              | The same workflows complete against the fresh final target-testnet deployment and reconcile exact final chain/queue/economic state.                                                                                                                                                                                                                                                         |

### 9.5 Incorporated fault-proof-plan decision rows

The incorporated authority `docs/fault-proofs/execution-plan.md` §3 contains
required decision rows that are neither §9.3 proof families nor Q49
structural/N/A candidates. Each row below is a mandatory work package. Its
outcome must be exactly one of:

1. an implemented protection with adversarial positive/negative tests;
2. an executable structural `N/A` proving the protection is unnecessary; or
3. a named residual launch blocker, recorded in root
   `public_testnet_readiness.md` and the closure manifest with the owner's
   explicit acceptance — never silence.

| ID  | Deliverable                                       | Depends on        | Acceptance                                                                                                                                                                                                                                                                                                                                              |
| --- | ------------------------------------------------- | ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Q60 | Commit `end_time` bound (D-S12)                   | F20, Q00          | Header commitment anchors `end_time` to the commit transaction's bounded validity window (mirroring `Init`), or an executable proof shows a far-future `end_time` cannot distort event due-ness adjudication or stall queue/merge liveness. Tests cover the adjacent accepted/rejected bounds.                                                            |
| Q61 | Attestation-timeout removal (D-L1 i)              | F04, Q00–Q01, Q52 | A non-faulty committed block that never receives the required DA attestation becomes removable after the F04 `da_attestation_timeout` without slashing the operator bond (the D-L1 recommendation), unblocking the head of the state queue. Premature removal, attested-block removal, and post-timeout attestation races reject. Off-chain actuation has a named owner: a resumable permissionless command exists, the operator-node scheduler is the default production driver, and an alert fires when a block nears the timeout unattested; the watcher observes but does not own this liveness action. The wider escape hatch stays out of scope per §2.3. |
| Q62 | Non-retroactive DA committee rotation (D-DA4)     | F20, Q00          | State-queue apply references the current DA-params state and requires the frozen `committee_signers_hash`/`da_threshold` to still match the governed values; `get_da_params` re-derives the committee hash. A quorum gathered under a rotated-out committee cannot apply.                                                                                 |
| Q63 | DA-governor bounds and attestation rescue (D-DA5) | F04, F20, Q00     | Governed lower bounds prevent an owner quorum from dropping `da_threshold`/`update_threshold` below the F04 floor, and the owner set has drain protection; a mid-flight committee change cannot permanently strand a partially signed attestation's ADA — a rescue/refund path exists and is tested.                                                       |

## 10. G4 work packages — autonomous watcher

### 10.1 Package and trust-boundary foundation

| ID  | Deliverable                        | Depends on            | Acceptance                                                                                                                                                                                                                                                                                                                                                                                                                   |
| --- | ---------------------------------- | --------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| W00 | Workspace package scaffold         | F30                   | `demo/midgard-watcher` is a real pnpm package with build/typecheck/lint/test/start/replay commands and CI; docs-only status is removed only when implementation exists.                                                                                                                                                                                                                                                      |
| W01 | Strict configuration               | W00                   | Typed `local_node` or `external_providers` L1-source mode, secret-safe errors, bounded timeouts/concurrency, target network, DA peers, durable DB, wallet source, finality, deadlines, and fail-closed validation. Local-node mode binds chain-sync and query/index services to one watcher-operated node and requires no second provider; external-provider mode requires at least two operationally independent providers. |
| W02 | Deployment identity verifier       | W00–W01, F02          | Verifies network, one-shot, manifest signature/trust root, profile, features, catalogue, applied script hashes/reference scripts, rule/program commitments, DA mode, release evidence, and local durable marker.                                                                                                                                                                                                             |
| W03 | Durable schema/migrations          | W00–W02               | Stores deployment marker, L1 observations/chain points, protocol UTxOs, DA/proof inputs, reconstructed states, decisions, faults, submissions, confirmations, retries, deadlines, and correction result; all caches are reproducible.                                                                                                                                                                                        |
| W04 | Deterministic clock/deadline model | F04, W01–W03, Q53–Q54, Q58 | Worst-case DA fetch/publication, construction, proof steps, confirmation depth, retry, rollback, and removal fit the §3.3 maturity threshold; unsafe remaining time escalates/fails closed.                                                                                                                                                                                                                                      |

### 10.2 Independent L1 and protocol indexing

| ID  | Deliverable                            | Depends on | Acceptance                                                                                                                                                                                                                                                                                                                                                                                        |
| --- | -------------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| W10 | Source-neutral L1 adapter              | W01–W03    | Normalizes the selected source mode into exact chain point, block hash, slot, depth, source identity, and actual tx/UTxO/script/datum/redeemer bytes with deterministic fixtures. It binds data provenance and decoding, not a duplicate implementation of Cardano validator semantics.                                                                                                           |
| W11 | L1 source consistency                  | W10        | Local-node mode requires the watcher-operated node's network/chain point, chain-sync, and auxiliary Ogmios/Kupo/Kupmios/db-sync results to agree and propagate rollbacks; no second provider is required. External-provider mode requires at least two operationally independent providers at compatible network/chain point/content, and disagreement quarantines protocol decisions and alerts. |
| W12 | Finality engine                        | W10–W11    | Consumes the valid W11 result for the configured L1-source mode. No irreversible local state from first visibility; confirmation depth is release-bound, before-threshold rollback remains pending, and finalized rollback becomes an incident.                                                                                                                                                   |
| W13 | Rollback engine                        | W10–W12    | Rewinds pending observations, decisions, DA associations, reconstructed states, and submissions deterministically. A rollback deeper than the configured finality depth but within Cardano's security parameter k (2,160 blocks) triggers automated rewind/replay recovery plus an explicit incident record, with W33 reconciling affected submissions; verification resumes without manual state surgery (F04 owner condition).                                                                                                                                                                                                                        |
| W14 | State-queue/scheduler/operator indexer | W10–W13    | Consumes actual selected-source L1 transaction/output/datum bytes and deterministically indexes the linked queue, header chain/roots/times, scheduled operator, bond hold, append/merge/removal, and W13-authorized rollbacks. It rejects free-form snapshots and stale/mismatched source data but does not reimplement the on-chain validators.                                                  |
| W15 | User-event indexer                     | W10–W13    | Deposits, withdrawals, forced orders, NFT/datum/witness identity, inclusion time, status, content fidelity, and rollback.                                                                                                                                                                                                                                                                         |
| W16 | Settlement/reserve/payout indexer      | W10–W13    | Resolution claims and event processing through terminal status, including stuck/invalid states.                                                                                                                                                                                                                                                                                                   |
| W17 | Proof/computation-thread indexer       | W10–W13    | Init/step/success/cancel/token/removal state and tx confirmations reconcile with the local proof journal.                                                                                                                                                                                                                                                                                         |

`W14`, `W15`, and `W16` can be implemented in parallel after `W13`.

### 10.3 Public DA and deterministic verification

| ID  | Deliverable                        | Depends on       | Acceptance                                                                                                                                                                                                                                                                                                  |
| --- | ---------------------------------- | ---------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| W20 | Independent libp2p DA client       | W01–W03, Q03     | Uses retained DA/proof protocols, deployment binding, strict limits/deadlines/hash checks, peer fallback, and no operator node REST/DB as security input.                                                                                                                                                   |
| W21 | Canonical block/proof store        | W20, Q54         | Persists exact public bytes and metadata before verification/submission; hash-addressed, immutable, retention-aware, restart-safe.                                                                                                                                                                          |
| W22 | Header/root reconstruction         | W14, W20–W21     | Reconstructs header-bound root set/counts from public payload; non-circular binding; every mismatch is deterministic.                                                                                                                                                                                       |
| W23 | Versioned rule bundle              | W02              | Loads the exact canonical V1 transition order/features/parameters/program commitments from deployment identity; unknown version/feature fails closed.                                                                                                                                                       |
| W24 | Phase A verifier                   | W21–W23, CG3     | Reuses canonical validation semantics and produces exact deterministic rejection/evidence, not a looser watcher-only implementation.                                                                                                                                                                        |
| W25 | Phase B/block replay               | W14–W16, W21–W24 | Reconstructs prior state, dependencies, spends/references/scripts/value/events, every intermediate root, and exact post-state.                                                                                                                                                                              |
| W26 | Event/classification verifier      | W15–W16, W23–W25 | Due/omitted/out-of-window/fabricated/duplicate events, withdrawal validity, and forced classification match canonical semantics.                                                                                                                                                                            |
| W27 | Proof-bundle materializer          | W21–W26, Q03     | Produces complete canonical inputs for the selected family and prefers complete direct or inline-datum input/reference-input carriage: roots/roles/counts, membership/non-membership/deletion proofs, complete fields/items, any necessity-justified chunk openings, publication L1 refs, and ABI versions. |
| W28 | Deterministic violation classifier | W22–W27, Q55     | Stable first-fault/priority rule maps to exact family and evidence; unsupported maps to `unprovable_gap`.                                                                                                                                                                                                   |
| W29 | Block decision engine              | W22–W28          | Exactly one of `verified`, `pending_da`, `unprovable_gap`, `fault_detected`, `fault_proven`, `removed_or_resolved`; only complete replay yields `verified`.                                                                                                                                                 |

`W20`–`W23` can proceed in parallel with proof-family work. `W24`–`W29`
depend on canonical semantics and coverage being stable.

### 10.4 Autonomous actuation and recovery

| ID  | Deliverable                         | Depends on                | Acceptance                                                                                                                                                                                        |
| --- | ----------------------------------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| W30 | Proof engine adapters               | Q50–Q55, Q58–Q59, W27–W29 | Typed adapter per enabled family and availability challenge; no shell-text parsing or operator-local files; exact family/version/hash match.                                                      |
| W31 | Prover wallet/input manager         | W01, W04, W30             | Bounded ADA/collateral/reference inputs, explicit fee funding, no secret leakage, deterministic reservation, conflict recovery. Computes the F04-derived worst-case funding requirement (bonds, collateral, fees, min-Ada) for the complete acceptance sweep; C80 verifies wallet balances against it.                                                                   |
| W32 | Durable proof state machine         | W17, W30–W31              | Detect → persist evidence → init → steps → proof token → removal/slashing → terminal verification, with idempotent journal transitions.                                                           |
| W33 | Submission reconciliation           | W10–W13, W32              | Handles submitted/not-recorded, configured L1-source timeout, ambiguous confirmation, rollback, stale UTxO, fee bump/rebuild, and restart without duplicate unsafe action.                        |
| W34 | Deadline escalation                 | W04, W29–W33              | Alerts/retries/escalates before maturity; cannot report healthy while proof or DA deadline is unsafe.                                                                                             |
| W35 | Concurrent topology removal         | Q52, W14, W17, W32–W34    | Refetch after confirmation; handles concurrent append/merge, descendants, rotated operators, stale references, and verifies corrected queue.                                                      |
| W36 | Settlement-claim disproof actuation | W16–W17, W30–W34          | Detects false claims, completes disproof/slash, and verifies settlement state.                                                                                                                    |
| W37 | Offline deterministic replay        | W21–W29                   | Given manifest + stored/public bundle + L1 snapshots, reproduces byte-identical decision, roots, violation, family, and proof-input digest without a live node.                                   |
| W38 | API, metrics, and alerts            | W03–W37                   | Read-only status/diagnostics; queued age, verification/DA latency, deadlines, proof steps, events, L1-source freshness; alerts listed in watcher architecture; bounded/paginated and secret-safe. |
| W39 | Graceful operations                 | W03–W38                   | Readiness differs from liveness; stop admission/actuation safely; close resources; preserve journal; restart resumes; DB deletion is not normal recovery.                                         |

### 10.5 Watcher acceptance

| ID  | Deliverable                        | Depends on    | Acceptance                                                                                                                                                                                                                                                                                          |
| --- | ---------------------------------- | ------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| W40 | Unit/property/fuzz suite           | W00–W39       | Codec, state machine, classifier totality, configured L1-source inconsistency (including external-provider disagreement), rollback, malformed DA, proof mutation, restart, and valid-block no-proof properties. Property/fuzz runs are seed-recorded and time-bounded so failures reproduce deterministically.                                                                                     |
| W41 | Multi-process local integration    | W20–W40       | Exercises the configured L1-source mode, DA peers, watcher, and proof submitter with exact process/network boundaries and no shared private files. Local-node mode uses one watcher-operated node plus its aligned query/index services; external-provider mode uses independent provider fixtures. |
| W42 | Read-only clean replay corpus      | W21–W41       | Valid, invalid, forced, event, DA, script, and maximum-capability blocks reproduce expected decisions from public artifacts. Reuse the C72 boundary corpus and Q-family fixtures wherever applicable instead of authoring new blocks.                                                                                                                                                                        |
| W43 | Emulator autonomous correction     | QG2, W30–W42  | Watcher independently detects and completes every launch-scope family and unavailable-data timeout through corrected state without manual proof steps. It may target the same emulator deployment and fixture set as Q56: independence constrains the watcher's inputs (public data only), not the deployment under test.                                                                                                                                              |
| W44 | Crash/rollback matrix              | W32–W43       | Crash before/after every submit/confirm/journal transition plus L1 rollback/configured-source inconsistency, including at least one rollback deeper than the configured finality depth proving the automated k-depth recovery of W13; no double submit, lost evidence, false verified state, manual-surgery recovery step, or stuck unrecoverable workflow.                                                                                                |
| WG1 | Local watcher gate                 | W40–W44       | Watcher is independent, deterministic, complete for every enabled family locally, restart/rollback safe, observable, and ready for final deployment.                                                                                                                                                |
| W45 | Target-testnet unattended drill    | C81, QG3, WG1 | From public L1+DA only, watcher detects invalid committed blocks and unavailable-data timeout, proves/corrects them, observes slash/reward, and resumes verification.                                                                                                                               |
| W46 | Operational/security documentation | W38–W45       | Deployment, keys, funding, recovery, alerts, replay, incident response, supported families, limits, trust assumptions, and exact runbook.                                                                                                                                                           |
| WG2 | Live watcher gate                  | WG1, W45–W46  | Production watcher is independent, deterministic, complete for every enabled family, restart/rollback safe, observable, and proven against the fresh final target-testnet deployment.                                                                                                               |

## 11. Integration and review gates

### IG1 — ABI gate

After any schema, constructor, resolver index, codec, catalogue order, or
blueprint change:

- audit every TypeScript/Aiken encoder/decoder and fixed parser;
- rebuild with exact Aiken compiler version from `aiken.toml`;
- regenerate the blueprint once in the parent lane;
- run cross-language serialization and accepted-trace ABI replay;
- update applied script hashes, catalogue proofs, manifest, and fixtures; and
- reject dormant compatibility constructors.

Schedule for ABI stability once CG3 and QG1 pass. Any later schema,
constructor, resolver-index, or catalogue-order change pays the full IG1
cascade plus re-measurement of every bound §3.2 necessity artifact and C74
measurement (tracked by F05 invalidation triggers) — the most expensive
rework path in this program.

### IG2 — Proof-family gate

Do not integrate a family that has verifier logic but lacks reachability,
tooling, valid-block rejection, maximum proof-fit, correction, or matrix
evidence.

### IG3 — Watcher security gate

The watcher may be developed incrementally, but production readiness remains
false while any enabled feature maps to `unprovable_gap`, proof inputs require
private operator state, finality/rollback is undefined, or the maturity budget
cannot accommodate the complete path.

### IG4 — Release gate

Do not set or compile the release-evidence digest until CG1–CG4, QG1–QG2, and
WG1 pass against the same final blueprint and target parameter snapshot. CG5
then proves the resulting digest and generated release identity. QG3 and WG2
prove that exact release on the fresh target-testnet deployment; their live
evidence enters the final closure manifest rather than creating a
self-referential compiled digest.

### IG5 — Live acceptance gate

State-changing testnet acceptance starts only from:

- reviewed local green evidence;
- clean goal-owned state;
- exact final applied validators;
- a fresh deployment identity;
- matching durable local stores;
- explicit resource preflight; and
- required external credentials.

Never substitute a previous deployment, stale artifact, or emulator result.

## 12. Acceptance criteria

Every criterion is mandatory.

### Repository and identity

- **AC-00:** `GOAL_PROGRESS.md` contains a complete criterion/task/evidence
  ledger and no unresolved non-external blocker.
- **AC-01:** All pre-existing user work is preserved; all Goal-owned work is
  committed in coherent checkpoints; final state is clean relative to the
  recorded baseline; diff/static checks pass.
- **AC-02:** Exactly one canonical V1 schema/profile/feature tuple is active;
  obsolete undeployed branches are absent; unsupported or mismatched formats
  fail closed.
- **AC-03:** Final source, Aiken compiler/lock, blueprint, applied hashes,
  manifest, catalogue, parameter snapshot, DA framing, rule/program
  commitments, and release digest identify one exact release.

### Canonical V1 capability

- **AC-C10:** P0/P1 gates CG1 pass using actual parameterized publication
  transactions.
- **AC-C20:** Every P2 family and cell passes CG2: authenticated normal/forced
  source, exact typed count/length, complete-item direct or inline-datum
  input/reference-input proof-fit, exhaustive TS terminal, Aiken terminal, and
  applicable maximum/adjacent Cardano boundary. Any multi-output, chunked, or
  incremental fallback has a measured §3.2 necessity artifact.
- **AC-C21:** Production searches, ABI tests, and applied proof-fit evidence
  prove that public tooling accepts complete canonical proof items and that
  every fitting item uses a direct or inline-datum input/reference-input path
  without mandatory prover-managed chunking. Every bounded fallback is limited
  to a concretely justified family/shape, preserves the complete-item path for
  fitting cases, and is semantically identical.
- **AC-C30:** Every enabled validation instruction and rejection has a narrow,
  semantic, proof-fit L1 resolver; CG3 passes.
- **AC-C31:** Scripts, redeemers, reference inputs/scripts, script
  credentials, protected outputs, observers, Values, mint/burn, contexts, CEK,
  budgets, and ledger deltas are fully represented and disputed.
- **AC-C40:** Normal/forced valid, invalid, no-op, both misclassification
  directions, wrong-delta, and withholding cases pass CG4.
- **AC-C50:** Mainnet capability-floor and target-testnet effective/pending
  parameter snapshots, the complete parity map derived from their least
  restrictive applicable values (§3.1.10), deterministic boundary corpus,
  concrete path measurements, and final release-evidence digest pass CG5.
- **AC-C60:** Fresh bounded target-testnet deployment, functional lifecycle,
  fault/forced/recovery/withholding drills, then stress, pass CG6.

### State correction

- **AC-Q10:** Every enabled violation or state-correction rule — including the
  §9.5 rows Q60–Q63 — has a `PASS` (per §9.1: `LOCAL_PASS` plus, for
  launch-scope families, Q57/QG3 `LIVE_PASS`), an executable structural `N/A`,
  or an explicitly recorded §9.5 residual launch blocker; no partial, missing,
  stub, unreachable, unregistered, untooled, untested, or stale fund-safety
  row remains.
- **AC-Q11:** Every proof family satisfies the atomic closure contract in
  §9.1, including valid-block negative and maximum proof-fit tests.
- **AC-Q12:** All proof decoders bind canonical native V1 counted/typed roots;
  on-chain/off-chain accepted encodings are equivalent.
- **AC-Q13:** Catalogue root, IDs, first-step hashes, membership proofs,
  applied validators, reference scripts, and manifest inspection are exact and
  fail closed.
- **AC-Q14:** One resumable command/API completes every family from public
  evidence through removal/correction.
- **AC-Q15:** Tail/non-tail/cross-operator/rotated/stale/concurrent correction
  cannot deadlock; due events remain includable after pruning.
- **AC-Q16:** Non-placeholder slashing/prover economics and duplicate reward
  prevention are enforced and measured.
- **AC-Q17:** Retention and unavailable-data behavior keep faults provable for
  the complete maturity window and prevent silent unverifiable merge.
- **AC-Q18:** Coverage, local/emulator correction, and target-testnet family
  sweeps pass QG1, QG2, and QG3.

### Autonomous watcher

- **AC-W10:** `demo/midgard-watcher` is a built, typed, linted, tested,
  documented workspace package and deployable service, not a DA-committee
  alias or docs-only shell.
- **AC-W11:** Watcher security decisions depend only on authenticated L1,
  public/permissionless DA/proof data, the signed deployment identity, and
  deterministic local computation.
- **AC-W12:** The configured L1-source consistency policy, explicit finality,
  rollback quarantine, post-finality incidents, and durable chain points pass
  adversarial tests. Local-node mode proves node/indexer chain-point alignment
  and rollback propagation with no second-provider requirement;
  external-provider mode proves agreement of at least two operationally
  independent providers.
- **AC-W13:** Every queued block's public payload reconstructs all committed
  roots/counts and deterministic Phase A/B/event/settlement semantics.
- **AC-W14:** Every block receives exactly one canonical decision; only a
  complete valid replay can be `verified`; unknown/unprovable behavior is
  visible and fail closed.
- **AC-W15:** Every enabled violation maps deterministically to a complete
  proof adapter; proof-critical data is persisted before submission.
- **AC-W16:** The durable state machine reconciles retries, ambiguous
  submission, confirmation, restart, rollback, stale topology, removal,
  slashing, and prover reward without unsafe duplication.
- **AC-W17:** Offline replay reproduces the exact decision, roots, violation,
  proof family, and evidence digest.
- **AC-W18:** Required metrics, alerts, readiness, bounded API, secret handling,
  graceful shutdown, funding, replay, recovery, and incident runbooks exist.
- **AC-W19:** Emulator family sweep, crash/rollback matrix, multi-process public
  data path, and target-testnet unattended detect→prove→remove pass WG1 and
  WG2.

### Cross-system completion

- **AC-X10:** The enabled-feature manifest and proof-family coverage are
  total: no deployed feature is accepted without deterministic validation,
  L1 correction, public evidence, and watcher support.
- **AC-X11:** Complete worst-case DA fetch + proof construction + all L1
  confirmations/retries/rollback margin + removal completes within the §3.3
  maturity threshold: at most half the canonical maturity window under the
  configured response deadlines.
- **AC-X12:** All final evidence identifies the same `releaseCommit` and
  release identity (§0.2) and is reproducible by §13; anything recorded after
  `releaseCommit` lives only in evidence commits whose diffs are confined to
  the §0.2 declared evidence paths.
- **AC-X13:** No acceptance claim relies solely on documentation, a synthetic
  helper, representative framing, an emulator-only limit, test-name filtering
  that collects zero tests, skipped suites, or stale historical artifacts.

## 13. Specified verification

### 13.1 Required repository commands

`F40` must add and document these exact scripts in `demo/package.json`:

```text
goal:verify:static
goal:verify:capability
goal:verify:fault-proofs
goal:verify:watcher
goal:verify:local
goal:accept:testnet
goal:verify:evidence
goal:verify:all
```

Required behavior:

- `goal:verify:static` checks dirty-baseline policy, forbidden legacy,
  unauthenticated-object, and unjustified bounded-only patterns, format
  registry, generated docs, diff/format/lint/typecheck/build,
  compiler/blueprint identity, catalogue/manifest consistency, and exact test
  selectors.
- `goal:verify:capability` proves CG1–CG5 locally, including deterministic
  maximum/adjacent corpora, retained normal/forced reconstruction,
  complete-item direct/publication/reference paths, every required §3.2
  necessity artifact, exact equivalence for any bounded fallback, exhaustive
  TS terminals, exact Aiken vectors, proof fit, and release-digest
  reproduction.
- `goal:verify:fault-proofs` proves QG1 and QG2 for every family,
  including valid-block negatives, reachability, tooling, correction, and
  coverage-matrix verifier.
- `goal:verify:watcher` proves WG1: unit/property, multi-process,
  public-data-only, replay, classifier totality, crash/rollback, and autonomous
  emulator correction.
- `goal:verify:local` runs the four preceding commands serially with bounded
  resources.
- `goal:accept:testnet` is the explicit state-changing fresh target-testnet
  acceptance. It executes C80–C87, Q57/QG3, and W45–W46/WG2 against one exact
  final deployment. It requires credentials/preflight, uses the E2E acceptance
  skill and orchestrator as extended by C79, writes immutable redacted
  evidence, and never targets mainnet.
- `goal:verify:evidence` verifies the testnet artifacts, release identity,
  tx/chain evidence, QG3, WG2, all `AC-*`, that every referenced evidence
  path exists, and absence of secrets without resubmitting transactions. It enforces the §0.2
  revision model: the closure manifest binds `releaseCommit` — never the hash
  of the commit containing the manifest — and verification passes only when
  HEAD equals `releaseCommit` or is a descendant whose entire
  `releaseCommit..HEAD` diff is confined to the declared evidence paths.
- `goal:verify:all` runs `goal:verify:local` then
  `goal:verify:evidence`; it fails if target-testnet evidence is absent, stale,
  incomplete, mismatched, or from another revision/release.

F40 additionally provides a non-gating `goal:tasks:ready` helper that joins
the F05 task manifest with the `GOAL_PROGRESS.md` task queue and prints
dependency-ready tasks with their owned paths and focused commands. It is
scheduling tooling, not a gate; no acceptance claim may cite it.

No command may use `--passWithNoTests`, ignored exit codes, hidden skips, or a
test filter without asserting a positive exact collected count for a required
suite.

### 13.2 Minimum direct toolchain verification

The aggregate scripts must include, at minimum, equivalent coverage for:

```bash
# The workspace Nix shell is defined at demo/flake.nix — there is no root
# flake. With a non-login shell it must resolve the declared Node and pnpm
# versions and record them.
nix develop ./demo --command bash -c 'node --version && pnpm --version'

# The normative technical specification (§1 item 8) must build from the
# repository root. Skip it when technical-spec/ is unchanged in the diff.
make spec

# Aiken runs from onchain/aiken with the exact compiler version declared in
# aiken.toml. The verification harness must fail on a different version.
aiken --version
aiken fmt --check
aiken check --skip-tests
aiken check
aiken build

# Required exact large Aiken vectors use the guarded runner. One invocation
# may batch multiple unique exact selectors for one module; the runner must
# assert that exactly the requested tests are collected and pass.
node scripts/run-focused-check.mjs <module> <exact-test-name> [<exact-test-name> ...]

# TypeScript workspace checks are serialized.
pnpm --dir demo run build
pnpm --dir demo run typecheck
pnpm --dir demo run lint
pnpm --dir demo run format-check

# Package suites.
pnpm --dir demo/midgard-core test
pnpm --dir demo/midgard-validation test
pnpm --dir demo/midgard-fault-proofs test
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/da-committee-node test
pnpm --dir demo/midgard-node test
pnpm --dir demo/midgard-watcher test

# Existing strict retained-DA and breadth checks remain required if their
# scope is still applicable in the final tree.
pnpm --dir demo/midgard-fault-proofs run test:cardano-capability-p2-retained-da
pnpm --dir demo/midgard-validation run test:cardano-capability-p2:data-breadth

git diff --check
git status --short
```

The final commands actually used must come from the final package manifests
and repository help, not stale prose. Record command, exit code, nonzero test
count, duration, revision, and artifact identity in the evidence manifest.

### 13.3 Required live evidence

The testnet acceptance must record:

- both C70 snapshots — trusted mainnet capability-floor and target-testnet
  effective/pending protocol parameters — plus configured L1-source
  identities and chain points;
- fresh deployment and manifest identities;
- compiler, lock, blueprint, raw/applied validator and reference-script
  hashes;
- DA peers, public retrieval identities, payload/proof-bundle hashes, and
  retention policy;
- maximum/adjacent Cardano fixtures and all measured proof paths;
- complete-item direct and inline-datum publication/input/reference-input
  transaction identities, plus every §3.2 necessity artifact and any
  justified multi-output/chunked/incremental path;
- watcher L1-source observations/finality depths and deterministic decisions;
- every proof transaction, computation-thread transition, proof token,
  removal, slash, reward, and corrected queue state;
- forced/classification, missing/malformed DA, restart, rollback, configured
  L1-source inconsistency (including external-provider disagreement), and
  withholding outcomes;
- resource usage and maturity margin; and
- final DB/chain/API/replay reconciliation.

Evidence must be redacted and machine-verified for secret material before it
is retained or committed.

### 13.4 Evidence storage

Evidence is committed on a §0.2 evidence path and referenced by
repo-relative path. Git content-addresses it; `git log -p -- <path>` shows
any change. Nothing further is required — no byte hashes of tracked files, no
durable-URI registry, no per-binding size/media-type/retention/access
metadata.

Keep only what a reviewer needs to re-run the check: the command, and where
its output lives. Where an artifact is too large or too transient to commit
(§2.4), record how to regenerate it rather than trying to preserve it.

(Owner amendment 2026-08-01: the previous storage contract mandated an
immutable-URI regime with SHA-256, byte size, media type, redaction status,
retention commitments and access lists for every binding. None of it was ever
implemented — the closure schema has no such shape — and the byte-hash arm
that *was* implemented produced recurring red gates and one mutually
unsatisfiable pair of artifacts, while catching no defect. It cost context
and delivery speed for no protection.)

## 14. Genuine external blockers

Local implementation, tests, emulator acceptance, artifact construction, and
documentation continue even if external services are unavailable.

The only expected blockers that can prevent final Goal completion after local
work is exhausted are:

- no authorized funded target-testnet wallet/collateral;
- no access to the required trusted target-testnet L1-source topology: either
  a watcher-operated local Cardano node with its aligned query/index services,
  or the required independent external providers;
- no ability to run the required fresh deployment/DA topology within enforced
  resource limits;
- no authorized GitHub push/pull-request credential for delivering the Goal
  branch and the §4.4 pull request;
- a target-network outage or immutable external protocol state that makes the
  required test impossible; or
- a genuine contradiction among accepted protocol authorities.

For a blocker, `GOAL_PROGRESS.md` must contain the exact failed operation,
evidence, safe alternatives attempted, preserved completed work, and the
smallest user action needed. Missing effort, a failing test, a difficult
compiler issue, stale documentation, or an incomplete family is not a
blocker.

## 15. Completion rule

Before any response claiming Goal completion:

1. Re-read every `AC-*`.
2. Verify each is `PASS` with final-tree evidence.
3. Run `pnpm --dir demo run goal:verify:all`.
4. Verify goal-owned commits and baseline-relative worktree cleanliness.
5. Reproduce the release-evidence digest and completion manifest.
6. Confirm no external blocker or unresolved decision remains.
7. Verify that exactly one Goal pull request exists, it targets
   `tx-validation`, and its head contains every Goal-owned commit.
8. Push the final evidence commit and closure evidence to that pull request,
   verify that its remote head is `releaseCommit` or an evidence-only
   descendant of it per §0.2, and mark it ready for review without opening a
   replacement pull request.

Only then may the Goal be marked complete. The final response must contain
only completed changes, important decisions, exact validation results, and
unavoidable assumptions. If a genuine §14 external blocker remains after
local work is exhausted, the required response is the §14 blocked handoff —
exact blocker evidence, preserved completed work, and the smallest unlock
action — without claiming completion; this rule never forbids that honest
report.
