# Non-interactive fault-proof thread completion program V1

- **Status:** Planned
- **Scope:** canonical V1 transaction-validation fault coverage
- **Execution model:** rolling window of at most three concurrent family agents
  across at most two adjacent waves
- **Review model:** one final, continuous review pass over the completed on-chain diff
- **Primary authority:** `onchain/aiken/lib/midgard/rejection-reason-v1.ak`

## 1. Objective

Move every deterministic validation fault that one prover can establish from
retained public authenticated evidence onto a single-party computation thread.
The proof may use multiple Cardano transactions and resumable state; it remains
non-interactive when no operator response, competing trace, or withholding
deadline is required.

When this program closes:

1. Every `RejectionReasonV1` constructor except `PlutusExecutionFailed` has a
   production-usable non-interactive proof in both applicable directions:
   wrongful acceptance and wrongful forced rejection.
2. Each enabled family has complete on-chain validators, off-chain evidence and
   submission code, watcher installation, and realistic Lucid Evolution
   lifecycle coverage.
3. Every applied reference script and every lifecycle transaction fits the Van
   Rossem L1 byte and execution-unit limits without an oversized route or local
   protocol-parameter override.
4. Deterministic reasons no longer fall back to `validationTraceDispute` in
   production classification.
5. The completed on-chain implementation receives one independent review pass
   after all waves and integration gates are green.

`PlutusExecutionFailed` remains interactive because the current protocol
classifies CEK failure and ExUnit exhaustion as requiring authenticated
execution traces. This program does not weaken or replace that execution
dispute.

## 2. Design decision: narrow deployed threads, shared engines

The seven broad areas identified during planning are implementation
workstreams, not deployed fault-proof categories. Aiken includes all reachable
branch code in an applied script, so a validator that handles unrelated
subjects or predicates pays their compiled-byte cost even when one branch is
executed.

The deployed seam is therefore narrow:

- one authenticated subject shape;
- one decisive predicate or one inseparable predicate family;
- one resumable state-machine shape; and
- one maximum-evidence frontier.

Pure Aiken rules, scanners, canonical encoders, TypeScript twins, off-chain
carriage planners, and emulator assertions are shared behind that seam. Two
families may converge on a shared downstream scan validator only after their
source-specific bind validators have reduced their state to the same canonical
authenticated commitment and scan control.

No category is generalized merely to reduce catalogue entries. Splitting a
physical validator or category is the required remedy when its signed
publication or any maximum-path transaction lacks L1 margin.

## 3. Scope and disposition

### 3.1 New narrow categories

Append the following 22 categories in the listed order. The IDs assume the
current 32-entry catalogue remains the deployed source of truth; confirm the
catalogue baseline before reserving them.

| Proposed ID | Category                              | `RejectionReasonV1` coverage                                                                                                    |
| ----------- | ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `00000020`  | `fieldPreimageLengthMismatch`         | `FieldPreimageLengthMismatch`                                                                                                   |
| `00000021`  | `fieldItemWidthIllegal`               | `FieldItemWidthIllegal`                                                                                                         |
| `00000022`  | `witnessScriptDecoding`               | `WitnessScriptHeaderMalformed`, `WitnessNativeScriptMalformed`, `WitnessNativeScriptNodeLimit`, `WitnessNativeScriptDepthLimit` |
| `00000023`  | `scriptIntegrityHashMissing`          | `ScriptIntegrityHashMissing`                                                                                                    |
| `00000024`  | `observersForbiddenOnUntaggedNetwork` | `ObserversForbiddenOnUntaggedNetwork`                                                                                           |
| `00000025`  | `observerOrderInvalid`                | `ObserverOrderInvalid`                                                                                                          |
| `00000026`  | `resolvedOutputNonCanonical`          | `InputSpentOutputNonCanonical`                                                                                                  |
| `00000027`  | `spendInputSignerMissing`             | `SpendInputSignerMissing`                                                                                                       |
| `00000028`  | `redeemerCanonicity`                  | `RedeemerMalformed`                                                                                                             |
| `00000029`  | `transactionOutputNonCanonical`       | `OutputNonCanonical`                                                                                                            |
| `0000002a`  | `outputReferenceScriptDecoding`       | `OutputReferenceScriptMalformed`, `OutputReferenceScriptNodeLimit`, `OutputReferenceScriptDepthLimit`                           |
| `0000002b`  | `protectedOutputSignerMissing`        | `ProtectedOutputSignerMissing`                                                                                                  |
| `0000002c`  | `mintDeclaredAssetLimit`              | `MintDeclaredAssetLimit`                                                                                                        |
| `0000002d`  | `missingScriptSource`                 | `ScriptSourceMissing`                                                                                                           |
| `0000002e`  | `missingRedeemer`                     | `RedeemerMissing`                                                                                                               |
| `0000002f`  | `unusedScriptWitness`                 | `UnusedScriptWitness`                                                                                                           |
| `00000030`  | `unusedRedeemer`                      | `UnusedRedeemer`                                                                                                                |
| `00000031`  | `executionSourceScriptDecoding`       | `ExecutionNativeScriptMalformed`, `ExecutionNativeScriptNodeLimit`, `ExecutionNativeScriptDepthLimit`                           |
| `00000032`  | `executionNativeScriptInvalid`        | `ExecutionNativeScriptFalse`                                                                                                    |
| `00000033`  | `scriptIntegrityHashMismatch`         | `ScriptIntegrityHashMismatch`                                                                                                   |
| `00000034`  | `receivePurposeLanguage`              | `ReceivePurposePlutusV3Forbidden`                                                                                               |
| `00000035`  | `distinctAssetAccumulationLimit`      | `InputAssetAccumulationLimit`, `OutputAssetAccumulationLimit`, `MintAssetAccumulationLimit`                                     |

These entries take the catalogue to 54 leaves. Catalogue membership becomes
one sibling deeper than the current 32-leaf tree; record the exact signed-byte
effect in the baseline ledger rather than treating it as zero.

### 3.2 Existing categories requiring wrongful-rejection coverage

The following existing categories already prove the accepted-invalid
direction. Extend them to authenticate a forced leaf, bind its exact typed
reason and subject coordinate, and prove that reason false. Preserve the
existing accepted-invalid path.

| Existing category     | Typed reason covered in the new direction                        |
| --------------------- | ---------------------------------------------------------------- |
| `zeroInput`           | `EmptyInputs`                                                    |
| `inputSetUniqueness`  | `DuplicateInput`                                                 |
| `invalidRange`        | `ValidityIntervalMalformed`, `ValidityIntervalExcludesBlockSlot` |
| `networkId`           | `NetworkIdMismatch`                                              |
| `minFee`              | `FeeBelowMinimum`                                                |
| `invalidSignature`    | `AddressWitnessSignatureInvalid`                                 |
| `missingSignature`    | `RequiredSignerUnsigned`                                         |
| `nativeScriptInvalid` | `WitnessNativeScriptFalse`                                       |
| `nonExistentInput`    | spend-source `InputNotFound`                                     |
| `noReferenceInput`    | reference-source `InputNotFound`                                 |
| `minAda`              | `OutputBelowMinAda`                                              |
| `valueNotPreserved`   | `ValueNotPreserved`                                              |

The existing `nativeScriptDecoding` family already covers both directions for
`ResolvedReferenceScriptMalformed`, `ResolvedReferenceScriptNodeLimit`, and
`ResolvedReferenceScriptDepthLimit`; retain it as the resolved-input subject
family instead of broadening its bind validators.

### 3.3 Interactive residue

After this program, `PlutusExecutionFailed` is the only typed reason that may
select challenge-response proving. Production readiness must fail if any
other typed reason is routed to `validationTraceDispute` because its direct
family, evidence, deployment identity, or runner is missing.

Validation-machine code may remain as canonical validation logic. Retiring a
non-CEK resolver deployment occurs only after the corresponding direct family
passes the equivalence, fit, lifecycle, watcher, and final-review gates.

## 4. Required logical topology

Logical step counts below exclude repeated transactions through a resumable
self-loop and exclude generic computation-thread `Init`. Physical validators
may split one logical step into authenticated dispatch, rewarding, or staging
scripts to satisfy fit. A physical split must preserve one canonical successor
and one canonical wire representation of the carried state.

| Category                              | Logical steps | Required state progression                                                                                                                                                       |
| ------------------------------------- | ------------: | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `fieldPreimageLengthMismatch`         |             2 | bind source/reason/field → authenticate raw preimage and finalize declared-versus-actual length contradiction                                                                    |
| `fieldItemWidthIllegal`               |             3 | bind source/reason/coordinate → authenticate selected item and field rule → finalize illegal width or wrongful-rejection contradiction                                           |
| `witnessScriptDecoding`               |             4 | bind field-6 subject → authenticate exact script item → resumable structural scan → finalize decoder result                                                                      |
| `scriptIntegrityHashMissing`          |             2 | bind transaction/reason → prove effectful requirement and zero integrity hash, or contradict rejection                                                                           |
| `observersForbiddenOnUntaggedNetwork` |             2 | bind transaction/reason → authenticate non-empty observer field and untagged network scalar, or contradict rejection                                                             |
| `observerOrderInvalid`                |             4 | bind reason/index → initialize authenticated observer walk → compare adjacent canonical items/resume → finalize ordering result                                                  |
| `resolvedOutputNonCanonical`          |             5 | bind input coordinate/prior root → authenticate out-ref → bind descriptor and exact output commitment → resumable canonical reconstruction → finalize                            |
| `spendInputSignerMissing`             |             5 | bind spend coordinate → authenticate payment credential → initialize valid-witness frontier → resumable signature scan → finalize presence/absence                               |
| `redeemerCanonicity`                  |             3 | bind redeemer coordinate → authenticate and total-decode exact item → finalize canonicality result                                                                               |
| `transactionOutputNonCanonical`       |             4 | bind output coordinate → authenticate exact field-2 item → resumable canonical reconstruction → finalize                                                                         |
| `outputReferenceScriptDecoding`       |             5 | bind output coordinate → authenticate canonical output descriptor → bind exact reference-script item → resumable structural scan → finalize                                      |
| `protectedOutputSignerMissing`        |             5 | bind output coordinate → authenticate protected payment credential → initialize valid-witness frontier → resumable signature scan → finalize                                     |
| `mintDeclaredAssetLimit`              |             4 | bind mint-policy coordinate → authenticate policy item → resumable declared-count fold → finalize first crossing or complete contradiction                                       |
| `missingScriptSource`                 |             6 | bind purpose coordinate → authenticate purpose frontier → authenticate transaction sources → authenticate resolved sources → resumable source match → finalize absence/presence  |
| `missingRedeemer`                     |             5 | bind purpose coordinate → authenticate purpose frontier → authenticate redeemer field → resumable pointer match → finalize absence/presence                                      |
| `unusedScriptWitness`                 |             6 | bind witness coordinate → authenticate script item → authenticate purpose frontier → authenticate alternate sources → resumable reverse match → finalize unused/used result      |
| `unusedRedeemer`                      |             6 | bind redeemer coordinate → authenticate redeemer item → authenticate purpose frontier → authenticate execution selection → resumable reverse match → finalize unused/used result |
| `executionSourceScriptDecoding`       |             5 | bind execution coordinate → authenticate selected source → bind exact script item → resumable structural scan → finalize decoder result                                          |
| `executionNativeScriptInvalid`        |             6 | bind execution coordinate → authenticate selected native source → bind signer/interval inputs → initialize evaluator → resumable evaluation → finalize false/true result         |
| `scriptIntegrityHashMismatch`         |             5 | bind transaction/reason/hash → authenticate selected language set → initialize language-view fold → resumably derive expected hash → finalize equality/mismatch                  |
| `receivePurposeLanguage`              |             3 | bind execution coordinate → authenticate receive purpose and source → finalize forbidden/allowed language result                                                                 |
| `distinctAssetAccumulationLimit`      |             6 | bind typed coordinate → initialize accumulator → fold resolved inputs → fold transaction outputs → fold mint assets → finalize first crossing or complete contradiction          |

Before coding a category, its agent must write a short state/transition sketch
under `docs/fault-proofs/size-plans/` naming every physical applied validator,
its imported semantic engine, maximum dynamic evidence, and planned fit test.
Implementation begins only after the sketch demonstrates that unrelated
subject adapters do not enter the same applied script.

## 5. Vertical-slice assignment contract

One family agent owns one category or one named existing-family upgrade. Its
assignment is complete only when all sections below are present and green.
Self-testing during implementation is not the independent final review.

### 5.1 On-chain deliverable

- Pure decisive rule or a call to the existing canonical rule; no off-chain
  verdict is trusted.
- Canonical computation-thread state and wire encoding.
- Every applied spend/reward/mint validator required by the physical chain.
- Exact accepted/forced source, direction, typed-reason, and subject-coordinate
  binding.
- Authenticated state continuity, deterministic successor script, cancel path,
  and terminal thread burn/permanent proof mint.
- Resumable scans carry a domain-separated checkpoint over source identity,
  cursor, total count/length, accumulator, and next expected script.
- Aiken selectors for every semantic arm: both successful directions, honest
  refusal, out-of-range coordinate, substituted source, substituted bytes,
  substituted root/descriptor, malformed checkpoint, and wrong successor.
- Cross-language golden vectors for every new state/redeemer encoding.

**Completion criterion:** all named focused checks collect a nonzero expected
test count and pass against the exact source module.

### 5.2 Off-chain deliverable

- Finding and classification type with exact `RejectionReasonV1` coordinates.
- Evidence preparation from retained DA and authenticated Cardano L1 state.
- Direct, published, and certified/chunked carriage selection where the
  protocol permits those tiers.
- Submission builder for every physical step, including cancel and resume.
- Durable workflow journal, restart reconciliation, and transaction identity
  checks.
- Deployment-manifest resolution for every reference script and role NFT.
- Production workflow runner, CLI/manual entry point where the package exposes
  them, and watcher installation.
- Classification refuses another category's typed reason and refuses fallback
  to validation dispute for this reason.
- TypeScript unit and property/boundary tests for evidence mutation, encoding
  parity, checkpoint reproduction, and deterministic action selection.

**Completion criterion:** a fresh process can reconstruct the next action from
the durable journal and authenticated chain state without node-database-only
authority.

### 5.3 Lucid Evolution lifecycle deliverable

For every typed reason arm owned by the family, run realistic transactions
from generic computation-thread `Init` through the actual applied reference
scripts. The suite must include:

1. wrongful acceptance success;
2. wrongful forced rejection success;
3. honest accepted-block refusal;
4. honest forced-rejection refusal;
5. reason/subject-coordinate mutation;
6. transaction, root, descriptor, item, chunk, or checkpoint substitution at
   every authentication seam the family uses;
7. cancel from each nonterminal physical step;
8. interruption and resume after at least one real checkpoint for every
   resumable family;
9. permanent proof-token mint followed by state-queue target and descendant
   removal;
10. maximum supported evidence shape; and
11. adjacent-over-bound refusal when a consensus bound exists.

Tests construct evidence from canonical retained payload and ledger fixtures.
They may not start from a fabricated mid-thread datum merely to bypass earlier
authentication. Unit tests may call individual validators, but lifecycle
acceptance requires the complete chain.

**Completion criterion:** every reason arm reaches both a real successful
correction lifecycle and its matching honest/mutation refusals.

### 5.4 Proof-fit deliverable

Use the freshly built testnet blueprint by setting
`MIDGARD_REAL_BLUEPRINT_PATH` to the absolute
`onchain/aiken/plutus.json` path. Record the pinned compiler version and build
with:

```sh
cd onchain/aiken
aiken build --env testnet
```

Hard transaction limits:

- signed transaction bytes `<= 16,384`;
- total transaction memory `<= 16,500,000`;
- total transaction CPU `<= 10,000,000,000`.

Every applied script must be published in a complete signed reference-script
transaction. Raw blueprint size is diagnostic, not acceptance evidence. Every
proof step, carriage publication, cancel, final proof mint, and removal
transaction records its signed bytes, memory, CPU, and remaining margins.

The reliable publication target is `<= 15,872` signed bytes, retaining the
repository's 512-byte reserve. The hard `16,384` boundary is never exceeded.
The implementation uses the shared Van Rossem emulator parameters and local
UPLC evaluation. Positive lifecycle tests have no `oversized: true`, raised
`maxTxSize`, raised ExUnits, or disabled local evaluation route.

Wave 0 inventories every fault-proof emulator configuration and size escape
hatch in the repository. Remove an override when it supports a positive,
production-readiness, or merge-gating claim. An intentionally over-limit
negative diagnostic may remain only when its name and assertion state that the
transaction is unpublishable; it is excluded from coverage, fit, and lifecycle
acceptance counts. The closure gate fails if any success path can select that
configuration, directly or through a helper default.

For dynamic scans, measure the maximum supported frontier and the branch that
executes the most expensive semantic path. A convenient small fixture is not
proof-fit evidence.

**Completion criterion:** the family-specific machine-readable fit ledger
contains a positive margin for every applied publication and lifecycle
transaction at the maximum supported shape.

## 6. Coordination, rolling dispatch, and shared-file ownership

### 6.1 Rolling family window

Keep a rolling window of at most three active family agents across at most two
adjacent waves. Wave membership fixes integration order; it is not a dispatch
barrier.

The primary agent maintains the window in this order:

1. Fill each open family slot with the earliest eligible unstarted family.
2. Treat a next-wave family as eligible when all shared engines it imports are
   frozen or integrated, its owned paths are disjoint from active assignments,
   and its local gate can finish without editing a cross-family file.
3. When a family reaches its local gate, collect its wiring manifest, release
   its slot, and immediately dispatch the next eligible family.
4. Integrate and commit waves in numbered order. Family-local work for the next
   wave may proceed while the current wave's central integration is red. Its
   central registration waits for every earlier wave integration commit.

**Dispatch criterion:** three eligible families are active, or every open slot
has a recorded eligibility blocker naming the missing shared dependency,
overlapping owner, or two-wave-window boundary.

Family agents report at three boundaries: a blocking dependency, a required
shared-substrate decision, and the green handoff. The primary uses those
reports as wakeups and spends the intervening time on the earliest incomplete
wave's integration.

The primary may assign up to two additional support agents to bounded
read-only analysis, wiring-manifest checks, or test-failure triage. A support
agent returns evidence to the owning family or primary; it does not own a
family, a central file, or the final independent review.

### 6.2 Ownership and ordered integration

Each family agent edits only its family directories, shared engines explicitly
assigned to it, and its own tests and size-plan document.

The primary agent owns these cross-family files and integrates them serially
in wave order:

- catalogue order/root and SDK category unions;
- blueprint/deployment/manifest reference-script identities;
- generic computation-thread init topology;
- watcher installed-category lists;
- central workflow runner registries;
- coverage, status, testing, and off-chain-reference summaries; and
- generated blueprint and catalogue artifacts.

A family agent supplies an exact wiring manifest at handoff: category name,
first-step hash role, ordered physical scripts, parameter order, state schemas,
runner factory, watcher classifier, and lifecycle test paths. The primary
agent applies that wiring only after the family's local gate is green. This
prevents concurrent edits to identity-bearing files.

The primary batches the green handoffs for the earliest incomplete wave,
applies their central wiring, and produces one integration commit after all
assigned vertical slices and the wave gate pass. A later wave may reach local
green first; its registration and integration commit remain ordered behind
the earlier wave.

**Integration criterion:** the wave commit contains all and only the green
wiring manifests assigned to that wave plus required shared changes, every
identity surface agrees, and the next wave's family-local edits remain outside
the commit.

### 6.3 Verification cadence

Use one verification tier for each claim:

1. During implementation, a family agent runs the smallest focused check that
   proves the behavior it just changed.
2. Before handoff, the family agent runs every named Aiken selector, focused
   TypeScript/lifecycle/fit test, touched-package typecheck, scoped format and
   lint check, and the fresh locked testnet build used by its fit ledger.
3. After batching the wave's central wiring, the primary runs the shared
   identity, deployment, watcher, and workflow checks once, then runs one
   complete wave regression including the full Aiken suite and affected
   package suites.
4. After a failure, fix the diagnosed surface, run its narrow reproducer, and
   rerun the complete wave regression once the central diff is stable.
5. The closure and review reruns remain the exhaustive final verification in
   Sections 8 and 9.

Generated blueprint and catalogue work uses one shared build lease so each fit
record names the exact artifact it measured.

**Verification criterion:** every family handoff records nonzero focused test
counts and exact commands, and the wave commit records one green consolidated
regression against its final central diff.

## 7. Work waves

The waves below define family membership, gates, and integration order. Apply
the rolling eligibility and ordered-integration rules from Section 6 when
dispatching them.

### Wave 0 — baseline and shared proof substrate

Use at most two agents.

- **Agent A, on-chain substrate:** accepted/forced subject binding, typed-reason
  binding helpers, canonical direction/source state, shared terminal
  contradiction rules, and Aiken mutation/ABI tests.
- **Agent B, off-chain/test substrate:** TypeScript twins, forced-leaf evidence
  extraction, common workflow journal interface, complete-lifecycle assertions,
  and the shared Van Rossem fit-ledger writer.

The primary agent records the clean baseline commit, current catalogue root,
testnet blueprint digest, compiler version, all existing oversized scripts,
every raised emulator transaction/ExUnit setting or `oversized` escape hatch,
and the exact set of unrelated working-tree changes to preserve.

**Gate:** substrate vectors agree across Aiken and TypeScript; the baseline
ledger is reproducible; no positive or merge-gating fault-proof test uses a
raised limit; no new category is registered yet.

### Wave 1 — committed-item and static-hash primitives

Run three family agents:

1. `fieldPreimageLengthMismatch`
2. `fieldItemWidthIllegal`
3. `scriptIntegrityHashMissing`

**Gate:** six direction lifecycles, all honest/mutation negatives, all
publications and maximum transactions fit, and IDs `20`, `21`, and `23` are
integrated serially.

### Wave 2 — output and mint-subject authentication

Run three family agents:

1. `transactionOutputNonCanonical`
2. `resolvedOutputNonCanonical`
3. `mintDeclaredAssetLimit`

**Gate:** own-output and prior-ledger output substitutions are independently
refused; output descriptor reconstruction agrees cross-language; maximum mint
frontier fits; IDs `26`, `29`, and `2c` are integrated.

### Wave 3 — credential and simple observer rules

Run three family agents:

1. `spendInputSignerMissing`
2. `protectedOutputSignerMissing`
3. `observersForbiddenOnUntaggedNetwork`

**Gate:** only cryptographically valid witnesses enter signer frontiers;
wrong-credential and invalid-signature mutations fail; both empty/non-empty
observer polarities pass; IDs `24`, `27`, and `2b` are integrated.

### Wave 4 — structural native-script subjects

Run three family agents:

1. `witnessScriptDecoding`
2. `outputReferenceScriptDecoding`
3. `executionSourceScriptDecoding`

All three reuse the frozen native structural scan engine after their subject
binds converge. Each retains its own category, bind validators, state type,
off-chain evidence path, and emulator suite.

**Gate:** malformed, exact node/depth boundary, adjacent refusal, decodable
wrongful-rejection, resume, and subject-substitution lifecycles pass for all
three; IDs `22`, `2a`, and `31` are integrated.

### Wave 5 — item and scalar purpose rules

Run three family agents:

1. `observerOrderInvalid`
2. `redeemerCanonicity`
3. `receivePurposeLanguage`

**Gate:** ordering tests exercise first/middle/last and duplicate positions;
redeemer tests cover malformed and canonical payloads at the real carriage
frontier; receive-purpose tests bind both purpose and language; IDs `25`, `28`,
and `34` are integrated.

### Wave 6 — missing material

Run three family agents:

1. `missingScriptSource`
2. `missingRedeemer`
3. `unusedScriptWitness`

**Gate:** complete-purpose/source scans prove the universal absence or reverse
use claim, alternate-source substitution fails, all purpose kinds and source
locations are exercised, and IDs `2d`, `2e`, and `2f` are integrated.

### Wave 7 — unused redeemers and aggregate folds

Run three family agents:

1. `unusedRedeemer`
2. `scriptIntegrityHashMismatch`
3. `distinctAssetAccumulationLimit`

**Gate:** redeemer reverse matching covers all purpose kinds; language-view
hash vectors match; each input/output/mint accumulator arm crosses the limit at
the authenticated typed coordinate and accepts the exact boundary; IDs `30`,
`33`, and `35` are integrated.

### Wave 8 — deterministic native execution and first direction upgrades

Run three family agents:

1. `executionNativeScriptInvalid`
2. wrongful-rejection extension for `zeroInput`
3. wrongful-rejection extension for `inputSetUniqueness`

**Gate:** the native evaluator reaches false and true terminals through bounded
steps; both set families prove the complete negation required for a wrongful
rejection; ID `32` and both existing-family deployment changes are integrated.

### Wave 9 — scalar direction upgrades

Run three family agents:

1. `invalidRange`
2. `networkId`
3. `minFee`

**Gate:** every typed rejection coordinate or transaction-global reason is
bound from the forced leaf; equality/boundary cases refuse conviction; all
existing accepted-invalid lifecycles remain green.

### Wave 10 — signature and witness direction upgrades

Run three family agents:

1. `invalidSignature`
2. `missingSignature`
3. `nativeScriptInvalid`

**Gate:** valid signature, required-signer membership, and true native-script
evaluation each contradict the exact forced rejection; malformed or unrelated
witnesses cannot frame the operator.

### Wave 11 — ledger-membership and min-Ada direction upgrades

Run three family agents:

1. `nonExistentInput`
2. `noReferenceInput`
3. `minAda`

**Gate:** spend/reference membership and exact-floor output evidence contradict
the corresponding forced rejection; wrong root/index/output evidence fails;
existing accepted-invalid paths retain their correction lifecycles.

### Wave 12 — value preservation direction upgrade

Run one family agent:

1. `valueNotPreserved`

The wrongful-rejection direction must prove the universal conservation claim,
not merely fail to find one differing asset. Use a resumable canonical union
fold across input, output, and mint asset domains.

**Gate:** Ada-only, token, mint, burn, multi-policy maximum, and adjacent
mutation cases pass in both directions under real limits.

## 8. Program closure gate

After Wave 12, freeze family implementation and perform integration without
concurrent family edits.

1. Rebuild the testnet blueprint from the working tree.
2. Regenerate catalogue root, deployment identity, applied-script registry,
   reference-script roles, SDK schemas, and watcher topology.
3. Run contract inspection and require every category and every physical step
   to be present exactly once.
4. Run every family lifecycle at its maximum supported shape with the shared
   Van Rossem parameters.
5. Run an all-category accepted-invalid and wrongful-rejection classification
   sweep and assert that only `PlutusExecutionFailed` selects
   `validationTraceDispute`.
6. Run durable watcher detect → prove → remove journeys for every new and
   extended category.
7. Update `coverage-matrix.md`, `catalogue-status.md`, `testing-status.md`,
   `execution-plan.md`, `architecture.md`, and `offchain-reference.md` from the
   executable results.
8. Search the complete fault-proof test surface again for raised transaction
   size, raised ExUnits, `oversized` publication, and disabled-evaluation
   routes; reconcile every hit against the Wave 0 inventory.

**Closure criterion:** 54/54 catalogue categories are compiled, registered,
publishable, locally lifecycle-proven, and watcher-installed; every
non-interactive reason has both applicable proof directions; no fit ledger has
a non-positive margin; and the working tree is frozen for review.

## 9. Single final on-chain review pass

Start one independent reviewer only after the closure gate passes. Give the
reviewer the baseline commit and the frozen final commit. No implementation
agent edits on-chain code while the reviewer is sampling it.

Review scope:

- every new or modified validator under
  `onchain/aiken/validators/fraud-proofs/`;
- every new or modified semantic/state module under
  `onchain/aiken/lib/midgard/fraud-proofs/`;
- shared source/reason binding and computation-thread helpers;
- catalogue/deployment parameter ordering and applied-script identities; and
- Aiken tests and fit ledgers as evidence for the on-chain claims.

The reviewer checks:

1. correctness of every decisive predicate against canonical validation;
2. completeness of the 47-reason disposition matrix;
3. honest-block and honest-forced-rejection non-convictability;
4. exact transaction, source kind, direction, reason, and coordinate binding;
5. descriptor, field, item, chunk, MPF, and checkpoint authentication;
6. state continuity, unique successor, cancellation, burn, and permanent mint;
7. total behavior on malicious committed bytes where abort would make a proof
   unavailable;
8. omission, duplication, reordering, replay, and cross-family substitution;
9. TypeScript/Aiken wire and semantic parity; and
10. signed publication and maximum-path byte/ExUnit evidence.

This is one continuous review pass. The reviewer records findings, the owning
family agent remediates them while the reviewer remains assigned, and the
reviewer verifies those remediations before closing the pass. Affected focused
checks and the complete closure suite rerun after remediation; that
verification is part of the same pass, not a second broad review.

**Review criterion:** no unresolved correctness, completeness, soundness,
publishability, or test-evidence finding remains.

## 10. Delivery and final acceptance

After the reviewer closes the pass, the primary agent verifies that the diff
contains only the planned program and preserved pre-existing user work, creates
one final reviewed integration commit, and pushes the current feature branch.
The handoff reports the commit hash, branch, catalogue root, blueprint digest,
review disposition, exact commands run, test counts, and the minimum measured
byte/memory/CPU margins. A push is not attempted before the review and closure
rerun are green.

The work is complete only when all of the following are simultaneously true:

- all 31 deterministic typed reason arms that previously lacked complete
  standalone coverage have narrow direct families;
- all 15 previously direct typed reason arms retain their accepted-invalid
  paths, and the 12 arms that lacked the opposite direction have successful
  wrongful-rejection paths;
- `PlutusExecutionFailed` is the sole interactive typed reason;
- every new and extended family has on-chain, off-chain, watcher, and complete
  Lucid Evolution lifecycle coverage;
- every semantic arm has realistic success, honest-failure, mutation,
  cancel/resume where applicable, permanent mint, and state-queue removal
  evidence;
- every applied reference script and every maximum-path transaction fits the
  real Van Rossem byte, memory, and CPU limits without overrides;
- catalogue, blueprint, manifest, watcher, SDK, and documentation identities
  agree; and
- the single final on-chain review pass is closed.
