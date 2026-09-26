# Test-quality review

Status: Active review backlog
Last reviewed: 2026-09-07

This document records test-quality concerns and their current interpretation.
The [task index](test-quality/README.md) provides scoped proposals and closure
criteria. [Testing principles](test-quality/principles.md) define the review
rules. A search match is a candidate, not evidence that an assertion is useless.

The semantic documentation review corrected several premises in the original
2026-09-05 audit. Its historical counts, blanket deletion recommendations,
claimed removable line totals, and claim of an exhaustive test review are not
current verification evidence. This review does not claim that the runtime
remediation tasks have been implemented or that every test has been executed.
Source names below identify the behavior to review; task cards carry the work.

## 1. Assertions that cannot fail

An assertion over the same already-computed local value on both sides cannot
check the producer. An assertion comparing separate fields, saved data with a
recomputed value, or a producer's output with its required property can fail
when the producer or saved data is wrong. Do not conflate these cases.

### 1.1 Fit-ledger provenance and integrity

There are 43 retained `*fit-ledger.json` files under
`docs/fault-proofs/size-plans`. Their consumers split into 25 current-build/live
comparison inputs and 18 saved self-consistency fixtures. The latter remain
required test inputs; they are not fresh release evidence.

Against the existing local `onchain/aiken/plutus.json`, these files contain 29
distinct recorded blueprint digests and one matching digest. This is a local
inventory comparison, not a fresh compilation or an assertion that every
nonmatching historical fixture is defective. [TQ-01](test-quality/tq-01-blueprint-provenance.md)
provides the measurement and consumer classification procedure.

A test comparing an artifact digest with a literal can detect a changed
artifact, but cannot establish that it describes the current compiler output.
Current-evidence consumers need the actual build and measurement basis.
Conversely, positive-margin checks, arithmetic consistency, and stored checksums
can detect producer regressions or malformed artifacts. They should not be
removed merely because the current builder enforces those invariants.

Generation and verification must remain distinguishable: writing a fixture and
then comparing against that just-written fixture does not verify its previous
content. See [TQ-08](test-quality/tq-08-tautological-assertions.md) and
[TQ-15](test-quality/tq-15-fit-ledger-consolidation.md).

### 1.2 Corrected examples

- `demo/da-committee-node/tests/conflict-evidence.test.ts` deliberately uses the
  same header hash with different availability commitments. The commitments,
  including different bond owners, are the signed identities. Equal header
  hashes do not make this an unexercised equivocation case.
- `demo/midgard-core/tests/plutus-data-wellformed.test.ts` combines fixed corpus
  entries with 6,000 generated entries. Its greater-than-6,000 guard can detect
  loss of the fixed corpus; it is not a check on a lone 6,000-element literal.
- `demo/midgard-sdk/tests/fraud-proof-rebind-604.test.ts` uses the SDK schema and
  `Data.to` to check that a changed opening changes the redeemer. That is a
  meaningful encoding sensitivity control, although it does not establish
  validator refusal by itself.
- `demo/da-committee-node/tests/public-retained-da-store.test.ts` includes a
  direct DELETE call against its own fake. That particular assertion checks the
  fake's configured barrier, not PostgreSQL permissions. The surrounding store
  call is a separate check and must not be deleted with it.
- The reverse-checkpoint encoding test in
  `onchain/aiken/lib/midgard/fraud-proofs/unused-script-witness/rule.test.ak`
  already compares with concrete schema-derived bytes. The old identical-call
  finding is closed in the current source.

## 2. Hand-maintained constants that mirror current output

Classify a pin before changing it. Public API export rosters, wire vectors,
category identities, deployment identity, and exact measured execution values
may be deliberate contracts. A pin changing after an intentional implementation
change does not establish that it was useless.

`demo/lucid-midgard/tests/api-export-snapshot.test.ts` detected an unintended
public export change until f69a0ddb2 deleted it; nothing replaces it. <!-- doc-links:historical -->
In `demo/midgard-node/tests/reference-scripts.test.ts`, `toContain` checks
notice removal of named targets, but do not reject unexpected additional ones.
An exact independently defined roster may be appropriate when both matter.

`onchain/aiken/scripts/exec-ledger-within-basis-v1.mjs` and related verifiers use
exact execution measurements as part of their evidence contract. Replacing
these with a ratchet changes what the gate proves; retain the existing contract
unless an explicitly reviewed requirement changes it. Likewise a publication
reserve below the ledger maximum is not automatically arbitrary.

See [TQ-12](test-quality/tq-12-derive-not-photograph.md).

### 2.1 The unfit snapshot

`demo/midgard-validation/tests/resolver-proof-fit-sweep.test.ts` checked the exact
unfit set against `resolver-proof-fit-sweep-v1.unfit-pin.json`, detecting both
new and closed gaps, as well as identity and prose changes. f69a0ddb2 deleted
the test without a replacement, so the pin has no consumer and nothing guards
the unfit set today. <!-- doc-links:historical --> The row-consistency
checks beside it do not independently prevent growth of the unfit set. Preserve
that regression guard while separating stable identities from explanatory prose
if warranted. See [TQ-13](test-quality/tq-13-snapshot-artifacts.md).

## 3. Assertions over production source text

Source-fragment and textual ordering checks can survive a behavioral defect or
fail on formatting. Candidates include `benchmark-regression.test.mjs`, the
node's commit-order tests, and validation carriage-policy checks. Establish the
actual contract and its replacement before removal.

Some source checks intentionally enforce imports, package declarations, or
cross-language ABI conventions. They are not all behavioral-test defects.
Use an existing lint facility where it better enforces the same rule, and keep
an executable behavioral check when the rule is about runtime effects.

The old lucid `local-validation-shared.test.ts` file is already deleted in this
working tree. README heading checks in `documentation-examples.test.ts` and
prose-based necessity checks in `validation-machine.test.ts` have also been
replaced or removed. Do not schedule those old forms again. See
[TQ-11](test-quality/tq-11-source-text-assertions.md).

## 4. Assertions on error and log prose

A broad error regex can accept an unrelated failure. Prefer a structured cause
where available, retaining message checks when wording is itself the public
contract or no stronger diagnostic exists.

`demo/midgard-core/src/codec/errors.ts` already exposes an `as const` code object,
a code union, and `MidgardTxCodecError`. A TypeScript enum is not required to
make this interface typed. Extend diagnostics only for a demonstrated missing
contract. See [TQ-04](test-quality/tq-04-typed-refusal-codes.md).

## 5. Weak and assertion-free tests

Bare `toThrow()` in a case promising a particular refusal does not establish
that cause. But absence of a literal `expect()` is not absence of an oracle:
awaited operations and shared helpers may fail on violated postconditions.
The Scalus AddSignatures suite awaits real builder/evaluator completion; review
what completion guarantees before adding a redundant assertion.

A non-empty error message is a weak cause discriminator, not a tautology:
`new Error()` has an empty message. Replace a weak refusal oracle with a stronger
one before removing it. Non-null assertions (`!`) likewise perform no runtime
check. An `arrayContaining` with one matcher only establishes one match; the
watcher's `l1/native-block-admission.test.ts` should be reviewed against its
claim about every malformed identifier.

Do not infer ledger rejection from a `CEKConst` containing False without checking
the evaluator's script-language success convention. See
[TQ-05](test-quality/tq-05-name-the-failure-mode.md) and
[TQ-10](test-quality/tq-10-vacuous-and-wall-clock.md).

### 5.1 Aiken `fail` tests

A fixture abort can satisfy `fail` before the intended function is reached.
Use appropriate positive controls and inspect the selected test's actual path.
The existing focused-check runner requires a full module selector for dotted
module names; a bare partial selector piped through `tail` is not sufficient
execution evidence. [TQ-07](test-quality/tq-07-aiken-fail-vacuity.md) records the
current controls and guarded invocation.

## 6. Over-mocking, call counts and call order

Mocks can test a legitimate interaction contract. Exact counts may matter for
submission, retries, resource use, or idempotency; order may matter for
persistence and authorization. Neither should be discarded solely because a
state assertion also exists.

Review the semantic `testOnlyJournalCategoryAlias` branches in the fault-proof
journals separately from ordinary injected constructors and diagnostic accessors.
A test-only behavior fork can make a test exercise different logic, while an
injected collaborator can make the real logic testable. Neither `ForTest` nor
`NODE_ENV` alone identifies the defect. See
[TQ-14](test-quality/tq-14-mocking-and-seams.md).

## 7. Redundant duplicates and template drift

Shared scenario drivers can prevent drift in the tier-2 and mirrored lifecycle
families, but preserve independent expected coverage and fresh mutable state per
scenario. A helper's existence does not make forgetting to call it a compile
error. A single-row `it.each` is not inherently defective.

Round trips, determinism checks, and scenarios deploying different validators
can look duplicated while checking distinct behavior. See
[TQ-16](test-quality/tq-16-template-drift.md).

### 7.1 Fit evidence and bounds

The current tree has 22 `*-publication-fit.test.ts` files and 40
`*-fit-ledger.test.ts` files. A saved publication row does not replace a test
that builds, signs, and evaluates the current transaction. Consolidation must
preserve a current producer and required consumer for every removed live check.

The 16,384-byte ledger maximum and 15,872-byte publication target serve different
purposes. So do protocol execution maxima and reserved execution budgets. Apply
the governing requirement to each claim, not a uniform number chosen only for
consistency. Build-from-literals and rebuild-from-stored describe workflows,
not necessarily different schemas. See
[TQ-15](test-quality/tq-15-fit-ledger-consolidation.md).

## 8. Dead code and never-executed gates

Trace tests through package scripts and workflow invocations before declaring
them orphaned. `node --test` suites outside a Vitest directory can have their
own valid command. Optional PostgreSQL, operator, platform, and measurement
lanes need documented prerequisites and invocation; missing default CI coverage
is not a reason to delete useful tests.

Required suites must fail clearly on missing prerequisites. Distinguish a
missing blueprint file, which may already fail during module loading, from a
present blueprint lacking a family, which a `runIf` may skip. A warning alone
cannot fulfill a mandatory acceptance gate.

The obsolete KNOWN RED chronology in
`demo/midgard-fault-proofs/tests/support/emulator/blueprints.ts` has been replaced
with current build prerequisites and emulator assurance guidance. See
[TQ-02](test-quality/tq-02-fail-closed-skips.md).

## 9. Restatements of the type system

The original deletion category was incorrectly defined. TypeScript `string`
and branded hex types do not validate runtime length or contents. Imports can
also have runtime packaging failures distinct from typechecking. Keep or
strengthen runtime shape and existence checks according to their actual
producer and promised behavior. See
[TQ-09](test-quality/tq-09-type-system-restatements.md).

## 10. Cost disproportionate to unique coverage

Timeouts are upper limits, not measured runtimes. Do not report twenty
600-second timeouts as twenty ten-minute executions or claim a speedup without
measurement. Measure the affected lane and retain its unique behavior before
sharing setup or relocating benchmarks.

Use controlled clocks for deterministic scheduling policy tests where possible.
Real process, database, timer, and network integration behavior can still need
real clocks and bounded waits. A structural count is not always equivalent to
a latency or memory regression guard. See
[TQ-03](test-quality/tq-03-timeout-and-budget-basis.md) and
[TQ-10](test-quality/tq-10-vacuous-and-wall-clock.md).

## 11. Gates that cannot see what they claim to check

A test reading `dist` needs its build prerequisite; an artifact gate needs the
provenance its claim requires. Freshness, stored consistency, and current live
execution are separate checks. A static grep or passing fixture test cannot
establish that a live deployment is ready.

Keep current failing verifier inputs until the underlying requirement is
satisfied. Do not make an evidence gate green by removing its input or replacing
its recorded hash without remeasurement. See TQ-01, TQ-02, and TQ-12.

## 12. Harness options that weaken every suite that uses them

The emulator's `alwaysFraudProofCatalogue` and `alwaysStateQueue` defaults are
false; individual suites opt into substitutes. A suite using one cannot claim
to have exercised the substituted validator's authorization, although other
validators and builder behavior may still be tested.

`support/emulator/reference-scripts.ts` has already replaced the old 14,000-byte
threshold with the protocol maximum. Diagnostic handling of oversized scripts
still does not establish ordinary publication fit. A named exception is not
proof of fit either.

Registration coverage needs the correct applied canonical chain and membership,
not merely distinct hashes. Legitimate scripts may share hashes. Shared
assertion helpers are valid; only hidden scenario execution inside a supposedly
pure fixture builder needs separation.

The common refusal helper recognizes script-execution failure, while an existing
stronger example distinguishes Spend and Mint purpose. Do not describe purpose
matching as exact script identification. See
[TQ-06](test-quality/tq-06-discriminating-onchain-refusal.md) and
[TQ-17](test-quality/tq-17-harness-flags.md).

## 13. Tests inverted into pins of their own failure

A declared remaining-gap set can prevent regression. If changing its structure,
preserve rejection of newly missing coverage. The old four-entry
`typed-reason-disposition.test.ts` residue claim is closed: current source
expects no missing noninteractive installations.

`e2e-state-correction-acceptance.test.ts` explicitly supplies a missing
availability capability, and separately supplies an aggregate without reconciled
independent provenance. Blocking those fixtures is correct fail-closed behavior.
An assertion about an ambient compiled registry should be reviewed separately;
it is not a reason to remove these refusal cases.

A null release-evidence digest records current release status. Update that
expectation with a legitimate release transition, preserving the prohibition on
claiming absent evidence. See
[TQ-18](test-quality/tq-18-inverted-and-misnamed.md).

## 14. Names that promise what the body does not check

Keep names proportional to the executed path. A process harness running
synthetic marker writers checks supervision or report parsing, not a real node
election. A router-existence assertion does not establish a route set. An
encoding mutation control is useful under that name without claiming on-chain
refusal.

Either strengthen a case to establish its promised behavior or narrow the name.
Recheck references to renamed tests and preserve intentionally failing
production assurance cases. [TQ-18](test-quality/tq-18-inverted-and-misnamed.md)
owns this review; [TQ-19](test-quality/tq-19-guardrails.md) proposes proportionate
guardrails for confirmed defects, not blanket bans on testing techniques.
