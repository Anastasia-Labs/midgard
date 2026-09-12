# Task 2: Implement one committed forced-transaction verdict

- Status: Implemented and locally verified; integration/release dependencies recorded
- Last reviewed: 2026-09-11
- Implementation boundary: complete local protocol implementation, rebuilt validators, and behavioral tests on one integration branch.
- Dependencies: Task 1 completed, including its ADR, impact inventory, acceptance matrix, and reconstructible baseline.
- Non-goals: live deployment/reset, changes to normal transaction semantics, rejection-policy redesign, unrelated cleanup, or partial deployment of the new format.

## What this task accomplishes

Make the software implement the agreed model everywhere. The transaction the
user submitted remains identical through settlement. The operator adds a verdict,
and a challenger can independently prove that verdict wrong in either direction.
Successful compilation is an intermediate checkpoint; this task finishes with
the complete behavior working under the rebuilt contracts.

Read [Task 1](01-protocol-design.md), including its shared execution contract,
and its resulting ADR before edits. Use the same integration state. Confirm the
acceptance matrix still matches the code; record changes with their reasons.
Read `demo/AGENTS.md`, `onchain/aiken/AGENTS.md`, and nested guides for touched
directories. Use `.agents/skills/aiken-contract-build/SKILL.md` before any Aiken
compile or debug operation.

## Implement in this order

1. **Source types, codecs, and commitments.** Implement the forced-specific
   validity-free encoding in Aiken and TypeScript. Reuse body, witness, and field
   primitives. Enforce the ADR's exact wire shape and authenticate `tx_id` from
   the body. Add golden vectors whose expected bytes/hashes come from the written
   encoding and independent cross-language checks, rather than only calling the
   implementation twice. Keep ordinary transaction admission semantics intact.
2. **Submission, order authentication, and settlement.** Update SDK construction,
   L1 mint/spend builders, validator schemas, field carriage, and settlement
   membership. Carry the immutable source unchanged. Authenticate the correct
   order key, source commitment, and verdict-bearing leaf; equality of `tx_id`
   alone is insufficient. Cover the existing cancellation/expiry paths where
   they consume the changed payload without altering their policy.
3. **Replay and all proof consumers.** Update block/DA codecs, operator
   classification, watcher reconstruction, validation entry points, direct
   fault proofs, interactive disputes, and their field-access doors. Use the
   committed verdict for claimed polarity and independent replay for the actual
   result. Remove forced-source normalization and acceptance of the obsolete
   forced wire shape. Any retained machine view is ephemeral and cannot supply
   an independent committed bit. Preserve immutable dispute claims on rejection.
4. **Artifacts and documentation.** Update all affected parameter applications,
   builders, blueprint consumers, deployment-identity checks, fixtures, and
   normative specification sections in the same integration change. Regenerate
   validators with the pinned compiler and explicit testnet environment. Invalidate
   old measurements for changed identities and regenerate evidence through its
   owning executable checks. Explain the new deployment requirement without
   resetting existing state or presenting generated artifacts as deployed.
5. **Close the integration.** Run the frozen acceptance matrix and the applicable
   direct checks required by Task 3. Resolve failures attributable to the change
   and required affected baseline failures. Record unrelated failures separately;
   their existence cannot satisfy a failing release gate. Review the whole diff,
   including newly added/untracked files, before handing it to Task 3.

## Required behavioral evidence

Task 1 maps these obligations to concrete tests and commands. Shared tests may
cover several rows, but the report must show which scenarios actually ran.

| Scenario                                            | Required result                                                                                                                                                                                |
| --------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Aiken/TypeScript encoding and decoding              | Both implementations agree on canonical bytes, transaction IDs, submission commitments, and leaf hashes for each vector; malformed/obsolete forced envelopes refused at the specified boundary |
| Verdict changes over one submission                 | `tx_id` and submission bytes/commitment remain identical; the forced leaf hash changes, including when only the rejection reason changes                                                       |
| Witness or material substitution with the same body | The original L1 order cannot settle or authenticate a proof against altered witness bytes, field material, or declared lengths                                                                 |
| Two L1 orders for one L2 transaction                | Distinct order identities remain represented and authenticated; membership for one cannot satisfy the other; replay applies the existing ordering/state rules                                  |
| Honest accepted forced transaction                  | Exact accepted ledger effect; settlement authenticates the original submission; a false challenge fails                                                                                        |
| Honest rejected forced transaction                  | Exact no-op and unchanged pre-state; correct reason/subject binding; settlement authenticates the original submission; a false challenge fails                                                 |
| Operator accepts an invalid transaction             | Independent replay rejects; the applicable proof completes correction under the rebuilt deployed-in-emulator contracts                                                                         |
| Operator rejects a valid transaction                | Independent replay accepts; wrongful-rejection proof completes correction under the rebuilt deployed-in-emulator contracts                                                                     |
| Wrong rejection reason or subject                   | The existing reason-selection policy is enforced; rejected polarity alone cannot authenticate a different claim                                                                                |
| Malicious claimed delta and foreign context         | Rejection preserves the original disputed delta commitment while deriving no operations; a proof from another source, order, header, deployment, or execution context cannot be substituted    |
| Public-data reconstruction                          | Watcher/DA replay reconstructs the same immutable submission and claim without operator-private normalized bytes; missing/mismatched material follows the existing fail-closed behavior        |
| Restart/replay and identity mismatch                | Relevant persisted evidence resumes under the matching identity and fails closed under the incompatible old identity; no inference of a missing schema or implicit reset                       |
| Normal transaction regression                       | Existing normal acceptance/admission and proof behavior survives the shared-helper changes                                                                                                     |
| Proof capability and fit                            | Changed order-authentication and registered proof paths retain required admissible shapes and pass fresh maximum/adjacent size and execution-margin checks                                     |

For each changed validator, use Lucid Evolution emulator scenarios for successful
behavior and a transaction the validator must refuse, following
`docs/agents/contracts.md`. Fault correction evidence must exercise the actual
registered workflow through its terminal correction and applicable economics;
constructing a proof object or checking an intermediate predicate is insufficient.
Include both direct-family and validation-dispute routes wherever their shared
source bindings changed. Do not substitute one representative family for the
affected-family inventory.

## Build and evidence requirements

Use the exact command/environment matrix from Task 1, informed by
[Task 3's verification contract](03-verification.md#verification-contract).
Build the real blueprint before dependent emulator scenarios and point them to
its absolute path with `MIDGARD_REAL_BLUEPRINT_PATH` where the harness uses it.
If verbose traces are used to diagnose a failure, rebuild the normal testnet
artifact and rerun the affected checks before final evidence.

Keep checkpoint results with revision, command, exit status, collected scenario
count, and the relevant artifact identity. If code, parameters, or blueprint
changes afterward, rerun the dependent checks. A search showing no normalization
calls supports review but cannot replace working submission and challenge tests.

## Completion gate

- Every impact-inventory row is implemented or justified unchanged against the
  ADR, including shared carriage and proof doors.
- The committed forced source has no validity field in its outer or nested
  encoding. The verdict is the only committed operator validity decision.
- Submission identity, exact settlement binding, independent execution, both
  challenge directions, reason binding, and immutable claimed deltas satisfy
  the required behavioral evidence above.
- All required affected checks pass using current source and real regenerated
  validators. No synthetic validator, skipped case, reduced capability, stale
  measurement, or zero-test collection substitutes for a required result.
- Specifications, generated artifacts, deployment-identity handling, and code
  describe one coherent format. Runtime normalization and obsolete forced-source
  fallbacks are absent; normal transaction semantics remain covered.
- The handoff identifies the complete diff, baseline provenance, final artifact
  identities, test evidence, known limitations, and any live acceptance still
  required. Task 2 completion establishes implementation readiness, not launch.

## Progress and handoff

Task 1 is complete. Task 2 has an isolated implementation checkout:

- Worktree: `/home/gumbo/midgard-hub/midgard-forced-inclusion`.
- Branch: `codex/forced-inclusion-verdict`.
- Baseline snapshot: `a46ec6222a46650872061570cbe7f462d18e88e1`.
- Snapshot parent: `9797ce41ce5d436e309eca07e2020ee29c395859` on
  `colll78/canonical-v1-watcher-l1-source-checkpoint`.

The snapshot includes Task 1 documents and the current uncommitted dependencies
from the watcher checkout (298 changed or untracked paths). It was captured with
a private Git index; the watcher branch and staged tree were not changed.
This is an unverified integration baseline, not an implementation or release
commit. Review Task 2 changes against this snapshot so inherited watcher work
does not get attributed to the schema redesign. Capture provenance is in the
original repository's `.git/codex-task-baselines/forced-inclusion-verdict-task2/baseline.json`.

Dependencies were installed independently with the frozen lockfile, offline,
using pnpm's copy import method. No ignored deployment configuration, durable
state, Aiken build directory, or blueprint was copied. Worktree-local Git hook
configuration points to `.githooks` in this checkout.

Before commands in this worktree, load its local environment:

```bash
cd /home/gumbo/midgard-hub/midgard-forced-inclusion
source "$(git rev-parse --absolute-git-dir)/task2-env.sh"
```

That file sets `MIDGARD_TEST_DATABASE_PREFIX=midgard_forced_verdict_task2`, points
`MIDGARD_REAL_BLUEPRINT_PATH` at this checkout's future `onchain/aiken/plutus.json`,
and separates Graphify state and fit evidence under this worktree's Git directory.
Database suites now run only in the configured isolated namespace. Inspect any
harness that bypasses the shared database helper before running it. Allocate fresh fit evidence
directories and run identifiers for subsequent measurement runs as Task 1 requires.

The Nix shell selects Node 22.22.2, but its pnpm 9.15.9 launcher has a Node 24
shebang. Invoke pnpm through the shell's Node explicitly:

```bash
nix develop ./demo --command bash -c 'node "$(command -v pnpm)" --dir demo/midgard-core exec vitest run tests/native-codec.test.ts --reporter=verbose'
```

This baseline check passed: one file, 17 tests, exit 0, Vitest 3.0.7, Node 22.22.2,
against snapshot `a46ec6222a46650872061570cbe7f462d18e88e1`. It establishes only
the existing codec baseline; the full Task 2 acceptance matrix remains pending.

Keep the watcher checkout running independently. At explicit integration
checkpoints, compare its new work with this snapshot, reconcile overlapping
source/codec/identity changes, and rerun the affected tests against the combined
revision and rebuilt artifact. A separate worktree prevents concurrent file
edits; it does not eliminate semantic integration work.

Hand the complete integration state and evidence to
[Task 3](03-verification.md), not an isolated package diff.

### Implementation and focused verification (2026-09-11, 18:23 UTC)

The schema migration is implemented across Aiken, core, SDK, validation, node,
DA, watcher and all proof consumers. The full/compact forced source has no
validity decision; the committed verdict is authoritative. Exact order-key
selection, immutable SQL classification, independent replay, source-kind
bindings, and incompatible-identity refusal have behavioral coverage.

The current normal testnet blueprint contains 1,149 validator entries, SHA-256
`e91e45ab81fc779a7b404cdbfde347c4f148aadd7bbf5a12160392640bb419e0`.
Compiler: `v1.1.23+5adf783`; command: `aiken build --env testnet`.
Nine deployment/initialization tests passed and regenerated watcher/DA fixtures
from this identity. A late value-not-preserved size repair saves 23 compiled
bytes while preserving exact opening authentication; eight focused Aiken tests
and seven emulator lifecycle tests passed.

The final Aiken run passed all 4,152 cases in 33 batches with exact inventory
coverage. The full serialized fault-proof suite passed all 2,587 cases in all
350 files, with no skips; the fresh namespace contains all 20 expected recorder
fragments and 1,489 evaluated transaction rows. The complete local
transaction-preparation command passed, including all 432 fault-proof emulator
cases in 100 files. The focused T1–T7 matrix passed 410 cases across 29 files.

Core (553), SDK (518), validation (461), DA (338) and node (1,609) have passing
full-suite checkpoints. The watcher run passed 1,160 cases and failed one stale
query-response assertion; the corrected file's four tests passed on recheck.
The installed correction/healthy-successor journey passed. This is local
regression evidence, not completion of the separate devnet watcher objective.
Whole workspace lint, formatting and diff checks passed. Workspace typecheck
still reports only the inherited missing recovery API. Final bounded reviews
closed the identified source-migration defects.

The [evidence index](../forced-inclusion-verdict-verification.md) records final
counts, artifact identities, commands, limitations and the durable evidence
archive. The [maximum-order dependency](order-mint-capacity-dependency.md)
remains open under Task 1 M1's explicit reporting exception; maximum-order
admission is not established. Reconcile the inherited watcher API dependency
before workspace acceptance.

The implementation and requested focused local verification are complete for
handoff to Task 3's independent review, with those integration/capacity limits
recorded. This is not release acceptance. No deployment, durable-state reset or
merge has run. Task 3 and manual Preprod acceptance have not started.

Command corrections: forward test flags with `pnpm run test --no-file-parallelism`.
For fit writes, use `MIDGARD_FIT_FRAGMENT_DIR` and
`MIDGARD_FIT_MEASUREMENT_RUN`; similarly named variables are ignored. Pin
`MIDGARD_AIKEN_BIN` for the root-fixture generator as well as contract builds.

### Scope clarification (2026-09-11)

The user confirmed that making the already failing autonomous watcher detect and
complete proofs on a Cardano devnet with Van Rossem configuration belongs to the
separate watcher task. This change still owns its source/claim/replay migrations
and introduced regressions. Run the prescribed watcher regression checks and
record inherited failures separately; do not make completion of the independent
autonomous watcher objective a redesign gate. Full Lucid Evolution fault-proof
emulator confirmation has now passed as recorded above. Manual Preprod deployment, deposit, L2
transactions, fraud, CLI proof completion and block removal have not run for this
identity; Task 3's current live-submission exclusion means its handoff alone will
not execute that separate acceptance flow.
