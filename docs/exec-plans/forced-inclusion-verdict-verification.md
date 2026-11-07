# Forced inclusion verdict: implementation evidence

Status: Task 2 implementation and focused local verification complete;
integration/release dependencies, Task 3 and deployment acceptance remain open.
Updated: 2026-09-12. Consumer: review and release decisions for this change.

## Reviewed state

The implementation follows the [ADR](../midgard/decisions/forced-inclusion-submission-verdict.md)
and [Task 1 acceptance matrix](forced-inclusion-verdict/01-protocol-design.md).
It lives in `/home/gumbo/midgard-hub/midgard-forced-inclusion`, branch
`codex/forced-inclusion-verdict`, against snapshot
`a46ec6222a46650872061570cbe7f462d18e88e1`. The snapshot contains 298 inherited
watcher paths; its parent is `9797ce41ce5d436e309eca07e2020ee29c395859`.
The implementation was committed as `742df026e9215842101ea2e60b2c887e7e033fbd`.
Review its difference from the snapshot for the original redesign. Subsequent
local merge commits integrate watcher work as recorded below. No reset or
deployment has been performed; these commits are not release approval.

The forced full envelope is `[1, body, witness]`; its compact envelope and
three-field source contain no validity decision. L1 orders commit that immutable
source. A block leaf adds only `verdict`, the operator's claim; validation
independently computes the actual result from the unchanged submission.
Normal transaction encoding and commitment semantics remain
separate. Proof decoding and commitment selection use authenticated source kind;
an order is selected by its full order key and transaction ID.

Current normal testnet blueprint: SHA-256
`e91e45ab81fc779a7b404cdbfde347c4f148aadd7bbf5a12160392640bb419e0`, 1,149 validator
entries, pinned compiler `v1.1.23+5adf783`. Build:
`/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet`
from `onchain/aiken`, exit 0, log `/tmp/forced-aiken-build-final-13.log`.
The latest change saves 23 compiled bytes in the legacy value-not-preserved
step 02 by checking opening count at the end of its existing authenticated
fold and specializing the token-only helper. Its two blueprint entries are the
only changes from intermediate blueprint
`655eadbec91eabfe60f7cb4f868ce43edc0bd9bc97598ec24d6a14d95dff212b`.

## Reproduction and results

Run demo commands from this worktree after
`source "$(git rev-parse --absolute-git-dir)/task2-env.sh"`. The test database
prefix is `midgard_forced_verdict_task2`; PostgreSQL uses port 5433. Invoke pnpm
through Node 22 in the declared Nix environment, for example:

```bash
nix develop ./demo --command bash -c 'node "$(command -v pnpm)" --dir demo/midgard-core run test'
```

Use serialized workspace builds/typechecks and one DB-backed node Vitest run at
a time. Use the pinned Aiken executable, not the default PATH compiler. The
transaction-root fixture check passes with the pinned formatter; the default
compiler produced a false stale-fixture result.

These are scoped verification results, not a blanket workspace or release pass. Each full package
command is `pnpm --dir demo/<package> run test`, with isolated environment above;
validation, watcher and proof runs use `--no-file-parallelism` where recorded.
Raw logs retain collected filenames, counts and timings.

| Check                                  | Result                                                                             | Evidence                                    |
| -------------------------------------- | ---------------------------------------------------------------------------------- | ------------------------------------------- |
| Full intermediate Aiken suite          | 4,151 passed; exact inventory coverage; no missing or unexpected cases             | `/tmp/forced-aiken-batches-11/summary.json` |
| Final Aiken suite after size repair    | 4,152 passed; 33 batches, 1,044.94 seconds, exact coverage                         | `/tmp/forced-aiken-batches-13/summary.json` |
| Size repair Aiken cases                | 8 passed, including truncated, excessive and forged openings                       | `/tmp/forced-aiken-value-size-13b.log`      |
| Size repair emulator lifecycle         | 7 passed in 5 files                                                                | `/tmp/forced-legacy-value-size-13.log`      |
| Core full suite                        | 553 passed in 51 files                                                             | `/tmp/forced-midgard-core-full-11.log`      |
| SDK full suite                         | 518 passed in 60 files                                                             | `/tmp/forced-midgard-sdk-full-11.log`       |
| Validation full suite                  | 461 passed in 56 files                                                             | `/tmp/forced-validation-full-12.log`        |
| DA full suite                          | 338 passed, 1 existing skipped test, 34 files; 26.25 seconds                       | `/tmp/forced-da-full-15.log`                |
| Node full suite                        | 1,609 passed; 3 existing skips; 625.43 seconds                                     | `/tmp/forced-node-full-14.log`              |
| Watcher full diagnostic                | 1,160 passed, 1 stale assertion failed; its 4-test recheck passed                  | `/var/tmp/forced-watcher-full-16.log`       |
| Final full serialized proof suite / F4 | 2,587 passed in all 350 files; zero skips; exit 0; 13,410.22s                      | `/var/tmp/forced-proof-final-16.log`        |
| Exact same-ID order selection          | 14 passed, 7 factories with matching and foreign keys                              | `/tmp/forced-order-selection-12d.log`       |
| Typed reason and output binding        | 121 passed in 2 files                                                              | `/tmp/forced-reason-output-repairs-12.log`  |
| Missing-redeemer full lifecycle        | 17 passed, both directions, maximum carriage and all purposes                      | `/tmp/forced-missing-redeemer-11b.log`      |
| Data breadth                           | 7 passed                                                                           | `/tmp/forced-data-breadth-12.log`           |
| Workspace build                        | Passed serialized final build, exit 0                                              | `/tmp/forced-workspace-build-23.log`        |
| Workspace typecheck                    | Exit 2: inherited missing recovery API; no other package error                     | `/var/tmp/forced-types-31.log`              |
| Retained DA corpus                     | Update and strict no-update verification passed: 21 producer, 20 DA, 3 proof cases | `/tmp/forced-retained-da-verify-15.log`     |

The focused Task 1 boundary matrix was rerun on 2026-09-11 against the current
blueprint: **410 tests across 29 files passed**, exit 0. T1: 87; T2: 82;
T3: 47; T4: 45; T5: 33; T6: 48; T7: 19 node, 33 watcher, 16 core.
Command script: `/var/tmp/forced-boundaries-final-17.sh`; individual logs:
`/var/tmp/forced-boundary-<group>-17.log`. The complete group/count manifest and
logs are preserved under this worktree's Git directory in <!-- doc-links:external -->
`evidence/forced-submission-task2-final-16/focused-boundary-results-17.json`.
These focused passes do not replace the complete fault-proof suite result.

The final watcher regression run completed with 1,160 passing cases and one
failure in 83 files (`/var/tmp/forced-watcher-full-16.log`, exit 1, 4,265.40s).
Its installed correction-and-healthy-successor journey passed (2,324.247s).
The single failure was an expected-response mismatch in
`tests/runtime/user-event-origin-fixture.test.ts`: the synthetic Kupo response
now carries `{ language, script }`, while the assertion still expected an outer
script CBOR string. The assertion now expects the native script payload and
language; all four tests in that file passed on recheck
(`/var/tmp/forced-watcher-query-fixture-18.log`, exit 0, 37.22s). This is a full
regression run plus a focused correction, not a claim that the original full
command exited successfully. No additional autonomous devnet watcher work was
performed.

The full Aiken check exceeded memory as one process. Batches preserve every
module-qualified exact test selector and the default property-test count,
record source hashes, and fail on missing/unexpected tests or nonzero status.
The final batch inventory and summary must be preserved with the handoff.

F4 uses `MIDGARD_WRITE_FIT_LEDGER=1`,
`MIDGARD_FIT_FRAGMENT_DIR=/var/tmp/midgard-forced-fit-final-16.ozs1ucv1`, and
`MIDGARD_FIT_MEASUREMENT_RUN=forced-submission-final-16`.
Provenance: `/var/tmp/forced-proof-final-16-provenance.json`.
Run 13 and transaction-preparation run 12 ended without completion summaries;
they are interrupted diagnostics, not completed acceptance. Run 16 records
its process ID and exit status alongside the durable log.
Earlier run 12 used ignored environment names and inherited a shared measurement
directory; it was stopped and cannot establish final fit evidence. Diagnostic
parallel run 11 also cannot replace the required serialized F4 result.
No fragments from those attempts may be mixed into the final namespace.

The final coverage/provenance check confirmed all 350 inventoried proof files
ran, all 2,587 case results passed, no cases were skipped, and inventoried test
files and the blueprint were unchanged. All 20 explicit recorder producers
emitted fresh fragments containing 1,489 evaluated transaction rows; their
identities and nonnegative margins were checked. Other family-specific writers
and maximum-coverage assertions ran inside the same successful full suite.
Guard: `/var/tmp/verify-forced-focused-final-17.py`.

The durable evidence archive is this worktree's Git directory plus
`evidence/forced-submission-task2-final-16`. It contains final logs, exit statuses,
commands, inventories, the Aiken batches, `full-proof-results-16.json`,
`focused-boundary-results-17.json`, `tx-prep-results-16.json`, and
`fresh-fit-fragments/`. Twelve runtime-only measurement reports with no regression
reader were preserved under `runtime-reports/` and removed from the untracked
working tree, following the fit-evidence guide. Tracked regression inputs remain.

Other final checks: normalized pinned-Aiken formatting has zero drift;
`aiken check --skip-tests` and the exact A1–A4/M1 script passed
(`/tmp/forced-aiken-skip-final-15.log`, `/tmp/forced-named-aiken-15.log`).
Workspace lint passed in run 25; formatting and the later watcher-specific lint
passed in `/tmp/forced-final-style-27.log` (exit 0). `make spec` passed
(`/tmp/forced-spec-build.log`). Root fixture generation/check passed with the
pinned compiler (`/tmp/forced-root-fixture-check-final-14.log`); one-step fixture
check passed (`/tmp/forced-one-step-fixture-check-13b.log`). Docs facts and links
passed: 12 groups and 264 files (`/tmp/forced-docs-links-14.log`).
DA full-suite command was `pnpm --dir demo/da-committee-node exec vitest run
--maxWorkers=4 --minWorkers=1`. The full transaction-preparation command passed (exit 0) in
`/var/tmp/forced-tx-prep-final-16.log`: 1,350 test executions across 199 file
executions. Its six stages passed 175, 518, 123, 51, 51 and 432 tests; the final
stage ran all 100 selected fault-proof emulator files with 432 passing tests
and no skips or failures (5,129.03s). The broader F4 full-package run also passed, covering the additional lifecycle,
reconstruction and maximum scenarios.

Final focused-verification hygiene: whole workspace lint and formatting passed,
as did `git diff --check` (`/var/tmp/forced-lint-31.log`,
`/var/tmp/forced-format-31.log`). Serialized workspace typecheck completed with
exit 2 and only the inherited recovery API error (`/var/tmp/forced-types-31.log`).
No new redesign type error was reported.

## Findings and remaining gates

### Scope clarification (2026-09-11)

The user confirmed that autonomous fault detection and completion by the watcher
on a Cardano devnet with Van Rossem configuration is a separate, already failing,
in-progress task. Its overall completion is not a forced-source redesign gate.
This task owns migration of affected watcher source/claim/replay boundaries and
regressions introduced by that migration. Continue recording full watcher-suite
results, but attribute failures before doing further repairs; do not absorb the
separate autonomous watcher project or count an inherited failure as a redesign
regression. The captured missing recovery API was subsequently resolved in the
2026-09-12 integration recorded below.

All tests in the full fault-proof package passed on the rebuilt identity:
2,587 cases in 350 files, with no skipped cases. The separate complete
transaction-preparation command passed, including all 432 selected fault-proof
emulator cases. Earlier failures and their focused repairs remain diagnostic
history; the final successful runs establish current local regression evidence.

No manual Preprod acceptance has been performed for this redesigned identity:
no fresh Midgard deployment, deposit/L2 transaction sequence, deliberate fraud,
manual fault-proof CLI completion, or faulty-block removal on Preprod. Task 3
as currently written prepares a deployment handoff and explicitly excludes live
submissions; it does not itself execute that manual live acceptance. That flow
must be separately scheduled and recorded, independently of autonomous watcher
completion. No claim of live acceptance follows from local emulator results.

Resolved implementation findings include forced compact hash selection, CEK
context/source-kind authentication, source-kind propagation through validation
workers, exact same-ID order selection, and min-fee state serialization without
a second validity field. Independent bounded reviews found no additional issue
in the latest size optimization or watcher fixture corrections. This does not
replace Task 3's independent whole-boundary review.

The final standards review found a remaining test-helper boundary: the shared
retained-transaction fixture passed normal bytes into forced replay. It now
explicitly projects the prepared native transaction into the forced submission
before replay. Two new devnet tests pass without live artifacts and check both operator
claims, identical submitted DA bytes, independent actual rejection, unchanged
normal bytes, retained native-script classification and evidence preparation,
and accepted Plutus forced replay (`/var/tmp/forced-journey-source-17d.log`,
two cases, 10.59 seconds, exit 0). The native execution helper now decodes the
forced envelope and selects forced reconstruction explicitly; its retained
script test derives forced fault material. The real journey order builder also
uses the explicit client projection. The final bounded review closed both
findings. Focused lint, formatting, node-tools build and diff checks passed in
`/var/tmp/forced-journey-final-30.log`; typecheck still reports only the inherited
recovery API error (`/var/tmp/forced-journey-style-types-29.log`).
These helper changes do not change the validator artifact or production replay.
The retained helper is consumed only by node-tools journey fixtures, outside
the full proof package run; its two dedicated regression cases passed separately.
The older retained-deposit journey tests require a live run directory and were
not counted as passing or replaced by skipped collection.

The watcher fixture repairs preserve actual correction, exact economics,
operator removal and healthy successor assertions. Its synthetic Kupo script
response now follows the real response shape. Successor onboarding happens
after the last-active-operator removal, matching the scheduler's existing rule.
The synthetic native-block request log is recorded before delivering its block,
so subprocess closure cannot race the evidence assertion.
The corrected watcher journey then timed out while source ingestion was still
at block 1,627, before the successor commit at 1,709. Its background fixture
chain advanced four times faster than its declared block interval. The fixture
now generates real confirmations at the configured depth plus one (31), and
background blocks every 20 seconds. Production finality remains 30 blocks.
The full rerun `/var/tmp/forced-watcher-full-16.log` proved correction and healthy
continuation. Its separate stale query assertion was corrected and rechecked as
recorded above. This local result does not complete the separate devnet watcher task.

Four judgments remain separate:

1. **Implementation conformance:** implemented and reviewed; Task 3
   independent review remains open.
2. **Local behavior:** full proof suite, complete transaction-preparation
   acceptance and the focused boundary matrix passed; watcher results and the
   historical workspace typecheck failure are recorded separately. The integrated
   workspace now passes typechecking.
3. **Capability/fit:** final F4 passed. Maximum forced-order mint remains an
   explicit [bounded-authentication dependency](forced-inclusion-verdict/order-mint-capacity-dependency.md).
   Task 1 M1 permits this report, but it does not establish maximum-order support.
4. **Deployment acceptance:** not run and not accepted.

The inherited node-tools live test imports
`readAdmittedLocalKupmiosSignedTransactionRecovery`, absent from the captured
fault-proof baseline. The 2026-09-12 integration incorporates the watcher
implementation and its tests. The full workspace typecheck now passes; no test
was deleted and no fallback was added to bypass that dependency.

## Merge verification (2026-09-12)

Completed integration commit: `3742e4beb555b1901ce2b89af5dd5142d35e63ef`.
It includes watcher work through
`32e3e815974e4a4bae19465b613afe1420f84791` and the committed redesign above.
After the user confirmed the watcher task had paused at a safe checkpoint,
`colll78/canonical-v1-watcher-l1-source-checkpoint` in
`/home/gumbo/midgard-hub/midgard` was fast-forwarded to the integration commit.

**Destination update is complete.** The three remaining uncommitted documents
(`docs/fault-proofs/README.md`, `coverage-matrix.md`, and `remaining-gaps.md`)
were preserved byte-for-byte and remain uncommitted. The initial 44 uncommitted
paths were backed up; the owner subsequently committed the 43 code paths as
`920194bc7`. The initial measurement report is retained in the backup too.

Manual resolutions preserve both source kinds and exact order-key selection in
newly refactored proof workflows, while retaining watcher recovery after a
header leaves the queue. A new regression verifies redeemer workflow artifact
serialization and re-admission select the second of two same-body orders with
different witness data. Six newer field-publication prerequisite calls now carry
the authenticated source kind. The recovery API implementation matches the
watcher branch. The latest checkpoint/retry changes are preserved unchanged.

| Verification                                                  | Result                                                                                                  |
| ------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| Serialized full workspace typecheck                           | Passed on `427948f40`                                                                                   |
| Serialized workspace build                                    | Passed on `30f20157a`; only journey tooling changed afterward and its build passed again on `427948f40` |
| Forced-redesign T1–T7 boundary matrix                         | 410 passed, 29 files, zero skips                                                                        |
| Selected proof workflow and emulator regressions              | 251 passed, 16 files, zero skips                                                                        |
| Watcher runtime, funding, origin and finality regressions     | 119 passed, 7 files; 2 retained-run-dependent cases skipped                                             |
| Journey source, correction, timing and checkpoint regressions | 47 passed, 5 files; 1 retained-run timing case skipped                                                  |
| Workspace lint, formatting and diff checks                    | Passed                                                                                                  |
| Independent standards and spec merge reviews                  | No actionable findings in either review                                                                 |

The final watcher checkpoint changes only raw L1 header observation and its
regression test: observations bind to the original header NFT mint across DA
attestation output recreation. It merged cleanly. On `3742e4beb`, an additional
focused run passed 115 tests in four files with zero skips: raw L1 family
derivation, manifest-bound family recovery, terminal recovery, and Kupmios
source. The full workspace typecheck passed again, as did lint and formatting
for the two checkpoint files.

The destination had an older ignored `onchain/aiken/plutus.json` (SHA-256
`20302aad09931798b0b7e9775bdddc21b450b18e5d6470ee5132d93e24258cd4`).
It was backed up in the evidence directory and replaced with the exact verified
1,149-entry blueprint from the redesign worktree, matching the `e91e45ab…`
hash recorded above. Git does not transfer this generated artifact. An additional
28 tests in three files passed in the destination checkout using that blueprint:
Aiken blueprint data, forced submission lifecycle, and exact forced-order
source selection. The serialized full workspace build also passed in the
destination, rebuilding its local package outputs. Documentation checks passed
(12 fact groups, 272 files). No live service was restarted or redeployed.

Earlier integration total: 827 passing test executions and 3 explicitly recorded
skips. The selected proof run includes forced submission lifecycle/capacity, redeemer canonicity in
both directions, zero-input lifecycle, exact order selection, raw L1 recovery,
publication prerequisites and terminal recovery. The initially attempted tooling
command selected zero files under its default configuration; it was replaced by
the explicit `vitest.watcher-journeys.config.ts` run over all five named files.
The zero-file attempt is not counted as verification.

The boundary/proof/watcher runs used `30f20157a`; those package sources are
byte-identical in `427948f40`. Validator source and blueprint are byte-identical
to `742df026e`, including the SHA-256 recorded above. The earlier full 2,587-case
proof suite and 4,152 Aiken cases remain historical evidence against that
validator artifact; they were not rerun in full for this merge. The maximum-order
capacity limitation remains open. No deferred admission-grammar redesign,
Preprod acceptance, live submission or deployment reset was performed here.

Commands, logs, original working-tree backups and result metadata are archived
under `/home/gumbo/midgard-hub/midgard/.git/evidence/forced-merge-20260912`.
Final successful remaining-check script: `/var/tmp/forced-merge-final-checks.sh`
(exit 0). Boundary script: `/var/tmp/forced-merge-boundaries.sh` (exit 0).
Proof/watcher commands and passing results are preserved from
`/var/tmp/forced-merge-checks.sh`; its subsequent zero-file tooling attempt
returned 1 and was corrected by the final script.

## Deployment handoff to Task 3

Owner: Task 3's implementation/release reviewer and the subsequent live
acceptance operator. Target context is Cardano Preprod with local Kupmios.
No funded wallet roles, fresh one-shot, deployment run ID, or new deployment
has been allocated by Task 2.

Freeze a reviewed commit, pinned untraced blueprint, applied contract parameters,
validator hashes, consensus profile, catalogue and deployment manifests together.
Regenerate node/watcher/DA artifacts and release evidence from that identity.
The new source/order/leaf encodings, DA preimages, SQL classification, and
persisted replay/dispute evidence are incompatible with the old deployment.
Old-identity restart must fail closed. Preserve existing state and evidence;
do not attach the new runtime to old protocol UTxOs. A fresh deployment requires
an explicit operational action under [state reset rules](../agents/state-reset.md),
new one-shot/reference scripts, matching manifests and matching durable stores.
Ambiguous submitted transactions must be reconciled before retry or replacement.

Use the [E2E skill](../../.agents/skills/midgard-e2e-acceptance/SKILL.md) and its
[live reference](../../.agents/skills/midgard-e2e-acceptance/references/live-acceptance.md).
The runbook currency check previously failed on a stale
`--state-correction-release-evidence <path>` option that the finalizer never
implemented; the option has been removed from the runbook and its validator,
and the check passes against current finalizer source. The finalizer's two
local readiness gates (`state_correction_local_workflow_readiness`,
`availability_challenge_readiness`) were removed as redundant: the watcher
refuses to start without the exact catalogue installed, and the acceptance
aggregate requires one verified workflow journal per family. Do not copy stale
command blocks. No live operation was attempted. Retain current CLI help with
the release handoff.

Live acceptance must include both dishonest forced verdict directions through
public L1+DA reconstruction and installed production workflows, terminal proof
mint and correction, exact slash/reward, original-submission settlement,
healthy post-correction continuation, and restart/rollback/identity refusal.
It must also meet the selected run's broader gates: committee libp2p publication,
independent public retained-DA retrieval, deposit and L2 transfers, attestation,
automatic finality/merge, withdrawal payout, launch-scope proof-family coverage,
the complete recovery matrix, and independent final chain/database reconciliation.
Require zero pending mutations/finalizations and resumed watcher readiness.
Local emulator results do not replace any of these live observations.
