# Implementation and verification

Baseline: `43dc8e42b7706fc79978cfe1d4b4f5ec22144189`. Final composed source: `bce6e0dfb5931287cf9d022ed928c68c66014a1b`. All five planned changes are implemented and integrated, with independent component reviews and the integration results below. Publication targets `colll78/canonical-v1-watcher-l1-source-checkpoint`; the remote identity is recorded after the coordinated push. Hosted CI and the retained baseline acceptance failures remain separate from implementation completion. The [plan](REPORT.md) retains the investigated evidence and rejected alternatives.

## Completed components

The wallet hardening is commit `ce0d56b7c7044627d306e628a3ec0e209f4e7b91`. The completion Effect rejects an explicit empty pool before invoking Lucid and sends that refusal through the supplied error mapper. Omitted and nonempty pools retain their existing behavior. Three real-emulator regressions cover refusal, selected-input completion without wallet UTxO discovery, and ordinary wallet discovery. The tests do not claim that every collateral provider method is avoided.

The container derivation is commit `691d1851c5a9492697369c6032ac2d3a66bf2433`; it depends on the package-task migration. The three Dockerfiles derive manifest copies, source copies, and both build phases from the contributor package graph, including explicit build prerequisites. Native stages and public deployment-profile rebuilding retain their existing owners. Node CI also builds the committee image; each image job starts its actual CLI with `--help`.

Deployment metadata now has one core owner for member identities, compiler titles,
roles, family aliases, commands and positions. SDK construction and restoration,
node manifests/publication and watcher role maps consume its projections. Real
blueprint checks preserve all 527 ordered manifest tuples and 522 publication
members; the 52 catalogue categories also retain their identities and order.
Both independent consensus reviewers closed the public SDK restoration defect
after its four-file correction, with no new confirmed or plausible findings.

Package tasks are declared once in each package manifest. Ordinary package
scripts and contrib share validated argv and artifact prerequisites; the
ordinary frontend preserves Vitest arguments, environment, configured reporters
and exit status. Preludes retain the caller environment and stop at the first
failure. Contrib keeps its existing strict receipt and disposable-database
policy. All eleven migrated test recipes and ordered preparation closures match
the captured baseline. The task review closes its four ordinary-runner findings;
the boundary companion closes the remaining preflight-consumer finding.

Installed-package boundaries now probe production installs through declared
exports and bins, requiring complete per-mode evidence even when a child exits
successfully early. The independent successor review closes all four boundary
findings, plus task-review F5. Preflight selectors, SDK receipt eligibility and
DA build coverage read task declarations; CI filters and the composed workflow
census include the shared task helpers. The final installed-package executions and their explicit limits are recorded below.

## Component verification

Component executions used Node 22.22.2 and the repository-pinned pnpm unless a review report states otherwise. The independent task re-review used ambient Node 24.13.1 for its four focused fixtures. All observations below are dated 2026-10-16; other integration work ran concurrently. Counts describe separate runs, not disjoint coverage.

| Check                                                                        | Observed result                                                                                                                                                             |
| ---------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Focused SDK completion regression through `contrib test`                     | Original helper: 2 passed, 1 failed because empty inputs reached completion. Changed helper: 3 passed.                                                                      |
| SDK tests reached from `tx-completion.ts`                                    | 819 passed in 70 files.                                                                                                                                                     |
| `test:tx-prep:sdk`                                                           | Lucid 199 passed in 22 files; SDK 1,122 passed in 108 files.                                                                                                                |
| `test:tx-prep:node`                                                          | Node 149 passed in 7 files; node-tools 42 passed in 3 files.                                                                                                                |
| `test:tx-prep:emulator`                                                      | Node 64 passed in 8 files; fault-proof tail 448 passed, 1 failed, plus one suite teardown failure. See the retained failure below.                                          |
| Two affected emulator files with the supported durable SQLite test directory | All 15 cases passed; the history suite still failed its ledger comparison.                                                                                                  |
| History suite with the unchanged baseline helper                             | All 14 cases passed; the same ledger comparison failed. The changed helper was restored and its hash verified.                                                              |
| Wallet source/test ESLint and Prettier; committed fast pre-push checks       | Passed.                                                                                                                                                                     |
| Two independent consensus reviews of the complete wallet patch               | No confirmed or plausible findings.                                                                                                                                         |
| Docker plan and workflow-trigger tests                                       | 19 passed, including 4 graph/rendering tests; all 3 checked-in plans current.                                                                                               |
| Three component `docker build --no-cache` runs                               | Watcher, committee, and node passed.                                                                                                                                        |
| Three component image entrypoints with `--help`                              | All passed.                                                                                                                                                                 |
| Independent container scope review                                           | No actionable findings.                                                                                                                                                     |
| Metadata core omission/identity checks                                       | 4 passed in 1 file, exit 0, in the component checkout.                                                                                                                      |
| Metadata real-blueprint construction/restoration/publication checks          | 41 passed in 3 files, exit 0, after the SDK restoration correction.                                                                                                         |
| Metadata causal controls                                                     | Missing-member guard mutant killed (2 expected failures of 4 executed); swapped real selectors mutant killed (4 expected failures of 16 executed). Both sources restored.   |
| Two independent metadata fix reviews                                         | F1 closed; no new findings. Symbolic comparisons preserve 527 manifest members, 522 publication members and 52 categories; these are not real Plutus evaluations.           |
| Frozen task-v2 affected tooling tests                                        | 63 passed, 0 skipped, exit 0; all 33 source hashes matched the reviewed packet.                                                                                             |
| Ordinary transport tests with CI database variables and shard arguments      | 25 passed, 4 intentional live-only skips in 3 files, exit 0. Skipped cases were not exercised.                                                                              |
| Guarded real-tsup freshness checks                                           | 16 passed, 0 skipped, exit 0, before the ordinary-frontend-only split.                                                                                                      |
| Independent task successor review                                            | Four focused fixture checks passed; F1–F4 and lifecycle concerns closed. F5 closed by the boundary companion.                                                               |
| Independent boundary successor tooling checks                                | 97 passed, 0 skipped, exit 0; 21 owned and 13 borrowed source identities matched.                                                                                           |
| Composed task/boundary/preflight/workflow/container/portability tooling      | 169 passed, 0 skipped, exit 0, at 21:35 UTC in the integration checkout. This includes the four golden-channel portability tests.                                           |
| Composed guarded metadata and wallet regressions                             | Node 41 passed in 3 files; core 4 passed in 1 file; SDK wallet 3 passed in 1 file. Each exited 0 with no skips and without `--source-only`; receipts recorded at 21:45 UTC. |

The emulator failure is preserved, not counted as a pass. The first run rejected a temporary SQLite fixture path; rerunning with `MIDGARD_PERMISSIONLESS_SQLITE_TEST_DIRECTORY` under the checkout resolved that case. The remaining event-history ledger mismatch occurs with both the unchanged helper and the changed helper: its pinned blueprint identity/compiler metadata differs from the current build. Ledger reconciliation belongs to the concurrent protocol program; this change has not rewritten it.

The installed-package check had reproduced the existing `midgard-core` public
`golden-channel.mjs` portability failure in import and require modes. The R10
owner's repair and the private-fixture selector fix are now composed into the
integration tree. The four golden-channel portability tests pass within the
169-test composed command. The final real installed core boundary also exercises
all 59 export specifiers under both import and require conditions.

## Final integration verification

| Check                                                             | Result (2026-10-16)                                                                                                                                                                      |
| ----------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Full selected preflight                                           | 53 passed checks; 4 failed checks, exit 1. 6 hosted checks pending. Original receipts are preserved.                                                                                     |
| Aiken focused suite and blueprint                                 | 5,993 tests passed; 1,135 validator artifacts built, exit 0.                                                                                                                             |
| Transaction preparation, node                                     | Node 149 tests and node-tools 42 tests passed, exit 0.                                                                                                                                   |
| Transaction preparation, emulator                                 | Node 64 tests and fault-proof 449 tests passed. One fault-proof file failed its afterAll ledger comparison; lane exit 1.                                                                 |
| Installed package boundaries                                      | 11 packages passed: 106 import specifiers, 84 require specifiers and 7 bins. Thirty inactive conditions were recorded, not exercised.                                                    |
| Cold composed images                                              | Watcher, committee and node each built with --no-cache and passed their actual CLI help invocation. See input reconciliation below.                                                      |
| Final task-admission/module-cap corrections                       | 8 focused tests passed, zero skips, exit 0. The complete workspace-helper gate also passed all 82 tests, zero skips. The pre-push command reruns repository tooling and causal controls. |
| Causal scratch-copy correction                                    | 52 fixed tests passed and all 23 intentional mutants failed in an exact composed-source archive, exit 0 for the control harness.                                                         |
| CI routing correction                                             | 19 workflow census tests passed, zero skips; workflow lint clean, exit 0. Four causal cases failed before the routing correction and passed afterward.                                   |
| Docs site, specification, generated vectors and execution ledgers | All selected checks passed; exact commands and receipts are in the machine-readable ledger.                                                                                              |

The package manager is pinned to pnpm 11.18.0 because real nested production
installs exposed the old release writing dependency executables outside the
consumer directory. This uses the upstream fix rather than a local install
workaround. The migration preserves all 13 importers, 640 package resolutions,
642 snapshots and three patches; a frozen install leaves the committed lockfile
byte-identical. Both Nix shells route through the repository pin. The production
peer graph drops only the incompatible fast-check 4.7/pure-rand 8.4 pair in favor
of the already-locked compatible fast-check 3.23.2 binding. Independent review
and 23 focused checks pass. The workspace dependency graph is unchanged.

The installed follower boundary exposed an existing CLI convention gap: a lone
`--help` printed usage as an unknown-command error. The four-line correction
returns success and writes the existing help to stdout; the installed consumer
check, follower typecheck and lint verify that correction.

The existing family checklist now follows reached deployment member declarations
and SDK builder/title imports. Its 28 tests pass, including the three independently
reproduced false-green cases that were corrected before integration. All 52
categories preserve exact pre-migration results: 42 complete and the same 10
presence/filename gaps. These gaps are not new runtime findings.

The broader preflight exposed outdated task-admission fixtures, obsolete module
size exceptions after the metadata reduction, and a missing manifest in the
causal-control scratch copy. Their original failed receipts remain in the
ledger, alongside the corrected focused runs. Docs Site CI also now watches
the shared package-task helper and pnpm workspace settings in both event modes. The final validation records
distinguish a complete passing gate from a repaired subset of an earlier run.

The three repaired tooling failures are distinct from the retained emulator failure. The full preflight output remains exit 1; no whole-run pass is claimed. The final pre-push gate and remote identity are recorded after publication. The six ordinary CI-owned checks remain pending here.

The watcher image wrapper initially reported a changed-input identity because the Nix shell follow-up landed during its build. Reconstructing both input hashes proves that was the only changed input; that file is not copied into the watcher image. Its actual cold build and help invocation both passed. Committee and node input hashes were stable. After the follower CLI correction, comparison against all three tested images shows the other 31 follower output files byte-identical; their entrypoints do not use its standalone CLI.

No live deployment, durable-state reset, consensus parameter change or merge is
part of this publication. The separate protocol program retains its ledger and
publication-fit acceptance work. The program record marks these five source
components integrated; it does not promote hosted CI or baseline failures to
accepted checks.
