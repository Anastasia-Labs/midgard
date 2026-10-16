# Implementation and verification

Baseline: `43dc8e42b7706fc79978cfe1d4b4f5ec22144189`. Reviewed composed source: `b7cd3416c9f42dc3ec9128b7f12710d52719ae58`. The metadata, package-task and installed-boundary components have completed independent review; final integration and publication gates remain pending. The [plan](REPORT.md) retains the investigated evidence and rejected alternatives; [implementation checks](evidence/implementation-checks.json) records exact component identities and local evidence locations.

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
The final packet and both original/fix review identities are recorded in the
machine-readable evidence below.

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
census include the shared task helpers. These review results concern source and
tooling fixtures; final application-package acceptance remains an integration
gate.

## Verification recorded so far

Component executions used Node 22.22.2 and the repository-pinned pnpm unless a review report states otherwise. The independent task re-review used ambient Node 24.13.1 for its four focused fixtures. All observations below are dated 2026-10-16; other integration work ran concurrently. Counts describe separate runs, not disjoint coverage. Exact component commands, working directories and log/receipt locations are in [implementation checks](evidence/implementation-checks.json).

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
| Three `docker build --no-cache` runs                                         | Watcher, committee, and node passed.                                                                                                                                        |
| Three real image entrypoints with `--help`                                   | All passed.                                                                                                                                                                 |
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
169-test composed command; this does not replace final installed-package
acceptance across the application packages.

## Remaining integration gates

The reviewed component states are bound to source commit
`b7cd3416c9f42dc3ec9128b7f12710d52719ae58`. The installed-boundary
installation-placement correction and final application-package acceptance
remain pending, along with the final selected integration checks and publication
to the authorized target. No component is marked integrated, published or
accepted in the program record. No full-program, hosted-CI, deployment or live
acceptance claim is made by the component results above.
