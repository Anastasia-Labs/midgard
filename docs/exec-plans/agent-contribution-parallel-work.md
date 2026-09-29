# Contribution work alongside the DA integration

Scope and observations: 2026-09-28, isolated branch `codex/agent-preflight`,
starting from `c240d955a`. The shared checkout continued changing during this
work. Nothing here claims those concurrent changes are verified or integrated.

## Completed local work

| Work                           | Reason and result                                                                                                                                                                                                                                                        |
| ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Independent preflight coverage | Existing docs-site link/build/type checks, spec build, phase4 asset checks, workspace lint and helper tests were outside preflight. They are selected with explicit prerequisites; the generated check list stays identical with or without an untracked blueprint.      |
| Diagnostics and guidance       | Missing docs-site dependencies and Nix availability have separate capability results, including unknown probe failures. Corrected stale statements about serial CI and absent blueprint/test guards.                                                                     |
| Stress-wallet split            | A 5,473-line command combined persistence, parsing and four wallet operations. Moved 175 declarations into 16 concern modules plus a public barrel; all 87 public symbols preserved. Two unchanged lint exceptions moved with their source lines.                        |
| Stress-throughput split        | A 4,146-line command combined configuration, artifact validation, measurements and orchestration. Moved 115 declarations into 15 concern modules plus a public barrel. All 27 public symbols are preserved; consumer imports and package source exports follow the move. |
| Benchmark design               | [Frozen pilot prompts, acceptance rubrics and experiment protocol](agent-contribution-benchmark.md), with isolated environments, paired trials, human correction costs and explicit incomplete/blocked outcomes. No baseline was fabricated.                             |
| Merge-gate preparation         | [Proposal procedure](agent-merge-gates.md) and a tested generator for disabled ruleset JSON plus prerequisite workflow changes. Active workflows and remote policy are unchanged.                                                                                        |

Each source split is committed alone. The source-move verifier compares every
top-level declaration against git, and the package's build/typecheck/lint/tests
check its wiring. It does not prove deployed behavior. Exact verification
commands and results are in the split commit messages and the session handoff.

## Oversized-module sweep

The follow-up uses the audit's 5,000-line threshold and the stable local base
`44faf3f67`. On 2026-09-28 it split all nine remaining source/test modules above
that threshold. A fresh inventory includes tracked and new source files across
TypeScript, JavaScript, Aiken, Haskell, Go, Python, shell, Nix, Lean, C/C++ and
SQL. It finds zero files above 5,000 lines and 93 above 2,000. This does not claim
that every smaller module has an ideal boundary.

The user explicitly requested preparation of the overlapping splits. They are
isolated from the active checkout; integration must preserve that checkout's
concurrent changes.

| Original module                                  | Result                                                                                                                                                                                                                                                                             |
| ------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Core `deployment-manifest-identity.ts`           | Public entrypoint plus 12 modules for catalogue roles, script mappings, identity, protocol parameters and finalized manifests; 89 declarations preserved.                                                                                                                          |
| Fault-proofs `validation-dispute/submit.ts`      | Public entrypoint plus 14 modules for transaction material, reference scripts, evidence and dispute stages; 165 declarations preserved.                                                                                                                                            |
| Watcher `indexers/user-event-indexer.ts`         | Public entrypoint plus five modules for types, policy, decoding, snapshots and local history; 197 declarations preserved.                                                                                                                                                          |
| Watcher `l1/rollback-engine.ts`                  | Public entrypoint plus five modules for types, records, rollback state, recovery and durable authority; 161 declarations preserved.                                                                                                                                                |
| Node `tests/database.test.ts`                    | Original test entrypoint registers eight concern suites and shared setup; 124 registration expressions preserved, with explicit relative-path rebasing for helpers and subprocesses.                                                                                               |
| DA committee `tests/committee-service.test.ts`   | Original entrypoint registers seven concern suites and shared cleanup; 76 registration expressions preserved. Parameterized registrations collect more tests than expressions.                                                                                                     |
| Validation `validation-machine/trace-builder.ts` | Entry point, preparation phase, execution/completion phase and shared types. The 137 executable statements retain their order; execution budgets use a shared record so witness-recording closures observe updates across phases. This is a refactor, not a pure declaration move. |
| Aiken validation-machine suite                   | Seven phase test modules and nine fixture/type modules under `onchain/aiken/lib/midgard/validation-machine-tests/`; all 475 declarations and 199 test bodies preserved. The ordered-boundary generator now owns the public constants fixture.                                      |
| Generated CEK core-step suite                    | Generator emits one module per curated program under `onchain/aiken/lib/midgard/cek-core-step-goldens/`; all 132 test bodies and vector bytes preserved. JSON metadata names the 26 output paths.                                                                                  |

The four production declaration moves preserve their public import paths and
exports. Their module graph has no runtime cycles. Existing lint exceptions move
with the same source lines. Test registration modules retain the original suite
entrypoints, hooks and registration order.

Preflight follows JSON artifact manifests as well as generator helpers, so a
change to any generated CEK module selects its golden check. The regression test
checks all 26 paths and fails when JSON-manifest traversal is removed.

The [verification record](agent-contribution-module-splits.md) records exact
commands, counts, baseline comparisons and unresolved failures. A source-move
proof does not establish deployment readiness.

## Observed verification limitation

The first full node-tools run after the throughput split passed 268 of 269
Vitest tests; the process-ownership cleanup test failed with “process cmdline
mismatch.” A full repeat passed all 269. The earlier wallet-split run also
passed all 269. The failing test, process-ownership implementation and imported
repository helpers are unchanged from `c240d955a` and do not import either split.
This is one failure in three full-package observations of that unchanged test,
not a reliable failure-rate estimate or a claim that the flake is fixed.

The failure arises in the existing post-termination identity-validation path.
The exact interleaving was not captured, so its root cause is not confirmed.
A separate deterministic process-state regression and sized rerun are needed
before calling it fixed. No retry, skip, weakened assertion or timeout change
was added. The declaration proofs, package typecheck/lint/build and all tests
that import the split modules passed.

## Remaining after this parallel pass

1. At a stable DA checkpoint, integrate these isolated commits and resolve any
   newly introduced overlap. No push, PR update or merge is part of this pass.
2. Run current-head integration CI and acceptance for the combined branch;
   resolve the integration PR's conflicts. Local tooling tests cannot establish
   protocol or deployment readiness.
3. Run the benchmark baseline, then blind holdout trials. The pilot task corpus
   and treatment must not leak task solutions into a measured run.
4. Choose protected branches and check provenance, land the reviewed workflow
   changes with updated trigger expectations, then approve/activate the policy.
5. Reconcile all nine follow-up splits with changes made after the stable base,
   then repeat the affected verification on the combined tree.

The larger hardening plan still contains separate projects such as model
checking and historical reviewer evaluation. The local structural work does not relabel that entire program complete.
