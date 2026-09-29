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

The sweep covered every tracked TypeScript/Aiken file over 5,000 lines at the
starting checkpoint, plus the adjacent 4,146-line throughput command. The
starting checkpoint has ten files over that threshold; the older skill inventory
has nine because it was measured before the DA committee tests grew. This is
not a claim that every smaller file is optimally structured.

| Candidate                                                            | Disposition and evidence                                                                                                                                                                                                                                                       |
| -------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Node-tools `commands/stress-wallets.ts`                              | Split. Independent tooling, three recent commits including a September 27 fix; neither shared dirty paths nor commits after the checkpoint overlap it. Pure declaration seams; not generated.                                                                                  |
| Node-tools `commands/e2e-stress-l2-throughput.ts`                    | Split. Independent stress measurement/tooling; recent September 27 command change; no observed concurrent path changes. Parsing, observation and orchestration have pure declaration seams.                                                                                    |
| Fault-proofs `validation-dispute/submit.ts`                          | Defer until DA checkpoint. Changed by pooled-bond integration `93a16f8d8`; moving the submission orchestration would create conflicts in the active protocol surface.                                                                                                          |
| Core `deployment-manifest-identity.ts`                               | Defer until DA checkpoint. Changed by the pooled-bond timing/economics and off-chain integration commits; shared deployment-profile sources are also being edited.                                                                                                             |
| DA committee `tests/committee-service.test.ts`                       | Defer. Directly exercises the committee service under the active bond redesign.                                                                                                                                                                                                |
| Node `tests/database.test.ts`                                        | Defer. Observed dirty in the shared checkout during this work; moving it would directly overlap another session.                                                                                                                                                               |
| Watcher `indexers/user-event-indexer.ts` and `l1/rollback-engine.ts` | Defer. Event-history recovery integration is active, including watcher origin, timeout and recovery tests and shared emulator fixtures. These modules implement the behavior that those changes are validating.                                                                |
| Validation `validation-machine/trace-builder.ts`                     | Separate behavioral refactor. Its principal function spans lines 291–6777 at the checkpoint; moving small helpers would leave the actual large function intact. Cutting it requires control-flow changes and consensus review.                                                 |
| Aiken `validation-machine-v1.test.ak`                                | Separate generator-aware test split after the protocol checkpoint. The ordered-collection golden generator rewrites named constants in this module, and the module consumes active forced-event fixtures. A direct test-file move alone would break its regeneration contract. |
| Aiken `cek-core-step-v1-golden.test.ak`                              | Separate generator change. This is generated output, so splitting the file directly would be overwritten on regeneration.                                                                                                                                                      |

These boundaries apply the
[split skill](../../.agents/skills/splitting-oversized-modules/SKILL.md):
“**Nobody else is editing it**,” “**The seam is a pure move**,” and
“**It is not generated**.” The active-scope decisions above are conservative
judgments from changed paths and recent commits; they are not claims to know
another session's future edits. Recheck the scope before integrating the splits.

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
5. Reassess the deferred modules at the checkpoint. The trace builder and
   generated Aiken outputs remain dedicated behavioral/generator projects even
   after concurrent editing stops.

The larger hardening plan still contains separate projects such as model
checking and historical reviewer evaluation. This pass completes the scoped
parallel work; it does not relabel that entire program complete.
