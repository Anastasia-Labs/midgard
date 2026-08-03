# Canonical V1 Goal verification

This document describes the repository commands required by `GOAL_SPEC.md`
§13. The executable command graph is
`docs/exec-plans/evidence/canonical-v1-goal-verification-plan-v1.json`; prose
does not override that plan, the closure manifest, or `GOAL_SPEC.md`.

Run commands from the repository root. The declared Node/pnpm environment is
the non-login Nix shell for `demo`; Aiken must be the exact compiler declared
by `onchain/aiken/aiken.toml`.

## Local commands

- `pnpm --dir demo run goal:verify:static` checks the protected dirty baseline,
  active workspace command policy, forbidden legacy and whole-item bindings,
  registry and current-truth artifacts, generated documentation, Git diff,
  compiler and blueprint identity, Aiken format/check/build, and serialized
  workspace format/lint/typecheck/build.
- `pnpm --dir demo run goal:verify:capability` runs the local CG1–CG5 evidence
  producers and consumers, including retained-DA/data-breadth suites and
  separately guarded exact Aiken boundary selectors.
- `pnpm --dir demo run goal:verify:fault-proofs` runs QG1/QG2 reconciliation,
  proof tooling, SDK, and node correction suites.
- `pnpm --dir demo run goal:verify:watcher` runs the watcher dependency,
  build/type/lint/format, and WG1 test surfaces.
- `pnpm --dir demo run goal:verify:local` runs the preceding four commands
  serially and stops at the first nonzero result. It also runs the §13.2
  specification build: `local-conditional-make-spec` executes `make spec` from
  the repository root when `technical-spec/` changed against the diff base
  (`MIDGARD_GOAL_SPEC_DIFF_BASE`, else `origin/main`, else `main`, else the
  worktree alone) and records an explicit `SKIPPED` line with its reason when it
  did not. It never claims a skip it cannot justify: an uninspectable diff is a
  failure, not a pass.

Each phase ends by requiring its exact closure-manifest acceptance-criterion
group to be `PASS` with bound file evidence. A passing inventory verifier does
not turn an open criterion into a pass.

## Scheduling helper (non-gating)

`pnpm --dir demo run goal:tasks:ready` joins the F05 task manifest with the
first `## Task queue` table in `GOAL_PROGRESS.md` and prints the tasks whose
declared dependencies are all `PASS`, with their owned paths and focused
commands. Rows still at `detailStatus: PENDING_ASSIGNMENT` are printed as
ready-to-detail, not ready-to-run: §5.1/F05 requires writable paths, source
anchors and focused commands before assignment. `--json` emits the same content
for tooling and `--limit=<n>` truncates the list.

This helper is scheduling tooling only. It is not a phase, it gates nothing,
and no acceptance claim may cite its output (§13.1).

## Resource bounds and recorded results

- Every plan command runs under a wall-clock bound:
  `execution.resourceBounds.defaultCommandTimeoutMs` (one hour), a per-command
  `timeoutMs` where a measured command needs more (the full `aiken check`
  declares twelve hours), or `MIDGARD_GOAL_COMMAND_TIMEOUT_MS` for one run. The
  bound exists to kill hangs, not to truncate known-slow gates; a timeout is
  reported as `timedOut` and fails the phase.
- `MIDGARD_GOAL_RECORD_COMMAND_RESULTS=YES` makes the runner record each
  executed command into the closure manifest's `commandResults` (§13.2:
  command, exit code, duration, revision, artifact identity, finish time),
  upserting by command id. Failures are recorded too — release mode then
  correctly refuses the manifest until a green re-run replaces the entry.
  Recording is opt-in precisely because it rewrites a committed evidence file;
  a plain verification run never mutates the manifest. `testCount` is `null`
  for runner-recorded entries: this runner streams child output rather than
  capturing it, and the exact collected counts are asserted by the commands
  themselves, which exit nonzero on a wrong count.

## Continuous verification (F40)

`.github/workflows/evidence-integrity-ci.yml` runs `goal:verify:static`
verbatim — no added commands, no removed ones — in a job scoped to the branch
the closure manifest binds, so CI and a local run cannot disagree. The
format-registry verifier stays a separate direct CI step in its
incomplete-tolerant form: the plan forbids `--allow-incomplete` in any plan
command, and the registry's strict run is the `evidence` phase gate for
release.

## State-changing Preprod acceptance

The published state-changing Goal route is retired while C79 remains OPEN.
The verification plan therefore contains no state-changing phase and the demo
package publishes no `goal:accept:testnet` command. This is deliberate: the
required C80–C87, Q57/QG3, and W45–W46/WG2 primitives are not complete, so a
partial live command would violate the acceptance invariant and could imply
evidence that does not exist. Reintroduce the route only when C79's complete
orchestrator, runbook rehearsal, and prerequisite gates are implemented.

## Evidence and release commands

- `pnpm --dir demo run goal:verify:evidence` verifies the canonical closure
  manifest without submitting transactions. It requires final revision and
  release identity, both C70 parameter snapshots, blueprint/validator/
  deployment/fixture bindings, successful command results, all 35 exact `AC-*`
  entries at `PASS`, owner-accepted and evidenced §9.5 residual launch blockers
  (an empty list is the healthy case), a clean secret scan, baseline-relative
  cleanliness, and a reproduced SHA-256 release digest.

### Manifest fields the release gate binds

- `parameterSnapshot` is the C70 trusted **mainnet** effective/pending snapshot
  that fixes the capability floor; `targetTestnetParameterSnapshot` is the C70
  **target-testnet** effective/pending snapshot the deployment is validated
  against. §13.3 and §3.1.10 need both — capability parity derives from the
  least restrictive applicable value across the pair — so the schema requires
  two distinct paths and release requires both `BOUND`. Both are `OPEN` today
  and an `OPEN` binding is not file-verified, which is what lets the manifest
  name the snapshots C70 will produce before they exist.
- `residualBlockers` carries the §9.5 named residual launch blockers. An entry
  with `ownerAccepted: true` must bind at least one existing evidence file
  recording that acceptance, and the same blocker must appear in root
  `public_testnet_readiness.md`. Silence is not an outcome; neither is
  unevidenced acceptance.
- `commandResults[].transactionHashes` is optional and records the exact
  32-byte transaction identities a state-changing acceptance command submitted
  (§13.3, F41). Omit the key rather than recording an empty list.
- `pnpm --dir demo run goal:verify:all` runs `goal:verify:local` and then
  `goal:verify:evidence`. Missing, stale, mismatched, incomplete, or
  wrong-revision Preprod evidence is a failure.

The in-progress closure manifest is intentionally valid as a schema and
current-tree inventory while release mode remains nonzero. It must never be
marked `BOUND` or populated with `PASS` criteria until final-tree evidence
exists.

### When a criterion may be recorded `PASS`

"Final-tree evidence exists" is the whole test, and it is three concrete
conditions. All must hold:

1. the `GOAL_PROGRESS.md` criterion ledger (the first `## Criterion ledger`
   table) records that `AC-*` as `PASS` with its final-tree evidence;
2. the manifest binds at least one evidence file that a reviewer can re-run or
   read at the current tree, and every bound path exists; and
3. that evidence is machine-re-executable and was actually executed at a
   recorded revision — the run appears in `commandResults` with exit code 0.

`AC-W12` is recorded `PASS` on that basis. The ledger row credits W10–W13
final-tree evidence after an independent adversarial re-audit; the manifest
binds `demo/scripts/verify-canonical-v1-watcher-focused-tests.mjs` (which
asserts the exact per-file collected counts declared in the watcher dependency
map, and fails on any deviation), the dependency map itself, and the ledger row;
and the `watcher-focused-tests` command result records that verifier passing
14 files / 356 of 356 tests at revision `ff4a7fc3`. The same verifier runs on
every pull-request commit in `evidence-integrity-ci.yml`, so the evidence is
continuously re-checked rather than asserted once.

A criterion whose ledger row is `PASS` but whose evidence has not been executed
at the final tree stays `OPEN` here. A green inventory verifier, a hash record,
or a prose claim is not condition 3.
