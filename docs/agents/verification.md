# Verification

Commands run from the repository root unless stated otherwise. [review]

## Required checks

Run `node scripts/preflight.mjs` before pushing; gate a merge with
`--full-local --base <target-ref>` ([how](required-checks.md#gate-before-a-merge)).
Ordinary PRs assign the package build/typecheck/suite matrix to Node CI,
reported as pending apart from local results. Read generated
[required-checks.md](required-checks.md) for selectors, ownership and
capabilities. Unknown workflow routing retains local execution; the fast
pre-push slice stays mandatory. [script: scripts/preflight/run.mjs]

Focused local regressions and causal red/green checks remain mandatory. Prepare
fresh compiled/native dependencies and blueprints in this checkout; read
[focused tests and builds](contrib/tests-and-builds.md) before running them.
Report every result and capability refusal. A smoke test does not replace a
required check; path selection can miss behavior, so CI remains final. [review]

Deployment checks remain manual; preparation lanes are selected by preflight. [review]

| Change                                                                   | Checks                                                                                                                                                                              |
| ------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Validator parameters, deployment profiles                                | `pnpm --dir demo deployment:check preprod-testing`; `node --test demo/scripts/deployment-profiles.test.mjs`; the emulator scenarios in both polarities (`docs/agents/contracts.md`) |
| L1 transaction builders, wallet or input selection, validity, submission | `pnpm --dir demo run test:tx-prep:sdk`; `pnpm --dir demo run test:tx-prep:node`; `pnpm --dir demo run test:tx-prep:emulator`                                                        |

`docs/exec-plans/GOAL_SPEC.md` §13 governs the wider Goal completion checks.

## Merge checklist

1. Fetch the intended target, preserve foreign work, and run preflight against
   that target. A feature branch's own upstream is not its acceptance base.
   Review the actual proposed merge tree; a historical ancestor does not
   describe work still unmerged. [script: scripts/preflight/run.mjs]
2. Run locally scheduled checks and focused regressions once on frozen inputs.
   Node CI owns the ordinary PR runtime matrix, including core/test-support
   typechecks. The SDK preparation recipe is exactly its full Lucid/SDK suites
   and shares that owner; changed recipes retain local execution. Required jobs
   follow actual workflow paths: verify census, exact head and conclusions.
   Node CI gate requires every dependency to succeed. Conditional traced-refusal
   selectors and distinct acceptance profiles remain intact. [review]
3. `--strict --ci-run <Repo-Tools-run-id>` admits only required-checks generation
   and build-guard enrollment reuse, verifying merge tree/parents, inputs,
   current remote target, Node profile, commands and completed steps. Dirty,
   stale, absent or different evidence is refused. Record coverage as reused;
   scheduling supplies no pass. Failures retain gates. [script: scripts/preflight/ci-evidence.mjs]
4. Complete genuinely distinct gates: deployment parameters and both emulator
   polarities when changed; all three transaction-preparation lanes for L1
   builder changes; applicable installed/runtime, retained-data and live
   devnet/Preprod acceptance. Unproven suite overlaps leave these lanes intact.
   Preserve failures; distinguish confirmed environment refusals from source
   defects. An unexplained failure is not a waiver. [review]
5. Obtain required reviews and branch checks, then merge the authorized target
   using the reviewed head. Record PR/head/remote merge SHA. Reverify only when
   inputs, target, environment or uncovered behavior materially change; a target
   advance needs composition/overlap assessment, not an automatic whole-suite
   or review restart. [review]

## Local environment

- **Aiken.** Use the fork pinned by `AIKEN_FORK_VERSION` in
  `.github/workflows/aiken-ci.yml`, or point `MIDGARD_AIKEN_BIN` at it.
  `pnpm --dir demo deployment:build preprod-testing` builds
  `onchain/aiken/plutus.json` and refuses any other compiler.
- **Test Postgres, test databases, dist and the blueprint stamp.** Read the
  [local-test-environment](../../.agents/skills/local-test-environment/SKILL.md)
  skill before running a Postgres-backed or blueprint-reading suite in a fresh
  checkout or worktree, or when `node scripts/doctor.mjs` fails.
- **Devnet names and ports.** A linked worktree's phase 4 process devnet
  derives its compose project name and host ports from the worktree path (see
  `demo/midgard-node-tools/devnet/phase4-process/README.md`). So does the
  operator stack in `demo/midgard-node` when it is run through
  `demo/midgard-node/scripts/operator-compose.sh`; the main checkout keeps
  project `midgard-node` and today's ports, and bare `docker compose` in a
  linked worktree still takes them.
- **Hooks.** `bash .githooks/install` makes every checkout run its own copy of
  `.githooks`. With `core.fileMode=false`, git neither shows that a hook lost
  its executable bit nor runs it; re-running the installer restores the bit.
  `MIDGARD_SKIP_HOOKS=1` skips the hooks for one commit.
