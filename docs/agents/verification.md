# Verification

Required checks by change type and their local environment. Commands run from
the repository root unless a row says otherwise. [review]

## Required checks

Run `node scripts/preflight.mjs` before pushing. It selects the checks your
change needs from the registry in `scripts/preflight/registry.mjs`, runs them,
and exits nonzero when one fails or could not run; the pre-push hook runs its
fast slice. The generated
[required-checks.md](required-checks.md) lists every check, what selects it,
and the capability it needs; `node scripts/doctor.mjs` names a missing
capability's fix. Add the narrow tests that prove the behavior you touched, and
report each command with its result. A smoke test does not replace a required
check. Blind spot: selection is by path, so a check whose trigger is too narrow
is silently skipped; CI is the final gate. [hook: pre-push]

Deployment parameter checks below still need a manual invocation. Transaction
preparation lanes are selected by preflight and listed here for direct use. [review]

| Change                                                                   | Checks                                                                                                                                                                              |
| ------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Validator parameters, deployment profiles                                | `pnpm --dir demo deployment:check preprod-testing`; `node --test demo/scripts/deployment-profiles.test.mjs`; the emulator scenarios in both polarities (`docs/agents/contracts.md`) |
| L1 transaction builders, wallet or input selection, validity, submission | `pnpm --dir demo run test:tx-prep:sdk`; `pnpm --dir demo run test:tx-prep:node`; `pnpm --dir demo run test:tx-prep:emulator`                                                        |

`docs/exec-plans/GOAL_SPEC.md` §13 lists the full verification for Goal
completion, which is wider than this table.

## Merge checklist

1. Fetch the intended target, preserve foreign work, and run preflight against
   that target. A feature branch's own upstream is not its acceptance base.
   Review the actual proposed merge tree; a historical ancestor does not
   describe work still unmerged. [script: scripts/preflight/run.mjs]
2. Run the selected local checks and the narrow regressions proving changed
   behavior once on frozen inputs. Preflight-only modules select tooling tests;
   shared probes, derivation and execution helpers retain full scope. Required
   hosted jobs follow the actual workflow path filters; check their census,
   exact head and conclusions, not just a green summary. [review]
3. Reuse completed CI only through `--strict --ci-run <Repo-Tools-run-id>` for
   the two admitted file-only validators: required-checks generation and build
   guard enrollment. It verifies merge tree/parents, source/discovered inputs,
   current remote target, Node profile, commands and completed steps; dirty,
   stale, absent or different evidence is refused. Record run and coverage as
   reused. Runtime failures, database/native suites, transaction preparation and
   live acceptance retain their gates. [script: scripts/preflight/ci-evidence.mjs]
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
