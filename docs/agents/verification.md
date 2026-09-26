# Verification

The per-change checks that `AGENTS.md` refers to: what to run for each kind of
change before finishing, and the local environment they need. Commands run from the
repository root unless a row says otherwise.

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

These checks are not in the preflight registry yet; run them by hand when you
change their paths. [review]

| Change                                                                   | Checks                                                                                                                                                                              |
| ------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Validator parameters, deployment profiles                                | `pnpm --dir demo deployment:check preprod-testing`; `node --test demo/scripts/deployment-profiles.test.mjs`; the emulator scenarios in both polarities (`docs/agents/contracts.md`) |
| L1 transaction builders, wallet or input selection, validity, submission | `pnpm --dir demo run test:tx-prep:sdk`; `pnpm --dir demo run test:tx-prep:node`; `pnpm --dir demo run test:tx-prep:emulator`                                                        |
| Phase 4 devnet generator                                                 | `node --test demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs`                                                                                                   |
| `technical-spec/**`                                                      | `make spec` (needs Nix)                                                                                                                                                             |
| `docs-site/**`                                                           | `pnpm --dir docs-site run check:links`; `pnpm --dir docs-site run build`; `pnpm --dir docs-site run types:check`                                                                    |

`docs/exec-plans/GOAL_SPEC.md` §13 lists the full verification for Goal
completion, which is wider than this table.

## Local environment

- **Aiken.** Use the fork pinned by `AIKEN_FORK_VERSION` in
  `.github/workflows/aiken-ci.yml`, or point `MIDGARD_AIKEN_BIN` at it.
  `pnpm --dir demo deployment:build preprod-testing` builds
  `onchain/aiken/plutus.json` and refuses any other compiler.
- **Test Postgres.** The node and node-tools suites need a server on
  127.0.0.1:5433; `scripts/start-test-postgres.sh` starts one, or reuses one
  that is already listening without restarting it.
- **Test databases per worktree.** Each suite creates sharded databases named
  `<prefix>_w<N>`. The main checkout keeps the prefixes `midgard_test` and
  `midgard_tools_test`; a linked worktree appends a hash of its path, so
  concurrent checkouts get separate databases. An explicit
  `MIDGARD_TEST_DATABASE_PREFIX` takes precedence.
  `node scripts/lib/worktree-identity.mjs` prints the checkout's identity.
- **Devnet names and ports.** The phase 4 process devnet derives its compose
  project name and host ports the same way (see
  `demo/midgard-node-tools/devnet/phase4-process/README.md`). The operator
  compose files in `demo/midgard-node` fix their container names and host
  ports instead, so only one of those stacks runs per machine.
- **Hooks.** `bash .githooks/install` makes every checkout run its own copy of
  `.githooks`. With `core.fileMode=false`, git neither shows that a hook lost
  its executable bit nor runs it; re-running the installer restores the bit.
  `MIDGARD_SKIP_HOOKS=1` skips the hooks for one commit.
