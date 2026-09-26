# Verification

The per-change checks that `AGENTS.md` refers to: what to run for each kind of
change before finishing, and the local environment they need. Commands run from the
repository root unless a row says otherwise.

## Required checks

Run every row whose paths you changed, plus the narrow tests that prove the
behavior you touched, and report each command with its result. A smoke test
does not replace a required check. Blind spot: rows are selected by path, and
nothing verifies that the rows were run; CI is the final gate. [review]

This table is maintained by hand until the preflight registry generates it.

<!-- required-checks:begin -->

| Change                                                                                 | Required checks                                                                                                                                                                                                                      |
| -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Anything                                                                               | `git diff --check`                                                                                                                                                                                                                   |
| Aiken sources, `onchain/aiken/**/*.ak`                                                 | `node onchain/aiken/scripts/pinned-compiler.mjs`; from `onchain/aiken`: `aiken check` (or `node scripts/run-focused-check.mjs <module> <test> ...` while iterating); the pre-commit hook formats staged `.ak` files                  |
| A module pinned by an execution ledger                                                 | from `onchain/aiken`: the matching `node scripts/verify-<name>-exec-ledger-v1.mjs`                                                                                                                                                   |
| Validator parameters, deployment profiles                                              | `pnpm --dir demo deployment:check preprod-testing`; `node --test demo/scripts/deployment-profiles.test.mjs`; the emulator scenarios in both polarities (`docs/agents/contracts.md`)                                                  |
| TypeScript or Markdown in `demo/**`                                                    | `pnpm --dir demo run lint`; `pnpm --dir demo run format-check`; `pnpm --dir demo/<package> run typecheck`; `pnpm --dir demo/<package> test`                                                                                          |
| `demo/midgard-node`, `demo/midgard-node-tools`                                         | `scripts/start-test-postgres.sh` first; then `pnpm --dir demo/midgard-node test` or `pnpm --dir demo/midgard-node-tools test`                                                                                                        |
| L1 transaction builders, wallet or input selection, validity, submission               | `pnpm --dir demo run test:tx-prep:sdk`; `pnpm --dir demo run test:tx-prep:node`; `pnpm --dir demo run test:tx-prep:emulator`                                                                                                         |
| Golden-vector generators and their fixtures                                            | the owning package's `fixtures:<name>:check` script, for example `pnpm --dir demo/midgard-core run fixtures:native-tx-vector-v1:check`                                                                                               |
| Consensus profile or its documentation                                                 | `pnpm --dir demo/midgard-core run docs:consensus-profile-v1:check`                                                                                                                                                                   |
| `demo/midgard-watcher`                                                                 | `pnpm --dir demo/midgard-watcher run build`; `pnpm --dir demo/midgard-watcher run typecheck`; `pnpm --dir demo/midgard-watcher run lint`; `pnpm --dir demo/midgard-watcher run format-check`; `pnpm --dir demo/midgard-watcher test` |
| Phase 4 devnet generator                                                               | `node --test demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs`                                                                                                                                                    |
| Agent instructions: `AGENTS.md` files, `CLAUDE.md`, `docs/agents`, `.agents/skills`    | `node scripts/agents/check-enforcement-tags.mjs`; `node scripts/agents/check-doc-links.mjs`; `node scripts/agents/check-agent-config.mjs`                                                                                            |
| The e2e acceptance skill                                                               | `node .agents/skills/midgard-e2e-acceptance/scripts/validate-runbook.mjs`                                                                                                                                                            |
| Repository scripts and hooks: `scripts/**`, `.githooks/**`, `onchain/aiken/scripts/**` | `node --test "scripts/**/*.test.mjs" "onchain/aiken/scripts/*.test.mjs"`                                                                                                                                                             |
| `technical-spec/**`                                                                    | `make spec` (needs Nix)                                                                                                                                                                                                              |
| `docs-site/**`                                                                         | `pnpm --dir docs-site run check:links`; `pnpm --dir docs-site run build`; `pnpm --dir docs-site run types:check`                                                                                                                     |

<!-- required-checks:end -->

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
