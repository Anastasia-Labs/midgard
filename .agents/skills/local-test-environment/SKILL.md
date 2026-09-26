---
name: local-test-environment
description: Get a Midgard checkout ready to run its test suites, and read what is wrong when it is not. Covers the shared test Postgres on 127.0.0.1:5433 (scripts/start-test-postgres.sh, synchronous_commit on), the per-worktree test-database prefix (MIDGARD_TEST_DATABASE_PREFIX), which suites need a built dist and why, the blueprint staleness guard (MIDGARD_BLUEPRINT_STAMP, deployment:build), the pinned Aiken and the hooks, and each linked worktree's own operator compose project and host ports. Use before running a Postgres-backed or blueprint-reading suite in a fresh checkout or worktree; when vitest reports "No test files found", ECONNREFUSED 127.0.0.1:5433, a "[blueprint-stamp]" refusal, a missing dist/ file, or rows a test did not write; when two checkouts run suites or the operator stack at once; or when `node scripts/doctor.mjs` fails.
---

# Local test environment

The main checkout and any number of `git worktree` checkouts share one test
Postgres, one Docker daemon and one set of host ports. Most local trouble is a
missing prerequisite or two checkouts touching the same resource.

## Start with `doctor`

`node scripts/doctor.mjs` (`--json` for machine-readable output) is
read-only: it starts, installs and writes nothing, and prints a fix under each
failure. It checks the pinned Aiken, the test Postgres, the test-database prefix, `demo/node_modules`, the
blueprint stamp, the `midgard-core`, `midgard-sdk` and `midgard-validation`
dist, the installed hooks, and the Node and pnpm versions.

| Exit | Meaning                                                                   |
| ---- | ------------------------------------------------------------------------- |
| 0    | Nothing failed. Warnings (another checkout's hook copy, Node ≠ CI's) pass |
| 1    | Something failed; run the `fix:` line under it                            |
| 3    | Nothing failed, but something could not be checked. Never a pass          |

Blind spot: only `midgard-core` stamps its dist with a source digest; the
other dist checks compare timestamps, so sources moved backwards in time read
as fresh. `scripts/doctor.test.mjs` covers the exit codes.

## Test Postgres on 5433

The `midgard-node` and `midgard-node-tools` suites (and `da-committee-node`)
need a server on `127.0.0.1:5433`; without one, global setup fails and vitest
reports "No test files found".

```bash
bash scripts/start-test-postgres.sh status   # is one listening?
bash scripts/start-test-postgres.sh start    # start one, or reuse a live one
```

`start` is a no-op when anything already listens on the port. Otherwise it
runs `initdb` once into `~/.midgard-pg/5433` and starts the server with
`synchronous_commit=on` (CI parity; `tests/database.test.ts` reads it back with
`SHOW synchronous_commit`), `fsync=off`, no unix socket and 200 connections.

- **Leave a server you did not start alone.** Other checkouts share it.
  `stop` only stops a server running from this script's own data directory.
  [script: scripts/start-test-postgres.sh]
- **Never point a suite at another Postgres through `POSTGRES_PORT`.** The live
  end-to-end stack keeps its own on 55433. `doctor` fails when an ambient
  `POSTGRES_PORT` differs from 5433 and refuses to probe 55433. Blind spot:
  vitest itself does not look. [runtime: probePostgres]
- In the main checkout the operator compose stack
  (`demo/midgard-node/docker-compose.yaml`) also publishes Postgres on 5433, so
  only one of the two runs there. A linked worktree's operator stack moves off
  5433 (below).

## One database family per checkout

Each suite creates sharded databases named `<prefix>_w<N>`, one per vitest
worker. The prefix comes from `scripts/lib/worktree-identity.mjs` and its
TypeScript twin `demo/midgard-node/tests/worktree-identity.ts`:

| Checkout        | `midgard-node` suites      | `midgard-node-tools` suites      |
| --------------- | -------------------------- | -------------------------------- |
| Main checkout   | `midgard_test`             | `midgard_tools_test`             |
| Linked worktree | `midgard_test_<path hash>` | `midgard_tools_test_<path hash>` |

The hash is the first 8 hex digits of the sha256 of the worktree's real path.
An explicit `MIDGARD_TEST_DATABASE_PREFIX` wins over both. `da-committee-node`
creates a randomly named database per test instead. See the identity with
`node scripts/lib/worktree-identity.mjs`; `doctor` prints the prefix.

- **Do not export one fixed `MIDGARD_TEST_DATABASE_PREFIX` in a shell that
  runs suites in two checkouts.** The explicit value wins in both, and the two
  runs drop each other's shards mid-test. [review]

## Suites that need a built dist

Vitest resolves the workspace packages to `src/` through the `midgard-source`
export condition (`midgardSourceSsr` in `demo/midgard-test-support/vitest.js`).
Plain `node` does not: it follows the `import` condition to `dist/`. So
anything a suite runs outside vitest needs dist built first.

| Package              | `pretest` builds                                | What runs outside vitest                                                                       |
| -------------------- | ----------------------------------------------- | ---------------------------------------------------------------------------------------------- |
| `midgard-node`       | `lucid-midgard`, `midgard-sdk`, the node itself | the bundled worker `dist/validation.js` (`tests/validation-worker-pool.test.ts`), crash probes |
| `midgard-node-tools` | `lucid-midgard`, `midgard-sdk`                  | the `node --test` files its `test` script runs before vitest                                   |

- **A focused `vitest run` skips `pretest`.** `pnpm test` runs it; after a
  source edit, run `pnpm --dir demo/midgard-node run pretest` (or the
  node-tools one) before a focused run, or the children run old code. Nothing
  compares dist with src at test time. [review]

## The blueprint stamp

The suites of `midgard-node`, `midgard-sdk`, `midgard-validation`,
`midgard-fault-proofs` and `midgard-watcher` read the untracked
`onchain/aiken/plutus.json`. Their global setup
(`demo/midgard-test-support/blueprint-stamp-setup.js`) refuses the run with a
`[blueprint-stamp]` error when the blueprint was built from other sources or
by another compiler; an absent one is left to the suites that read it. Rebuild
with `pnpm --dir demo deployment:build preprod-testing`.

- **`MIDGARD_BLUEPRINT_STAMP=warn` is for a deliberate run against a stale
  build only**; never report such a run as a result for the current tree.
  CI never sets it, and nothing stops a local run from setting it. [review]

## Parallel operator stacks

In a linked worktree, run the `demo/midgard-node` compose stack through
`demo/midgard-node/scripts/operator-compose.sh` (same arguments as
`docker compose`). It gives the worktree project `midgard-node-<path hash>`,
prefixed container names and its own block of host ports; in the main checkout
it adds nothing. `--print-env` shows the values.

- **Never run bare `docker compose` on the operator stack in a linked
  worktree.** It takes the main checkout's project name and ports and replaces
  or collides with that stack. Compose cannot tell the checkouts apart, so
  only the wrapper derives the values; its test
  (`demo/midgard-node/scripts/operator-compose.test.mjs`) proves the values,
  not that anyone used it. [review]

The phase 4 process devnet derives its own names and ports the same way; see
[running-the-devnet](../running-the-devnet/SKILL.md).
