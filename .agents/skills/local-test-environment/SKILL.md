---
name: local-test-environment
description: Get a Midgard checkout ready to run its test suites, and read what is wrong when it is not. Covers the shared test Postgres on 127.0.0.1:5433 (scripts/start-test-postgres.sh, synchronous_commit on), the per-worktree test-database prefix (MIDGARD_TEST_DATABASE_PREFIX), which suites need a built dist and why, the blueprint staleness guard (MIDGARD_BLUEPRINT_STAMP, deployment:build), the pinned Aiken and the hooks, and each linked worktree's own operator compose project and host ports. Use before running a Postgres-backed or blueprint-reading suite in a fresh checkout or worktree; when vitest reports "No test files found", ECONNREFUSED 127.0.0.1:5433, a "[blueprint-stamp]" refusal, a missing dist/ file, or rows a test did not write; when two checkouts run suites or the operator stack at once; before running one file of a package, a gate over several suites or anything longer than one Bash call; when reusing another worktree's blueprint; or when `node scripts/doctor.mjs` fails.
---

# Local test environment

The main checkout and any number of `git worktree` checkouts share one test
Postgres, one Docker daemon and one set of host ports. Most local trouble is a
missing prerequisite or two checkouts touching the same resource.

## Start with `doctor`

Run suites with `node scripts/contrib.mjs test --package <name> [--file <path> | --related <changed>]`
([how](references/running-suites.md#one-file-the-files-a-change-reaches-or-the-whole-package)): it runs
the package's `test` script as CI does, after preparing the blueprint and
dists, under its own database family, and counts what ran.

`node scripts/doctor.mjs` (`--json` for machine-readable output) is
read-only: it starts, installs and writes nothing, and prints a fix under each
failure. It checks the pinned Aiken, the test Postgres, the test-database prefix, `demo/node_modules`, the
blueprint stamp, the `midgard-core`, `midgard-sdk` and `midgard-validation`
dist, the installed hooks, and the Node and pnpm versions.

Exit 0 passes, 1 names a failure and its fix, and 3 means unknown. [script: scripts/doctor.mjs]

Dist verdicts use the guarded build's input and emitted-content digests;
sources moved backwards in time still invalidate them. Raw builds without a
stamp are unavailable. `scripts/doctor.test.mjs` covers the exit codes.

## Test Postgres on 5433

The `midgard-node`, `midgard-node-tools`, `da-committee-node`,
`midgard-l1-follower` and `midgard-watcher` suites need a server on `127.0.0.1:5433`; without one, global setup fails and vitest
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

Raw suites shard databases as `<prefix>_w<N>`. The prefix comes from
`scripts/lib/worktree-identity.mjs` (TypeScript twin
`demo/midgard-node/tests/worktree-identity.ts`): `midgard_test` and
`midgard_tools_test` in the main checkout, `<family>_<path hash>` in a linked
worktree (first 8 hex digits of the sha256 of its real path). `contrib test`
uses `midgard_contrib_<path hash>_<random>` per run and drops it afterwards.
An explicit `MIDGARD_TEST_DATABASE_PREFIX` wins over both.

- **Create and remove lane worktrees with `contrib worktree create|remove`.**
  `remove` refuses unsaved work and drops the worktree's own databases;
  see [lane worktrees](../../../docs/agents/contrib/worktrees.md).
  [script: scripts/contrib/worktree.mjs]
- **Do not export one fixed `MIDGARD_TEST_DATABASE_PREFIX` in a shell that
  runs suites in two checkouts.** The explicit value wins in both, and the two
  runs drop each other's shards mid-test. [review]

## Suites that need a built dist

Vitest resolves the workspace packages to `src/` through the `midgard-source`
export condition, and a single run bundles that source ([the workspace
bundle](references/running-suites.md#the-workspace-bundle)). Plain `node`
follows the `import` condition to `dist/`, so code run outside vitest needs it.

`contrib prepare` expands declared pretest builds and their runtime dependency
closure in order. Node suites need their bundled workers; tooling also needs
plain-Node imports. `contrib test` owns these artifacts until the run joins.

- **A raw `vitest run` skips `pretest`.** `contrib test` rebuilds stale
  compiled consumers before execution. [script: scripts/contrib.mjs]
- **Build dist in each checkout; never copy it from another.** Guarded stamps
  authenticate checkout, source closure and emitted dependencies. [script: scripts/contrib/build.mjs]

## The blueprint stamp

Suites that read the untracked `onchain/aiken/plutus.json` run
`demo/midgard-test-support/blueprint-stamp-setup.js`, which refuses with a
`[blueprint-stamp]` error a blueprint built from other sources or by another
compiler, or one `MIDGARD_REAL_BLUEPRINT_PATH` names without a fresh build
record. An absent default one is left to the suites that read it.

- **`contrib prepare` and `contrib test` make the blueprint ready; nobody
  chooses between copying and building.** A stale or other-profile blueprint
  is copied from a checkout whose build record matches this tree
  (`scripts/sync-blueprint-from.mjs` re-verifies the copy), else built with
  `deployment:build`. [script: scripts/contrib/blueprint.mjs]
- **`MIDGARD_BLUEPRINT_STAMP=warn` is for a deliberate run against a stale
  build only**; never report such a run as a result for the current tree.
  CI never sets it, and nothing stops a local run from setting it. [review]

## One file, and gate runs

Read [references/running-suites.md](references/running-suites.md) before
passing a file to a package `test` script, before a gate run over several
suites, and before a run longer than one tool call.

- **Put test files before every flag.** vitest's kebab-case booleans such as
  `--disable-console-intercept` swallow the next argument and drop the file
  filter; package scripts use camelCase. [review]

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
