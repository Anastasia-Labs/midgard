# Running suites and gate runs

Read this before running one test file of a package whose `test` script
carries flags, before a gate run over several suites, and before starting a
run that takes longer than one tool call allows.

## One file, not the whole suite

Measured 2026-09-28 with vitest 3.0.7 by counting `vitest list` output in
`demo/midgard-core` (4 tests in the file, 682 in the package) and
`vitest list --filesOnly` in `demo/midgard-node` (1 file against 270); the
kebab-case trap below still held on vitest 5.0.3 (2026-10-06). Since Vitest 5,
`vitest list` parses test files statically and misses generated tests; pass
`--no-staticParse` to collect them by running each file.

**A multi-word boolean flag written in kebab case swallows the next
argument.** vitest registers its options with camelCase names and hands those
names to its argument parser as the boolean list, so
`--disable-console-intercept` is not recognised as boolean and takes the next
token as its value. When that token is the file, the filter is gone and the
whole package runs.

| Command                                              | Runs       |
| ---------------------------------------------------- | ---------- |
| `vitest run --disable-console-intercept <file>`      | every file |
| `vitest run --disable-console-intercept -- <file>`   | every file |
| `vitest run <file> --disable-console-intercept`      | the file   |
| `vitest run --disable-console-intercept=true <file>` | the file   |
| `vitest run --disableConsoleIntercept <file>`        | the file   |

The same holds for every multi-word boolean (`--pass-with-no-tests`,
`--allow-only`, `--hide-skipped-tests`, `--log-heap-usage`,
`--expand-snapshot-diff` all listed the whole package). One-word booleans
(`--silent`, `--run`, `--update`), camelCase spellings
(`--passWithNoTests`), `--no-` negations (`--no-file-parallelism`) and
`--flag=value` forms are safe. An option that takes a value always takes the
next token: `--bail <file>` swallows the file, `--bail 1 <file>` does not.
**Put the files first**, right after `vitest run`, and every flag after them.

The package scripts use the camelCase spelling, so
`pnpm --dir demo/midgard-node test <file>` runs that one file (after
`pretest`, which rebuilds the dists). `pnpm` appends extra arguments to the end
of the script, so a script that ended in a kebab-case boolean would run every
file instead; `scripts/ci/vitest-script-flags.test.mjs` fails CI on one
[ci: Repo Tools CI/Run the repository tool tests].
`pnpm --dir demo/midgard-node-tools test <file>` runs its `node --test`
preludes first.

A `-t "<name>"` that matches nothing is a different trap: it exits 0 with
every test skipped. See [writing-tests](../../writing-tests/SKILL.md#5-keep-it-able-to-fail).

## Gate runs over several suites

- **One process per suite, in parallel.** Start the node, watcher,
  fault-proofs and Aiken runs as separate processes, not one after another in
  one shell. Each checkout already has its own database prefix; two runs of the
  same package that could overlap (a full run and a focused rerun, or two
  agents in one checkout) each get their own
  `MIDGARD_TEST_DATABASE_PREFIX=<prefix>_<suite>` on that command only.
- **Keep each suite's report and log.** Delete the old report first
  (`rm -f <dir>/<suite>.json`): vitest writes it only at the end, so a killed
  run leaves the previous one. Then run
  `<env> pnpm --dir demo/<package> exec vitest run <files> <flags> --reporter=default --reporter=json --outputFile=<dir>/<suite>.json 2>&1 | tee <dir>/<suite>.log`,
  so the failures can be diffed against the program's accepted list instead of
  re-triaged by hand. Pass the log too: the JSON report leaves out errors
  raised outside any test. The diff tool is described in
  [fixing-flaky-tests](../../fixing-flaky-tests/SKILL.md#known-failures-in-a-gate-run).
  `<env>` and `<flags>` are what the package's `test` script sets, which is
  how CI runs it; `exec vitest` does not apply them:
  - `midgard-watcher`: `env MALLOC_MMAP_THRESHOLD_=131072`, no flags.
  - `midgard-node`: `NODE_ENV=emulator`, flag `--disableConsoleIntercept`.
  - `midgard-node-tools`: the same as `midgard-node`, after its `node --test`
    preludes, run as
    `pnpm --dir demo/midgard-node-tools run test:phase4:journal-kill-recovery-summary-verifier`
    and `pnpm --dir demo/midgard-node-tools run test:phase4:devnet-assets`.
  - Every other package: nothing (its `test` script is a plain `vitest run`).
- **A fix loop reruns only the suites that failed**, and within a suite only
  the files that failed, before one final full pass.
- **Run in the foreground, in chunks under the tool's time limit.** A job an
  agent leaves in the background dies when the agent returns, and a bare
  `sleep N` is refused. Start a long run with `nohup <command> > <log> 2>&1 &`
  and poll it in bounded chunks:

  ```sh
  timeout 540 bash -c 'until grep -q "<end marker>" <log>; do sleep 15; done'
  ```

  Repeat the poll until the marker appears; each call stays under ten minutes.

- **An agent never returns while a job it started is still running.** Wait for
  it, or stop it by its PID and say so. Never stop a process you did not start.

## Blueprint and dist in another checkout

Each checkout has its own `onchain/aiken/plutus.json` and its own `dist/`
directories; neither is shared through git. A fresh worktree has neither, and
switching branches leaves both describing the old sources.

- The blueprint can be copied from another checkout whose stamped inputs are
  identical and whose blueprint was built for the profile this checkout
  selects (`SELECTED_DEPLOYMENT_PROFILE`, with the same digest):
  `node scripts/sync-blueprint-from.mjs <checkout> [--dry-run]`. It refuses a
  stale source, names the first differing input or both profiles, and replaces
  nothing here unless the copied pair is fresh.
- A `dist/` is not copied. Nothing checks a copied dist against this
  checkout's sources, and only `midgard-core`'s dist carries a source digest.
  Rebuild it here:

  | Needed for                                  | Command                                               |
  | ------------------------------------------- | ----------------------------------------------------- |
  | `midgard-node` suites (runs outside vitest) | `pnpm --dir demo/midgard-node run pretest`            |
  | `midgard-node-tools` suites                 | `pnpm --dir demo/midgard-node-tools run pretest`      |
  | what `doctor` reports as a missing dist     | `pnpm --dir demo --filter <package name> run build`   |
  | every package                               | `pnpm --dir demo build` (checks profiles, builds all) |

## The workspace bundle

A single run (`vitest run`, CI, non-interactive stdin) loads all workspace
code outside the package's own `tests/`, its own `src/` included, from a
per-run esbuild bundle of that same source (`workspaceBundleProjects` in
`demo/midgard-test-support/vitest.js`); `tests/` loads module by module. The
bundle's key covers every byte it could read, so an edited source file means a
new bundle. Watch mode never bundles.

- Files that mock code outside `tests/`, spy on its namespace or reset modules
  run in the `<name>:source` project. A computed mock or import the routing
  misses fails by name instead of passing. [runtime: workspace-bundle-guard.js]
- `MIDGARD_TEST_WORKSPACE_BUNDLE=0` forces plain source mode, `=1` forces the
  bundle; `MIDGARD_TEST_WORKSPACE_BUNDLE_REPORT=1` prints the routing.
