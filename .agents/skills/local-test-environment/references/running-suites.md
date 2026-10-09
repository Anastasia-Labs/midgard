# Running suites and gate runs

Read this before running one test file or a whole package, before a gate run
over several suites, and before starting a run that takes longer than one tool
call allows.

## One file, the files a change reaches, or the whole package

```sh
node scripts/contrib.mjs test --package midgard-node --file tests/<file>.test.ts [--name "<regex>"]
node scripts/contrib.mjs test --package midgard-node --related <changed path>...
node scripts/contrib.mjs test --package midgard-watcher [--maxWorkers 2] [--exclude "tests/slow/**"]
```

After a change, run `--related` with the changed files for each package they
can reach, not a whole package and not a hand-picked file list: it graphs
imports from source across packages, plus fixtures, worker entries and
directories named in string literals, and widens to the whole package whenever
it cannot decide. `no test reached` means no import or name can reach that
package's tests. [script: scripts/contrib/reached.mjs]

`contrib test` runs the package's own `test` script, the command CI runs: its
environment, its Vitest flags and, for a whole-package run, its `node --test`
preludes. It adds only `--maxWorkers`, `--exclude` and
`--disableConsoleIntercept`, under Vitest's spelling, and asks `vitest list`
first which files the run collects: a named file that the config or an
`--exclude` drops, or a path that also selects other files, is refused before
anything runs. A `--name` that matches nothing fails the receipt (0 tests
executed). Failures print as `FAIL <file> > <test>` with their first message
line, then the log and receipt paths. The workspace lease covers only the
builds before the suites, so two runs in one checkout proceed side by side,
each with its own database family. [script: scripts/contrib/vitest-command.mjs]

**Raw `vitest run` has a flag trap that `contrib test` avoids.** A
multi-word boolean written in kebab case (`--disable-console-intercept`,
`--pass-with-no-tests`) is not registered as boolean and swallows the next
token: `vitest run --disable-console-intercept <file>` runs every file.
`contrib test` passes only camelCase and `--flag=value` forms, and
`scripts/ci/vitest-script-flags.test.mjs` keeps every package `test` script
free of the trap [ci: Repo Tools CI/Run the repository tool tests].

## Gate runs over several suites

- **One process per suite, in parallel.** Start the node, watcher,
  fault-proofs and Aiken runs as separate processes, not one after another in
  one shell. Every `contrib test` invocation has its own database prefix.
- **Keep each suite's receipt.** `--output <dir>/<suite>.json` writes it; its
  `report.path` is Vitest's JSON report and its `steps[].logPath` the logs, the
  inputs of the [accepted-failures diff](../../fixing-flaky-tests/SKILL.md#known-failures-in-a-gate-run).
  Both live in a fresh run directory, so a killed run never leaves an older
  report in their place.
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

- `contrib prepare` copies the blueprint from another checkout whose stamped
  inputs are identical and whose blueprint was built for the profile this
  checkout selects, through `node scripts/sync-blueprint-from.mjs <checkout>`
  (which re-verifies the copy and replaces nothing unless it is fresh), and
  builds it only when no checkout has one.
- A `dist/` is not copied. Nothing checks a copied dist against this
  checkout's sources. `contrib test` rebuilds every stale prerequisite dist of
  the package before its suites start;
  `node scripts/contrib.mjs prepare --package <name> --execute` does the same
  without running them, and `node scripts/contrib.mjs worktree setup` does it
  for every tested package.

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
