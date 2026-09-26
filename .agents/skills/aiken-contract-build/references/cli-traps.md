# Aiken CLI and language traps

Read this when a focused check reports a surprising count, when an `aiken`
run printed nothing, or before writing an `and {}` or a large fold.

Unless stated otherwise, behaviour here was probed on 2026-09-26 with
`aiken v1.1.23+5adf783` (the pin) in a throwaway project, and the selector
rules were read from the fork's source,
`crates/aiken-project/src/lib.rs` `collect_test_items` (same code at fork
revisions `133151f`, `2a78108` and `5adf783`). <!-- doc-links:external -->

## How `aiken check -m` reads a selector

1. If the selector contains neither `.` nor `/`, the **whole selector is a
   test-name filter** across every module. `-m name_v1` looks for tests whose
   name contains `name_v1`; there usually are none.
2. Otherwise the text before the **first** `.` is a module filter, matched as
   a substring of the module name (`contains`, not equality, not prefix). The
   text after the first `.` has `{` and `}` stripped and is split on `,` into
   test-name filters; an empty list means every test.
3. Test names match as substrings, or exactly with `-e`.

What that means in practice:

| Selector                             | Collects                                                                                                                          |
| ------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------- |
| `-m name_v1`                         | tests whose **name** contains `name_v1`: usually zero, exit 0                                                                     |
| `-m midgard/foo_v1`                  | every test in every module whose name contains `midgard/foo_v1`, including `midgard/foo_v1.test` and `midgard/foo_v1_golden.test` |
| `-m 'midgard/foo_v1.{..}'`           | the same set as the line above                                                                                                    |
| `-m 'midgard/foo.max.test'` (dotted) | module filter `midgard/foo`, test filter `max`: usually zero, exit 0                                                              |
| `-m 'midgard/foo.{exact}' -e`        | test `exact` in every module whose name contains `midgard/foo`                                                                    |

A run that collects zero tests exits 0. The fork's
`Suspicious test filter (-m) yielding no test scenarios` warning fires only
when the selector carries exactly one test-name filter and nothing matched,
and it did not appear at all when output was piped.

This is why the two repository scripts exist:

- `onchain/aiken/scripts/run-focused-check.mjs <module> <test> [<test> …]`
  resolves the module from `lib/` or `validators/`, builds one exact selector
  per name, and exits 1 unless the report contains only that module and
  exactly as many passing tests as names given (exit 2 on bad usage or an
  unknown module).
- `onchain/aiken/scripts/guard-focused-selector.mjs <module-selector> …`
  fails on a zero collected total, a nonzero `aiken` status, an unparseable
  report or a failing test. It does not pin the count, so an over-broad
  substring match passes it.

## Output that hides failures

- **Compile errors vanish when stdout is not a terminal.** `aiken check` and
  `aiken build` with a type error, output redirected to files: exit 1, stderr
  holds only the `Compiling …` line, no diagnostic. Under a pseudo-terminal
  (`script -qec "aiken check" /dev/null`) the full diagnostic prints. Read the
  exit code, and rerun under a pseudo-terminal to see why.
- **The JSON report appears only when stdout is not a terminal**, with or
  without `-e`. Under a pseudo-terminal you get the human table instead. So a
  wrapper that parses the report (both repository scripts do) must not run
  `aiken` under `script`. On a failing test the piped run still prints the
  JSON report and exits 1.
- An older pin (v1.1.22, commit `f9310109b`) showed a silent-exit pathology
  on detached runs; see the writing-tests catalogue,
  [Green because nothing ran](../../writing-tests/references/mistakes-we-make.md#green-because-nothing-ran).
  Not re-probed on the current fork beyond the two points above.

## Processes and build directories

- `pkill -f aiken` signals every process whose command line contains
  `aiken`: other sessions' builds and checks, and any wrapper such as
  `node scripts/run-focused-check.mjs` running from `onchain/aiken`. Stop only
  a process you started, by its PID.
- For a build of a copied tree, start from a directory with no `build/` and
  no `plutus.json` (see "Disposable Final-Tree Builds" in
  [SKILL.md](../SKILL.md)).

## Language traps

- **A one-item `and {}` does not compile.** The fork rejects it with
  `aiken::check::illegal::logical_op_chain` ("an and chain with less than 2
  expressions"). Write the single expression without the `and`.
- **`test … fail` over a multi-conjunct `and {}`** passes as soon as any
  conjunct is false. The case and its rule are in the writing-tests
  catalogue:
  [`7d01f2b71`](../../writing-tests/references/mistakes-we-make.md#green-whatever-the-code-does).

## Measured limits

- **About a hundred items per single-step fold of transaction outputs.** In
  the networkId forced direction, reading one output cost about 120k memory
  on chain, so one transaction fit roughly a hundred outputs under the 20%
  reserve, against a raw-carriage bound of 352 outputs (commit `36546279`,
  2026-09-04). The fix was a resumable, checkpointed walk as a separate
  script (`fraud_proofs/network_id/forced_scan`). Treat the figure as a
  budgeting guide for folds over variable-width items, not a constant:
  re-measure for your own per-item work.
