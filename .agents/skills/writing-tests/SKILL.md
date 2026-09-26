---
name: writing-tests
description: Gates whether a Midgard test should exist, places it at the cheapest level that catches the bug, and keeps it able to fail. Use before adding or changing what an Aiken test, vitest suite, golden-vector channel, lucid-evolution emulator scenario, Postgres-backed node test or devnet check asserts, down to one case added to an existing test; when fixing a bug, which starts with a regression test that is red before the fix; and when a test, gate, focused check or verifier reports green and you need to know it could have gone red.
---

# Writing tests that earn their place

A test is worth its runtime only if it catches a realistic regression that no
existing test catches, at the cheapest level that can see it, and only if it
can go **red**. Every rule below serves one of those three.

The bugs this repository has shipped, and the tests that missed them, are in
[references/mistakes-we-make.md](references/mistakes-we-make.md). Read it when
you are choosing what a new test should assert, or when a green result looks
too easy.

## 1. The gate: two questions

Answer both, one sentence each, before writing anything. Put the answers in
the commit message or the report. `[review]`

1. **What realistic regression does this test catch that no existing test
   already catches?** Name the code path and the input that breaks it: "if
   `externalProviderBindingsMatchPolicy` accepts a strict subset of the
   configured providers, this fails". "Coverage", "good practice" and "the
   function exists" are not answers. If you cannot answer, do not write it.
2. **Why can't this be a case in the nearest existing test?** Search first
   (`rg -l '<function or redeemer name>' demo/*/tests onchain/aiken`). Add a
   row to an existing `it.each` table, a vector to an existing golden channel,
   or a negative next to its positive in an existing emulator scenario. A new
   file needs one of three reasons: the setup differs, the behavior belongs to
   a different unit, or no relevant test exists.

Done when both sentences are written and each names a concrete bug.

## 2. Bug fixes are regression-first

A fix lands with a test that is **red before the fix and green after**.
`[review]`

- Write the test against the unfixed code and run it. Record the failing
  assertion text; it must name the bug, not a setup error.
- Apply the fix; run the same command; record it green.
- For a validator fix, the red run is the negative scenario succeeding (or the
  honest one refused) against the unfixed validator.
- Show red by running the test before you edit the source, or by
  reintroducing the bug as your own edit and undoing it the same way. Never
  `git checkout` or `git stash` a file to do it: other work may share the tree
  ([AGENTS.md](../../../AGENTS.md), "Preserve user work").

Done when the report shows both runs with the same command and the red
run's failure message.

## 3. Pick the cheapest level that catches it

Each level costs roughly an order of magnitude more than the one before it.
Aim for many tests low down and few at the top. When a behavior can only be
reached through a database, an emulator or a devnet, extract the logic into a
pure function and test that; escalate only when the regression lives in the
composition itself.

| Level                         | Catches                                                                              | Lives in                                                                       |
| ----------------------------- | ------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------ |
| 1. Aiken unit test            | A predicate or transition in one validator or library module                         | `test` blocks under `onchain/aiken/{lib,validators}`                           |
| 2. TypeScript unit test       | Codecs, digests, builders' pure parts, decision tables, parsers                      | vitest in `midgard-core`, `midgard-sdk`, `midgard-validation`, `lucid-midgard` |
| 3. Golden-vector channel      | TypeScript and Aiken disagreeing on the same bytes                                   | a `fixtures:<name>:check` generator plus its vitest and generated `.ak` module |
| 4. lucid-evolution emulator   | The deployed, parameterized validator accepting or refusing a real transaction       | `*-emulator.test.ts`, `submit-init-emulator*` suites                           |
| 5. Postgres-backed node suite | Durable state: journals, migrations, claims, recovery across restarts                | the database-touching files listed in `demo/midgard-node/vitest.config.ts`     |
| 6. Devnet / live acceptance   | Runtime composition, transport, provider behavior, durable recovery on real services | [midgard-e2e-acceptance](../midgard-e2e-acceptance/SKILL.md)                   |

Before running a level, read [references/test-levels.md](references/test-levels.md)
for its focused command, its prerequisites (pinned compiler, fresh blueprint,
built `dist/`, Postgres on 5433) and what a pass there does not prove.

A pure-logic test placed in `demo/midgard-node` still pays that suite's global
setup: Postgres shard provisioning and a cargo build of the native owner
binary (`demo/midgard-node/tests/global-setup.ts:187-197`). Put pure logic in
the package that owns it.

## 4. Contract tests run in both polarities

Every contract needs a lucid-evolution emulator happy path and a rejection
where the validator must refuse, re-run in both polarities whenever parameters
change. The rule and its completion bar live in
[docs/agents/contracts.md](../../../docs/agents/contracts.md#scenario-coverage);
this section is how to make the negative real. `[review]` No check maps each
validator to its two scenarios.

- **The negative must reach the validator.** Assert the script failure, not
  "it threw". The operator-exit suite shows the shape: builder-side refusals
  are listed and rejected as proof of nothing, and the refusal must match
  `failed script execution (Spend|Mint)[n]`
  (`demo/midgard-node/tests/operator-exit-emulator.test.ts:871-916`). A loose
  matcher also passes on a validity-window error or a balancing failure.
- **Use a differential pair.** An accepted control and the identical
  transaction with one field changed. The control passing shows every other
  check is satisfied, so the changed field is the only thing left to fail.
- **Kill the guard.** Delete or weaken the check you are covering as a
  temporary edit, run the negative, and undo the edit. The negative must go
  red; if nothing fails, it covers something else.
- **Adversary against honest.** A real fault succeeds (the fraud proof mints,
  the dispute resolves). An adversary replaying an honest commitment is
  refused at the exact check.

## 5. Keep it able to fail

A test or gate that cannot fail looks exactly like a healthy one. Report the
collected count and the assertion, not the exit code. Before you trust a green
result, check it against each trap below: what goes wrong, then what to do
instead.

A test that fails only sometimes is a different problem: read
[fixing-flaky-tests](../fixing-flaky-tests/SKILL.md) before retrying, raising
a timeout or skipping it.

- **Bare `aiken check -m <module>`, or a dotted module name.** Collects zero
  tests and exits 0. Run
  `node onchain/aiken/scripts/run-focused-check.mjs <module> <test>...`, which
  fails unless exactly N tests from that one module pass
  `[script: onchain/aiken/scripts/run-focused-check.mjs]`. Blind spot: a raw
  `aiken check -m` is still unguarded, and CI does not run the runner's own
  `run-focused-check.test.mjs`.
- **`test t() fail { and { a, b, c } }` in Aiken.** Green as soon as any
  conjunct is false, so it cannot tell the intended rejection from an
  unrelated one. Pin one disposition per test, asserted directly. `[review]`
- **vitest `-t "<name>"` matching nothing.** Exits 0 with every test reported
  skipped (vitest 3.0.7, checked 2026-09-25). Read `Tests N passed` in the
  summary and check N. `[review]`
- **`<command> | tail` or `| grep`.** The pipeline's status is the last
  command's, so a red suite reads green. Use `set -o pipefail`, or redirect to
  a log and record `$?`. `[review]`
- **`.complete({ localUPLCEval: false })`.** Lucid hands evaluation to the
  provider, and the emulator's `evaluateTx` echoes the budgets already in the
  transaction without running any script, so the validator never executes.
  Use `localUPLCEval: true`, per
  [transaction-finalization.md](../../../docs/agents/transaction-finalization.md)
  `[ci: Midgard Node CI/Build, typecheck, and test fault-proof tooling]`
  through `demo/midgard-fault-proofs/tests/wave0-shared-substrate.test.ts:131`.
  Blind spot: that scan covers `midgard-fault-proofs` only;
  `demo/midgard-sdk/tests/scheduler-refresh.test.ts` uses `false` three times
  as of 2026-09-25.
- **A test double answering a shape the real service never sends.** The code
  passes against the double and fails against the service. Take the double's
  response from the real service or its published schema, including the
  fields it omits or nulls. `[review]`
- **An expected value computed from the artifact under test.** A pin
  re-derived from a broken build verifies the broken build against itself.
  Take expected values from an independent source (the spec, the other
  language, a hand-checked derivation), or pair the pin with a behavioral
  negative. `[review]`
- **A fixture built by the producer under test.** The fixture shares the
  producer's bug, so the contradiction never surfaces. Build it the way the
  consumer, usually the validator, derives it. `[review]`
- **Relative-only cross-language agreement** (`adjacent == accepted + 1`).
  Both sides drift together and both pass. Pin the absolute boundary in both
  languages. `[review]`
- **Hand-written boundary constants.** The "maximum" is not the maximum, or a
  literal drifts from its formula. Derive the boundary from the formula the
  code uses and add the adjacent over-limit case. `[review]`
- **Degenerate inputs** (an empty delta, a one-element list). They hide the
  defect a realistic input shows. Use realistic inputs and keep one labelled
  degenerate case. `[review]`
- **Stale `dist/` or `plutus.json`.** The run exercises old code or old
  validators. Rebuild first (see
  [references/test-levels.md](references/test-levels.md)). `[hook: pre-commit]`
  refuses a staged `plutus.json`; nothing checks that the file matches the
  current `.ak` source.
- **`it.only` / `describe.only`.** Every other test in the file is skipped.
  vitest refuses `.only` when `CI` is set (`allowOnly: !isCI`), so CI catches
  it; a local run does not. `[review]`

## 6. Time and fees in emulator tests

- **Time is relative to the emulator.** The lucid-evolution `Emulator` starts
  its clock at `Date.now()`. A `Lucid` instance built from it takes its own
  creation instant as `zeroTime`, so a validity start before that instant is
  refused as "too far in the past". Write times as offsets from
  `emulator.now()`, clamp lower bounds to the instance's `zeroTime` or
  `zeroSlot` (`demo/midgard-node/src/transactions/operators/exit.ts:186-213`),
  and advance the emulator to a window's first slot before building inside
  it. `[review]`
- **Keep lower bounds off the wall clock.** Production and e2e builders follow
  the backoff rules in
  [transaction-finalization.md](../../../docs/agents/transaction-finalization.md#validity-windows).
- **Exact fees have no change output.** When a validator checks
  `fee == penalty`, Lucid's change output adds its own fee on top of
  `setMinFee`. Build with `coinSelection: false` and an explicit remainder
  output (`demo/midgard-sdk/src/operator-lifecycle/exact-fee.ts`), and assert
  the completed body's fee and output count
  (`demo/midgard-sdk/src/availability-challenge-transactions.ts:474-491`).
  `[review]`

## 7. Verifiers get their own negative tests

A gate, verifier or scan script that cannot fail is indistinguishable from a
healthy one, and no end-to-end run will reveal it. Give each one a self-test
that feeds it the failing case and asserts a nonzero exit or a refusal:
`onchain/aiken/scripts/guard-focused-selector.test.mjs` drives the guard
against a stub that returns a well-formed report with zero tests collected
`[ci: Aiken CI/Self-test the fail-closed focused-selector guard]`.
Scripts under `.agents/skills/*/scripts/` follow the same rule.

## 8. Report what ran

For each test added or changed, one line: the regression it catches, the
level, the exact command, the collected count, and for a fix the red run's
failure message. "Suite green" without a count is not evidence. `[review]`
