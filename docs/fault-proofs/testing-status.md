# Fault-Proof Testing Status

Status: Active

Last reviewed: 2026-09-17 (family assembly and emulator fixture acceptance).

## What the evidence establishes

[Catalogue status](catalogue-status.md) owns source and watcher-installation
inventory. It does not assert that all tests passed. Avoid test-file counts:
a file can contain skipped, uncollected, or failing scenarios.

| Evidence                                   | Establishes                                                                                          |
| ------------------------------------------ | ---------------------------------------------------------------------------------------------------- |
| Aiken unit/property results                | Predicate and transition behavior for the collected scenarios                                        |
| TypeScript unit/workflow results           | Codecs, evidence, admission, journals, and deterministic workflow behavior                           |
| Real-blueprint Lucid lifecycles            | Applied scripts, complete transaction fit, refusals, recovery, mint/removal for the exercised shapes |
| Independent-process / real-node acceptance | Runtime composition, transport, durable recovery, and correction                                     |
| Preprod acceptance on release identity     | Live challenge behavior for the exercised deployment and scenarios                                   |

The [installed validation workflow](validation-trace-dispute-installed-workflow.md)
and [transition replay](transition-trace-installed-replay.md) identify current
routing and executable acceptance surfaces. The [fit evidence index](size-plans/README.md)
distinguishes current-blueprint gates from historical snapshot consistency tests.
A saved measurement, passing digest check, or a test that returned early does not
establish current-build acceptance.

The [availability challenge](size-plans/availability-challenge.md) now has passing
signed-publication and registered contract-lifecycle gates using the pinned
mainnet protocol-11 cost model. Its operational challenger/watcher and live release
acceptance remain open. Remeasure any release evidence whose source or blueprint
binding changed; saved results do not replace the current-build gates.

## Family assembly and funding-publisher verification (2026-09-17)

The family assembly and funding-publication changes following
`7fbd02d7fd6aa06ab4f97917c0b76460c14ebc3b`
complete the [shared assembly](workflow-family-assembly.md) migration for 18
linear and 31 cursor families. Authenticated reference publication fixtures now
use the deployment funding wallet and reserve the deployment nonce. The prover
can consume the publication independently.

Verification used Node 22.22.2, pnpm 9.15.4, eight fault-proof forks, and the
fresh testnet blueprint with SHA-256
`04790ae612c2478aa089a7f08089f11e43638cf007a2f562fa8b445f4ee25f6e`.

- The complete fault-proof run took 3,178.11 seconds: 4,022 assertions passed,
  four were skipped, and none failed. All 864 assertions that failed in the
  baseline now pass, including repeated parameterized test names.
- That run exited unsuccessfully because the transition subvariant suite's
  final hook found an older blueprint hash in its pinned forced-window ledger.
  The ledger was remeasured with the complete six-case lifecycle suite and its
  normal read-only verification rerun; both passed. The complete 53-minute
  package run was not repeated for this ledger-only correction.
- The assembly/recovery selection passed 1,031 assertions, including seven new
  assembly-lifecycle checks collected separately from the full run.
- The node `test:tx-prep:emulator` gate passed all 55 assertions. Three SDK native
  publication-policy checks passed, including wrong-wallet and expiry refusals.
- Fault-proof build/declarations and compiled registry loading passed, as did
  package lint, changed-file formatting, and fault-proof/watcher/node/node-tools
  typechecks.

These results establish local workflow and emulator acceptance for the exercised
scenarios. They do not establish live deployment or release acceptance. Generated
transition-workflow measurement output was preserved with the local run evidence;
only the forced-window ledger consumed by a regression test was refreshed.

## Verification commands

From the repository root, build the real blueprint using the pinned compiler:

```sh
(cd onchain/aiken && aiken build --env testnet)
```

Use the [Aiken build skill](../../.agents/skills/aiken-contract-build/SKILL.md)
for focused selectors and cache isolation. Selectors must collect a nonzero
number of tests. For package suites, also from the repository root:

```sh
pnpm --dir demo/midgard-fault-proofs run typecheck
pnpm --dir demo/midgard-fault-proofs test
pnpm --dir demo/midgard-core test
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-validation test
pnpm --dir demo/midgard-watcher test
pnpm --dir demo/midgard-node test
pnpm --dir demo/da-committee-node test
```

Set `MIDGARD_REAL_BLUEPRINT_PATH` to the absolute freshly built blueprint when
running real-contract scenarios. Workspace source conditions resolve sibling
source for typecheck/lint/Vitest; scripts that invoke plain `node` still require
the package builds they document.

Positive publication and lifecycle acceptance must use the shared
`demo/midgard-fault-proofs/tests/support/emulator/protocol-parameters.ts`
limits. Raised size/ExUnit limits are diagnostic, not release acceptance.

## Emulator gate performance

The 2026-09-14 scheduling change preserves all 434 collected fault-proof
emulator cases across 103 files. It removes the command-level serialization
that overrode the existing fork limit and splits two expensive transition-trace
cases into independent files. The original 21 cases remain 19 + 1 + 1 with
identical full test names and an unchanged callback syntax tree, including all
39 assertions and three restored module spies. Each file retains an isolated
fork; real local UPLC evaluation, protocol limits and lifecycle checks remain
required. Optional fit ledgers from the two split files use distinct sibling
paths to avoid concurrent writes.

Run the complete named gate from the repository root with the measured worker
cap (eight is the default on this host):

```sh
MIDGARD_FAULT_PROOF_FORKS=8 pnpm --dir demo run test:tx-prep:emulator
```

The scheduling-only benchmarks below precede the evaluator optimization described
later and measure only the fault-proof stage. The separate node
stage previously passed all 52 cases in 432.61 seconds; add its runtime when
assessing the complete named gate. Both worker benchmarks use the same frozen
fault-proof test tree and blueprint, with fit-ledger writes disabled.

| Fault-proof workers                 | Cases / files | Result | Wall time     | Peak aggregate RSS | Peak aggregate PSS |
| ----------------------------------- | ------------- | ------ | ------------- | ------------------ | ------------------ |
| 1 (previous baseline, before split) | 434 / 101     | Passed | 79.97 minutes | Not sampled        | Not sampled        |
| 4                                   | 434 / 103     | Passed | 23.09 minutes | 6.50 GiB           | 6.24 GiB           |
| 8                                   | 434 / 103     | Passed | 21.17 minutes | 10.14 GiB          | 9.61 GiB           |

Both new runs completed with zero failed or pending cases and matched the exact
434-name baseline inventory. Each used 103 distinct worker processes. Eight
workers saved 115 seconds over four (8.3%); retain the existing eight-worker
default. At peak memory use, at least 35.97 GiB remained available on this host
(32 logical CPUs, 61 GiB RAM). Set `MIDGARD_FAULT_PROOF_FORKS` lower on smaller
machines. RSS was sampled across the process group every second and PSS every
five seconds; these are sampled peaks, not a per-process maximum.

The split cases must start early: the installed Vitest 3.0.7 falls back to
file-size ordering because its cache version check rejects its own version.
The two small wrappers would otherwise run last. A sequencer moves those two
files first while preserving Vitest's ordering for all others. The deep-deposit
case started within nine seconds and took 20.92 minutes in the eight-worker run;
its sequential computation now determines the suite's completion time.

The measured blueprint SHA-256 was
`11593a4edd6a400e08fd32e52d65cea498225d32ce28b471862ca6bd1435b079`.
All 1,575 frozen source/test/config/blueprint file hashes matched after both runs.

### Targeted profiles and evaluator optimization

The isolated maximum deposit profile passed in 1,164.56 seconds including module
setup (the test body took 1,155.99 seconds). Local UPLC evaluation consumed
1,044.12 seconds, or 89.7% of that wall time. Its deposit value-fold phase alone
spent 880.59 seconds in evaluation across 325 submitted checkpoints. CPU samples
independently attributed 1,035.95 seconds to the evaluator and its descendants.

All nine evaluator arguments were fingerprinted, including ordered UTxO CBOR,
cost models, CPU/memory budgets and slot configuration. Of 2,675 actual calls,
665 repeated an identical full request. Those repeated successful evaluations
cost 261.75 seconds (22.5% of profiled wall time). Whole checkpoint planning,
including output lookup, trace derivation and witness construction, took only
16.12 seconds (1.4%). Fingerprinting added 0.26 seconds outside the evaluation
timer. These profiles execute every original evaluation and assertion; they do
not substitute an evaluator or change transaction construction.

The 1,304-asset accepted-output profile also passed: 303.00 seconds including
setup (293.79 seconds in the test), with 215.48 seconds in local evaluation.
Its 505 identical repeat calls cost 53.94 seconds (17.8% of wall time); all
checkpoint planning took 14.21 seconds. The raw profile is under
`/tmp/midgard-emulator-perf/output-20260914T182944Z/`.

The existing Lucid dependency patch now retains one successful evaluator request
and result within a public transaction completion, across internal redeemer,
fee and collateral convergence passes. Reuse requires byte equality of all
transaction, ordered UTxO and cost-model inputs plus equality of all budget and
slot arguments. Retained bytes are detached; each hit decodes fresh redeemer
values. A changed request clears the entry, failures are not cached, and separate
completions share no results. Custom evaluators retain their original behavior.
All convergence, execution-unit and result-validation checks still run. Both
published module formats are patched through pnpm's locked patch mechanism.

The small real-evaluator regression failed before the change with four calls for
three distinct requests, then passed with three actual evaluations and unchanged
signed transaction bytes. It also exercises separate completions, changed
transaction/collateral context and deliberately failing custom evaluators.

Only focused regressions and the two expensive cases are rerun for this
optimization; the full-suite scheduling results above are not a post-cache gate
result. The targeted before/after profiles use isolated workers and preserve
all case assertions, protocol limits and actual submissions.

| Profiled test body              | Before reuse | After reuse | Real evaluations before / after |
| ------------------------------- | ------------ | ----------- | ------------------------------- |
| Accepted output, 1,304 assets   | 293.79 s     | 233.14 s    | 2,032 / 1,527                   |
| Deposit, 1,295 assets, depth 64 | 1,155.99 s   | 905.72 s    | 2,675 / 2,010                   |

The accepted-output case improved 20.6%. All 505 duplicate evaluations were
removed, with zero repeated full requests afterward; the same 574 transactions
were submitted and the same 498 phases planned. Its after-profile is retained
under `/tmp/midgard-emulator-perf/after-cache-many-assets/`.
The deposit case improved 21.6%. All 665 duplicate evaluations were removed,
with zero repeated full requests afterward; the same 736 transactions were
submitted in the same phase order and the same 658 phases planned. Its
post-change profile is under
`/tmp/midgard-emulator-perf/after-cache-deep-deposit/`. Both cases passed their
original assertions. All 1,579 frozen hotspot inputs, including the dependency
patch, lockfile and installed ESM/CJS bundles, matched after timing completed.

Final focused checks passed: 12 new evaluator regressions across ESM/CommonJS,
eight existing scheduler-refresh tests, and six transition-trace honest/corrupt
evidence refusal cases (13 other cases intentionally filtered in that invocation).
SDK and fault-proof package typechecks, scoped ESLint, and formatting also passed.
No full emulator gate was rerun after the evaluator change.

Reproduce the short correctness checks from the repository root:

```sh
pnpm --dir demo/midgard-sdk exec vitest run tests/lucid-completion-evaluation.test.ts tests/scheduler-refresh.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-transition-trace-final.test.ts -t 'honest=true|corrupt(Index|Datum|Source)=true'
```

Run each expensive case separately for timing; the figures above include CPU
profiling, so compare equivalent instrumentation when reproducing the numbers:

```sh
MIDGARD_FAULT_PROOF_FORKS=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-transition-trace-final-many-assets.test.ts
MIDGARD_FAULT_PROOF_FORKS=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-transition-trace-final-deep-deposit.test.ts
```

Planning is a smaller target: most of its measured 14–16 seconds is the first
value-trace derivation (11.8–12.2 seconds), rather than repeated checkpoint
lookups. After this change, further evaluator acceleration should be measured
before adding another optimization. A native build of the same engine would
need exact request/result and execution-unit parity; it is not introduced here.

The maximum-deposit CPU profile and exact-call measurements are retained under
`/tmp/midgard-emulator-perf/output-20260914T181119Z/`; the small three-asset hook
canary is under `output-20260914T181046Z/`. Diagnostic code lives under `/tmp`
and is absent from the production/test configuration.

The benchmark source inventory is
`/tmp/midgard-emulator-perf/benchmark-source-freeze.json`; commands, process-memory
samples and results are retained under `/tmp/midgard-emulator-perf/`.
The old/new collected test manifests and callback comparison are
`/tmp/midgard-transition-trace-final-split-inventory.json` and
`/tmp/midgard-transition-trace-final-body-preservation.json`.
These receipts establish the measured local gate result and preserved test scope;
they do not close deployment-bound or public-testnet acceptance.

## Release boundary

Use [remaining acceptance](execution-plan.md) for publication, maximum-shape,
public evidence lifetime, economics, independent-process, and preprod closure.
The [public readiness checklist](../public_testnet_readiness.md) owns launch
approval. A source installation or passing subset does not close these gates.
