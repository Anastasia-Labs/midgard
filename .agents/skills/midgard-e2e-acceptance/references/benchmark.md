# Optional Stress and Benchmark Evidence

Read this reference only when the user explicitly requests stress, throughput,
soak, or capacity evidence. Baseline E2E acceptance remains functional and does
not run stress by default.

## Contents

1. [Select the evidence class](#select-the-evidence-class)
2. [Use canonical scenarios](#use-canonical-scenarios)
3. [Placement and comparability](#placement-and-comparability)
4. [Bounded E2E stress](#bounded-e2e-stress)
5. [Reporting](#reporting)

## Select the evidence class

- **Functional E2E** proves the real deposit, L2, DA, finality, and automatic
  merge path. It is not a TPS benchmark.
- **Class A** measures durable admission and validation. It may gate regression
  but does not prove L1/DA/merge throughput.
- **Class B** measures the full pipeline through L1, DA, and merge. It must
  report observed Preprod conditions and cannot normalize away external block
  timing.

State the class before any rate or latency number. Never headline
`node-container` closed-loop output as TPS.

## Use canonical scenarios

Treat repository scenario docs and package scripts as the source of truth. Read
the one matching the request rather than copying an old local command:

- `docs/benchmark-scenarios/mixed-workload-multi-io.md`;
- `docs/benchmark-scenarios/phase-2-validation-gates.md`;
- `docs/benchmark-scenarios/phase-3-architecture-g-closure.md`;
- `docs/benchmark-scenarios/phase-3-architecture-g-soak.md`;
- `docs/benchmark-scenarios/phase-4-pipelined-one-hour.md`; or
- `docs/benchmark-scenarios/phase-5-da-50k-distribution.md`.

Run the named gate and verifier from `demo/midgard-node/package.json`. Do not
substitute an ad hoc smoke for a named scenario. Never cite ignored or missing
local `logs/` paths as durable repository evidence.

## Placement and comparability

For an upper-bound claim, place the load generator on a separate host on the
same LAN. Record:

- `STRESS_LOAD_GENERATOR_PLACEMENT=separate-host`;
- `STRESS_LOADGEN_COHOSTED=false`;
- node and load-generator clock/NTP offset;
- node/Postgres CPU and memory pins;
- corpus, index, manifest, config, and binary hashes; and
- observability profile state.

If a second host is unavailable, use the documented cohosted container profile
with disjoint CPU sets. Label it `separate-container`; use it for relative
tracking, not absolute production-capacity claims. Loopback or execution inside
the node container is smoke/calibration only.

For Class B, record an `l1Observation` block with observed Preprod block count,
tip slots, and min/median/max inter-block time. Do not average runs whose L1
conditions materially differ.

Reset local state between comparable benchmark runs only under the complete
fresh-deployment rules in the main skill. Use disjoint corpus slices and at
least three runs for median/deviation claims.

## Bounded E2E stress

When the user asks for bounded stress as part of a live E2E run, insert it after
the two baseline L2 submissions and before waiting for the final automatic
drain.

Use `e2e-stress-l2-throughput` for the bounded harness. Keep defaults explicit:

- `serial-chain` with concurrency 1 for the smallest functional stress;
- `parallel-fanout` only with independent, pre-funded stress wallets;
- no shared-wallet concurrency;
- zero tolerated submission failures unless the user requests a failure-budget
  experiment; and
- adaptive polling unless a scenario specifies a fixed interval.

For open-loop work, use `stress-corpus-generate`, `stress-corpus-verify`, and
the canonical engine scripts in `demo/midgard-node/scripts`. Match corpus
network, fee parameters, maximum submit size, and manifest to the live node.
Fund from the planner's per-wallet requirement; do not estimate it from the
transfer amount alone.

Preserve the canonical artifacts:

- config and environment fingerprint;
- corpus and manifest;
- engine report and events;
- submission records;
- no-op calibration;
- DB stage metrics; and
- stress `summary.json` and `summary.md`.

Append only a successfully parsed stress summary to the functional dashboard:

```bash
STRESS_SUMMARY_ARGS=()
if [ -f "logs/$RUN_ID/stress/summary.json" ]; then
  STRESS_SUMMARY_ARGS+=(
    --stress-summary "logs/$RUN_ID/stress/summary.json"
  )
fi
```

The stress admission gate is `metrics.l2Admission`. Immutable observation and
full automatic-drain finality are distinct evidence. A passed admission metric
does not prove full finality.

## Reporting

For Class A, report durable admission, accepted rate, latency percentiles,
three-run median, and max deviation. For Class B, add committed/finality rates
only with L1 observation evidence.

Always report:

- exact scenario/gate/verifier commands;
- git commit and dirty-state status;
- placement and resource isolation;
- manifest/corpus/config hashes;
- accepted, rejected, failed, timed-out, committed, and finalized counts;
- clean-run versus recovered-run status; and
- why the evidence does or does not support the requested claim.

Do not extrapolate a local generator sizing run, smoke test, or short Preprod
sample into production capacity.
