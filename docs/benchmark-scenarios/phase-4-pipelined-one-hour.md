# Phase 4 pipelined-commit one-hour gate

**Status:** Active acceptance procedure; only a retained, fingerprinted one-hour
report is evidence that the gate passed.

**Last reviewed:** 2026-07-22

This is the canonical fail-closed evidence surface for throughput Phase 4
§6.5. It runs one production-shaped Class B stage for exactly one measured
hour and verifies the resulting report. Run it only after the actual-process
crash/lease acceptance gate and a fresh local-devnet deployment are green.

The node must use `SPECULATIVE_COMMIT_BUILD=true` and
`COMMIT_MAX_L2_TX_COUNT=50000`. The workload must be a verified corpus whose
manifest covers at least 3,600 seconds. A separate-container or separate-host
load generator is mandatory.

## 1. Capture environment identity

Record immutable container/host facts from the deployed benchmark topology,
then capture the fingerprint. Image values must be inspected image IDs or
digests, CPU sets must be the effective assigned sets, and memory values must
be effective byte limits (not requested defaults).

```bash
export PHASE4_NODE_CPUSET='0-7'
export PHASE4_NODE_NANO_CPUS='4000000000'
export PHASE4_NODE_IMAGE_ID='sha256:...'
export PHASE4_NODE_MEMORY_LIMIT_BYTES='8589934592'
export PHASE4_LOADGEN_CPUSET='8-15'
export PHASE4_LOADGEN_NANO_CPUS='4000000000'
export PHASE4_LOADGEN_IMAGE_ID='sha256:...'
export PHASE4_LOADGEN_MEMORY_LIMIT_BYTES='4294967296'
export PHASE4_POSTGRES_CPUSET='16-23'
export PHASE4_POSTGRES_NANO_CPUS='4000000000'
export PHASE4_POSTGRES_IMAGE_ID='sha256:...'
export PHASE4_POSTGRES_MEMORY_LIMIT_BYTES='8589934592'
export STRESS_LOAD_GENERATOR_PLACEMENT='separate-container'
export STRESS_LOADGEN_COHOSTED='true'
export STRESS_CLOCK_OFFSET_MS='0'
export L1_PROVIDER='Kupmios'
export L1_KUPO_KEY='http://...'
export L1_OGMIOS_KEY='ws://...'
export MIDGARD_DEPLOYMENT_MANIFEST_PATH='/absolute/path/deployment-manifest.json'

cd demo/midgard-node
pnpm capture:phase4:environment -- \
  /absolute/evidence/phase4-environment.json
```

The artifact binds the exact benchmark profile's disjoint CPU sets, effective
4-CPU NanoCPU quotas, image IDs, minimum memory limits, hashed provider route,
deployment-manifest bytes, loadgen placement, and clock offset. Its canonical
document SHA-256 is recomputed by the verifier, so an embedded-document edit
fails closed. The workload report also records a framed SHA-256
over the full relevant source tree, including untracked files, workspace
package sources, patches, lockfile, benchmark scripts, native MPF source, and
the benchmark/Kupmios Compose overlays and non-secret benchmark environment.

## 2. Run the canonical scenario

```bash
export SPECULATIVE_COMMIT_BUILD='true'
export COMMIT_MAX_L2_TX_COUNT='50000'
export STRESS_TARGET_ACCEPTED_TPS='10000'
export STRESS_CORPUS_PATH='/absolute/corpus.ndjson'
export STRESS_CORPUS_INDEX_PATH='/absolute/corpus.ndjson.index.ndjson'
export STRESS_CORPUS_MANIFEST_PATH='/absolute/corpus.ndjson.manifest.json'
export STRESS_CORPUS_SLICE_ID='default'
export STRESS_PHASE4_ENVIRONMENT_FINGERPRINT_PATH='/absolute/evidence/phase4-environment.json'
export STRESS_LOAD_GENERATOR_PLACEMENT='separate-container'
export STRESS_LOADGEN_COHOSTED='true'
export STRESS_CLOCK_OFFSET_MS='0'
export STRESS_OBSERVABILITY_PROFILE='on'
export STRESS_REPORT_PATH='/absolute/evidence/phase4-one-hour/report.json'

pnpm gate:phase4:pipelined-one-hour
```

The wrapper forces the scenario name, Class B, formal mode, open-loop mode,
3,600 measured seconds, commit+merge drains, and the 50,000-tx block target.
It writes `verification.json` beside the raw report.

## Pass criteria

- measured and configured duration are at least 3,600 seconds;
- every observed non-zero commit-block size is exactly 50,000 transactions;
- L1 confirmation p50 and commit-cadence p50 each have at least two samples;
- cadence p50 is no more than confirmation p50 plus 5 seconds;
- speculation overlap p50 is at least 0.90 and hit rate is at least 0.95;
- Stage C is at least 2,500 TPS, or the verdict explicitly records
  `confirmation_latency_binding` with confirmation p50 above 19 seconds;
- state-queue and DA-publication backlog slopes are both non-positive;
- source, git/diff, runtime, corpus, environment, provider, and deployment
  identities are complete and SHA-bound.

Any missing metric, identity, duration, or queue evidence fails the gate.
