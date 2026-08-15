# Phase 2 validation gate matrix

**Status:** Active acceptance procedure; only a retained report from the exact
revision/configuration is evidence that the gate passed.

**Last reviewed:** 2026-07-22

This runbook is the repeatable evidence surface for throughput Phase 2
(parallel validation, B1–B3). Its ExecPlan was removed in the 2026-08 docs
cleanup and is preserved in git history as
`docs/exec-plans/throughput/phase-2-parallel-validation.md` (last commit
`c4e0ac9a7`); still-live items from the throughput ExecPlans were consolidated
into `docs/exec-plans/archive-extracts-2026-08.md`. Every asserted run
is fail-closed: the benchmark inspects the node and PostgreSQL containers, and
`verify-phase2-benchmark-report.mjs` rejects topology drift, transaction loss,
queue residue, latency/scaling regressions, or incomplete soak duration.

Run from the repository root. Choose `CPUSET` as eight logical CPUs backed by
eight distinct physical cores; verify that mapping before starting:

```bash
export REPO="$(pwd -P)"
export CPUSET="0,2,4,6,8,10,12,14"
export NODE_IMAGE="node:22.22.2"
export POSTGRES_IMAGE="postgres:15.15-alpine"
export PHASE2_NETWORK="midgard_phase2_gate"
export PHASE2_PG="midgard_phase2_pg"
export DOCKER_CLI="/mnt/wsl/docker-desktop/cli-tools/usr/bin/docker"
: "${SHORT_CORPUS:?set SHORT_CORPUS to the verified private-256 corpus path inside /workspace}"
: "${SHORT_MANIFEST:?set SHORT_MANIFEST to that corpus manifest path inside /workspace}"
: "${SHORT_WALLETS:?set SHORT_WALLETS to that corpus wallet directory inside /workspace}"
: "${FULL_CORPUS:?set FULL_CORPUS to the verified formal corpus path inside /workspace}"
: "${FULL_MANIFEST:?set FULL_MANIFEST to that corpus manifest path inside /workspace}"
: "${FULL_WALLETS:?set FULL_WALLETS to that corpus wallet directory inside /workspace}"

lscpu -p=CPU,CORE,SOCKET | sed '/^#/d' | awk -F, -v cpus="$CPUSET" '
  BEGIN {
    selectedCount = split(cpus, selected, ",")
    for (i = 1; i <= selectedCount; i += 1) wanted[selected[i]] = 1
  }
  $1 in wanted {
    physical = $3 ":" $2
    if (!(physical in seen)) {
      seen[physical] = 1
      physicalCount += 1
    }
    print
  }
  END {
    if (selectedCount != 8 || physicalCount != 8) exit 2
  }
'
test "$(docker run --rm "$NODE_IMAGE" node --version)" = "v22.22.2"
export NODE_IMAGE_ID="$(docker image inspect --format '{{.Id}}' "$NODE_IMAGE")"
test "${NODE_IMAGE_ID#sha256:}" != "$NODE_IMAGE_ID"
test "${#NODE_IMAGE_ID}" -eq 71
test -x "$DOCKER_CLI"
```

The short rehearsal must be a newly verified private 25,600-row corpus (256
chains × 100); there is no retained short artifact that may be treated as
formal evidence. The currently verified 4,096-wallet full corpus has 3,063,808
rows. It can cover only 10,212.69 tx/s for 300 seconds, so it is also not a
valid formal resource for the expected throughput envelope. The formal corpus
must contain at least 3,780,000 unique rows per replica: 12,600 tx/s of capacity
for one continuous 300-second run. The parser still recomputes measured
throughput from accepted rows and elapsed time, so extra rows never substitute
for either the 300-second duration or the 10,000 tx/s floor. Both sequential
replicas must independently clear both requirements. Generate and verify the
missing private-256 and >=3,780,000-row corpora before running the matrix; do
not relabel or pad retained reports. Then declare the full corpus identity:

```bash
(
  cd demo/midgard-node
  MIN_FEE_A=0 MIN_FEE_B=3110 MAX_SUBMIT_TX_CBOR_BYTES=32768 \
    node dist/index.js stress-corpus-verify \
      --corpus-path "${FULL_CORPUS#/workspace/demo/midgard-node/}" \
      --manifest-path "${FULL_MANIFEST#/workspace/demo/midgard-node/}" \
      --rebuild-wallets-dir "${FULL_WALLETS#/workspace/demo/midgard-node/}" \
      --amount-lovelace 1000000
)

export PHASE2_EXPECTED_FULL_CORPUS_SHA256="$(
  node -e 'const fs=require("node:fs"); const m=JSON.parse(fs.readFileSync(process.argv[1], "utf8")); process.stdout.write(m.files.corpus.sha256)' \
    "$REPO/${FULL_MANIFEST#/workspace/}"
)"
export PHASE2_EXPECTED_FULL_CORPUS_ROWS="$(
  node -e 'const fs=require("node:fs"); const m=JSON.parse(fs.readFileSync(process.argv[1], "utf8")); process.stdout.write(String(m.files.corpus.rowCount))' \
    "$REPO/${FULL_MANIFEST#/workspace/}"
)"
test "${#PHASE2_EXPECTED_FULL_CORPUS_SHA256}" -eq 64
test "$PHASE2_EXPECTED_FULL_CORPUS_ROWS" -ge 3780000
```

Start one private, un-published, ephemeral PostgreSQL container:

```bash
docker network create "$PHASE2_NETWORK"
docker run --detach --rm \
  --name "$PHASE2_PG" \
  --network "$PHASE2_NETWORK" \
  --network-alias pg \
  --cpuset-cpus "$CPUSET" \
  --tmpfs /var/lib/postgresql/data:rw,noexec,nosuid,size=16g \
  --env POSTGRES_PASSWORD=postgres \
  "$POSTGRES_IMAGE"
until docker exec "$PHASE2_PG" pg_isready -U postgres; do sleep 1; done
```

Define the exact Stage B runner. `short_assert` is `1` only for rehearsals.
The benchmark creates and destroys databases whose prefix is unique to the run.

```bash
run_stage_b() {
  label="$1"
  write_batch="$2"
  chunk="$3"
  short_assert="$4"
  minimum_tps="$5"
  corpus="$6"
  manifest="$7"
  wallets="$8"
  node_name="midgard_phase2_node_${label}"
  output="/workspace/demo/midgard-node/tests/benchmarks/output/phase2-${label}.json"

  docker run --rm \
    --name "$node_name" \
    --network "$PHASE2_NETWORK" \
    --cpuset-cpus "$CPUSET" \
    --volume "$REPO:/workspace" \
    --volume "$DOCKER_CLI:/usr/local/bin/docker:ro" \
    --volume /var/run/docker.sock:/var/run/docker.sock:ro \
    --workdir /workspace/demo/midgard-node \
    --env NODE_ENV=emulator \
    --env POSTGRES_HOST=pg \
    --env POSTGRES_PORT=5432 \
    --env POSTGRES_USER=postgres \
    --env POSTGRES_PASSWORD=postgres \
    --env BENCH_PHASE2_OPERATOR=1 \
    --env BENCH_ASSERT_PHASE2=1 \
    --env BENCH_PHASE2_SHORT_ASSERT="$short_assert" \
    --env BENCH_PHASE2_MIN_ACCEPTED_TPS="$minimum_tps" \
    --env BENCH_PHASE2_POOL_SIZE=6 \
    --env BENCH_PHASE2_BATCH_SIZE=2048 \
    --env BENCH_PHASE2_DRAIN_LOOPS=4 \
    --env BENCH_PHASE2_CHUNK_SIZE="$chunk" \
    --env BENCH_PHASE2_WARMUP_ITERATIONS=2 \
    --env WRITE_BEHIND_MAX_BATCH="$write_batch" \
    --env BENCH_PHASE2_DATABASE_PREFIX="midgard_phase2_bench_${label}" \
    --env BENCH_PHASE2_CORPUS_PATH="$corpus" \
    --env BENCH_PHASE2_CORPUS_MANIFEST_PATH="$manifest" \
    --env BENCH_PHASE2_WALLETS_DIRECTORY="$wallets" \
    --env BENCH_PHASE2_OUTPUT_PATH="$output" \
    --env BENCH_PHASE2_POSTGRES_CONTAINER="$PHASE2_PG" \
    --env BENCH_PHASE2_NODE_CONTAINER="$node_name" \
    --env BENCH_PHASE2_PRIVATE_NETWORK="$PHASE2_NETWORK" \
    --env BENCH_PHASE2_NODE_REPO_SOURCE="$REPO" \
    --env BENCH_PHASE2_NODE_REPO_DESTINATION=/workspace \
    --env BENCH_PHASE2_NODE_DOCKER_CLI_SOURCE="$DOCKER_CLI" \
    --env BENCH_PHASE2_POSTGRES_EPHEMERAL=1 \
    --env BENCH_PHASE2_NODE_IMAGE="$NODE_IMAGE" \
    --env BENCH_PHASE2_NODE_IMAGE_ID="$NODE_IMAGE_ID" \
    --env BENCH_PHASE2_POSTGRES_IMAGE="$POSTGRES_IMAGE" \
    --env PHASE2_EXPECTED_FULL_CORPUS_SHA256="$PHASE2_EXPECTED_FULL_CORPUS_SHA256" \
    --env PHASE2_EXPECTED_FULL_CORPUS_ROWS="$PHASE2_EXPECTED_FULL_CORPUS_ROWS" \
    "$NODE_IMAGE" \
    bash -lc 'corepack enable && pnpm run bench:validation:stage-b'
}
```

Run the write-behind A/B experiment as one new, timestamp-bound sequence of
three interleaved controls and three candidates. The parser binds all twelve
replica databases to the same `wab_YYYYMMDDtHHMMSSz` identity, requires
canonical strictly increasing timestamps within 24 hours of that identity, and
rejects mixed or reused database pairs. It requires the candidate median to be
at least 10,500 TPS and at least 3% above the matched control median. One fast
or retained run cannot change a default.

```bash
export WRITE_BEHIND_AB_RUN_ID="wab_$(date -u +%Y%m%dt%H%M%Sz)"
case "$WRITE_BEHIND_AB_RUN_ID" in
  wab_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]t[0-9][0-9][0-9][0-9][0-9][0-9]z) ;;
  *) exit 2 ;;
esac

run_stage_b "${WRITE_BEHIND_AB_RUN_ID}_control_1" 1000 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${WRITE_BEHIND_AB_RUN_ID}_candidate_1" 2048 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${WRITE_BEHIND_AB_RUN_ID}_control_2" 1000 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${WRITE_BEHIND_AB_RUN_ID}_candidate_2" 2048 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${WRITE_BEHIND_AB_RUN_ID}_control_3" 1000 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${WRITE_BEHIND_AB_RUN_ID}_candidate_3" 2048 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"

node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs write-behind-ab \
  "demo/midgard-node/tests/benchmarks/output/phase2-${WRITE_BEHIND_AB_RUN_ID}_control_1.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${WRITE_BEHIND_AB_RUN_ID}_control_2.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${WRITE_BEHIND_AB_RUN_ID}_control_3.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${WRITE_BEHIND_AB_RUN_ID}_candidate_1.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${WRITE_BEHIND_AB_RUN_ID}_candidate_2.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${WRITE_BEHIND_AB_RUN_ID}_candidate_3.json"
```

Run the chunk-size experiment as one new, timestamp-bound experiment. The six
commands and the six verifier arguments are deliberately interleaved in the
exact order `64,128,64,128,64,128`; do not group the controls and candidates.
`cab_YYYYMMDDtHHMMSSz` is short enough that its derived PostgreSQL template
database remains below PostgreSQL's 63-byte identifier limit.

```bash
export CHUNK_AB_RUN_ID="cab_$(date -u +%Y%m%dt%H%M%Sz)"
case "$CHUNK_AB_RUN_ID" in
  cab_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]t[0-9][0-9][0-9][0-9][0-9][0-9]z) ;;
  *) exit 2 ;;
esac

run_stage_b "${CHUNK_AB_RUN_ID}_chunk64_1" 1000 64 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${CHUNK_AB_RUN_ID}_chunk128_1" 1000 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${CHUNK_AB_RUN_ID}_chunk64_2" 1000 64 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${CHUNK_AB_RUN_ID}_chunk128_2" 1000 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${CHUNK_AB_RUN_ID}_chunk64_3" 1000 64 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
run_stage_b "${CHUNK_AB_RUN_ID}_chunk128_3" 1000 128 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"

node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs chunk-ab \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk64_1.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk128_1.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk64_2.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk128_2.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk64_3.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk128_3.json"
```

The `chunk-ab` parser requires exactly those six bound identities, canonical
strictly increasing timestamps within 24 hours of the run identity, one exact
corpus and topology, batch 2,048, four drains, six workers, two warmups,
write-behind 1,000, fresh databases, and the production tx-delta path. Every
report and each of its two replicas must sustain at least 10,000 tx/s. The
chunk-128 median must be at least 10,500 tx/s and at least 3% above the chunk-64
median.

Every Stage B mode records the inspected immutable Node image ID and requires
it to equal the operator-declared `NODE_IMAGE_ID`. The A/B parsers additionally
require that exact ID to match across every report, and the joint chunk-128
authorization requires the script-heavy candidate to match it as well.

Passing `chunk-ab` alone never authorizes a production-default change. Chunk 64
remains the default unless the separate, asserted five-minute chunk-128
script-heavy candidate below also passes and the joint
`authorize-chunk128-default` parser accepts the seven reports together. The
standalone `script-heavy` gate remains fixed to chunk 64 and proves the
unchanged production default.

Prove the unchanged production defaults (`chunk=64`, write-behind batch 1000),
then run the five-minute exit gate on those same defaults:

```bash
run_stage_b production_default 1000 64 1 10000 "$SHORT_CORPUS" "$SHORT_MANIFEST" "$SHORT_WALLETS"
node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs production-default \
  demo/midgard-node/tests/benchmarks/output/phase2-production_default.json

run_stage_b full_five_minute 1000 64 0 10000 "$FULL_CORPUS" "$FULL_MANIFEST" "$FULL_WALLETS"
node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs full \
  demo/midgard-node/tests/benchmarks/output/phase2-full_five_minute.json
```

For the full gate, each replica's deposit-projection fiber must remain active
through the measured drain. At most five seconds may be attributed to the final
write-behind flush, and the required five-second delta-bump count is derived
from the replica duration minus only that bounded allowance.

Run the script-heavy UPLC gate in the same eight-core container. Every
transaction is a real Plutus V3 spend, every UPLC evaluation uses the worker
pool, and its complete verdict/state patch is compared to the inline reference.
The report parser unconditionally requires
`gateMode=production_default_chunk64`, Plutus V3, exact state-patch parity, the
exact `node:22.22.2` reference, and the inspected immutable `sha256:` image ID;
the report must match the operator-declared `NODE_IMAGE_ID`, not merely any
well-formed image ID. Missing fields and retained image mismatches fail closed.

```bash
docker run --rm \
  --name midgard_phase2_node_script \
  --cpuset-cpus "$CPUSET" \
  --volume "$REPO:/workspace" \
  --volume "$DOCKER_CLI:/usr/local/bin/docker:ro" \
  --volume /var/run/docker.sock:/var/run/docker.sock:ro \
  --workdir /workspace/demo/midgard-node \
  --env NODE_ENV=emulator \
  --env BENCH_ASSERT_PHASE2_SCRIPT=1 \
  --env BENCH_PHASE2_POOL_SIZE=6 \
  --env BENCH_PHASE2_CHUNK_SIZE=64 \
  --env BENCH_PHASE2_DURATION_MS=300000 \
  --env BENCH_PHASE2_NODE_IMAGE="$NODE_IMAGE" \
  --env BENCH_PHASE2_NODE_IMAGE_ID="$NODE_IMAGE_ID" \
  --env BENCH_PHASE2_OUTPUT_PATH=/workspace/demo/midgard-node/tests/benchmarks/output/phase2-script-heavy.json \
  "$NODE_IMAGE" \
  bash -lc 'corepack enable && pnpm run bench:validation:script-heavy'
node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs script-heavy \
  demo/midgard-node/tests/benchmarks/output/phase2-script-heavy.json
```

Only after the six-report `chunk-ab` gate passes, run the exact chunk-128
candidate. This mode is separate from the production gate and fail-closes
unless the chunk is exactly 128, the duration is at least five minutes, every
transaction is Plutus V3, all UPLC evaluation runs in workers, and both verdict
and complete state-patch output match the inline reference. It also rehashes
and recounts the same short corpus and records the chunk experiment, topology,
and Node runtime identity so retained evidence cannot be mixed.

```bash
docker run --rm \
  --name "midgard_phase2_node_script_${CHUNK_AB_RUN_ID}" \
  --cpuset-cpus "$CPUSET" \
  --volume "$REPO:/workspace" \
  --volume "$DOCKER_CLI:/usr/local/bin/docker:ro" \
  --volume /var/run/docker.sock:/var/run/docker.sock:ro \
  --workdir /workspace/demo/midgard-node \
  --env BENCH_PHASE2_CHUNK_AB_EXPERIMENT_ID="$CHUNK_AB_RUN_ID" \
  --env BENCH_PHASE2_CORPUS_PATH="$SHORT_CORPUS" \
  --env BENCH_PHASE2_CORPUS_MANIFEST_PATH="$SHORT_MANIFEST" \
  --env BENCH_PHASE2_NODE_IMAGE="$NODE_IMAGE" \
  --env BENCH_PHASE2_NODE_IMAGE_ID="$NODE_IMAGE_ID" \
  --env BENCH_PHASE2_OUTPUT_PATH="/workspace/demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}-script-heavy-chunk128.json" \
  "$NODE_IMAGE" \
  bash -lc 'corepack enable && pnpm run bench:validation:script-heavy:chunk128-candidate'

node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs script-heavy-chunk128 \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}-script-heavy-chunk128.json"

node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs authorize-chunk128-default \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk64_1.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk128_1.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk64_2.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk128_2.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk64_3.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}_chunk128_3.json" \
  "demo/midgard-node/tests/benchmarks/output/phase2-${CHUNK_AB_RUN_ID}-script-heavy-chunk128.json"
```

The standalone candidate parser never returns default authorization. The joint
parser revalidates both gates, requires the candidate timestamp to follow all
six interleaved reports and remain within 24 hours of the experiment identity,
and requires exact corpus, CPU topology, worker/runtime, and experiment
bindings. Any stale or mismatched artifact fails closed.

The leak soak uses a warmed pool, throttles accepted work to 2,500 TPS, samples
process RSS once per minute and records `RSS / worker-count` only as a process
average. It separately samples every stable worker slot's used heap plus
external/WASM memory and requires each comparable per-worker footprint to grow
by less than 10%. A separate five-minute steady-state warmup runs at the same
2,500 TPS before either memory baseline is captured. The asserted 24-hour
measurement starts only after that warmup and requires less than 10% aggregate
process-RSS growth over the full measured day.
The report records accepted/rejected/batch counts for both windows. The parser
requires an exact requested 300,000 ms warmup and 86,400,000 ms measurement,
recomputes both reported rates from accepted counts and observed durations, and
requires every accepted count to equal `batch-size * batches`; declarative TPS
or padded duration fields cannot pass.

```bash
docker run --rm \
  --name midgard_phase2_node_leak \
  --cpuset-cpus "$CPUSET" \
  --volume "$REPO:/workspace" \
  --volume "$DOCKER_CLI:/usr/local/bin/docker:ro" \
  --volume /var/run/docker.sock:/var/run/docker.sock:ro \
  --workdir /workspace/demo/midgard-node \
  --env NODE_ENV=emulator \
  --env BENCH_ASSERT_PHASE2_LEAK_SOAK=1 \
  --env BENCH_PHASE2_POOL_SIZE=6 \
  --env BENCH_PHASE2_CHUNK_SIZE=64 \
  --env BENCH_PHASE2_BATCH_SIZE=512 \
  --env BENCH_PHASE2_TARGET_TPS=2500 \
  --env BENCH_PHASE2_STEADY_STATE_WARMUP_MS=300000 \
  --env BENCH_PHASE2_DURATION_MS=86400000 \
  --env BENCH_PHASE2_NODE_IMAGE="$NODE_IMAGE" \
  --env BENCH_PHASE2_NODE_IMAGE_ID="$NODE_IMAGE_ID" \
  --env BENCH_PHASE2_OUTPUT_PATH=/workspace/demo/midgard-node/tests/benchmarks/output/phase2-leak-soak.json \
  "$NODE_IMAGE" \
  bash -lc 'corepack enable && pnpm run bench:validation:workers'
node demo/midgard-node/scripts/verify-phase2-benchmark-report.mjs leak-soak \
  demo/midgard-node/tests/benchmarks/output/phase2-leak-soak.json
```

After all artifacts pass, remove the ephemeral benchmark environment:

```bash
docker rm --force "$PHASE2_PG"
docker network rm "$PHASE2_NETWORK"
```
