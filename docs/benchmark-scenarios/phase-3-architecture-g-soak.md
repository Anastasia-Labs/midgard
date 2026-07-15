# Phase 3 Architecture G 24-hour live soak gate

This is the formal live-soak closure gate for Throughput Phase 3. It observes
one already initialized, production-shaped `MPF_ENGINE=architecture_g`
deployment while the canonical L2 workload runs for exactly 86,400 measured
seconds at exactly 5,000 offered and accepted-target TPS. The wrapper pins the
achieved-rate policy to offered rate `>= 4,900/s` (98%), accepted rate
`>= 4,950/s` (99%), and offered/accepted saturation ratio `>= 1.0`; inherited
environment values cannot weaken those floors. A shorter run, a
different target rate, a Phase 2 leak report, or Phase 4 one-hour evidence
cannot satisfy this gate.

The runner is
`demo/midgard-node/scripts/phase3-architecture-g-soak.mjs`; the independent
offline verifier is
`demo/midgard-node/scripts/verify-phase3-architecture-g-soak-report.mjs`.
Neither script claims a soak unless the workload exits zero after the full
duration and every required sample is present.

## External capacity blocker

The exact 5,000 TPS day is not runnable from the retained 3,063,808-row corpus
or its present funding. Including the harness's required 2% row headroom, the
gate needs 440,640,000 dependency-valid rows. Current sizing projects roughly
1.225 TB for the corpus and its shards plus 138.843 GB of submit-record
evidence, so reserve at least 2 TB of fast local NVMe before starting. The
generator assigns one uniform depth to all 4,096 wallets, so the minimum depth
is 107,579 and the generated corpus is 440,643,584 rows (3,584 rows above the
gate minimum). The current fee/amount model also requires exactly
1,614.86837 test ADA per chain, or 6,614,500.84352 test ADA total. These are
external storage and Preprod-funding prerequisites; reducing duration, target
rate, corpus cardinality, evidence retention, or wallet independence does not
close the formal gate.

The 2026-07-14 audit host is a hard no-go for the production run. `/dev/sdd`
has 1,372,805,320,704 bytes total and 148,776,550,400 bytes free, so the volume
is smaller than the 2 TB contract before accounting for its current contents.
The retained Phase 1 binding records 45,990.825984 test ADA, leaving a
6,568,510.017536 test ADA shortfall against the generator requirement. CPU
affinity `28-31` is available and the host's 66,177,814,528 bytes of RAM is
sufficient for the separately scheduled 8 GiB V8 / 10-12 GiB fixture work;
disk capacity and independently verified Preprod funding remain the blockers.

## Preconditions and bound identities

Run only after the Phase 3 differential, crash/recovery, retained-growth, 50k,
release-image, and clean live-E2E gates have passed on the same source and
deployment. The invocation binds all of the following into `report.json`:

- git commit, dirty-status digest, tracked-diff digest, complete source-tree
  digest, and runner/verifier digests;
- the captured runtime fingerprint and its node image;
- the finalized deployment manifest and `manifestId`;
- the exact pinned Architecture G owner executable and its SHA-256 manifest;
- the exact Phase 1 live-corpus binding, including deployment, image, and
  container identities plus the corpus/index/manifest absolute paths,
  SHA-256 values, and selected slice; and
- the workload script and the resulting throughput report.

The runtime fingerprint's deployment SHA and image must match the supplied
deployment and Phase 1 binding before the workload starts. Before the soak
lifecycle clock begins, the runner creates `corpus-preflight.json` next to the
other run evidence. That immutable artifact binds the complete source
identity, Phase 1 binding, corpus/index/manifest paths, SHA-256 values,
size/mtime/device/inode snapshots, manifest cardinality, the selected-index
digest, and exact full-corpus transaction-hash and selected-input uniqueness
counts. The full validation uses external-sort spools on the soak output
volume, not the system temporary volume, and removes them when validation
finishes.

Only after that artifact exists, the source identity is unchanged, the exact
Phase 1 container is measured, and an initial readiness/metrics/database probe
passes does the runner set `startedAtMs`, take the initial lifecycle sample,
and spawn the measured workload. The preflight retains the successful initial
readiness observation and requires its timestamp to precede both preflight
completion and the lifecycle clock. The workload verifies the artifact,
source and Phase 1 identities,
file snapshots, the small index/manifest hashes, full cardinality, and selected
slice digest. It does not repeat the multi-hour full-corpus hash/uniqueness scan
inside the measured lifecycle. Each logical dequeue advances one bounded
per-chain prefix count and digest over the chain id, row index, transaction id,
canonical-body digest, and exact NDJSON-row digest; physical retries do not
advance it. The wrapper repeats the cheap file-identity check after the
workload exits. The offline verifier replaces its former plain corpus hash with
one streaming pass that both re-hashes the complete corpus and recomputes all
consumed prefixes. This catches an in-place, same-size rewrite even if its
mtime is restored and the original bytes return before offline verification.
Missing, reduced, stale, relocated, rewritten, or tampered evidence fails
closed.

Before the lifecycle clock starts, the wrapper also creates
`load-generator-isolation.json`. The runner must already be a non-root process
in a finite-memory cgroup-v2 scope whose effective cpuset equals its process
affinity and is disjoint from the node cgroup's effective cpuset. It parses and
retains all four `/proc/self/status` `Uid` values and rejects effective UID
zero. The artifact binds both process start ticks, executable paths, command
line SHA-256 values, cgroup paths, `cpu.max`, `memory.max`, effective cpusets,
PID namespaces, boot id, and the shared-kernel zero clock offset.

There is no caller-supplied node PID or Docker target. The runner rejects
`DOCKER_HOST`, `DOCKER_CONTEXT`, and `DOCKER_CONFIG`, requires the first
executable `docker` in caller `PATH` to realpath to `/usr/bin/docker`, then
executes that trusted realpath directly with a sanitized `PATH`, nonexistent
home/config directory, and the explicit local
`unix:///var/run/docker.sock` endpoint. It binds the client bytes/SHA/stat,
realpathed Unix-socket device/inode/mode/owner, and the connected Linux daemon
identity. A PATH shim, remote daemon/context, caller Docker config, non-socket,
changed client, replaced socket, or changed daemon fails closed.

Using only that client/daemon, the runner invokes `docker inspect` on the exact
64-hex `nodeContainerId` in the SHA-bound Phase 1 artifact, requires the
inspected ID and immutable image ID to match Phase 1, requires a running,
healthy `MPF_ENGINE=architecture_g` runtime, and takes only its inspected
`State.Pid` as the host PID. It proves that the exact readiness and metrics
loopback URLs are unique published TCP ports of that same container and that
its Docker healthcheck names `/readyz`. Every `/proc` identity capture reads
the process start tick before and after all other fields and rejects a change.
Docker inspection runs again after those captures and must retain exact
container ID, image, PID, start time, restart count, health, and port mappings.

After the initial readiness/metrics/database observation, but before the
lifecycle timestamp exists, the wrapper repeats the trusted Docker/daemon,
container, coordinator, and node-process captures on both sides of the process
reads and retains `node-pre-lifecycle-revalidation.json`. The offline verifier
SHA-checks both immutable artifacts, revalidates the local client/socket bytes,
and cross-checks them against Phase 1 and the report instead of trusting an
operator assertion. The completed preflight observation becomes lifecycle
sample zero without another network call; its true pre-lifecycle timestamp is
retained with measured elapsed zero. Thus readiness/metrics failure cannot be
misclassified after lifecycle start. A changed host PID/start tick is an
unplanned node restart; the readiness response supplies native owner
diagnostics, and a changed owner `childRestarts` value is an unplanned owner
restart.

The PostgreSQL variables are read from the environment but never written to
the report. Each sample queries `mpf_engine_state` and requires both sticky and
latest audit divergence to be false and the last completed audit to be no more
than six hours plus one cadence allowance old.

## Exact operator command

Run from `demo/midgard-node` with Node `v22.22.2`. All artifact paths must be
absolute and the output directory must not already exist. Source the deployed
node's environment first so `POSTGRES_HOST`, `POSTGRES_PORT`, `POSTGRES_USER`,
`POSTGRES_PASSWORD`, and `POSTGRES_DB` are available without placing secrets
on the command line.

```bash
set -euo pipefail
test "$(node -p 'process.version')" = "v22.22.2"
unset DOCKER_HOST DOCKER_CONTEXT DOCKER_CONFIG
test "$(command -v docker)" = /usr/bin/docker
test -S /var/run/docker.sock

export SOAK_OUT="$PWD/logs/phase-3-architecture-g-soak-$(date -u +%Y%m%dT%H%M%SZ)"
export PHASE3_SOAK_FAILURE_OUT_DIR="$SOAK_OUT.setup-failure"
export RUNTIME_FINGERPRINT=/absolute/path/to/phase4-environment-fingerprint.json
export DEPLOYMENT_MANIFEST=/absolute/path/to/contract-deployment-info.json
export PHASE1_BINDING=/absolute/path/to/phase1-formal-binding.json
export OWNER_BINARY=/absolute/path/to/release-image/architecture-g-owner
export OWNER_SHA256_MANIFEST=/absolute/path/to/release-image/architecture-g-owner.sha256
export READY_URL=http://127.0.0.1:3000/readyz
export METRICS_URL=http://127.0.0.1:9464/metrics
export STRESS_LOAD_GENERATOR_PLACEMENT=measured-cgroup
export STRESS_LOADGEN_COHOSTED=true
export STRESS_CLOCK_OFFSET_MS=0

# Enter a dedicated cgroup-v2 scope before this command, configured with a
# finite MemoryMax and a cpuset disjoint from the node (for example via
# systemd) and run it as a non-root effective UID. The scope must retain
# `/usr/bin/docker`, local `/var/run/docker.sock`, and host /proc visibility so the Phase 1 container's
# inspected State.Pid and both cgroup directories remain readable. Root UID,
# root cgroup, memory.max=max, hidden node PID, overlapping cpusets, or
# taskset-only affinity fails preflight.
SELF_CGROUP="$(awk -F: '$1 == "0" {print $3}' /proc/self/cgroup)"
test "$SELF_CGROUP" != /
test "$(cat "/sys/fs/cgroup$SELF_CGROUP/memory.max")" != max
test "$(awk '/^Uid:/ {print $3}' /proc/self/status)" -ne 0

pnpm run gate:phase3:architecture-g-soak -- \
  --out-dir "$SOAK_OUT" \
  --runtime-fingerprint "$RUNTIME_FINGERPRINT" \
  --deployment-manifest "$DEPLOYMENT_MANIFEST" \
  --phase1-binding "$PHASE1_BINDING" \
  --owner-binary "$OWNER_BINARY" \
  --owner-binary-sha256-manifest "$OWNER_SHA256_MANIFEST" \
  --ready-url "$READY_URL" \
  --metrics-url "$METRICS_URL" \
  --workload-script "$PWD/scripts/throughput-valid-stress.mjs"

pnpm run verify:phase3:architecture-g-soak -- "$SOAK_OUT/report.json"
```

The workload inherits the named, manifest-verified Phase 1 corpus variables.
The wrapper measures and pins the formal load-generator scope described above;
operator declarations alone cannot select or weaken it. The wrapper overrides
the immutable gate fields: scenario,
class B, formal mode, open-loop mode, exact 86,400-second measured duration,
commit+merge observation, and fresh report/event/submit-record paths inside the
output directory. It also overrides both accepted-target and open-loop rates to
exactly `5000`; the runner and offline verifier reject any report or primary
stage that records another target. They also reject threshold-policy drift,
achieved rates below the three fixed floors, a primary measured interval below
86,400 seconds, or more than 90 seconds of unexplained measured overrun. The
offline verifier independently re-summarizes the SHA-bound workload report; it
does not trust the copied summary in the soak report.

Warmup transactions and seconds are pinned to zero so submit-record
cardinality covers the primary stage exactly. Cooldown is also zero, and the
workload drain timeout is pinned to 600 seconds. From process spawn through
exit, the complete measured stage plus setup/drain/report tail may not exceed
the measured duration by more than the fixed 15-minute lifecycle grace. The
full-corpus validation is intentionally before both process spawn and the
lifecycle clock; it is still SHA-bound evidence, but it cannot consume that
grace period.

## Report contract and pass conditions

The runner takes its first sample before spawning the workload, samples every
60 seconds while the workload is alive, and takes a final sample after the
workload process closes. This encloses setup, the complete measured stage, and
the workload's commit/merge drain rather than stopping observation at the
nominal 24-hour boundary. The report binds spawn/exit and first/last sample
timestamps; the primary stage start/end plus drain duration must fit inside
that observed process lifecycle. Adjacent elapsed and wall-clock observations
must both be monotone and no more than 90 seconds apart. Missing, non-finite,
stale, reset, or malformed data fails closed. `/readyz` must return HTTP 200
with `ready=true` and no reasons at every observation.

The gate requires:

- MPF audit divergence `0` at every sample with audit evidence no older than
  six hours plus 90 seconds;
- unplanned `confirmed_ledger_full_scan_total` delta `0`: the verifier binds
  each monotone `last_audit_at` advance to exactly one expected background
  audit scan, then requires the counter delta minus those required audit scans
  to equal zero (an extra or unpaired scan fails);
- cumulative validation-job plus L1-control-plane timeout delta `0`, and zero
  timeout-bearing submit records. Every streamed NDJSON record must have the
  exact submit-attempt schema and a lowercase transaction hash; the evidence
  binds absolute path, file SHA-256, byte count, success/error/total
  cardinality, timeout count, and an ordered transaction-attempt identity
  digest. Total records must equal the workload's explicit logical attempt
  count, split exactly into reported successes and errors; physical retry
  attempts are separately required to be no smaller than logical attempts;
- least-squares slopes `<= 0` for `da_publish_reconciler_backlog` and
  `blocks_in_queue`;
- owner resident nodes `<= 2,000,000`, resident/RSS/peak RSS each `<= 2 GiB`,
  generated closure `<= 1,000,000` nodes and `<= 1 GiB`;
- no node PID/start-tick change, no owner child-restart delta, no cap breach,
  and continuous readiness; and
- final node-process RSS growth strictly below 10% relative to the initial
  sample over the exact day.

The workload report must have zero submit errors and rejected delta, no
missing required metrics, one exact primary stage passing the fixed achieved
rate policy, all drains complete, and a zero exit status after the duration.
An early exit, signal, missing report, incomplete drain, copied-summary drift,
or submit-record cardinality/identity drift fails the gate.

Failures before traffic retain `report.json` plus an explicit failed
`verification.json`, labeled with the exact output-directory, timing, runtime,
endpoint-arguments, arguments, closure-identity, corpus-preflight,
source-revalidation, load-generator-isolation, initial-readiness, or
node-pre-lifecycle-reinspection phase. When a valid fresh `--out-dir` was accepted, evidence
is written there. If output-directory parsing itself fails, the runner uses
the fresh absolute `PHASE3_SOAK_FAILURE_OUT_DIR` when supplied, otherwise a
fresh path under the system temporary directory, and prints that retained path
to stderr. `startedAtMs` stays null through corpus validation, measured
isolation, and initial readiness. A failed setup never becomes soak evidence,
but it remains structured and reviewable instead of existing only on stderr.

## Test-only timing path

Unit/integration tests may shorten the sampler only when both
`NODE_ENV=test` and `PHASE3_SOAK_TEST_MODE=1` are present. The only accepted
overrides are `PHASE3_SOAK_TEST_DURATION_SEC` (1–300 seconds) and
`PHASE3_SOAK_TEST_SAMPLE_INTERVAL_MS` (1–5,000 ms). Supplying either override
outside those two guards is an error. A report produced by this path carries
`testOnly: true`; the normal offline verifier always rejects it as production
evidence.

No 24-hour run is recorded by this document. The Phase 3 soak remains open
until the exact command above produces a passing production report on the
bound clean deployment.
