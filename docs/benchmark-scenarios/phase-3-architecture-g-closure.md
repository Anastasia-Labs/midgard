# Phase 3 Architecture G operator closure

This runbook closes the three Architecture G operator surfaces that are not
performance or soak measurements: the combined final-tree safety suite, the
release-image boundary, and one clean live functional/recovery run. A pass from
one surface does not substitute for either of the others or for the formal 50k,
growth, and 24-hour gates.

All three reports bind the same source tree, Node `v22.22.2`, Phase 4 runtime
fingerprint, deployment manifest, Phase 1 formal binding, and pinned native
owner binary plus SHA manifest. Paths must be absolute, regular files. Reports
and raw artifacts are created once with atomic replacement and are never
overwritten. Run from `demo/midgard-node` with `TMPDIR=/tmp`.

Define the common paths first:

```bash
set -euo pipefail
export ROOT="$(git rev-parse --show-toplevel)"
cd "$ROOT/demo/midgard-node"
test "$(node --version)" = v22.22.2
export RUN="$PWD/logs/phase-3-architecture-g-closure-$(date -u +%Y%m%dT%H%M%SZ)"
mkdir -p "$RUN"
chmod 700 "$RUN"
export RUNTIME_FINGERPRINT="/absolute/path/to/runtime-fingerprint.json"
export DEPLOYMENT_MANIFEST="/absolute/path/to/contract-deployment-info.json"
export PHASE1_BINDING="/absolute/path/to/phase1-formal-binding.json"
export OWNER_BINARY="$PWD/native/mpf-event-flat-wasm/target/release/architecture-g-owner"
export OWNER_SHA_MANIFEST="/absolute/path/to/architecture-g-owner.sha256"
```

## 1. Combined final-tree crash/recovery gate

Use a disposable local Postgres database. The gate refuses a public database,
an unscoped database name, or a missing authorization token. It runs, in a
fixed order:

1. locked Rust formatting/tests;
2. the seeded legacy/overlay/Architecture G differential;
3. malformed-frame, child SIGKILL/stale-epoch, both atomic-promotion crash
   boundaries, and post-submit replay tests;
4. the database atomic journal/process-kill suite; and
5. the shared Phase 4 planner, lifecycle, recovery, and process-harness suite.

It stops on the first failed command and emits a failed, non-authorizing
report. It never silently omits a suite.

```bash
export MIDGARD_PHASE3_FINAL_TREE=architecture-g-final-tree-isolated-v1
export POSTGRES_HOST=127.0.0.1
export POSTGRES_PORT=5433
export POSTGRES_DB="midgard_phase3_arch_g_final_tree_$(date -u +%Y%m%d%H%M%S)"
PGPASSWORD="${POSTGRES_PASSWORD:?set POSTGRES_PASSWORD}" \
  createdb -h "$POSTGRES_HOST" -p "$POSTGRES_PORT" -U postgres "$POSTGRES_DB"
pnpm gate:phase3:architecture-g:final-tree -- \
  --report "$RUN/final-tree-report.json" \
  --runtime-fingerprint "$RUNTIME_FINGERPRINT" \
  --deployment-manifest "$DEPLOYMENT_MANIFEST" \
  --phase1-formal-binding "$PHASE1_BINDING" \
  --owner-binary "$OWNER_BINARY" \
  --owner-sha256-manifest "$OWNER_SHA_MANIFEST"
pnpm verify:phase3:architecture-g:final-tree -- \
  --report "$RUN/final-tree-report.json"
```

Pass criteria are five exact zero-exit commands, all required coverage labels,
unchanged source identity, and unchanged raw-log hashes under offline
verification.

## 2. Release-image gate

Build the release image through the normal repository Dockerfile, start the
Architecture G node with the exact pinned SHA, and wait for its Docker health
status to become `healthy`. This gate inspects that running container; it does
not build, redeploy, restart, or remove it. The explicit token authorizes only
read-only Docker inspection and `docker exec` probes.

```bash
export RELEASE_IMAGE="midgard-node:phase3-architecture-g"
export RELEASE_CONTAINER="midgard-node-1"
export MIDGARD_PHASE3_RELEASE_IMAGE_INSPECTION=architecture-g-release-image-inspection-v1
pnpm gate:phase3:architecture-g:release-image -- \
  --report "$RUN/release-image-report.json" \
  --image "$RELEASE_IMAGE" \
  --container "$RELEASE_CONTAINER" \
  --runtime-fingerprint "$RUNTIME_FINGERPRINT" \
  --deployment-manifest "$DEPLOYMENT_MANIFEST" \
  --phase1-formal-binding "$PHASE1_BINDING" \
  --owner-binary "$OWNER_BINARY" \
  --owner-sha256-manifest "$OWNER_SHA_MANIFEST"
pnpm verify:phase3:architecture-g:release-image -- \
  --report "$RUN/release-image-report.json"
```

The image passes only when `/app/native` contains exactly
`architecture-g-owner` and `architecture-g-owner.sha256`; the in-image binary
matches the bound SHA, is executable ELF64, has no static symbol table or debug
sections; Cargo, Rust, C/C++ compilers, and `make` are absent; the container
uses the Phase 1-bound image ID; Docker and cgroup limits agree; the limit is at
least the observed V8 heap limit plus 3 GiB; and Docker health plus `/readyz`
are both clean. The exact `--image` reference must appear in the inspected
RepoTags/RepoDigests, must be the reference stored on the running container,
and the in-container runtime must report Node `v22.22.2`. A release binary with
`.symtab` or `.debug*` fails and must be
stripped in the release build before rerunning.

## 3. Clean live E2E and recovery gate

This gate is intentionally deployment-driver based: each state-changing step
is an explicit executable in an immutable command manifest. That keeps network,
wallet, compose project, DA topology, and reset details reviewable instead of
embedding a demo shortcut in production defaults. The orchestrator runs every
step once, sequentially, and stops on the first failure. Drivers must use the
normal production commands and automatic merge fiber; test injection, manual
`/merge`, remote L1 failover, local UPLC disablement, SQL repair, and seed or
signed-CBOR output are forbidden.

The command manifest schema is
`midgard-phase3-architecture-g-clean-live-commands-v1`. It contains the exact
authorization and common binding hashes, followed by these ordered IDs:

```text
fresh-deployment-preflight
deposit-projection
l2-submit
da-attestation
merge-finalization
db-balance
owner-child-restart
post-submit-recovery
final-readiness
```

Each item has `{id, command, args, cwd, timeoutMs}`. `command` and `cwd` are
absolute; commands are executable non-symlink regular files; timeout is between
10 seconds and 3 hours. The orchestrator exports the following to each driver:

```text
PHASE3_ARCH_G_STEP_ID
PHASE3_ARCH_G_STEP_OUTPUT_PATH
PHASE3_ARCH_G_RUNTIME_SHA256
PHASE3_ARCH_G_DEPLOYMENT_SHA256
PHASE3_ARCH_G_PHASE1_SHA256
PHASE3_ARCH_G_OWNER_SHA256
MIDGARD_PHASE3_ARCH_G_E2E=architecture-g-clean-live-e2e-v1
```

The driver atomically creates the requested output with schema
`midgard-phase3-architecture-g-clean-live-step-v1`, its exact `stepId`, the
four exported binding hashes, start/completion milliseconds, and `evidence`.
It must also declare `completed: true` and `verdict: "passed"`; an omitted or
non-pass declaration stops the orchestrator.
Result files are capped at 1 MiB, and sensitive key names, raw/signed CBOR,
seed or mnemonic material, secrets, private values, and oversized strings are
rejected before a result can be copied into the combined report.
Driver stdout and stderr are line-buffered through the same fail-closed
boundary before retention: a sensitive or oversized line is replaced with a
fixed redaction marker, never written verbatim, and increments a bound scan
counter that makes the step fail. Non-sensitive diagnostics remain intact and
the offline verifier requires both retained log artifacts to carry a clean
`midgard-secret-scanned-log-v1` result with zero redactions. Thus a failing
driver can preserve useful diagnostics without allowing a seed, signing key,
password, or raw transaction body into retained evidence.
The verifier enforces the field-level evidence contract: fresh/Kupmios/local
UPLC readiness; confirmed and projected deposit; at least two unique L2
admissions; DA metadata, payload CBOR, watcher status, and attestation hashes;
automatic merge and finalized headers; exact three-address balances and zero DB
residue; real owner `SIGKILL` with a stable Node PID and restart delta one; a
submitted, complete replay journal killed before promotion and recovered to the
candidate marker under a higher owner epoch; and clean final node/DA readiness.

Create the manifest only after the fresh deployment identity and driver scripts
are final, then run:

```bash
export LIVE_COMMANDS="/absolute/path/to/phase3-architecture-g-live-commands.json"
export MIDGARD_PHASE3_ARCH_G_E2E=architecture-g-clean-live-e2e-v1
pnpm gate:phase3:architecture-g:live-e2e -- \
  --report "$RUN/live-e2e-report.json" \
  --commands "$LIVE_COMMANDS" \
  --runtime-fingerprint "$RUNTIME_FINGERPRINT" \
  --deployment-manifest "$DEPLOYMENT_MANIFEST" \
  --phase1-formal-binding "$PHASE1_BINDING" \
  --owner-binary "$OWNER_BINARY" \
  --owner-sha256-manifest "$OWNER_SHA_MANIFEST"
pnpm verify:phase3:architecture-g:live-e2e -- \
  --report "$RUN/live-e2e-report.json"
```

The `owner-child-restart` step proves routine child supervision. The later
`post-submit-recovery` step is separate and must kill the owner after a real L1
submission is durable but before native promotion acknowledgement, then prove
byte-identical replay roots, candidate-marker authority, finalized journal, and
committed L2 status. One restart cannot be relabeled as both gates.

## Focused verifier tests

```bash
pnpm test:mpf:architecture-g-closure
```

The fixtures include a positive report and negative mutations for every
verifier: missing/failed suites, source drift, unstripped/toolchain image,
wrong SHA, insufficient headroom, readiness failure, skipped live steps, DA or
automatic-merge failure, DB residue, and replay-root divergence.
They also include arbitrary submit-record JSON, malformed attempt identity,
logical-attempt cardinality drift, achieved-rate/elapsed-policy drift,
sampling that misses the drain tail, and a sentinel secret that must be absent
from retained driver output.
