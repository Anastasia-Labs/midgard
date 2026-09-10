# Midgard Watcher

Status: Active

Last reviewed: 2026-09-08 (availability lifecycle wiring; not deployment acceptance).

`midgard-watcher` is the independent verifier and challenger. The DA committee
service is a separate package, `demo/da-committee-node`.

## Runtime and commands

From `demo/midgard-watcher`, run `pnpm run build` and `pnpm run native:build`
before invoking the CLI. Configure `nativeChainSyncBinaryPath` to the resulting
`dist/native/midgard-chain-sync` binary:

```sh
export MALLOC_MMAP_THRESHOLD_=131072
node dist/cli.js authority --config /absolute/path/authority.json
node dist/cli.js start --config /absolute/path/watcher-process.json
node dist/cli.js replay --config /absolute/path/watcher-process.json
```

On glibc/Linux hosts, set `MALLOC_MMAP_THRESHOLD_=131072` **before Node starts**.
The `start`, `replay`, and `test` package scripts set this default; direct Node
invocations, service managers, and `pnpm exec vitest` need the same environment.
A fixed 128 KiB mapping threshold lets glibc return freed large snapshot buffers
to the OS. Dynamic threshold growth can otherwise retain gigabytes of unused
native heap during catch-up and make native query process creation progressively
slower. This setting is ignored by allocators that do not implement it. See the
[GNU allocator documentation](https://sourceware.org/glibc/manual/latest/html_node/Memory-Allocation-Tunables.html)
and [Node.js memory guidance](https://nodejs.org/api/process.html#processmemoryusage).

`authority` runs the trusted-head authority until shutdown. `start` constructs
the watcher runtime and runs until SIGINT/SIGTERM. `replay` constructs the same
runtime, waits for durable catch-up, and closes it; it is not an offline or
transport-free command. Both may drive proof and availability workflows.

Startup requires admitted proof runners, recovered workflows, an accepting
supervisor, and safe proof deadlines before emitting `productionReady: true`.
That field describes this runtime's readiness checks, not public-testnet launch
approval. Invalid arguments exit 64; runtime failures fail closed with exit 70.
See [launch readiness](../../docs/public_testnet_readiness.md) and the
[challenger runbook](../../docs/fault-proofs/challenger-runbook.md) for acceptance
and operating boundaries.

Committed rollback snapshots bind completed validation to its schema, policy,
deployment, blueprint, and authentication key. Restart authenticates that saved
result and its independently protected head without replaying retained history.
Changed bindings or unusable persisted state fail closed for explicit recovery.
Forward progress validates new evidence and state transitions before committing.

SQLite progress markers reference shared records in the existing evidence archive.
Unchanged state, decoded events, and raw block bytes are stored once; derived
positional caches are rebuilt when reading cold state. The same transaction
writes newly referenced records and advances the marker before the independent
head is published. Ordinary event-history restart restores its authenticated
semantic validation and corroborates the exact current native head. Full replay
remains an explicit recovery operation.

The prelaunch database format is replaced in place: earlier development databases
must be redeployed, not migrated. Evidence remains pinned by event, recovery, and
proof dependencies; this change does not delete archives. See the
[persistence record inventory](../../docs/midgard/decisions/watcher-persistence.md)
for consumers and retention rules.

## Configuration and trust boundaries

CLI configuration uses `midgard-watcher-production-process-config-v1`; the
separate authority uses `midgard-watcher-trusted-head-authority-process-config-v1`.
The nested watcher configuration uses `midgard-watcher-config-v1`. A nested
configuration by itself is not a complete CLI process configuration.

The exact schemas and validation rules live in
[src/runtime/process-config.ts](src/runtime/process-config.ts) and
[src/runtime/config.ts](src/runtime/config.ts). Unknown/missing fields fail
closed. Process configuration binds deployment identity, durable storage,
trusted-head authority, transports, proof funding, and operational endpoints.
Keep secrets in the supported environment/file references.

Availability actuation requires an independent payment key and a durable journal:

```json
"availability": {
  "keySource": { "kind": "environment", "variable": "WATCHER_AVAILABILITY_KEY" },
  "journalPath": "/var/lib/midgard-watcher/availability.sqlite",
  "minimumFundingLovelace": "20000000000"
}
```

The funding amount is an operator-selected floor, not a protocol constant. Supply
the deployed challenger bond, fees and separate plain-ADA collateral. The actor
prepares the exact bond-plus-open-fee denomination when necessary. Every process
using this payment key must share the same journal. The watcher checks removal
capital while an unanswered challenge is live and rechecks the complete live
queue before timeout takes the correction lock. The timeout references that
queue's tail, so a concurrent append invalidates the quoted removal budget.

Public retrieval failure for an attested header schedules an availability
challenge before fault classification. The actor settles answered or expired
tranches, closes complete responses, and prunes descendants before removing an
unavailable head. Pending availability is not a healthy or faulty classification.
After close, canonical L1 publication history supplies the committed envelope if
the original peers still withhold it. Startup reconciles signed intents before
new actions; rollback revokes actuation immediately. A rollback through finalized
availability state halts the journal and requires authenticated recovery.

The nested wire parser accepts both `local_node` and `external_providers`.
External-provider mode requires independent provider identities. The installed
CLI process parser is narrower: it requires `mode: "acceptance"`,
`targetNetwork: "Preprod"`, and `local_node` authority, with confirmation depth
30, prefinality rollback depth 30, and postfinality recovery bound 2160.
The authority process enforces the same policy.

Local authority binds the Cardano node socket, node/genesis configuration, and
genesis identity; the native chain-sync process provides ordered chain evidence.
Configured query services are subordinate to that authority. Authenticated
rollback recovery and the independent trusted head protect durable replay.
DA retrieval authenticates the configured peer and payload commitments.
Acceptance of a generic nested configuration does not establish support by the
installed process launcher.

## Source and verification map

- [CLI](src/cli.ts) and [command lifecycle](src/runtime/scaffold.ts).
- [Runtime composition](src/runtime/watcher-runtime.ts).
- [Installed proof categories](src/fault-proofs/fault-proof-application.ts):
  `WATCHER_INSTALLED_WORKFLOW_CATEGORIES` is the executable installation scope.
- [Catalogue status](../../docs/fault-proofs/catalogue-status.md): source
  inventory, deployment identity, and acceptance boundaries.
- `pnpm run typecheck`, `pnpm run lint`, and `pnpm test` check this package.

The [verification-boundary decision](../../docs/midgard/decisions/watcher-verification-boundaries.md)
explains the trust and recovery model; the runtime and its tests determine
implemented behavior.
