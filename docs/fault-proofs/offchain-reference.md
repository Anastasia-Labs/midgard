# Off-Chain Fault-Proof Reference

Status: Active

Last reviewed: 2026-09-07 (catalogue and installation wiring).

Builder and fixture boundary reviewed: 2026-09-19.

## SDK and catalogue

[`catalogue.ts`](../../demo/midgard-sdk/src/fraud-proof/catalogue.ts) owns
explicit category IDs and presentation order. See [catalogue status](catalogue-status.md)
for the checked source inventory; array position is not category identity.

`demo/midgard-sdk/src/common.ts` and
`demo/midgard-sdk/src/fraud-proof/contracts/` carry the matching contract
types. Node/core deployment manifests and reference-script maps use the same
keys.

State-queue operations use five authenticated rewarding-script references:
commit, unattested-timeout removal, unavailable-timeout removal, fraud removal,
and merge. SDK and node transaction builders supply the arm's indexed
reference-script UTxO and exact zero withdrawal; initialization publishes each
script under its dedicated reference-script-auth role token and registers all
five reward accounts.

## Fault-proof package

`demo/midgard-fault-proofs` provides:

- canonical evidence decoding and retained-DA replay;
- deterministic violation classification;
- family preparation and proof artifact construction;
- field publication/certification and reference-script resolution;
- Init, step, cancel, timeout/award, and removal transaction builders;
- durable journals, funding reservations, retry/resume, and reconciliation;
- manifest-bound production runner admission.

Family modules live under `src/` in `demo/midgard-fault-proofs`; use the
catalogue and their exported contracts/preparation/workflow surfaces to locate a
family, rather than a manually maintained "newest families" list.

## Production runners and watcher installation

`WORKFLOW_RUNNER_FACTORIES` in
[`workflow/runtime.ts`](../../demo/midgard-fault-proofs/src/workflow/runtime.ts)
provides manifest-bound runner factories. The
[watcher application](../../demo/midgard-watcher/src/fault-proofs/fault-proof-application.ts)
composes installations and exposes `WATCHER_INSTALLED_WORKFLOW_CATEGORIES` and
`WATCHER_MISSING_WORKFLOW_CATEGORIES`; the latter is currently empty.

The static adapter registry remains unready until an application supplies an
admitted runner. Runtime startup checks proof readiness, durable recovery,
supervision, and deadlines. See the [watcher README](../../demo/midgard-watcher/README.md)
for the operational CLI. Source installation is distinct from public acceptance.

## Emulator acceptance basis

Real proof families in envelope, publication-fit, and emulator tests use the
public `build*FaultProofContracts` SDK builders. Keep low-level chain assembly
internal to the SDK; parameter-sensitivity tests that require it belong there.
Node deployment and the main and seeded-timeout emulator scenarios share
`buildCorrectionLockValidator` and `buildStateQueueValidator`, including all
five queue yields.

The harness reads raw compiler JSON and normalizes it through
`parseFaultProofBlueprint` at the SDK boundary. Its application and identity
adapters delegate to the SDK's strict checks and constructors. Do not reparse
normalized blueprint objects: raw `schema.$ref` and normalized `schemaRef`
are different representations, and losing that metadata weakens shape checks.
Separate production runtime/node application paths remain outside this
test-construction boundary.

Keep test-only scenario composition explicit. The always-succeeds registry
avoids deploying unrelated protocols; its isolated validator prevents scaffold
identity collisions. `alwaysStateQueue` admits malformed headers only where a
focused proof scenario needs to reach its real validator. Seeded state and
valid placeholder policies remain local fixture inputs. Validation-dispute
fixtures start from production traces, use
`encodeValidationTerminalWitnessCbor` for canonical terminal bytes, then
deliberately restamp claims or replace terminal states. These mutations must
not become permissive production trace-builder options.

Source-condition tests cannot establish published package resolution. From
`demo/`, using its declared Node/pnpm toolchain and a prepared testnet
`onchain/aiken/plutus.json`, run:

```sh
pnpm --dir midgard-validation run build
pnpm --dir midgard-sdk run build
pnpm --dir midgard-fault-proofs run check:builder-exports
```

The first build includes core. The export check loads the local built core,
validation, and SDK through ordinary ESM and CommonJS package resolution,
checks a canonical terminal vector, and exercises queue/lock construction and
strict refusal. Core bundles the ESM-only `cborg` dependency so its advertised
CommonJS exports remain usable. After builder changes, run the affected
positive/refusal scenarios and `pnpm test` from `demo/`; also run
`pnpm --dir da-committee-node test`, which the workspace lane runner omits.

`tests/support/emulator/protocol-parameters.ts` is the single fault-proof
emulator configuration. It pins Van Rossem's `maxTxSize` to 16,384 bytes,
transaction memory to 16,500,000 units, and transaction CPU to
10,000,000,000 steps. Positive lifecycle tests must submit their real compiled
transactions under those limits; a raised per-test limit is diagnostic only
and cannot establish completion.

## Node and DA integration

- `demo/midgard-node/src/deployment-manifest.ts` binds the complete contract
  and catalogue identity.
- `demo/midgard-node/src/transactions/reference-scripts.ts` publishes the
  required family step scripts.
- `demo/da-committee-node` serves and attests retained `DaPayload` data.
- `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts` derives and
  submits structural correction.
- Node correction services re-include removed transactions and L1 events after
  confirmation.

## Operational boundary

Release readiness requires all enabled families to have:

1. a concrete public retained-DA/L1 authority;
2. an admitted manifest-bound runner;
3. action-specific funding and durable resume;
4. exact reference-script deployment;
5. terminal state reconciliation;
6. emulator, real-node, and preprod acceptance.
