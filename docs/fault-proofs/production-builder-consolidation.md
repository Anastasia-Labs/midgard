# Production builder consolidation

Baseline: `2bc9bdbcdc2104e79996df05657daffd56724be8`.

## Construction ownership

The SDK's blueprint helper owns unique-title lookup, exact parameter arity,
declared type checks, application caching, and Plutus V3 identity construction.
Parsing an already-normalized blueprint preserves its schema references. The
node parses its blueprint once and calls shared builders with that metadata.
The runtime's raw-size measurement checks uniqueness and its pinned declared
arity, but does not use either deployable-script entry point.

`midgard-sdk/src/protocol-contracts.ts` owns the operator directory, scheduler,
DA governor, DA attestation, availability challenge and yields, settlement,
reserve, payout, fraud-proof catalogue, computation-thread, fraud-proof token,
and shared withdrawal recipes. Computation-thread and token recipes are also
used by the SDK's full fault-proof family construction. Existing user-event,
state-queue, correction-lock, and individual fault-proof recipes remain in their
dedicated SDK modules.

The following runtime family modules now adapt SDK chain builders:

- distinct-asset-accumulation-limit, execution-native-script-invalid,
  execution-source-script-decoding, field-item-width-illegal;
- l2-tx-mistag, mint-declared-asset-limit, mint-item-non-canonical,
  missing-redeemer, missing-script-source;
- observer-order-invalid, observers-forbidden-on-untagged-network,
  output-reference-script-decoding, protected-output-signer-missing;
- receive-purpose-language, redeemer-canonicity, resolved-output-non-canonical,
  script-integrity-hash-mismatch, spend-input-signer-missing;
- transaction-output-non-canonical, unused-redeemer, unused-script-witness,
  witness-script-decoding, zero-input.

## Intentional adapters

- Node configuration, file loading, Effect errors, construction ordering, and
  manifest restoration/verification remain node responsibilities.
- Native reference-script authorization remains separate from Plutus V3
  identity helpers. The hub oracle still uses its mint-policy credential with
  the existing placeholder spending slot.
- Runtime family records retain their existing blueprint titles, placeholder
  reference outrefs, frozen records, and tuple order. Native execution retains
  its non-enumerable accepted prelude. Families without those metadata fields
  do not gain them.
- PHAS reward registration uses the shared zero-arity deployment helper while
  retaining its stricter JSON parser's nonempty-code checks.
- Explicit adversarial emulator adapters remain available. Ordinary fixtures
  continue to use production builders. Funding-wallet publication/deployment
  authority and prover consumption are unchanged.

No contract source, deployment state, manifest schema, registry, watcher
routing, journal, actuator, or submission interface was changed.

## Verification

Pre-refactor digests were captured by executing the original construction
paths. Tests pin all 23 runtime adapters on Preprod and Mainnet and the complete
node Preprod bundle, including bytes, identities, addresses, and metadata.
Additional rejection checks cover duplicate/missing titles, under/over-application,
bare parameterized loads, malformed hashes, metadata retention, and malformed
governor initialization references. Duplicate-title, metadata-retention, and
governor-reference tests were observed failing before their fixes.

Validation uses Node 22.22.2 and pnpm 9.15.4 for the final checks. Early focused
checks also ran on the shell's Node 24.13.1. The workspace runner covers watcher,
fault proofs, validation, core, SDK, Lucid, node, and node tools; it excludes DA
committee tests, which are run separately.

The full workspace run completed successfully once, with 9,326 Vitest tests
passing, 10 skipped, and one todo. The node-tools command also passed 22 Node
runner tests. Separately, DA passed 339 tests with one skip.

| Package                     | Passing Vitest tests | Skips / todos     |
| --------------------------- | -------------------: | ----------------- |
| Watcher                     |                1,481 | 3 skipped         |
| Fault proofs                |                4,111 | 4 skipped         |
| Validation                  |                  462 | —                 |
| Core                        |                  576 | —                 |
| SDK                         |                  576 | —                 |
| Lucid Midgard               |                  175 | —                 |
| Node                        |                1,688 | 3 skipped, 1 todo |
| Node tools                  |                  257 | —                 |
| DA committee (separate run) |                  339 | 1 skipped         |

Commands run successfully from the repository root:

```sh
pnpm --dir demo test
pnpm --dir demo/da-committee-node test
pnpm --dir demo typecheck
pnpm --dir demo lint
pnpm --dir demo format-check
pnpm --dir demo/midgard-core build
pnpm --dir demo/midgard-validation build
pnpm --dir demo/midgard-sdk build
pnpm --dir demo/midgard-fault-proofs build
pnpm --dir demo/midgard-fault-proofs run check:builder-exports
```

The workspace runner's node and node-tools pretest steps also rebuilt their
compiled entrypoints and dependencies. The export check passed both ESM and
CommonJS loading after refreshing stale core build artifacts; its initial run
had failed against an old CommonJS artifact that did not bundle `cborg`.
The initial focused node run required starting the repository's isolated test
Postgres with `bash scripts/start-test-postgres.sh start`.

Focused validation during implementation additionally passed:

- SDK blueprint, user-event, and state-queue tests: 46 tests across three files.
- Fault-proof construction parity, runtime boundary, L2 mistag, and emulator
  boundary tests: 79 tests across four files before the final L2 parity addition.
- Node `midgard-contracts.test.ts`: all eight tests, including the full-bundle
  pre-refactor digest.
- SDK governor-reference, PHAS, and blueprint review-fix tests: 10 tests across
  three files.
- Final runtime production-builder parity/refusal file: all 67 tests.
- Runtime funding-policy checks plus the initial parity file: 26 tests.

The full run includes these cases and the affected positive/negative emulator,
deployment publication, operator lifecycle, availability, reserve/payout, and
fault-proof workflows. The generated transition-trace fit ledger was saved
outside the repository and its tracked copy restored after testing; this
refactor does not update historical size evidence. No live deployment ran.

## Review

Independent Standards and Spec reviews found two actionable issues: the new
public governor builder needed to validate its output reference, and PHAS still
owned duplicate bare-load checks. Both were fixed and re-reviewed with no
unresolved findings. An optional duplication observation about the small runtime
record projections was retained deliberately to keep their differing metadata
contracts explicit.
