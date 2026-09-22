# Delete list (companion to test-quality-audit-2026-09-09.md)

**Status 2026-09-09:** Tier 1 applied except `TransactionProofs.hs` (owner kept
it off the list): 61 files removed via `git rm` (staged, not committed).
Follow-on edits: `demo/midgard-node/package.json` lost `test:operator-lifecycle:preprod`;
`verify-phase3-architecture-g-final-tree-report.mjs` no longer lists the
speculative-commit guard; `generate-resolver-proof-fit-sweep-v1.mjs` now fails
closed because its vitest worker is gone; `docs/fault-proofs/size-plans/README.md`
no longer points at the ledger readers.

Tier 2 applied later the same day for every TypeScript row (13 files `git rm`'d,
staged, not committed); both Haskell rows (`offchain/tests/**`,
`onchain/plutarch/tests/**`) were kept by owner instruction. Follow-on: deleting
the scan-bench emulator test orphaned its orchestrator, so
`scripts/run-scan-bench-evidence-v1.mjs`, `tests/helpers/native-script-scan-exunits-ledger.ts`
and the `test:evidence` / `test:evidence:update` scripts in
`demo/midgard-validation/package.json` were removed with it; comments in
`value-accounting.ts`, `consensus-profile.ts`, `validation-fixtures.ts` and
`phase-b.test.ts` no longer cite `min-ada-twin-cross-check`. The "move first"
items (plutus.json ABI rows into `sdk-aiken-schema-parity`, the min-Ada Aiken
golden, the `satisfies` clause into the src config module) were NOT moved.
`generate-resolver-proof-fit-sweep-v1.mjs` and its two fixture JSONs are now
unreachable and left in place for an owner call.

Tier 3 applied the same day across all listed files, except two rows kept on
purpose: `validation-machine.test.ts` L3028-3200 (the map feeds the live §3.2
necessity-drift gate) and `block-replay.test.ts` L1356-1381 (removal cascades
through ~20 harness locals). Deviations: `assets.test.mjs` counts did not match
the file, so the rule was applied (20 greps cut, 5 process-spawning tests kept);
`spend-input-cardinality` cut only the literal-vs-literal Q1X-F6 loop because
the 74/75 and 195/196 constants drive real journeys; `workflow-runtime` cut the
54-key list without substituting `validateWorkflowAdapterCoverage` (order
differs from the catalogue order; `adapters.ts` already runs it at load).
Removing the `fault-proof.test.ts` early return exposed a stale
`filterBlueprint` allowlist (missing the CEK core/context stage titles), so that
test is now red until the allowlist is refreshed.

Criterion for Tier 1: every assertion in the file is one of (a) a comparison of current output to a hand-maintained copy of that same output, (b) a comparison of two static files to each other, (c) a source-text grep, (d) a decision replaced by a mock that is then asserted, (e) a same-schema round trip, or (f) never executed by any workflow. Deleting the file loses no detection capability that a running test currently provides. Where the file's one real claim is already made by another test or by production code, that is noted.

## Tier 1: delete outright (64 files, ~9,500 lines)

### All 40 `*fit-ledger` tests (4,980 lines)
The lifecycle suites that write these ledgers already assert every margin live (`expectProofFit`, `l1ByteMargin > 0`) and close with `assertCompleteLifecycleCoverage`. A ledger test can only detect a hand-edited JSON or a ledger left stale after a rebuild; 26 of the 40 are either tautological (digest compared to a copy of itself) or red at HEAD (derived digest does not match the built blueprint). If ledger drift must be caught, that is a `--check` generator in CI, not a vitest file. The `docs/fault-proofs/size-plans/*.json` files and the `MIDGARD_WRITE_FIT_LEDGER` writers in the lifecycle suites stay.

```
demo/midgard-fault-proofs/tests/cek-context-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/cek-core-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/cek-selection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/cross-block-workflow-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/distinct-asset-accumulation-limit-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/execution-source-script-decoding-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/field-item-width-illegal-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/field-preimage-length-mismatch-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/input-set-uniqueness-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/invalid-range-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/invalid-signature-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/min-ada-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/min-fee-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/mint-authorization-workflow-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/mint-declared-asset-limit-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/missing-redeemer-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/missing-script-source-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/missing-signature-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/native-script-decoding-workflow-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/native-script-invalid-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/network-id-wrongful-rejection-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/no-reference-input-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/non-existent-input-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/observer-order-invalid-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/observers-forbidden-on-untagged-network-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/output-reference-script-decoding-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/protected-output-signer-missing-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/receive-purpose-language-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/redeemer-canonicity-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/resolved-output-non-canonical-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/script-integrity-hash-missing-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/spend-input-signer-missing-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/transaction-output-non-canonical-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/unused-script-witness-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/value-and-mint-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/value-not-preserved-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/withdrawal-mistag-workflow-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/witness-script-decoding-fit-ledger.test.ts
demo/midgard-fault-proofs/tests/zero-input-wrongful-rejection-fit-ledger.test.ts
```

### Zero-information files (10)
| File | Lines | Why nothing is lost |
|---|---|---|
| `demo/midgard-fault-proofs/tests/execution-native-script-invalid.test.ts` | 47 | exported constant compared to a transcribed copy of itself; version string self-compare; `typeof === "function"` |
| `demo/midgard-node/tests/speculative-commit-safety-guard.test.ts` | 215 | every assertion is a grep over two source files; no production code runs |
| `onchain/plutarch/tests/Testing/TransactionProofs.hs` | 526 | defines no test, not imported by `Test.hs`, only prints to stdout |
| `demo/midgard-fault-proofs/tests/emulator-van-rossem-limits.test.ts` | 17 | `toMatchObject(VAN_ROSSEM_TRANSACTION_LIMITS)` cannot fail (the constant is spread into the object it checks) |
| `demo/midgard-fault-proofs/tests/missing-redeemer-registration.test.ts` | 40 | asserts two exported constants equal transcribed copies; runner built with a throwing loader so nothing executes |
| `demo/da-committee-node/tests/runtime-capability.test.ts` | 18 | `engines.node` compared to a string read from the same package.json; second case asserts only `resolves.toBeUndefined()` |
| `demo/midgard-fault-proofs/tests/transition-trace-output-summary.test.ts` | 59 | two 32-byte roots that appear nowhere else in the repo; current output pinned as a literal |
| `demo/lucid-midgard/tests/api-export-snapshot.test.ts` | 201 | 150-name list transcribed from the package's own exports, compared to a regex scrape of `dist/index.d.ts`; the one real claim (no `export *` from the codec package) is an ESLint `no-restricted-syntax` rule |
| `demo/midgard-node/tests/operator-lifecycle-preprod.test.ts` | 221 | never runs (`MIDGARD_RUN_PREPROD_OPERATOR_LIFECYCLE_TESTS`), and `expectHashOrNull` passes when the tx hash is null, so a run that did nothing is green |
| `demo/midgard-fault-proofs/tests/resolver-proof-fit-sweep-generate.test.ts` | 1,283 | a generator misfiled as a test: `describe.skipIf` on a regeneration flag, zero `expect` calls; move to `demo/midgard-validation/scripts` |

### Same-schema round trips with no vector and no negative (3)
`Data.from(Data.to(x, S), S) === x` cannot fail for any self-consistent schema; sibling ABI files in the same package carry the real Aiken byte vectors.
```
demo/midgard-sdk/tests/native-script-family.test.ts            (146)
demo/midgard-sdk/tests/native-inclusion-carriage-abi.test.ts   (88)
demo/midgard-sdk/tests/no-reference-input-carriage.test.ts     (53)
```

### Mock-verifies-mock (3)
```
demo/midgard-fault-proofs/tests/historical-native-script-preimage.test.ts          (216)  both authority modules vi.mock'ed; two assertions verify the mock's own WeakSet
demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch-manifest-workflow.test.ts (159)  every collaborator mocked; asserts the ORDER of 19 mocked calls
demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch-replay.test.ts       (83)   the retained-DA authentication that computes the hash is stubbed; expected value is an unexplained literal
```

### Verbatim clones of a kept sibling (6, 1,205 lines)
Same seven or three tests with the family name swapped. Keep the named sibling; if the eight `src/*/central-journal.ts` adapters are ever deduplicated, parameterize the one survivor.
```
demo/midgard-fault-proofs/tests/witness-script-decoding-central-journal.test.ts         -> keep spend-input-signer-missing-central-journal
demo/midgard-fault-proofs/tests/output-reference-script-decoding-central-journal.test.ts
demo/midgard-fault-proofs/tests/protected-output-signer-missing-central-journal.test.ts
demo/midgard-fault-proofs/tests/resolved-output-non-canonical-central-journal.test.ts
demo/midgard-fault-proofs/tests/protected-output-signer-missing-authenticated-workflow.test.ts -> keep spend-input-signer-missing-authenticated-workflow
demo/midgard-fault-proofs/tests/resolved-output-non-canonical-authenticated-workflow.test.ts
```

## Tier 2: delete the file after moving one thing (or decide to own it)

| File | Lines | Move first, then delete |
|---|---|---|
| `demo/midgard-validation/tests/complete-item-carriage-policy.test.ts` | 272 | the plutus.json ABI rows (L223-249) into the existing `sdk-aiken-schema-parity` gate; everything else is grep |
| `demo/midgard-validation/tests/min-ada-twin-cross-check.test.ts` | 767 | emit the Aiken min-Ada floor as a `cek-core-step-v1`-style golden; then the in-test Aiken interpreter goes |
| `demo/midgard-validation/tests/resolver-proof-fit-sweep.test.ts` | 360 | default lane only compares two committed JSONs; keep the generator with `--check` in CI |
| `demo/midgard-fault-proofs/tests/observer-order-invalid-authenticated.test.ts`, `observers-forbidden-on-untagged-network-authenticated.test.ts`, `mint-declared-asset-limit-authenticated.test.ts` | 264 | the `satisfies` clause into the src config module (one line); the reconcile cases are covered by the lifecycles |
| `demo/midgard-fault-proofs/tests/execution-native-script-invalid-evidence-machine.test.ts` | 251 | parameterize `native-script-invalid-evidence-machine.test.ts` over both modules, then delete the clone |
| `demo/midgard-fault-proofs/tests/field-preimage-length-mismatch-config.test.ts` | 271 | routing cases assert only which of 25 mocks was called; keep the two resume-refusal cases if they survive un-mocking |
| `demo/da-committee-node/tests/postgres-store.test.ts` | 702 | never runs (`WATCHER_TEST_DATABASE_URL` set nowhere). Wire a Postgres service into the da-committee-node CI job this week or delete; unrun coverage is zero coverage |
| `demo/midgard-node/tests/ed25519-wycheproof.operator.test.ts` | 184 | vendor the 150 vectors and drop the env gate, or delete; today it fetches from GitHub and never runs |
| `demo/midgard-node-tools/tests/phase1-admission-acceptance.operator.test.ts`, `phase1-exact-crash.operator.test.ts` | 1,017 | operator drills that no job runs and that need an 8-core pinned host; give them a scheduled workflow or delete |
| `demo/midgard-validation/tests/native-script-scan-fault-proof-exunits-emulator.test.ts` | 1,070 | 858-second suite behind `MIDGARD_VALIDATION_EVIDENCE`, no workflow; schedule `test:evidence` or delete |
| `offchain/tests/**` (5 files) | 362 | no workflow; sole oracle is `mockchainSucceeds`; no negative case (the one that existed is commented out). The Aiken suite covers these policies. Delete unless someone owns the Haskell offchain |
| `onchain/plutarch/tests/**` (remaining 6 files) | 1,887 | no workflow; `MerklePatriciaForestry.hs` and `Crypto.hs` carry real vectors, `MembershipValidator.hs` is `psucceeds`-only, `ScriptContextBuilder.hs` is an unverified ledger fake. Decide with the plutarch package: wire it or retire it |

## Tier 3: delete these blocks inside files that otherwise stay

These are the assertions the owner ruling targets; the file around them is sound.

| File | Delete |
|---|---|
| `demo/midgard-node/tests/l1-control-plane.test.ts` | L1422-1556, the ~50-assertion source grep over 12 files; the runtime semaphore probes above it already cover the ordering |
| `demo/midgard-node/tests/benchmark-regression.test.mjs` | L761-765 undici version pin; L767-836 three grep tests over `throughput-valid-stress.mjs` |
| `demo/midgard-node/tests/commit-submission-publication-order.test.ts` | L88-117 grep; the L29 loop variable runs the same test twice |
| `demo/midgard-node/tests/commit-worker-failure-lease-classification.test.ts` | L30-61 grep |
| `demo/midgard-node/tests/commit-block-header-worker-output.test.ts` | L257-276 grep |
| `demo/midgard-node/tests/migration-runner.test.ts` | L72-84 checksum recompute; L135-248 nine DDL-substring cases (keep the SQL-splitter cases) |
| `demo/midgard-node/tests/deployment-manifest.test.ts` | L274-277 the 524/517/518/54 length pins (set-equality at L262-270 already covers truncation) |
| `demo/midgard-core/tests/deployment-manifest-identity.test.ts` | L142-186 roster transcription; L397-400 catalogue-root pin; L522-525 manifestId pin (re-pinned five times) |
| `demo/midgard-core/tests/consensus-profile.test.ts` | L16-44 restated profile object; L57-127 and L157-259 ~40 literal measurement constants (keep the relational invariants and the CML re-measurement) |
| `demo/midgard-core/tests/plutus-data-deep-datum-retained.test.ts` | L273-296 sha256/length pins |
| `demo/midgard-core/tests/cek-data-ledger-blob.test.ts` | the 15841 literal and the five-row root table |
| `demo/midgard-fault-proofs/tests/workflow-runtime.test.ts` | L1101-1156 the 54-key factory list (use `validateWorkflowAdapterCoverage` at L1727) |
| `demo/midgard-sdk/tests/fraud-proof-catalogue-registration.test.ts` | L53-108 and L176-220 transcribed order array and id map (keep L110-117) |
| `demo/midgard-sdk/tests/fault-proof.test.ts` | L1409 the 205 hash-count pin; L1356-1361 length-vs-own-constant; **and the L1856 early return** (so the 485-line test actually runs) |
| `demo/midgard-sdk/tests/validation-auxiliary-witness.test.ts` | L59-62 sha256 self-check; L133-190 source greps and hand-listed field names |
| `demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts` | L96-108 hash recompute via the same `applyParamsToScript` call |
| `demo/midgard-validation/tests/validation-controls-abi.test.ts` | L254-260 fixture self-consistency; L370-402 tests of helpers defined in the test file |
| `demo/midgard-validation/tests/validation-machine.test.ts` | L420-446 source substring checks; L3028-3200 hand-maintained scanner-consumer map |
| `demo/midgard-validation/tests/validation-tail-controls-abi.test.ts` | L543-575 retired-identifier grep |
| `demo/midgard-validation/tests/cml-wasm-shadow-stack.test.ts` | L119-120, L154-155 wasm byte-length and sha256 pins, L123-157 `.pnpm` store scan (the 16 MiB stack-pointer read and the depth-4043 parse already prove the property) |
| `demo/midgard-validation/tests/ordered-collection-boundary.test.ts` | L24-50 inline copy of the vector the CI generator already checks |
| `demo/midgard-fault-proofs/tests/missing-signature-envelope.test.ts` | L63-91 `EXPECTED_UNAPPLIED_BYTES` |
| `demo/midgard-fault-proofs/tests/native-script-decoding-envelope.test.ts` | L105-112 size table; L396 the 601-byte pin |
| `demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch.test.ts` | L207-216 local-literal tautology; L261-263 five compiled sizes |
| `demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch-publication-fit.test.ts` | L56 exact-size tuple |
| `demo/midgard-fault-proofs/tests/missing-native-script-tx-envelope.test.ts` | L26-35 eight compiled byte sizes (keep arity, distinct-hash and envelope checks) |
| `demo/midgard-fault-proofs/tests/submit-init-emulator-spend-input-cardinality.test.ts` | L218-221 the 74/75 and 195/196 "boundary" pins (both sides now fit) |
| `demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-*-frontier*.test.ts` (3) | every exact stage byte / execution-unit literal; keep `<= maxTxSize`, `projected == signed`, and the +1 refusal |
| `demo/midgard-fault-proofs/tests/state-queue-yield-publication-admission.test.ts` | L116 `toEqual(pinned)` |
| `demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts` | L143 `toEqual(pinned)` |
| `demo/midgard-fault-proofs/tests/inspect-contracts.test.ts` | L100-105 and L260-261 hash pins plus the 160 lines of re-pin history |
| `demo/midgard-fault-proofs/tests/native-script-invalid-evidence-machine.test.ts` | L131-135 batch constants equal to themselves |
| `demo/midgard-fault-proofs/tests/support/emulator/catalogue-registration.test.ts` | L40 the count 43 |
| `demo/midgard-node/tests/midgard-contracts.test.ts` | L113-146 ~16 title constants vs literal copies; L218-233 step/final counts |
| `demo/midgard-node/tests/da-payload.test.ts` | L514, L533 sha256 via the same function; L501-503 sorted-copy-of-itself |
| `demo/midgard-node/tests/contract-deployment-info.test.ts` | L224-226 digest recompute; L633, L677-685 consensusProfile self-compare |
| `demo/midgard-node/tests/fraud-proof-catalogue.test.ts` | L56 two builds of the same MPF compared |
| `demo/midgard-node/tests/ed25519-verifier.test.ts` | L151-181 RSS bound |
| `demo/midgard-node/tests/initialization-emulator.test.ts` | L268, L279 the 13 / 9 counts |
| `demo/midgard-node/tests/reference-scripts.test.ts` | L159-188 the 24-name ordered list |
| `demo/midgard-watcher/tests/verification/phase-a-verifier.test.ts` | L707-802 vocabulary size pins; L768 prose-length check |
| `demo/midgard-watcher/tests/verification/block-replay.test.ts` | L1264-1266 partition cardinalities; L1277-1303 reimplemented reject-code mapping; L1356-1381 fixture-isolation evidence object |
| `demo/midgard-watcher/tests/runtime/scaffold.test.ts` | L22-42 package.json pin; L44-58 src grep (to lint) |
| `demo/midgard-core/tests/plutus-data-wellformed.test.ts`, `cek-proof.test.ts` | L381-391 and L2306 wall-clock gates (redundant beside structural assertions) |
| `demo/midgard-core/tests/retention-window.test.ts` | L459 `typeof … === "function"` |
| `demo/midgard-fault-proofs/tests/zero-input-wrongful-rejection.test.ts`, `input-set-uniqueness-wrongful-rejection.test.ts` | L164 / L171 `Object.keys({ block: null })` |
| `demo/midgard-node-tools/devnet/phase4-process/tests/assets.test.mjs` | the 14 grep-based tests (keep the six that execute scripts against fixture dirs) |
| `demo/midgard-node-tools/tests/e2e-state-correction-acceptance.test.ts` | L162-167 two assertions that cannot fail |
| `demo/lucid-midgard/tests/safe-program.test.ts`, `documentation-examples.test.ts` | L205-213 and L42-52 forbidden-token greps (to lint) |

## Not on the list, deliberately

- The 14 `*-publication-fit` files: they publish real applied validators on a real emulator against an external 15,872 / 16,384-byte limit. Fix the `describe.runIf` to fail closed; do not delete.
- The ordered-collection boundary files and `mainnet-protocol-parameters.test.ts`: brittle to a dependency bump, but the pinned value is a real ledger boundary and the vector feeds a CI-checked Aiken generator.
- `submit-init-emulator-soundness-honest-operator.test.ts`: deliberately red under #605 with an owner; leave it.
- `kupmios-ogmios-v7-protocol-parameters.test.ts`, `emulator-submit-slot-snapshot.test.ts`, `funding-wallet-emulator.test.ts`: dependency-conformance guards; label them as such, keep them.
