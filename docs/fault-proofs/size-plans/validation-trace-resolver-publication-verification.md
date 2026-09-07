# Validation-trace resolver publication verification

Measured on 2026-09-07 with `aiken v1.1.23+5adf783`, using the `testnet`
environment and a freshly rebuilt `onchain/aiken/plutus.json`.

- Blueprint SHA-256: `5602f4a9449e39291b17ca393e25b943f8d6d7624e4215f964ad845560671ad0`.
- Blueprint entries: 1,131 validators and 1,844 definitions.
- Signed transaction ceiling: 16,384 bytes; mandatory publication reserve:
  512 bytes; publication target: at most 15,872 bytes.

## Measured publication fit

| Resolver                 | Before, signed bytes | After, signed bytes | Hard-ceiling headroom | Headroom after reserve |
| ------------------------ | -------------------: | ------------------: | --------------------: | ---------------------: |
| `signaturesRequiredItem` |               16,223 |              15,388 |                   996 |                    484 |
| `nativeScriptsNative`    |               16,012 |              15,048 |                 1,336 |                    824 |
| `nativeScriptsEffectful` |               15,756 |              12,797 |                 3,587 |                  3,075 |

All **91** semantic resolvers were parameterized through the SDK, signed,
submitted as reference-script publications in the emulator, and measured.
Every row meets the target. The largest is `signaturesRequiredItem` at
15,388 bytes. Before measurements are the preceding handoff's recorded
measurements; after measurements were collected in this run.

The [91-row ledger](validation-trace-resolver-publication-fit-ledger.json)
is independent of the lifecycle sweep's fixture coverage. The publication test
checks the exact roster count and uniqueness, the reserve for every resolver,
and equality of the complete live ledger with the saved ledger, including
blueprint and compiler identity. `MIDGARD_WRITE_FIT_LEDGER=1` explicitly
regenerates the ledger, after checking all publications fit. Normal runs fail
on missing or changed rows. The existing CI command runs the whole
`midgard-fault-proofs` package, which includes this test without an opt-in flag.

## Implementation and semantic verification

Required-signer items use a fixed-stride field door that preserves field
commitment, carriage authentication, canonical headers, count consistency,
item width, and exact range checks. Variable-width fields are refused by that
entry point. Native and effectful descriptor resolvers compile only their
served branches. A continuation shares the prepared pre-state decode with the
resolution checks. No validator parameters, wire fields, limits, or semantic
predicates were relaxed.

The legacy step-publication waiver now starts at raw script bytes greater
than `PROTOCOL_PARAMETERS_DEFAULT.maxTxSize` (16,384), replacing 14,000.
The catalogue-root snapshot was re-derived by the inspection test's producer
against the fresh blueprint; its four separately pinned step hashes remain
unchanged.

Twelve new field-door scenarios exercise every fixed field, empty fields,
inline/raw/certified carriage (including a chunk-crossing item), wrong
commitments, variable-stride refusal, malformed committed counts and item
wrappers, truncation, and out-of-range reads. Two new descriptor scenarios
compare general and specialized effectful verification in both language
families with honest and forged transitions. The existing native-script
scenario now also checks the specialized native entry and refuses a forged
successor and the wrong resolver family.

## Commands and measured results

Commands below ran from the repository root unless a different working
directory is stated. Passing publication rows are measurements inside tests;
**91 publication rows do not mean 91 Vitest tests**.

### Aiken

Working directory: `onchain/aiken`.

| Command                                                             | Result                                               |
| ------------------------------------------------------------------- | ---------------------------------------------------- |
| `aiken --version`                                                   | `aiken v1.1.23+5adf783`                              |
| `aiken build --env testnet`                                         | Passed, run twice; both produced the SHA-256 above   |
| `aiken check --env testnet --plain-numbers` before additional tests | **4,014 passed / 0 failed**: 3,988 unit, 26 property |
| `aiken check --env testnet --plain-numbers` on the final tree       | **4,028 passed / 0 failed**: 4,002 unit, 26 property |

The first check reconciles exactly with the last-known-good 4,014 count. The
final count increases by the 14 added tests. The earlier receipt's 6,359
claim is not the collected test count for this tree. One build invocation
from the repository root failed to find an Aiken project; it was immediately
rerun in `onchain/aiken` and is not counted as a successful build.

### TypeScript

1. `MIDGARD_REAL_BLUEPRINT_PATH=/home/gumbo/midgard-hub/midgard/onchain/aiken/plutus.json MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs test tests/validation-trace-resolver-publication.test.ts`
   — **1 test passed**, 1 file; all 91 publications measured and ledger written.
2. `MIDGARD_REAL_BLUEPRINT_PATH=/home/gumbo/midgard-hub/midgard/onchain/aiken/plutus.json pnpm --dir demo/midgard-validation test`
   — **473 passed / 4 failed / 2 skipped**, 479 total;
   53 files passed / 3 failed / 1 skipped.
3. `MIDGARD_REAL_BLUEPRINT_PATH=/home/gumbo/midgard-hub/midgard/onchain/aiken/plutus.json pnpm --dir demo/midgard-fault-proofs test tests/validation-trace-resolver-publication.test.ts tests/submit-init-emulator-validation-dispute.test.ts tests/submit-init-emulator-validation-dispute-phase-a-item.test.ts tests/van-rossem-fit-ledger.test.ts`
   — **12 passed / 2 failed**, 14 total, in 3 files. The final filename filter
   matched no file and contributes no tests. The publication test passed in
   normal ledger-comparison mode; all nine phase-A scenarios passed. Both
   failures were dispute fixture script-hash mismatches at `init`.
4. `MIDGARD_PRINT_PROOF_FIT=1 pnpm --dir demo/midgard-fault-proofs test tests/inspect-contracts.test.ts`
   — **11 passed / 1 failed**, 12 total. The producer emitted catalogue root
   `4b423f0493264b33d2a4f8b632de0e1ed4103537249281f1aeb74f6ef3f01cfd`;
   the failure was the old root pin, subsequently updated.
5. `pnpm --dir demo/midgard-fault-proofs test tests/inspect-contracts.test.ts tests/emulator-van-rossem-limits.test.ts`
   — **13 passed / 0 failed**, 2 files, after updating the root pin.
6. `pnpm --dir demo/midgard-fault-proofs typecheck` — passed.
7. With one saved ledger row deliberately removed:
   `MIDGARD_REAL_BLUEPRINT_PATH=/home/gumbo/midgard-hub/midgard/onchain/aiken/plutus.json pnpm --dir demo/midgard-fault-proofs test tests/validation-trace-resolver-publication.test.ts`
   — **1 failed**, intentionally, at `expect(ledger).toEqual(pinned)`.
   The original 91-row file was restored byte-for-byte and checked with `cmp`.
8. `MIDGARD_REAL_BLUEPRINT_PATH=/home/gumbo/midgard-hub/midgard/onchain/aiken/plutus.json MIDGARD_FAULT_PROOF_FORKS=1 pnpm --dir demo/midgard-fault-proofs test tests/validation-trace-resolver-publication.test.ts tests/native-script-invalid-wrongful-rejection-lifecycle.test.ts`
   — **17 passed / 0 failed**, 2 files, after restoring the ledger. This also
   clears the native-script forged-signature scenario that exceeded its
   existing five-second timeout during the broad concurrent run; neither the
   test nor the timeout was changed.

9. `MIDGARD_REAL_BLUEPRINT_PATH=/home/gumbo/midgard-hub/midgard/onchain/aiken/plutus.json pnpm --dir demo/midgard-fault-proofs test`
   — **interrupted; no final suite total available**. Its log is
   `/tmp/midgard-resolver-full-fault-proofs.log`. The publication gate passed.
   Observed failures include existing fixture/roster and evidence-pin assertions,
   general dispute fixture script-hash mismatches at `init`, the subsequently
   corrected catalogue-root pin, and the subsequently passing native-script
   timeout. No aggregate pass/fail count is inferred from partial output.
10. `MIDGARD_REAL_BLUEPRINT_PATH=/home/gumbo/midgard-hub/midgard/onchain/aiken/plutus.json pnpm --dir demo/midgard-validation test`
    outside the sandbox — **476 passed / 1 failed / 2 skipped**, 479 total;
    **55 files passed / 1 failed / 1 skipped**. All three `EPERM` failures
    cleared. The only failure is the existing uncommitted necessity-pin gate
    described below. The missing auxiliary-definition failures remain absent.

The three changed TypeScript files passed ESLint with `--max-warnings=0`
and Prettier `--check`. The first lint attempt found an import-order error in
the new test; `eslint --fix` corrected it before the successful check.
Touched Aiken files were formatted with `aiken fmt` and the repository's
trailing-whitespace normalization. Scoped `git diff --check` passed.

## Auxiliary-witness receipt correction

The historical **18 missing-`ValidationAuxiliaryWitnessV1` failures are
stale**. The regenerated blueprint correctly omits that internal sum type:
it is not exposed in an ABI position. The affected validation tests now use
the frozen 40-arm tag/arity fixture and actual resolver redeemer schemas;
none fails on the missing definition in this run.

The final full validation suite is **not green**: its one remaining failure
is the necessity-pin gate. The initial sandbox run additionally exposed
three environment failures, all cleared by the unrestricted rerun:

- Two `cml-wasm-shadow-stack.test.ts` tests and one
  `plutus-data-unary-depth-boundary.test.ts` test fail on sandbox child-process
  execution (`spawnSync node EPERM`).
- The existing uncommitted necessity gate in `validation-machine.test.ts`
  reports eleven stale pin fields across `ledger-output-incremental-proof-v1.md`,
  `redeemer-collection-total-decode-v1.md`, and `redeemer-item-traversal-v1.md`.
  These include obsolete blueprint/compiler identities and validator/definition
  counts. No measurements were silently re-labelled or assertions weakened.

The [previous implementation receipt](ledger-output-proof-shared-implementation.md)
has been corrected to distinguish these failures from the stale auxiliary
report. Other lanes' uncommitted test and evidence edits were preserved.

## Commit scope and environment

The initial sandbox blocked hook installation and staging with a read-only
`.git` error. After escalation became available, `bash .githooks/install`
succeeded. The commit includes only the 16 task files listed in
`/tmp/midgard-resolver-publication-paths.txt`; the four previously staged
files and other lanes' working-tree edits are excluded.

The broad run regenerated `transition-trace-forced-window-fit-ledger.json`
as a test side effect. Its starting contents were restored; the measured
output was retained separately in `/tmp/midgard-resolver-transition-ledger-test-output.json`.
