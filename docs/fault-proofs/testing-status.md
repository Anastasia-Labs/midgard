# Fault-Proof Testing Status

Current test inventory reviewed against the working tree on 2026-09-01.

## Inventory

| Surface                                                      | Current inventory | Judgement                                              |
| ------------------------------------------------------------ | ----------------: | ------------------------------------------------------ |
| Aiken fault-proof test declarations under validator families |               765 | Broad; family and shared-boundary depth varies         |
| TypeScript test files in `demo/midgard-fault-proofs/tests`   |               170 | Broad unit, workflow, and emulator coverage            |
| `submit-init-emulator*.test.ts` files                        |                82 | Broad Lucid Evolution coverage; remaining family gaps  |
| Catalogue categories                                         |                32 | All compiled and registered                            |
| Production workflow runner factories                         |                25 | Library runtime incomplete for seven categories        |
| Watcher-installed workflow categories                        |                25 | Autonomous application incomplete for seven categories |

The generated testnet blueprint contains 563 validators and has SHA-256
`b885c3abb0eeaace296011a108fbe4a06d0e5303bfb9d73bbec48fc30f32f9de`.
The inspection suite pins catalogue root
`85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90`.

## Fidelity

| Level                              | Status                    | What it establishes                                                                                                                  |
| ---------------------------------- | ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Aiken unit/property                | Broad                     | Family predicates, exact successor binding, cancellation, maximum frontiers, shared machinery, removal, and protocol controls        |
| TypeScript unit                    | Broad                     | Codecs, evidence, retained-DA replay, production artifacts, journals, runner admission, funding, reconciliation, and classifiers     |
| Lucid Evolution                    | Family-specific blockers  | Shared state-queue setup fits; native-script-invalid is green, while missing-native-script-UTxO and min-ADA remain incomplete        |
| Real-node/cross-process            | Partial adjacent coverage | DA and correction components have focused integration, but no complete fault proof is driven across independent production processes |
| Preprod                            | Missing current artifact  | No reproducible proof-through-removal artifact is bound to the current 32-category identity                                          |
| Autonomous detect → prove → remove | Incomplete                | Watcher application installs 25/32 categories; no complete release acceptance artifact exists                                        |
| Van Rossem resource admission      | Enforced, partially green | Shared harness pins 16,384 bytes, 16.5M memory, and 10B CPU; state-queue publication and exercised lifecycle transactions obey them  |

## Exact emulator status

Dedicated standalone lifecycle tests now cover `missingNativeScriptUtxo`,
`nativeScriptInvalid`, and both `minAda` polarities. Fabricated deposit and
withdrawal now drive removal, and value-not-preserved and mint-authorization
drive cancellation/resume.

The shared harness is pinned to Van Rossem's 16,384-byte transaction limit,
16,500,000 memory units, and 10,000,000,000 CPU steps. State-queue setup now
uses five authenticated withdraw-zero rewarding scripts for commit,
unattested-timeout removal, unavailable-timeout removal, fraud removal, and
merge. The 5,222-byte applied minting policy publishes in 5,498 bytes; the
5,652–8,347-byte rewarding scripts publish in 6,161–8,842 bytes. A focused
admission test publishes all six under the shared limit.

The direct native-script-invalid lifecycle passes. Missing-native-script-UTxO
passes setup and currently fails at proof submission validation. Both min-ADA
polarities pass setup and stop at their own 28,658-byte
`fraudProofMinAdaStep02` script, whose publication transaction is 28,727 bytes.

The native-script-invalid maximum frontier also remains above the execution
budget: the direct 29-witness path and staged 33-witness path do not submit.

## Verification commands

```bash
# Aiken source and generated blueprint
cd onchain/aiken
aiken fmt --check
aiken check
aiken build --env testnet

# Fault-proof package
pnpm --dir demo/midgard-fault-proofs typecheck
pnpm --dir demo/midgard-fault-proofs test

# Shared codecs and validation
pnpm --dir demo/midgard-core test
pnpm --dir demo/midgard-sdk test
pnpm --dir demo/midgard-validation test

# Watcher, node, and DA
pnpm --dir demo/midgard-watcher test
pnpm --dir demo/midgard-node test
pnpm --dir demo/da-committee-node test
```

The Aiken project must use the repository-pinned fork and the `testnet`
environment. The legacy Plutarch/Haskell helper suites remain manual unless
their dependencies are retired with replacement evidence.

## CI

Primary Aiken formatting/check/build and the core, SDK, validation,
fault-proof, Lucid, node, watcher, and DA package checks are CI-wired. CI does
not currently establish:

- a clean public deployment;
- a full rollback/restart/concurrent-correction matrix;
- all 32 watcher installations;
- green lifecycle tests for every category under the shared Van Rossem limits;
- preprod proof-through-removal acceptance.

## Last focused verification

On 2026-09-01:

- `pnpm --dir demo/midgard-fault-proofs typecheck` passed;
- the Van Rossem limit-pin regression passed (1/1);
- state-queue mint plus all five authenticated rewarding-script publications
  passed under the shared Van Rossem limit (1/1);
- the real commit and fraud-removal idempotency lifecycles passed (2/2);
- the direct native-script-invalid lifecycle passed (1/1);
- both min-ADA polarities failed at `fraudProofMinAdaStep02` publication
  (28,727 > 16,384), and missing-native-script-UTxO failed at proof submission;
- 42 distinct focused tests passed across native-script evidence/history,
  production runner admission, catalogue inspection/root binding, SDK category
  registration, and watcher application/launch-scope enforcement.

These focused checks are not a substitute for the complete commands above.
