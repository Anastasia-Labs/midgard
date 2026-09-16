# Fault-Proof Catalogue Status

Status: Active

Last reviewed: 2026-09-07 (source inventory and installation scope).

## Source inventory

The 55 source catalogue categories and all 55 watcher installations are listed
below. This is source coverage, not a claim that a particular network has these
contracts deployed or that every acceptance suite passed on this revision.

Category IDs come from `FRAUD_PROOF_CATALOGUE_CATEGORY_IDS` in
[`catalogue.ts`](../../demo/midgard-sdk/src/fraud-proof/catalogue.ts).
`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` is presentation order; do not derive IDs
from array position. Installation comes from `WATCHER_INSTALLED_WORKFLOW_CATEGORIES`
in the [watcher application](../../demo/midgard-watcher/src/fault-proofs/fault-proof-application.ts).
The documentation facts check compares this table to both sources.

| ID         | Category                              | Watcher installed |
| ---------- | ------------------------------------- | ----------------- |
| `00000000` | `doubleSpend`                         | Yes               |
| `00000001` | `nonExistentInput`                    | Yes               |
| `00000002` | `nonExistentInputNoIndex`             | Yes               |
| `00000003` | `invalidRange`                        | Yes               |
| `00000004` | `transitionTrace`                     | Yes               |
| `00000005` | `zeroInput`                           | Yes               |
| `00000006` | `validationTraceDispute`              | Yes               |
| `00000007` | `daHashPreimage`                      | Yes               |
| `00000008` | `noReferenceInput`                    | Yes               |
| `00000009` | `referenceInputNoIdx`                 | Yes               |
| `0000000a` | `invalidSignature`                    | Yes               |
| `0000000b` | `fabricatedDeposit`                   | Yes               |
| `0000000c` | `fabricatedWithdrawal`                | Yes               |
| `0000000d` | `nativeScriptDecoding`                | Yes               |
| `0000000e` | `missingSignature`                    | Yes               |
| `0000000f` | `missingNativeScriptTx`               | Yes               |
| `00000010` | `withdrawnReferenceInput`             | Yes               |
| `00000011` | `canonicalDecodability`               | Yes               |
| `00000012` | `committedFieldShape`                 | Yes               |
| `00000013` | `minFee`                              | Yes               |
| `00000014` | `withdrawalMistag`                    | Yes               |
| `00000015` | `doubleWithdraw`                      | Yes               |
| `00000016` | `crossBlockDuplicateEvent`            | Yes               |
| `00000017` | `l2TxMistag`                          | Yes               |
| `00000018` | `withdrawnInput`                      | Yes               |
| `00000019` | `valueNotPreserved`                   | Yes               |
| `0000001a` | `inputSetUniqueness`                  | Yes               |
| `0000001b` | `mintAuthorization`                   | Yes               |
| `0000001c` | `networkId`                           | Yes               |
| `0000001d` | `missingNativeScriptUtxo`             | Yes               |
| `0000001e` | `nativeScriptInvalid`                 | Yes               |
| `0000001f` | `minAda`                              | Yes               |
| `00000020` | `fieldPreimageLengthMismatch`         | Yes               |
| `00000021` | `fieldItemWidthIllegal`               | Yes               |
| `00000022` | `witnessScriptDecoding`               | Yes               |
| `00000023` | `scriptIntegrityHashMissing`          | Yes               |
| `00000029` | `transactionOutputNonCanonical`       | Yes               |
| `00000026` | `resolvedOutputNonCanonical`          | Yes               |
| `0000002c` | `mintDeclaredAssetLimit`              | Yes               |
| `00000027` | `spendInputSignerMissing`             | Yes               |
| `0000002b` | `protectedOutputSignerMissing`        | Yes               |
| `00000024` | `observersForbiddenOnUntaggedNetwork` | Yes               |
| `00000025` | `observerOrderInvalid`                | Yes               |
| `00000028` | `redeemerCanonicity`                  | Yes               |
| `0000002a` | `outputReferenceScriptDecoding`       | Yes               |
| `00000031` | `executionSourceScriptDecoding`       | Yes               |
| `00000034` | `receivePurposeLanguage`              | Yes               |
| `0000002f` | `unusedScriptWitness`                 | Yes               |
| `0000002d` | `missingScriptSource`                 | Yes               |
| `0000002e` | `missingRedeemer`                     | Yes               |
| `00000030` | `unusedRedeemer`                      | Yes               |
| `00000032` | `executionNativeScriptInvalid`        | Yes               |
| `00000033` | `scriptIntegrityHashMismatch`         | Yes               |
| `00000035` | `distinctAssetAccumulationLimit`      | Yes               |
| `00000036` | `mintItemNonCanonical`                | Yes                |

Shared verifier directories, including `mpf-chunked-proof`, are not additional
catalogue categories. Count categories from the SDK map, not directories or
blueprint entrypoints (multiple entrypoints can share one compiled body).

## Deployment identity

`onchain/aiken/plutus.json` is generated and gitignored. Rebuild with the pinned
compiler and explicit `testnet` environment before blueprint-dependent acceptance;
see the [build skill](../../.agents/skills/aiken-contract-build/SKILL.md).
Record the source revision, compiler, build flags, blueprint digest, applied
script hashes, and catalogue root together in the acceptance/release artifact.
This page deliberately does not maintain a second deployment hash pin.

The deployed manifest and authenticated catalogue determine which applied
scripts a challenger may use. An installation in source does not authenticate
reference-script UTxOs. Changed catalogue identity requires a fresh development
deployment under the prelaunch policy.

## Publication and lifecycle acceptance

The validation/transition resolvers use split/yield implementations. Their
current workflows are described
in [installed validation disputes](validation-trace-dispute-installed-workflow.md)
and [installed transition replay](transition-trace-installed-replay.md).

The [availability challenge size plan](size-plans/availability-challenge.md)
retains the unresolved publication work and its dated measurements. Rebuild and
measure the complete signed publication before declaring that blocker closed.
Raw blueprint size alone does not prove transaction fit.

For each enabled category, acceptance must cover real-blueprint publication,
positive and valid-block negative paths, maximum supported shapes, interruption
and cancellation where applicable, permanent mint, and header removal under the
shared limits. Test presence and installation counts do not substitute for a
retained passing result. See [testing status](testing-status.md),
[remaining acceptance](execution-plan.md), and
[public-testnet readiness](../public_testnet_readiness.md).
The [emulator scheduling measurements](testing-status.md#emulator-gate-performance)
retain the same test cases and do not change category or installation inventory.
