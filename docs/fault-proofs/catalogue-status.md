# Fault-Proof Catalogue Status

Current inventory reviewed against the working tree on 2026-09-01.

Legend: **L1** means the applied validator chain is compiled and registered;
**off-chain** means family preparation/submit surfaces exist; **emulator**
records dedicated Lucid Evolution family coverage. A row is not accepted until
its lifecycle passes under the shared Van Rossem limits.

| ID         | Category                   |     Applied chain | L1  | Off-chain | Dedicated Lucid family coverage                        |
| ---------- | -------------------------- | ----------------: | :-: | :-------: | ------------------------------------------------------ |
| `00000000` | `doubleSpend`              |                 4 | ✅  |    ✅     | ✅ proof and removal                                   |
| `00000001` | `nonExistentInput`         |                 4 | ✅  |    ✅     | ✅ proof and removal                                   |
| `00000002` | `nonExistentInputNoIndex`  |                 4 | ✅  |    ✅     | ✅ proof and removal                                   |
| `00000003` | `invalidRange`             |                 2 | ✅  |    ✅     | ✅ proof and removal                                   |
| `00000004` | `transitionTrace`          |  route + 8 finals | ✅  |    ✅     | ✅ representative finals and removal                   |
| `00000005` | `zeroInput`                |                 2 | ✅  |    ✅     | ✅ proof and removal                                   |
| `00000006` | `validationTraceDispute`   | interactive graph | ✅  |    ✅     | ✅ bisection/resolution/award paths                    |
| `00000007` | `daHashPreimage`           |                 2 | ✅  |    ✅     | ✅ proof and removal                                   |
| `00000008` | `noReferenceInput`         |                 4 | ✅  |    ✅     | ✅ proof and removal                                   |
| `00000009` | `referenceInputNoIdx`      |                 4 | ✅  |    ✅     | ✅ proof and removal                                   |
| `0000000a` | `invalidSignature`         |                 2 | ✅  |    ✅     | ✅ proof and removal                                   |
| `0000000b` | `fabricatedDeposit`        |                 4 | ✅  |    ✅     | ✅ proof, permanent mint, and removal                  |
| `0000000c` | `fabricatedWithdrawal`     |                 4 | ✅  |    ✅     | ✅ proof, permanent mint, and removal                  |
| `0000000d` | `nativeScriptDecoding`     |      6 validators | ✅  |    ✅     | ✅ both directions; removal covered                    |
| `0000000e` | `missingSignature`         |                 4 | ✅  |    ✅     | ✅ frontier, cancel/resume, removal                    |
| `0000000f` | `missingNativeScriptTx`    |                 8 | ✅  |    ✅     | ✅ direct/staged, cancel, removal                      |
| `00000010` | `withdrawnReferenceInput`  |                 3 | ✅  |    ✅     | ✅ proof, negatives, cancel/resume                     |
| `00000011` | `canonicalDecodability`    |                 2 | ✅  |    ✅     | ✅ both fields, cancel/resume, removal                 |
| `00000012` | `committedFieldShape`      |                 2 | ✅  |    ✅     | ✅ both polarities and removal                         |
| `00000013` | `minFee`                   |                 2 | ✅  |    ✅     | ✅ both polarities, cancel/resume, removal             |
| `00000014` | `withdrawalMistag`         |                 5 | ✅  |    ✅     | ✅ both polarities and removal                         |
| `00000015` | `doubleWithdraw`           |                 2 | ✅  |    ✅     | ✅ proof, refusal, cancel/resume, removal              |
| `00000016` | `crossBlockDuplicateEvent` |                 2 | ✅  |    ✅     | ✅ both event kinds and removal                        |
| `00000017` | `l2TxMistag`               |                 2 | ✅  |    ✅     | ✅ proof, adversarial refusal, removal                 |
| `00000018` | `withdrawnInput`           |                 3 | ✅  |    ✅     | ✅ proof, refusal, cancel/resume, removal              |
| `00000019` | `valueNotPreserved`        |                 4 | ✅  |    ✅     | ✅ ADA/token polarities, cancel/resume, and removal    |
| `0000001a` | `inputSetUniqueness`       |                 2 | ✅  |    ✅     | ✅ all duplicate/overlap polarities and removal        |
| `0000001b` | `mintAuthorization`        |                 5 | ✅  |    ✅     | ✅ both directions, cancel/resume, and removal         |
| `0000001c` | `networkId`                |                 2 | ✅  |    ✅     | ✅ proof, honest refusal, cancel, removal              |
| `0000001d` | `missingNativeScriptUtxo`  |                 7 | ✅  |    ✅     | 🔶 lifecycle present; Van Rossem-limited setup red     |
| `0000001e` | `nativeScriptInvalid`      |                 5 | ✅  |    ✅     | 🔶 lifecycle present; setup red; frontier also limited |
| `0000001f` | `minAda`                   |                 5 | ✅  |    ✅     | 🔶 both lifecycles present; Van Rossem setup red       |

`mpf-chunked-proof` is shared verifier machinery and is excluded from the 32
catalogue rows. There are therefore 33 directories directly under
`onchain/aiken/validators/fraud-proofs/`.

## Compiled identity

| Surface                      | Current value                                                      |
| ---------------------------- | ------------------------------------------------------------------ |
| Catalogue size               | 32                                                                 |
| Category range               | `00000000`–`0000001f`                                              |
| Catalogue root               | `85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90` |
| Testnet blueprint validators | 563                                                                |
| Testnet blueprint SHA-256    | `b885c3abb0eeaace296011a108fbe4a06d0e5303bfb9d73bbec48fc30f32f9de` |

The inspection suite pins and verifies the current root; there is no outstanding
static-root mismatch.

## Production orchestration

All categories have catalogue, deployment, classifier, and proof-thread
topology representation. That is broader than executable application
installation:

- `PRODUCTION_WORKFLOW_RUNNER_FACTORIES_V1` exposes 25 categories.
- The watcher application installs 25 categories.
- Watcher-installed categories are `doubleSpend`, `nonExistentInput`,
  `nonExistentInputNoIndex`, `invalidRange`, `zeroInput`, `daHashPreimage`,
  `noReferenceInput`, `referenceInputNoIdx`, `invalidSignature`,
  `fabricatedDeposit`, `fabricatedWithdrawal`, `missingSignature`,
  `missingNativeScriptTx`, `withdrawnReferenceInput`,
  `canonicalDecodability`, `committedFieldShape`, `minFee`, `doubleWithdraw`,
  `l2TxMistag`, `withdrawnInput`, `inputSetUniqueness`, `networkId`,
  `missingNativeScriptUtxo`, `nativeScriptInvalid`, and `minAda`.
- The seven categories not installed in that watcher application are
  `transitionTrace`, `validationTraceDispute`, `nativeScriptDecoding`,
  `withdrawalMistag`, `crossBlockDuplicateEvent`, `valueNotPreserved`, and
  `mintAuthorization`.

## Completion judgement

The catalogue and all planned validator families are implemented. The complete
system is not yet release-ready because the Van Rossem-limited emulator,
watcher application, data-lifetime, economics, real-node, and preprod gates
remain. Those gates are tracked in [`execution-plan.md`](execution-plan.md),
not as nonexistent proof families.
