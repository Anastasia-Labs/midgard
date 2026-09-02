# Fault-Proof Catalogue Status

Current inventory reviewed against the working tree on 2026-09-01.

Legend: **L1** means the applied validator chain is compiled and registered;
**off-chain** means family preparation/submit surfaces exist; **emulator**
records dedicated Lucid Evolution family coverage. A row is not accepted until
its lifecycle passes under the shared Van Rossem limits.

| ID         | Category                   |     Applied chain | L1  | Off-chain | Dedicated Lucid family coverage                     |
| ---------- | -------------------------- | ----------------: | :-: | :-------: | --------------------------------------------------- |
| `00000000` | `doubleSpend`              |                 4 | ✅  |    ✅     | ✅ proof and removal                                |
| `00000001` | `nonExistentInput`         |                 4 | ✅  |    ✅     | ✅ proof and removal                                |
| `00000002` | `nonExistentInputNoIndex`  |                 4 | ✅  |    ✅     | ✅ proof and removal                                |
| `00000003` | `invalidRange`             |                 2 | ✅  |    ✅     | ✅ proof and removal                                |
| `00000004` | `transitionTrace`          |  route + 8 finals | ✅  |    ✅     | 🔶 finals/removal pass only with oversized refs     |
| `00000005` | `zeroInput`                |                 2 | ✅  |    ✅     | ✅ proof and removal                                |
| `00000006` | `validationTraceDispute`   | interactive graph | ✅  |    ✅     | 🔶 paths pass; 47 resolver bodies exceed L1 size    |
| `00000007` | `daHashPreimage`           |                 2 | ✅  |    ✅     | ✅ proof and removal                                |
| `00000008` | `noReferenceInput`         |                 4 | ✅  |    ✅     | ✅ proof and removal                                |
| `00000009` | `referenceInputNoIdx`      |                 4 | ✅  |    ✅     | ✅ proof and removal                                |
| `0000000a` | `invalidSignature`         |                 2 | ✅  |    ✅     | ✅ proof and removal                                |
| `0000000b` | `fabricatedDeposit`        |                 4 | ✅  |    ✅     | ✅ proof, permanent mint, and removal               |
| `0000000c` | `fabricatedWithdrawal`     |                 4 | ✅  |    ✅     | ✅ proof, permanent mint, and removal               |
| `0000000d` | `nativeScriptDecoding`     |      6 validators | ✅  |    ✅     | ✅ both directions; removal covered                 |
| `0000000e` | `missingSignature`         |                 4 | ✅  |    ✅     | ✅ frontier, cancel/resume, removal                 |
| `0000000f` | `missingNativeScriptTx`    |                 8 | ✅  |    ✅     | ✅ direct/staged, cancel, removal                   |
| `00000010` | `withdrawnReferenceInput`  |                 3 | ✅  |    ✅     | ✅ proof, negatives, cancel/resume                  |
| `00000011` | `canonicalDecodability`    |                 2 | ✅  |    ✅     | ✅ both fields, cancel/resume, removal              |
| `00000012` | `committedFieldShape`      |                 2 | ✅  |    ✅     | ✅ both polarities and removal                      |
| `00000013` | `minFee`                   |                 2 | ✅  |    ✅     | ✅ both polarities, cancel/resume, removal          |
| `00000014` | `withdrawalMistag`         |                 5 | ✅  |    ✅     | 🔶 both polarities pass; step 03 exceeds L1 size    |
| `00000015` | `doubleWithdraw`           |                 2 | ✅  |    ✅     | ✅ proof, refusal, cancel/resume, removal           |
| `00000016` | `crossBlockDuplicateEvent` |                 2 | ✅  |    ✅     | ✅ both event kinds and removal                     |
| `00000017` | `l2TxMistag`               |                 2 | ✅  |    ✅     | ✅ proof, adversarial refusal, removal              |
| `00000018` | `withdrawnInput`           |                 3 | ✅  |    ✅     | ✅ proof, refusal, cancel/resume, removal           |
| `00000019` | `valueNotPreserved`        |                 4 | ✅  |    ✅     | ✅ ADA/token polarities, cancel/resume, and removal |
| `0000001a` | `inputSetUniqueness`       |                 2 | ✅  |    ✅     | ✅ all duplicate/overlap polarities and removal     |
| `0000001b` | `mintAuthorization`        |                 5 | ✅  |    ✅     | ✅ both directions, cancel/resume, and removal      |
| `0000001c` | `networkId`                |                 2 | ✅  |    ✅     | ✅ proof, honest refusal, cancel, removal           |
| `0000001d` | `missingNativeScriptUtxo`  |                 7 | ✅  |    ✅     | ✅ direct and staged paths, cancel/resume, removal  |
| `0000001e` | `nativeScriptInvalid`      |                 5 | ✅  |    ✅     | ✅ direct, 29/33-signer staged frontiers, removal   |
| `0000001f` | `minAda`                   |      5 + 2 yields | ✅  |    ✅     | ✅ both polarities, cancel/resume, removal          |

`mpf-chunked-proof` is shared verifier machinery and is excluded from the 32
catalogue rows. There are therefore 33 directories directly under
`onchain/aiken/validators/fraud-proofs/`.

## Compiled identity

| Surface                      | Current value                                                      |
| ---------------------------- | ------------------------------------------------------------------ |
| Catalogue size               | 32                                                                 |
| Category range               | `00000000`–`0000001f`                                              |
| Catalogue root               | `85ecf82f70e409621d5324c54ae8e2deedbb7c37698e28ba7d76481c17bb6e90` |
| Testnet blueprint validators | 567                                                                |
| Testnet blueprint SHA-256    | `597c38912123f7f2c167bb73b61c3b37be44cd274be506538ee9bd4437711c96` |

The blueprint values are for the working-tree build of 2026-09-01
(`v1.1.23+5adf783`), and a rebuild with the pinned fork reproduces the same
digest. The inspection suite pins the root above, but it currently fails on deployment-fixture drift
after the reference-script role-NFT change (see
[`testing-status.md`](testing-status.md)), so the pin is not re-verified
against this blueprint.

## Scripts over the L1 transaction size limit

Measured on the reproducible working-tree blueprint (283 distinct compiled
scripts). Production publication refuses any raw body at or above 16,384
bytes (`assertReferenceScriptRawBodiesFitL1EnvelopeV1`), and the signed
publication transaction adds roughly 280 bytes plus 72–73 bytes of applied
parameters, so the practical raw ceiling is about 16,000 bytes.

| Family                   | Scripts over 16,384 raw | Largest                               |
| ------------------------ | ----------------------: | ------------------------------------- |
| `validationTraceDispute` |                      47 | 115,590 (`script_sources_non_output`) |
| `transitionTrace`        |                       2 | 40,869 (`accepted_transaction_v1`)    |
| `withdrawalMistag`       |                       1 | 25,518 (`step_03`)                    |
| availability challenge   |                       1 | 19,927 (20,017 applied)               |
| **Total**                |                  **51** |                                       |

Per-script size-fit plans (split, prune, chain or redesign, plus the
off-chain and emulator work each needs) are indexed in
[size-plans/README.md](size-plans/README.md).

Five more `validationTraceDispute` bodies sit between 16,193 and 16,332 raw
bytes and will not fit once applied and wrapped. The shared harness asserts a
positive L1 byte margin on every reference-script publication unless the
caller passes `oversized: true` (`publishPlainReferenceScriptUtxo` in
`tests/support/emulator/reference-scripts.ts`). The transition-trace and
validation-dispute suites pass it for the entries above, and the
mint-authorization, network-id, value-not-preserved, and withdrawal-mistag
helpers pass it for every step, so those families' publication fit is
unasserted rather than proven; only withdrawal-mistag step 03 among them is
actually over the limit. The three affected rows are therefore not accepted
under the legend above, and none of the three is installed in the production
watcher.

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

The catalogue and all planned validator families are implemented. Twenty-nine
families pass their dedicated Lucid lifecycles under the shared Van Rossem
limits; `validationTraceDispute`, `transitionTrace`, and `withdrawalMistag`
depend on 50 reference scripts that cannot be published on L1, and the
availability challenge is a 51st. The complete system is not yet
release-ready because those scripts must be split or redesigned, and the
all-category maximum-shape emulator sweep, fixture-drift repair, watcher
application, data-lifetime, economics, real-node, and preprod gates remain. Those gates are tracked in
[`execution-plan.md`](execution-plan.md), not as nonexistent proof families.
