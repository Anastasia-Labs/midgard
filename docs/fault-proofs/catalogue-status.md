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
| `00000014` | `withdrawalMistag`         |                 5 | ✅  |    ✅     | ✅ both polarities pass under the L1 fit assertion  |
| `00000015` | `doubleWithdraw`           |                 2 | ✅  |    ✅     | ✅ proof, refusal, cancel/resume, removal           |
| `00000016` | `crossBlockDuplicateEvent` |                 2 | ✅  |    ✅     | ✅ both event kinds and removal                     |
| `00000017` | `l2TxMistag`               |                 2 | ✅  |    ✅     | ✅ proof, adversarial refusal, removal              |
| `00000018` | `withdrawnInput`           |                 3 | ✅  |    ✅     | ✅ proof, refusal, cancel/resume, removal           |
| `00000019` | `valueNotPreserved`        |                 4 | ✅  |    ✅     | ✅ ADA/token polarities, cancel/resume, and removal |
| `0000001a` | `inputSetUniqueness`       |                 2 | ✅  |    ✅     | ✅ all duplicate/overlap polarities and removal     |
| `0000001b` | `mintAuthorization`        |                 5 | ✅  |    ✅     | ✅ both directions, cancel/resume, and removal      |
| `0000001c` | `networkId`                |                 4 | ✅  |    ✅     | ✅ both directions, resumable scan, cancel, removal |
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
| Catalogue root               | `690aee597bc1d432e8cfb7f45cdc27d42259708ce0962110be65c1f5094385e4` |
| Testnet blueprint validators | 807                                                                |
| Testnet blueprint SHA-256    | `1a30174645c166bd33f4060f22debfbac175a8fda053e307ab7a893520b413e9` |

The blueprint values are for the working-tree build of 2026-09-04
(`v1.1.23+5adf783`), and a rebuild with the pinned fork reproduces the same
digest. The inspection suite pins the root above and re-verified it against
this blueprint on 2026-09-04 (see [`testing-status.md`](testing-status.md)).

## Scripts over the L1 transaction size limit

Measured on the reproducible working-tree blueprint (283 distinct compiled
scripts). Production publication refuses any raw body at or above 16,384
bytes (`assertReferenceScriptRawBodiesFitL1Envelope`), and the signed
publication transaction adds roughly 280 bytes plus 72–73 bytes of applied
parameters, so the practical raw ceiling is about 16,000 bytes.

| Family                   | Scripts over 16,384 raw | Largest                               |
| ------------------------ | ----------------------: | ------------------------------------- |
| `validationTraceDispute` |                      47 | 115,590 (`script_sources_non_output`) |
| `transitionTrace`        |                       2 | 40,869 (`accepted_transaction_v1`)    |
| availability challenge   |                       1 | 19,927 (20,017 applied)               |
| **Total**                |                  **50** |                                       |

Per-script size-fit plans (split, prune, chain or redesign, plus the
off-chain and emulator work each needs) are indexed in
[size-plans/README.md](size-plans/README.md).

Five more `validationTraceDispute` bodies sit between 16,193 and 16,332 raw
bytes and will not fit once applied and wrapped. The shared harness asserts a
positive L1 byte margin on every reference-script publication unless the
caller passes `oversized: true` (`publishPlainReferenceScriptUtxo` in
`tests/support/emulator/reference-scripts.ts`). The validation-dispute suite
passes it literally for its unpublishable resolvers, the transition-trace
suites pass it per entry through `oversizedEntryNames`; no other publication
path skips the assertion. `publishFinalFamilyReferenceScripts` enforces the
envelope by default, so the missing-native-script-UTxO, native-script-invalid,
and min-ADA publications are asserted, and the mint-authorization, network-id,
value-not-preserved, and withdrawal-mistag helpers publish without the flag,
so every one of their publications is asserted to fit; withdrawal-mistag
additionally requires 1,024 bytes of headroom, and its step 03 now measures
11,457 raw bytes. The two affected rows are therefore not accepted under the
legend above, and neither is installed in the production watcher.

## Production orchestration

All categories have catalogue, deployment, classifier, and proof-thread
topology representation. That is broader than executable application
installation:

- `WORKFLOW_RUNNER_FACTORIES` exposes 25 categories.
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

The catalogue and all planned validator families are implemented. Thirty
families pass their dedicated Lucid lifecycles under the shared Van Rossem
limits; `validationTraceDispute` and `transitionTrace` depend on 49 reference
scripts that cannot be published on L1, and the availability challenge is a
50th. The complete system is not yet
release-ready because those scripts must be split or redesigned, and the
all-category maximum-shape emulator sweep, fixture-drift repair, watcher
application, data-lifetime, economics, real-node, and preprod gates remain. Those gates are tracked in
[`execution-plan.md`](execution-plan.md), not as nonexistent proof families.
