# Off-Chain Fault-Proof Reference

Current TypeScript/runtime map reviewed against the working tree on 2026-09-01.

## SDK and catalogue

`demo/midgard-sdk/src/fraud-proof/catalogue.ts` is the positional category
authority. It declares 32 categories through:

- `networkId` — `0000001c`
- `missingNativeScriptUtxo` — `0000001d`
- `nativeScriptInvalid` — `0000001e`
- `minAda` — `0000001f`

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

The three newest family packages are:

- `src/missing-native-script-utxo/`
- `src/native-script-invalid/`
- `src/min-ada/`

Each contains contracts, preparation, production-artifact, production-workflow,
and explicit submit modules.

## Production runner factories

`WORKFLOW_RUNNER_FACTORIES` currently exposes 25 categories:

```text
doubleSpend                 nonExistentInput
nonExistentInputNoIndex     invalidRange
zeroInput                   daHashPreimage
noReferenceInput            referenceInputNoIdx
invalidSignature            fabricatedDeposit
fabricatedWithdrawal        withdrawnReferenceInput
canonicalDecodability       committedFieldShape
minFee                      doubleWithdraw
l2TxMistag                  withdrawnInput
missingSignature            missingNativeScriptTx
inputSetUniqueness          networkId
missingNativeScriptUtxo     nativeScriptInvalid
minAda
```

The seven catalogue categories without a shared factory are
`transitionTrace`, `validationTraceDispute`, `nativeScriptDecoding`,
`withdrawalMistag`, `crossBlockDuplicateEvent`, `valueNotPreserved`, and
`mintAuthorization`. They retain family-specific tooling, but do not satisfy
the shared manifest-bound runner-factory interface.

The static adapter registry intentionally records missing readiness until a
compiled application overlays an exact admitted runner. A factory existing in
the library is therefore not the same as an installed production workflow.

## Watcher application

`demo/midgard-watcher/src/fault-proofs/production-fault-proof-application-v1.ts` installs
25 categories:

```text
doubleSpend                 nonExistentInput
nonExistentInputNoIndex     invalidRange
zeroInput                   daHashPreimage
noReferenceInput            referenceInputNoIdx
invalidSignature            fabricatedDeposit
fabricatedWithdrawal        missingSignature
missingNativeScriptTx       withdrawnReferenceInput
canonicalDecodability       committedFieldShape
minFee                      doubleWithdraw
l2TxMistag                  withdrawnInput
inputSetUniqueness          networkId
missingNativeScriptUtxo     nativeScriptInvalid
minAda
```

It does not install:

```text
transitionTrace             validationTraceDispute
nativeScriptDecoding        withdrawalMistag
crossBlockDuplicateEvent    valueNotPreserved
mintAuthorization
```

The watcher proof-thread indexer and deployment identity know the full
catalogue topology. That knowledge does not replace the missing runner
installations.

## Emulator acceptance basis

`tests/support/emulator/protocol-parameters.ts` is the single fault-proof
emulator configuration. It pins Van Rossem's `maxTxSize` to 16,384 bytes,
transaction memory to 16,500,000 units, and transaction CPU to
10,000,000,000 steps. Positive lifecycle tests must submit their real compiled
transactions under those limits; a raised per-test limit is diagnostic only
and cannot establish completion.

## Node and DA integration

- `demo/midgard-node/src/deployment-manifest-v1.ts` binds the complete contract
  and catalogue identity.
- `demo/midgard-node/src/transactions/reference-scripts.ts` publishes the
  required family step scripts.
- `demo/da-committee-node` serves and attests retained `DaPayload` data.
- `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts` derives and
  submits structural correction.
- Node correction services re-include removed transactions and L1 events after
  confirmation.

## Operational boundary

Library modules, classifiers, and unit tests are implemented more broadly than
the production watcher application. Release readiness requires all enabled
families to have:

1. a concrete public retained-DA/L1 authority;
2. an admitted manifest-bound runner;
3. action-specific funding and durable resume;
4. exact reference-script deployment;
5. terminal state reconciliation;
6. emulator, real-node, and preprod acceptance.
