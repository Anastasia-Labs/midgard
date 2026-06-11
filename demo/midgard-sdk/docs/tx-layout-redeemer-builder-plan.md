# Tx Layout Redeemer Builder Plan

## Purpose

Add generic SDK infrastructure for Lucid transaction redeemers that need indexes
from the final transaction layout.

The helper must be production-safe:

- resolve indexes after Lucid coin selection;
- preserve `.complete({ localUPLCEval: true })`;
- fail loudly instead of guessing;
- keep Cardano ledger ordering rules centralized;
- avoid one-off index math in transaction builders;
- stay generic rather than fault-proof-specific.

This replaces the earlier draft/probe transaction idea. The final design is a
Lucid-compatible dynamic `RedeemerBuilder` that resolves from the active
`TxBuilderConfig` during Lucid's existing partial-redeemer phase.

## Non-Goals

Do not:

- call `.config()` on a disposable transaction to pre-resolve indexes;
- patch Lucid Evolution internals for the MVP;
- make CML the primary runtime implementation path;
- support change-output selection in the MVP;
- add certificate, withdrawal, vote, or proposal selectors in the MVP unless a
  current caller needs them;
- add compatibility shims for older local transaction-builder shapes.

## Target API

Create one layout redeemer factory per Lucid `TxBuilder` instance.

```ts
const tx = lucid.newTx();
const layoutRedeemer = createTxLayoutRedeemerFactory({ tx });

const step04SpendRedeemer = layoutRedeemer({
  anchors: [threadUtxo],
  label: "double-spend step 04 spend redeemer",
  build: (layout) =>
    Data.to(
      {
        Continue: [
          {
            input_index: layout.inputs.byOutRef(threadUtxo).index(),
            output_index: layout.outputs.byUnit(fraudProofUnit).index(),
            fraud_proof_mint_redeemer_index: layout.redeemers
              .mintPolicy(fraudProofPolicyId)
              .txInfoIndex(),
            tx2_inputs_preimage: [...tx2Inputs],
            double_spent_input_index: doubleSpentInputIndex,
          },
        ],
      },
      DoubleSpendStep04SpendRedeemer,
    ),
});

const fraudProofMintRedeemer = layoutRedeemer({
  anchors: [threadUtxo],
  label: "fraud-proof token mint redeemer",
  build: (layout) =>
    Data.to(
      {
        computation_thread_token_asset_name: threadTokenAssetName,
        computation_thread_mint_redeemer_index: layout.redeemers
          .mintPolicy(computationThreadPolicyId)
          .txInfoIndex(),
      },
      FraudProofTokenMintRedeemer,
    ),
});

tx.collectFrom([feeInput])
  .collectFrom([threadUtxo], step04SpendRedeemer)
  .mintAssets({ [threadTokenUnit]: -1n }, computationThreadSuccessRedeemer)
  .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
  .pay.ToContract(
    fraudProofAddress,
    { kind: "inline", value: fraudProofDatum },
    fraudProofAssets,
  )
  .attach.SpendingValidator(step04SpendingScript)
  .attach.MintingPolicy(computationThreadMintingScript)
  .attach.MintingPolicy(fraudProofMintingScript);

const unsigned = await tx.complete({ localUPLCEval: true });
```

The names should be generic:

- `createTxLayoutRedeemerFactory`
- `TxLayoutReader`
- `TxLayoutRedeemerBuilder`
- `TxLayoutSnapshot`

Do not expose a global `txLayoutRedeemer`. The factory must be explicitly bound
to one builder.

## Lucid Lifecycle

The helper relies on Lucid Evolution's existing `RedeemerBuilder` timing.

Lucid's relevant flow is:

1. The caller adds ordinary programs to a `TxBuilder`.
2. `.complete({ localUPLCEval: true })` runs normal programs.
3. Lucid performs coin selection and may add wallet inputs.
4. Lucid calls `completePartialPrograms()`.
5. For each dynamic redeemer, Lucid calls `makeRedeemer(...)`.
6. The returned CBOR redeemer is applied to the corresponding partial program.
7. Lucid builds for local UPLC evaluation.
8. Lucid builds the final transaction.

`createTxLayoutRedeemerFactory` must hook into step 5 by returning a stock Lucid
`RedeemerBuilder`:

```ts
{
  kind: "selected",
  inputs: anchors,
  makeRedeemer: (anchorInputIndices) => {
    const config = tx.rawConfig();
    const snapshot = buildTxLayoutSnapshot(config);
    validateAnchors(snapshot, anchors, anchorInputIndices);
    return build(createTxLayoutReader(snapshot));
  },
}
```

This is the correct point because coin selection has already updated
`config.collectedInputs`. It also preserves Lucid's existing safety failure when
coin selection changes again after dynamic redeemers are built.

## Anchor Rules

`anchors` are a Lucid integration requirement, not a domain dependency model.

Rules:

- `anchors` must be non-empty.
- Duplicate anchors are rejected.
- Every anchor must be present in the post-selection collected input set.
- The `anchorInputIndices` passed by Lucid must match the indexes derived by the
  layout reader.
- Anchors do not imply that `build(layout)` uses those inputs.
- Mint-only transactions must explicitly collect at least one stable anchor
  input before using this helper.

For step 04, `threadUtxo` is the natural anchor because it is already collected
by the transaction.

## Layout Snapshot

Build `TxLayoutSnapshot` from Lucid `TxBuilderConfig` at `makeRedeemer` time.

Snapshot fields:

- `inputs`: `config.collectedInputs`, deduplicated only as validation and sorted
  lexicographically by `TxOutRef`.
- `referenceInputs`: `config.readInputs`, sorted lexicographically by
  `TxOutRef`.
- `outputs`: `config.payToOutputs`, preserving authored transaction order.
- `mintPolicies`: effective non-zero mint/burn policies from
  `config.mintedAssets`, grouped by policy id and sorted by policy id bytes.
- `redeemerPointers`: modeled from the current config, not from a completed CML
  transaction.

Ordering rules:

- transaction inputs in script context are sorted by `txHash`, then
  `outputIndex`;
- reference inputs are sorted by `txHash`, then `outputIndex`;
- outputs keep authored order;
- mint policies are sorted by policy id bytes;
- redeemer tx-info order uses purpose rank:

```text
spend < mint < cert < reward < vote < propose
```

## Redeemer Pointer Model

At `makeRedeemer` time, not all dynamic redeemers have been materialized into
CML witnesses. The helper must therefore model redeemer pointers from Lucid
config state.

For the MVP, model:

- Plutus spend redeemers;
- Plutus mint redeemers.

Spend pointer rules:

- Sort all transaction inputs lexicographically by `TxOutRef`.
- The spend pointer index is the target input's sorted input index.
- Include only Plutus script inputs in the modeled spend redeemer set.
- A Plutus script input is one whose payment credential is a script hash and
  whose attached script in `config.scripts` is `PlutusV1`, `PlutusV2`, or
  `PlutusV3`.
- Fail if a queried spend input is missing, duplicated, key-spent,
  native-script-spent, lacks an attached script source, or has no modeled
  Plutus redeemer.

Mint pointer rules:

- Group `config.mintedAssets` by policy id.
- Reject invalid policy ids.
- Reject or ignore zero-net policy groups before exposing them; prefer rejecting
  when a queried policy nets to zero.
- Include burns and mints; both are effective mint entries.
- Require that a queried mint policy is backed by an attached Plutus minting
  policy.
- The mint pointer index is the policy's byte-sorted index among effective mint
  policies.

Tx-info redeemer index rules:

- Construct modeled pointers for supported purposes.
- Sort by purpose rank, then pointer index.
- The tx-info index is the position in that sorted list.
- If an unmodeled purpose with equal or earlier rank could affect a queried
  index, fail instead of guessing.
- Later-rank unsupported purposes do not affect spend or mint tx-info indexes,
  but selectors for those purposes remain unsupported until implemented.

## Reader Surface

MVP reader:

```ts
layout.inputs.byOutRef(utxo).index();
layout.inputs.byScriptAddress(address).indices();
layout.inputs.byPaymentScriptHash(scriptHash).indices();

layout.referenceInputs.byOutRef(utxo).index();
layout.referenceInputs.byScriptAddress(address).indices();

layout.outputs.byUnit(unit).index();
layout.outputs.byAddress(address).indices();
layout.outputs.where(predicate, label).one().index();

layout.mint.policy(policyId).policyIndex();

layout.redeemers.spendInput(utxo).txInfoIndex();
layout.redeemers.mintPolicy(policyId).txInfoIndex();
```

Later reader extensions:

```ts
layout.redeemers.withdrawal(rewardAddress).txInfoIndex();
layout.redeemers.certificate(predicate, label).txInfoIndex();
layout.redeemers.vote(predicate, label).txInfoIndex();
layout.redeemers.proposal(predicate, label).txInfoIndex();
```

Cardinality:

- `index()` means exactly one match and throws otherwise.
- `indices()` returns all matching indexes.
- `optionalIndex()` can be added when a real caller needs zero-or-one behavior.
- `one()` can be used for predicate selectors before calling `index()`.

Selector errors must include:

- selector label;
- expected cardinality;
- actual match count;
- matching outrefs or output indexes when available.

## Output Selector Semantics

`layout.outputs` exposes authored outputs only.

Lucid appends change after authored outputs with `add_change_if_needed(...)`.
The MVP must not expose change output selection because change is not represented
in `config.payToOutputs`.

Rules:

- Authored output indexes are valid transaction output indexes as long as Lucid
  appends change after authored outputs.
- `layout.outputs.byUnit(unit).index()` means the unique authored output
  carrying `unit`.
- `layout.outputs.where(...)` predicates run only against authored outputs.
- Selectors must not claim to inspect or match change outputs.
- If a future validator needs a change output index, add a separate
  final-output model verified against CML before exposing it.

## Module Layout

Add SDK modules:

```text
demo/midgard-sdk/src/tx-layout/order.ts
demo/midgard-sdk/src/tx-layout/snapshot.ts
demo/midgard-sdk/src/tx-layout/reader.ts
demo/midgard-sdk/src/tx-layout/redeemer-builder.ts
demo/midgard-sdk/src/tx-layout/index.ts
```

Export from:

```text
demo/midgard-sdk/src/index.ts
```

Suggested responsibilities:

- `order.ts`: TxOutRef comparison, policy id byte comparison, purpose rank.
- `snapshot.ts`: `TxBuilderConfig` to `TxLayoutSnapshot`.
- `reader.ts`: selector and cardinality APIs.
- `redeemer-builder.ts`: Lucid-compatible `RedeemerBuilder` factory.
- `index.ts`: public exports.

Reuse existing SDK redeemer-order helpers where appropriate, but avoid making
CML transaction witnesses the source of truth for runtime resolution.

## Core Types

Shape the public types around the reader, not named dependency fields.

```ts
export type TxLayoutRedeemerBuilder = {
  readonly anchors: readonly UTxO[];
  readonly label?: string;
  readonly build: (layout: TxLayoutReader) => string;
};

export type TxLayoutSnapshot = {
  readonly inputs: readonly IndexedInput[];
  readonly referenceInputs: readonly IndexedInput[];
  readonly outputs: readonly IndexedOutput[];
  readonly mintPolicies: readonly IndexedMintPolicy[];
  readonly redeemerPointers: readonly ModeledRedeemerPointer[];
};

export type ModeledRedeemerPointer = {
  readonly purpose: "spend" | "mint";
  readonly pointerIndex: bigint;
  readonly txInfoIndex: bigint;
  readonly subject: string;
};
```

The implementation can keep more detailed internal types for selector matches
and diagnostics.

## Guardrails

The helper must throw before returning a redeemer when:

- the factory is unbound or bound to a different `TxBuilder`;
- `anchors` is empty;
- anchors contain duplicate outrefs;
- an anchor is absent from post-selection inputs;
- Lucid-provided anchor indexes disagree with the snapshot;
- collected inputs contain duplicate outrefs;
- reference inputs contain duplicate outrefs;
- `.index()` matches zero or multiple items;
- a queried mint policy has no effective non-zero mint or burn;
- a queried mint policy is not backed by a Plutus policy;
- a queried spend input is not a Plutus script input with a modeled redeemer;
- a queried script source is missing from `config.scripts`;
- an output selector would require inspecting change outputs;
- the queried redeemer purpose is unsupported.

Do not suppress Lucid's existing post-redeemer coin-selection failure:

```text
RedeemerBuilder: Coin selection had to be updated after building redeemers...
```

That failure is correct because another coin-selection pass after dynamic
redeemer construction can invalidate indexes.

## Step 04 Migration

Use step 04 as the first real consumer.

Current issue:

- `demo/midgard-fault-proofs/src/submit-step-04.ts` builds a draft
  transaction, derives layout from the completed draft, then builds the final
  transaction.
- The draft can fail local UPLC evaluation before the correct layout is known.
- The two-pass flow also duplicates transaction-building work.

Migration:

1. Create one `TxBuilder`.
2. Create one layout redeemer factory bound to that builder.
3. Replace `makeStep04SpendRedeemer(...)` with a layout redeemer that queries:
   - `layout.inputs.byOutRef(threadUtxo).index()`;
   - `layout.outputs.byUnit(fraudProofUnit).index()`;
   - `layout.redeemers.mintPolicy(fraudProofPolicyId).txInfoIndex()`.
4. Replace `makeFraudProofMintRedeemer(...)` with a layout redeemer that
   queries:
   - `layout.redeemers.mintPolicy(computationThreadPolicyId).txInfoIndex()`.
5. Keep the computation-thread success redeemer static.
6. Remove the draft `makeStep04Tx(initialLayout).complete(...)` pass.
7. Keep the final `.complete({ localUPLCEval: true })`.
8. Keep result metadata by resolving final indexes from the same reader logic or
   from the completed transaction in a verification-only path.

## Tests

Add SDK unit tests for:

- input index by outref;
- all input indexes by script address;
- all input indexes by payment script hash;
- reference input index by outref;
- all reference input indexes by script address;
- output index by unit;
- output indexes by address;
- output predicate `.one().index()`;
- mint policy index for one policy;
- three mint policies sorted by policy id bytes;
- spend redeemer tx-info index;
- mint redeemer tx-info index;
- mixed spend and mint tx-info ordering;
- one redeemer querying multiple fields inline;
- missing selector failures;
- ambiguous selector failures;
- duplicate collected input failure;
- duplicate reference input failure;
- zero-net mint policy failure for queried policy;
- non-Plutus mint policy query failure;
- non-Plutus spend input query failure;
- anchor missing failure;
- anchor index disagreement failure.

Add integration coverage for:

- step 04 using dynamic layout redeemers with no draft-complete pass;
- wallet balancing input changing the thread input's sorted input index;
- computation-thread and fraud-proof mint policies ordered differently from
  authoring order;
- completed CML redeemer pointers matching the reader-derived tx-info indexes in
  tests only.

## CML Verification Role

CML remains useful as a test oracle:

- complete a transaction;
- extract CML redeemer pointers;
- derive tx-info indexes from CML pointers;
- assert they match the layout reader's resolved indexes.

Do not make CML transaction witness extraction the runtime mechanism for
building the redeemer. At runtime the redeemer must be available before the
final CML witness set exists.

## Rollout

1. Add `tx-layout` SDK modules and unit tests.
2. Export the public API from the SDK.
3. Refactor step 04 to use dynamic layout redeemers.
4. Remove step 04 draft-complete layout probing.
5. Run SDK tests and the fault-proof emulator test that reaches step 04.
6. Keep existing narrow redeemer-index helpers until remaining callers are
   migrated.
7. Later migrate deposit and withdrawal once certificate redeemer selectors are
   implemented.

## Acceptance Criteria

The work is complete when:

- callers can build a layout-sensitive redeemer through a stock Lucid
  `RedeemerBuilder`;
- indexes are resolved after coin selection;
- no disposable probe transaction is required;
- step 04 no longer builds a draft transaction just to discover layout;
- final transaction completion uses `localUPLCEval: true`;
- selector failures are explicit and diagnostic;
- SDK unit tests cover ordering, cardinality, and failure modes;
- integration tests prove step 04 still completes and that CML-derived redeemer
  indexes match test-time expectations.
