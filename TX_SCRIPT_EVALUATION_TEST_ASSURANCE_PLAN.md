# Transaction Script Evaluation Test Assurance Plan

## Goal

Substantially improve assurance that Midgard transaction evaluation dispatches
and executes script purposes correctly across:

- PlutusV3-only transactions.
- MidgardV1-only transactions.
- Mixed transactions that contain both PlutusV3 and MidgardV1 executions.

The test suite must prove successful evaluation and failure propagation with
real UPLC contracts, realistic redeemers, realistic script-source resolution,
and full Phase A plus Phase B transaction validation. Primitive context tests
are useful supporting evidence, but they are not sufficient for this goal.

## Current Coverage Baseline

Existing full transaction coverage is strongest for PlutusV3:

- Inline PlutusV3 spend acceptance with the real local evaluator.
- PlutusV3 spend plus mint acceptance.
- PlutusV3 observer acceptance.
- PlutusV3 malformed scripts, always-fails scripts, missing redeemers,
  datum-hash output rejection, wrong redeemer indexes, and budget failures.

Existing MidgardV1 evidence is mostly primitive:

- Distinct PlutusV3 and MidgardV1 hash domains.
- Recovery of both hash identities from the same raw script bytes.
- Receiving redeemer tag decoding.
- Protected output decoding.
- Direct MidgardV1 context construction.

The gap is full transaction coverage:

- No MidgardV1-only transaction is currently run through Phase A and Phase B
  local evaluation.
- No mixed transaction proves one purpose resolves to MidgardV1 while another
  purpose in the same transaction resolves to PlutusV3.
- No mixed transaction proves that either script-version leg can independently
  reject the whole transaction.

## Assurance Requirements

The new tests should prove these properties:

- Script version is selected by the matched script hash domain, not by witness
  position, source form, or an implicit default.
- The same UPLC bytes can be supplied once and resolved as PlutusV3 and/or
  MidgardV1 depending on the purpose hash. Inline tests can use raw UPLC bytes;
  reference-script tests must use typed `CML.Script.new_plutus_v3(...)` script
  refs because transaction outputs expose reference scripts through CML.
- PlutusV3 purposes receive Cardano-compatible PlutusV3 script context.
- MidgardV1 purposes receive MidgardV1 script context.
- Receive purposes are accepted only through MidgardV1 context.
- Datum-hash outputs remain rejected at the Midgard output boundary; Midgard L2
  supports only absent datums or inline datums.
- A failure in any required script execution rejects the whole transaction.
- Redeemer pointer indexing is exercised for spend, mint, observe, and receive
  paths where practical.
- Inline and reference script sources are both covered for non-native scripts.
- Budget enforcement is tested for at least one PlutusV3 path and one MidgardV1
  path. Phase B enforces budgets after Harmonic evaluation regardless of the
  non-native script version.

## Test Fixture Contracts

Keep using the `demo/midgard-node/blueprints/always-succeeds` fixture project,
but extend it with context-sensitive contracts. The tests should not rely only
on always-succeeds contracts, because those do not prove that the evaluator
received the expected context or redeemer.

### Existing Contracts To Reuse

- `midgard.deposit_spend.else`: simple PlutusV3 success fixture.
- `midgard.deposit_mint.else`: simple PlutusV3 mint success fixture.
- `midgard.reserve_withdraw.else`: simple PlutusV3 observer success fixture.
- `midgard.always_fail.else`: explicit script failure fixture.
- `midgard.datum_equals_redeemer_spend.spend`: PlutusV3 context-sensitive
  spend fixture.

### New Contracts To Add

Add MidgardV1-specific contracts that accept a single script-context argument
and inspect the MidgardV1 context shape directly. These are mandatory assurance
fixtures, not optional niceties. They should avoid Cardano typed transaction
helpers, because MidgardV1 context intentionally differs from Cardano PlutusV3
context.

The MidgardV1 context probe must validate the top-level shape built by
`buildMidgardV1ScriptContext`:

- Top-level context is `Constr(0, [txInfo, redeemer, purpose])`.
- `txInfo` is `Constr(0, [...])` with exactly 10 fields:
  `inputs`, `referenceInputs`, `outputs`, `fee`, `validityRange`,
  `observers`, `signatories`, `mint`, `redeemers`, `txId`.
- Spend purpose is `Constr(1, [scriptHash, outRef])`, not just constructor `1`;
  this distinguishes MidgardV1 spend purpose from PlutusV3 spend script info.
- Receive purpose is `Constr(3, [scriptHash])`.
- For receive tests, the probe may inspect `txInfo.outputs` at field index `2`.
  It must not expect the protected-address bit to appear in context, because
  protectedness is used only to discover receive purposes and is normalized away
  before context construction.

Proposed fixtures:

- `midgard.midgard_v1_spend_guard.else`
  - Accepts only when the third field is Midgard script purpose constructor
    `Spend` with fields `[scriptHash, outRef]`.
  - Requires the redeemer to equal a deterministic value, for example `42`.
  - Checks that `txInfo.inputs` contains the same out-ref as the purpose.
- `midgard.midgard_v1_receive_guard.else`
  - Accepts only when the third field is Midgard script purpose constructor
    `Receive`.
  - Requires the redeemer to equal a deterministic value, for example `99`.
  - Checks that `txInfo.outputs` contains an output addressed to the receive
    script hash.
- `midgard.midgard_v1_always_fail.else`
  - Fails under the MidgardV1 evaluator for a clean negative case.

Add a PlutusV3 context probe contract as well:

- `midgard.plutus_v3_context_probe.spend`
  - Accepts only when the PlutusV3 context has the expected 16-field tx-info
    shape.
  - Checks spend script info carries the expected out-ref and resolved datum.
  - Checks reference inputs are lexicographically ordered.
  - Checks the mint map, signer list, observer withdrawal map, redeemer map, and
    transaction id fields are populated in the expected locations.
  - Has at least one negative fixture where the redeemer requests an expected
    out-ref or signer that is absent.

If Aiken typed helpers make these probes awkward, implement the probes as
untyped `Data` pattern checks inside the Aiken fixture project. Do not replace
them with TypeScript-only mocks; the point is to run real compiled UPLC through
the same evaluator path as production transactions.

Before adding transaction tests, add a narrow primitive test that directly
evaluates each new contract with `evaluateScriptWithHarmonic` and a manually
built MidgardV1 context. This catches fixture-shape mistakes before full
transaction tests fail for multiple possible reasons.

## Helper Work

Add focused helpers to `demo/midgard-node/tests/native-transaction-integration.test.ts`
or a local test helper module if the file becomes too large:

- `makeRawUplcWitness(scriptHex)` returns raw compiled UPLC bytes for inline
  witnesses when the same bytes must expose both PlutusV3 and MidgardV1 hashes.
- `makeTypedPlutusV3Witness(scriptHex)` wraps bytes with `CML.Script.new_plutus_v3`.
- `makeTypedPlutusV3ReferenceScript(scriptHex)` returns the only supported
  reference-script form for non-native reference tests.
- `midgardV1Hash(scriptHex)` calls `hashMidgardV1Script`.
- `plutusV3Hash(scriptHex)` uses the existing CML PlutusV3 hash path.
- `makeProtectedScriptOutput(scriptHash, lovelace, opts)` creates a map-form
  output and applies `protectOutputAddressBytes`.
- `attachNonCardanoScriptIntegrityMarker(fixture)` sets a deterministic,
  non-empty script-integrity hash for MidgardV1 redeemers that cannot be
  represented by Cardano `calc_script_data_hash`, especially receive tag `6`.
  This marker is only a Phase A admission marker; Phase B intentionally does
  not validate exact script-integrity hash equality. The marker must be set
  before signing, or the transaction must be re-signed afterward, because body
  mutations invalidate pubkey input signatures.
- `buildNativeTxWithInputs(...)` or equivalent lets tests author two or more
  spend inputs, reference inputs, protected outputs, mint policies, observers,
  and redeemers in intentionally reversed order while Phase B derives sorted
  indexes.

The helper names should make the hash-domain distinction explicit. Do not add
compatibility lookup behavior, fallback script-version behavior, or hidden
legacy modes.

## Test Matrix

### PlutusV3 Only

1. Accept context-sensitive PlutusV3 spend.
   - Spend a script output locked by the PlutusV3 hash of
     `datum_equals_redeemer_spend`.
   - Use inline datum `7` and redeemer `7`.
   - Assert Phase A accepts, Phase B accepts, and `requiresPlutusEvaluation` is
     true.

2. Reject context-sensitive PlutusV3 spend.
   - Same script and datum.
   - Use redeemer `8`.
   - Assert `RejectCodes.PlutusScriptInvalid`.

3. Accept PlutusV3 context probe.
   - Spend a script output locked by `plutus_v3_context_probe`.
   - Include at least two spend inputs and at least two reference inputs authored
     out of lexicographic order.
   - Include mint, a required observer, a required signer, and redeemers for
     spend, mint, and observe purposes.
   - Assert Phase B accepts only when the probe sees the expected tx-info field
     positions and sorted redeemer map.

4. Reject PlutusV3 context probe with wrong expected field.
   - Use the same transaction shape as test 3.
   - Make the redeemer request an absent signer, wrong out-ref, or wrong datum.
   - Assert `RejectCodes.PlutusScriptInvalid`.

5. Keep existing PlutusV3 spend plus mint and observer acceptance tests.
   - These continue to cover mint and observe purpose indexing.

### MidgardV1 Only

6. Accept MidgardV1 spend with inline raw UPLC witness.
   - Spend a script output locked by `hashMidgardV1Script(scriptBytes)`.
   - Supply the same raw UPLC bytes as an inline witness.
   - Use `MidgardRedeemerTag.Spend`, index `0`, redeemer `42`.
   - Assert Phase A accepts, Phase B accepts, and the PlutusV3 hash of the same
     bytes is not the spent script hash.

7. Reject MidgardV1 spend with wrong redeemer.
   - Same transaction shape as test 6.
   - Use redeemer `41`.
   - Assert `RejectCodes.PlutusScriptInvalid` and detail containing the
     MidgardV1 script hash.

8. Accept MidgardV1 receive through protected output.
   - Produce a protected output addressed to `hashMidgardV1Script(scriptBytes)`.
   - Supply raw UPLC witness and `MidgardRedeemerTag.Receiving`, index `0`,
     redeemer `99`.
   - Spend a pubkey input so the only script purpose is receive.
   - Assert Phase A accepts and Phase B accepts.

9. Reject PlutusV3 receive.
   - Produce a protected output addressed to the PlutusV3 hash of a typed
     PlutusV3 script.
   - Supply receive redeemer.
   - Assert `RejectCodes.PlutusScriptInvalid` with detail
     `ReceivingScript requires MidgardV1 context`.

10. Reject datum-hash output.
   - Spend a script output with a datum hash rather than inline datum.
   - Assert `RejectCodes.InvalidOutput` with detail mentioning datum hashes.

11. Accept MidgardV1 reference-script spend.
   - Lock the spent output by MidgardV1 hash.
   - Supply the UPLC bytes as a typed PlutusV3 reference script on a reference
     input. `decodeScriptSource` should recover both the PlutusV3 hash and the
     MidgardV1 hash from that typed reference script.
   - Assert Phase B resolves the reference source and accepts.

12. Accept MidgardV1 budget-covered spend.
   - Enable `enforceScriptBudget`.
   - Use declared ex-units larger than the measured Harmonic result.
   - Assert Phase B accepts.

13. Reject MidgardV1 low-budget spend.
   - Enable `enforceScriptBudget`.
   - Use declared ex-units below the measured Harmonic result.
   - Assert `RejectCodes.PlutusScriptInvalid` with `budget exceeded`.

14. Reject wrong non-zero MidgardV1 redeemer index.
   - Use two MidgardV1 script spends authored in reverse lexicographic order.
   - Put a valid redeemer on index `0` for the lexicographically first input and
     an intentionally wrong or missing redeemer for index `1`.
   - Assert the rejection proves Phase B derives spend indexes from sorted
     out-refs, not authored order.

15. Accept non-zero receive index.
   - Produce two protected MidgardV1 script outputs with hashes that sort in a
     known order.
   - Put the real receive guard on the second sorted hash and use receiving
     redeemer index `1`.
   - Assert Phase B accepts. Add a paired wrong-index rejection if test runtime
     remains reasonable.

### Mixed MidgardV1 And PlutusV3

16. Accept mixed MidgardV1 spend plus PlutusV3 mint.
    - MidgardV1 spend guard passes with redeemer `42`.
    - PlutusV3 mint always-succeeds policy mints one asset.
    - Assert Phase A accepts, Phase B accepts, and value preservation holds.
    - Do not treat Phase A script-hash presence as dispatch proof: Phase A records
      both PlutusV3 and MidgardV1 hashes for each inline non-native script.

17. Accept mixed PlutusV3 spend plus MidgardV1 receive.
    - PlutusV3 `datum_equals_redeemer_spend` passes.
    - MidgardV1 receive guard passes for a protected output.
    - Assert both purposes are evaluated in one transaction.

18. Reject mixed transaction when the MidgardV1 leg fails after a PlutusV3 leg
    has passed.
    - Use a PlutusV3 spend that passes.
    - Use a MidgardV1 receive guard with wrong redeemer. Receive executions occur
      after spend, mint, and observe discovery order, so this proves an earlier
      PlutusV3 leg did not hide a later MidgardV1 failure.
    - Assert the whole transaction is rejected with `PlutusScriptInvalid` and
      the rejection detail identifies the MidgardV1 purpose/hash.

19. Reject mixed transaction when the PlutusV3 leg fails after a MidgardV1 leg
    has passed.
    - Use a MidgardV1 spend guard that passes.
    - Use a PlutusV3 mint policy or observer that fails. Mint and observe
      executions occur after spend discovery order.
    - Assert the whole transaction is rejected with `PlutusScriptInvalid` and
      the rejection detail identifies the PlutusV3 purpose/hash.

20. Accept mixed transaction where the same UPLC bytes are used for both
    domains in different purposes.
    - One purpose address/policy uses the PlutusV3 hash.
    - Another purpose uses the MidgardV1 hash of the same bytes.
    - Prefer a single inline raw or typed PlutusV3 source, because each decoded
      non-native source exposes both hash domains. Use a typed reference source
      for the reference-script variant.
    - Assert both purposes pass. This is the strongest regression test for
      domain-specific resolution.

21. Accept mixed reference-script transaction.
    - Put one typed PlutusV3 reference script on a reference input.
    - Use that source to satisfy one PlutusV3 purpose and one MidgardV1 purpose
      by their respective hashes.
    - Assert Phase B accepts, proving reference script source resolution exposes
      both hash domains.

## Implementation Sequence

1. Extend the Aiken fixture project with the MidgardV1 guard contracts.
2. Rebuild `demo/midgard-node/blueprints/always-succeeds/plutus.json`.
3. Add primitive direct-evaluator tests for the new MidgardV1 contracts.
4. Add helper functions for raw UPLC witnesses, MidgardV1 hashes, protected
   script outputs, and deterministic script-integrity markers.
5. Add PlutusV3-only context-sensitive success and failure tests.
6. Add PlutusV3 context-probe tests.
7. Add MidgardV1-only spend, receive, failure, output datum-hash rejection, budget, and
   reference-script tests.
8. Add non-zero redeemer index tests.
9. Add mixed-version success and failure tests.
10. Run the focused validation test command.
11. If any test exposes implementation ambiguity, fix the validation behavior
   rather than relaxing the tests.

## Verification Commands

Focused test run:

```sh
NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/native-transaction-integration.test.ts tests/midgard-local-script-eval.test.ts --reporter=basic --disable-console-intercept
```

Fixture rebuild check:

```sh
cd demo/midgard-node/blueprints/always-succeeds
aiken build
```

Optional broader validation run:

```sh
NODE_ENV=emulator pnpm --dir demo/midgard-node exec vitest run tests/native-transaction-integration.test.ts tests/midgard-local-script-eval.test.ts tests/harmonic-uplc-contract-eval.test.ts --reporter=basic --disable-console-intercept
```

## Done Criteria

The plan is complete when:

- The tests include at least one full Phase A plus Phase B acceptance case for
  PlutusV3-only, MidgardV1-only, and mixed PlutusV3 plus MidgardV1 transactions.
- The tests include at least one full Phase A plus Phase B rejection case where
  the failing leg is PlutusV3 and one where the failing leg is MidgardV1.
- At least one MidgardV1 test is context-sensitive and would fail if evaluated
  with the PlutusV3 context shape.
- At least one PlutusV3 test is context-sensitive beyond datum/redeemer equality
  and inspects tx-info fields relevant to script evaluation.
- At least one mixed test proves both hash domains can be active in the same
  transaction.
- At least one non-zero redeemer index test covers sorted spend or receive
  indexing.
- At least one reference-script test proves a typed PlutusV3 reference script can
  satisfy a MidgardV1 hash.
- The focused verification command passes.
- The document and tests do not introduce compatibility modes, fallback
  behavior, or demo-only behavior in production validation paths.

## Independent Review Notes

This section records feedback from a read-only independent review and the
resulting changes made to this plan.

- P1 review: The MidgardV1 guard shape was under-specified. The plan now
  requires exact MidgardV1 context arity, tx-info field order, spend payload
  shape `[scriptHash, outRef]`, receive constructor `3`, and correct output
  field usage.
- P1 review: Raw UPLC reference scripts are not possible through the current CML
  output helper. The plan now uses typed PlutusV3 reference scripts for
  reference-source tests while still asserting that both hash domains are
  recovered.
- P1 review: Phase A script-hash presence is not dispatch evidence. The mixed
  tests now rely on context-sensitive Phase B success and failure.
- P1 review: The script-integrity marker is only a non-empty admission marker
  and must be applied before signing or followed by re-signing. The helper
  section now states this constraint.
- P2 review: PlutusV3 context construction needed stronger coverage. The plan
  now adds a PlutusV3 context-probe fixture that checks tx-info fields, reference
  input ordering, mint, signatories, observers, redeemers, and tx id.
- P2 review: Redeemer-index coverage used too many zero indexes. The matrix now
  includes non-zero sorted spend and receive index cases.
- P2 review: Mixed failure tests must account for Phase B execution order. The
  matrix now chooses failure positions that prove an earlier leg can pass before
  a later opposite-version leg fails.
- P3 review: MidgardV1 budget testing should be explicit. The matrix now
  includes covered-budget acceptance and low-budget rejection for MidgardV1.
