# Batch 01 report — demo/midgard-fault-proofs (9 files)

Package dir for every command below: `demo/midgard-fault-proofs`.

## Headline items for the orchestrator

1. **Deliberate deviation from the brief (reference-frontier row).** The brief said "Load the
   committed sweep JSON at test time instead of copying its rows". The committed
   `demo/midgard-validation/tests/fixtures/resolver-proof-fit-sweep-v1.generated.json`
   has since been **regenerated against the post-Option-B blueprint** (its rows are now
   1808/2600/1855/1880/623, i.e. the current build; the test's literals match
   `git show 2476d358:` of that file exactly). Loading the live fixture would have compared
   post-change output to post-change output — a circular oracle, rule 2. Instead I extracted the
   2476d358 rows into a new provenanced baseline fixture inside the fault-proofs package and load
   that: `demo/midgard-fault-proofs/tests/fixtures/pre-option-b-resolver-proof-fit-sweep-baseline-v1.json`
   (schema `midgard-fault-proofs-pre-option-b-sweep-baseline-v1`, `sourceFile`, `sourceCommit: "2476d358"`,
   and a `why` field explaining the regeneration). This satisfies the intent (rule 12: reviewed
   baseline with provenance) rather than the letter.
2. **Pre-existing red, not fixed** (COMMON lists it as the known "direct-frontier-exact refusal"):
   `tests/submit-init-emulator-option-b-direct-frontier-exact.test.ts > "refuses item 14,059 pre-sign
at a projected 16,385 bytes and completes by automatic publication fallback — demotion, not
stranding"`, failing with `Error: exact-frontier+1 journey recorded no pre-sign envelope refusal`
   at line 201. My diff to that file is only the fail-closed gate plus a test title and a comment;
   the failing assertion body is untouched. The recorded exact frontier 14,058 appears **stale**:
   item 14,059 no longer trips a pre-sign refusal at all, so the frontier has moved outward. Owner
   call needed on re-measuring it; per COMMON I did not fix it.
3. **Evidence gap, disclosed.** For the new flatness property in
   `submit-init-emulator-spend-input-cardinality.test.ts` ("memory cost flat in cardinality") I could
   not produce a genuine rule-3 mutation: the property is about an _on-chain_ per-item execution cost,
   and no off-chain mutation available to me (extra outputs, up to 296 unique ones; metadata padding)
   moves `ne-submit-step-02` execution memory at all. A mutation that changed the off-chain builder
   would have failed at construction, which COMMON explicitly disallows as evidence. The other
   assertions in that merged test (carriage tier, proof fit, memory/step bands) do have evidence.

## No production `src/` edits

Every src file I touched was mutated **temporarily** for failure evidence and restored. Verified
byte-identical against pre-mutation snapshots with `cmp` (all IDENTICAL):
`demo/midgard-core/src/consensus-profile.ts`, `demo/midgard-core/src/codec/native-tx-field-access.ts`,
`demo/midgard-fault-proofs/src/validation-dispute/submit.ts`,
`demo/midgard-fault-proofs/src/ne-submit-step-02.ts`,
`demo/midgard-fault-proofs/src/missing-native-script-tx/contracts.ts`,
`demo/midgard-fault-proofs/src/script-integrity-hash-mismatch/contracts.ts`,
`demo/midgard-fault-proofs/src/inspect-contracts.ts`.
(The repo-wide `git diff` on `src/` is non-empty, but that is the pre-existing uncommitted wave this
batch was told to work on top of; none of it is mine.)

Note on tests resolving to source: this package resolves siblings through the `midgard-source` export
condition, so src mutations take effect in vitest with no rebuild — mutations were real, not staged.

---

## 1. tests/missing-native-script-tx-envelope.test.ts

- **Contract.** Eight `missing_native_script_tx` step validators exist in the blueprint with their
  declared arities, apply to eight distinct script hashes, and each fits its publication host.
- **What changed.** Deleted the eight-entry compiled-size table and the comparison against it
  (rule 2 self-pin). The envelope bound is now stated against the consensus floor:
  `expect(appliedBytes + PUBLICATION_OVERHEAD_ALLOWANCE_BYTES, step.spendingScriptHash)
.toBeLessThanOrEqual(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes)`. Kept and sharpened the
  arity check (now also asserts the declared title set equals the audited arity key set, so a
  renamed/added validator cannot slip past), `expect(steps).toHaveLength(8)` and the eight-distinct-
  hash check.
- **Failure evidence.** Mutation: in `demo/midgard-fault-proofs/src/missing-native-script-tx/contracts.ts`,
  dropped one step from the applied step list. Failing assertion:
  `expect(steps).toHaveLength(8)` → `expected 7 to be 8`, and the title/arity set equality also
  failed. Reverted; `cmp` identical; re-ran green.
- **Verification.** `npx tsc --noEmit` OK; `npx eslint <file> --max-warnings=0` OK;
  `npx prettier --check <file>` OK; `npx vitest run <file>` 2/2 green.

## 2. tests/script-integrity-hash-mismatch-publication-fit.test.ts

- **Contract.** All five applied `scriptIntegrityHashMismatch` scripts publish under the 15,872-byte
  reliability reserve.
- **What changed.** Deleted the exact-size tuple `toEqual([14968, 12093, 1879, 5677, 2271])` (rule 2).
  Replaced the fail-open `describe.runIf` with a hard precondition (rule 14): the blueprint is read at
  module load, missing titles are collected, and a missing title now `throw`s
  `blueprint at ${realBlueprintPath} is missing …` instead of silently skipping the suite. The
  reserve is **derived**, not transcribed:
  `RELIABILITY_RESERVE_BYTES = MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes -
MIDGARD_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes`.
- **Failure evidence.** Mutation: in
  `demo/midgard-fault-proofs/src/script-integrity-hash-mismatch/contracts.ts`, removed the parameter
  application on one script so its published size crossed the reserve. Failing assertion:
  the `toBeLessThanOrEqual(RELIABILITY_RESERVE_BYTES)` inside the per-script loop, reporting the
  offending script title. Separately, deleting a blueprint title from the expected list exercised the
  new fail-closed throw. Reverted; `cmp` identical; re-ran green.
- **Verification.** tsc/eslint/prettier OK; `npx vitest run` 1/1 green.

## 3. tests/state-queue-yield-publication-admission.test.ts

- **Contract.** The state-queue mint policy and all five arm-specific rewarding scripts publish as
  reference scripts within the Van Rossem publication target.
- **What changed.** Dropped `expect(ledger).toEqual(pinned)` against the stored JSON copy (rule 2 —
  it pinned freshly measured bytes/mem/cpu against a transcript of themselves). Kept the live gate
  `completeSignedBytes <= VAN_ROSSEM_PUBLICATION_TARGET_BYTES`, and replaced the pinned-ledger role
  with a discriminating structural assertion that the six measured entries are exactly the six
  expected arms in order:
  `expect(measurements.map((m) => m.name)).toEqual(["mint","commit","unattestedTimeout",
"unavailableTimeout","fraudRemoval","merge"])` — so a dropped or renamed arm still fails.
- **Failure evidence.** Mutation: removed the `merge` rewarding script from the measured set in the
  builder path. Failing assertion: the `toEqual([...])` name list → array of 5 vs 6, naming `merge`.
  Reverted; re-ran green.
- **Verification.** tsc/eslint/prettier OK; `npx vitest run` 1/1 green.

## 4. tests/submit-init-emulator-option-b-direct-frontier-reserve.test.ts

- **Contract.** At the owner-signed 13,522-byte direct-route reserve frontier the observe door signs
  under the reliability budget, every non-observe stage is item-size-independent, and the adjacent
  13,523-byte probe still completes on the direct route.
- **What changed.** Rewritten below the header. Deleted the literal 56-byte headroom pin, the five
  transcribed stage sizes (1808/2600/1855/1880/623) and the three exact execution-unit pairs
  (889_960/308_292_651, 163_390/106_674_927, 595_300/309_207_534). The budget is derived
  (`minSupportedL1MaxTxBytes - proofItemEnvelopeReliabilityReserveBytes`) and the frontier item size
  comes from `MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes`, not a literal. The
  header now states the four claims explicitly and says "No absolute byte count is asserted here."
  Both journeys (`atFrontier`, `pastFrontier`) already existed; item-size independence is now asserted
  **A-vs-B** rather than against literals:
  `expect(pastSizes[kind], kind).toBe(atSizes[kind])` for every non-observe stage, with
  `expect(pastSizes["observe"]).toBeGreaterThan(atSizes["observe"]!)` as the discriminator proving the
  two journeys really differ (rule 4 — otherwise the equality would be vacuous). The env gate is now
  fail-closed: `if (!realBlueprintSpeaksOptionBV1()) { throw new Error(OPTION_B_SKIP_REASON); }`.
- **Failure evidence.** Mutation: `demo/midgard-core/src/consensus-profile.ts`,
  `proofItemEnvelopeReliabilityReserveBytes` 512 → 1024 (shrinking the budget below the signed observe
  size). Failing assertion: the observe-door `toBeLessThanOrEqual(RELIABILITY_BUDGET_BYTES)` in
  "signs the observe door inside the reliability budget at the owner-signed direct threshold".
  Reverted; `cmp` identical; re-ran green.
  (A first attempt — `maxReliableDirectCompleteItemBytes` 13_522 → 14_058 — was **discarded as invalid
  evidence** because it failed at fixture construction with "validation-dispute fixture is missing its
  selected fitting complete item", which COMMON rules out.)
- **Verification.** tsc/eslint/prettier OK; `npx vitest run` 4/4 green (~1.8 min journeys, long
  timeout `1_800_000` on `beforeAll`).

## 5. tests/submit-init-emulator-option-b-reference-frontier.test.ts

- **Contract.** The post-Option-B reference route carries a tier-1-ceiling item end to end and bills
  strictly below the pre-change sweep rows at the sweep's own shape.
- **What changed.**
  - `PRE_CHANGE_SWEEP_ROWS` is no longer hand-copied literals: it is read at test time from the new
    provenanced baseline fixture (see headline item 1) via `readFileSync(fileURLToPath(new URL(
"./fixtures/pre-option-b-resolver-proof-fit-sweep-baseline-v1.json", import.meta.url)))`, with a
    `baselineRow(name)` accessor that throws on a missing row.
  - Removed the five extra transcribed byte constants and the exact byte/unit pins (15107/1903/889/125535).
  - Kept the ordering claims (post < pre) as the contract, e.g.
    `expect(observeMeasurement.executionMemory).toBeLessThan(BigInt(PRE_CHANGE_SWEEP_ROWS.observe.memoryUnits))`.
  - **Added a discriminating claim the file lacked** (rule 4): the stages Option B did not touch must
    bill _identically_ to the baseline, so a global fee/serialisation drift cannot masquerade as an
    Option B saving —
    `for (const [index, name] of [[1,"source"],[3,"proof"],[4,"settle"]] as const) { … expect(
measurement.executionMemory, name).toBe(BigInt(PRE_CHANGE_SWEEP_ROWS[name].memoryUnits)); … }`.
  - `describe.skipIf(!optionB)` replaced with a fail-closed throw.
  - The bare `rejects.toThrow()` on "frontier + 1" is now **causal** (rule 5), asserting the tier
    demotion reason and both cap numbers, built by string concatenation over
    `MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES`: it requires the message to say the item
    `has a <cap+1>-byte … preimage, which … carries as tier-2 \`RawUtxo\` rather than tier-1 \`Inline\`
    (cap <cap> bytes)`.
- **Failure evidence.** Mutation: in `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`,
  widened the tier-1 inline cap check so an over-cap item was accepted as tier-1. Failing assertion:
  the causal `rejects.toThrow(...)` in "cannot stage the reference frontier + 1" — the promise resolved
  instead of rejecting. A second mutation lowering the observe-stage saving made the
  `toBeLessThan(baseline.observe.memoryUnits)` claim fail. Reverted; `cmp` identical; re-ran green.
- **Verification.** tsc/eslint/prettier OK (fixture prettier-checked separately; eslint has no config
  for `.json`, so the fixture is linted by prettier only); `npx vitest run` 3/3 green.

## 6. tests/submit-init-emulator-option-b-direct-frontier-exact.test.ts

- **Contract.** At the measured post-Option-B direct-route frontier the observe door signs inside the
  16,384-byte envelope, and one item byte past it the builder refuses pre-sign and completes the same
  thread by automatic publication fallback.
- **What changed — minimal, deliberately.** The `by_construction` for this row asks for a generated
  size ledger with a `--check` mode; that generator does not exist in this package and building one is
  outside the batch's file list, so I did **not** invent one. I made the env gate fail-closed, retitled
  test 1 to name the measured quantity it actually asserts ("signs the observe door inside the L1
  envelope at the contiguous exact frontier, item 14,058"), and updated the accompanying comment to
  record that the item count is a measurement, not a contract value. The derived assertions
  (`signed <= PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`, projected == signed, demotion at +1) were already
  independent of the literals and are unchanged.
- **Failure evidence.** None required: I added and materially changed no behavioral assertion here.
- **Verification.** tsc/eslint/prettier OK; `npx vitest run` **1 passed, 1 failed** — the failure is the
  pre-existing red described in headline item 2, at an assertion I did not touch.
- **Skipped and why.** The five stage literals remain, because replacing them per the row requires the
  fit-ledger generator this batch does not own. Flagged for a follow-up batch; recommend it be paired
  with re-measuring the stale 14,058 frontier.

## 7. tests/inspect-contracts.test.ts

- **Contract.** `inspect-contracts` reports per-category step names, applied script identities,
  envelope fit and catalogue readiness, and fails closed on stale, missing, duplicated or
  non-canonical deployment entries.
- **What changed.** Nothing by me. A prior pass on this working tree had already removed
  `Q13_APPLIED_STEP_HASHES` and `Q13_CATALOGUE_ROOT` and left the catalogue claim as the SDK-derived
  `rootMatchesDerived` comparison, which is exactly what the row's `by_construction` asks for. I
  verified this against the row and re-ran the file rather than churning it.
- **Failure evidence.** Mutation: perturbed the catalogue-root derivation in
  `demo/midgard-fault-proofs/src/inspect-contracts.ts`. Note a trap worth recording: applying it at the
  first derivation site (~line 812) produced **no** failure — that site is dead for this path. Applied
  at the second site (~line 950, inside the `Effect.tryPromise`), 7 tests failed on the
  `rootMatchesDerived` assertion, confirming the derived-root claim is load-bearing and fail-closed.
  Reverted; `cmp` identical; re-ran green.
- **Verification.** tsc/eslint/prettier OK; `npx vitest run` 13/13 green.

## 8. tests/submit-init-emulator-spend-input-cardinality.test.ts

- **Contract.** Q10 (double-spend) and Q11 (no-input) both fit the L1 envelope and the execution
  reserve at the admissible 296-input Cardano spend shape, inline and through §8 tier-2 published
  carriage, and the ladder selects `RawUtxo` on preimage size alone at 365 inputs.
- **What changed.**
  - Deleted the four fake "boundary" pins entirely, as the row directs:
    `DOUBLE_SPEND_LARGEST_FITTING_CARDINALITY = 74`, `DOUBLE_SPEND_FIRST_OVER_BYTES_CARDINALITY = 75`,
    `NO_INPUT_LARGEST_FITTING_CARDINALITY = 195`, `NO_INPUT_FIRST_OVER_BYTES_CARDINALITY = 196`, and
    the two tests titled "at the former … byte boundary" (both sides asserted `l1ByteMargin > 0`, so
    they encoded no boundary at all).
  - Deleted the acknowledged-stale `ROUTED_BINDING_STEP_MEMORY_CEILING = 1_200_000n` and **derived** the
    band instead: `bindingStepMemoryBand = () => executionCeilings().memory / 10n` (= 1,320,000).
  - The admissible cardinality is now derived from the consensus profile rather than transcribed:
    `ADMISSIBLE_CARDINALITY_BY_PREIMAGE_BYTES = Math.floor((MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes
    - SPEND_INPUT_PREIMAGE_ARRAY_HEADER_BYTES) / SPEND_INPUT_PREIMAGE_ITEM_BYTES)`(= 862), and the
per-item bound follows from it:`maxBindingStepMemoryPerInput = () => bindingStepMemoryBand() /
      BigInt(ADMISSIBLE_CARDINALITY_BY_PREIMAGE_BYTES)` (= 1,531).
  - Merged the surviving tier-2 coverage into one journey-sharing test,
    `it("routes both families through §8 tier-2 carriage at both tier-2 cardinalities, at a memory cost
flat in cardinality", …, 1_800_000)`, running four journeys (no-input × double-spend, routed-296 ×
    size-selected-365) and asserting `carriageTiers[binding] === "RawUtxo"`, proof fit, the memory and
    step bands, and the per-input spread property — this is the Q1X-F6 wall the row says is the one
    thing worth keeping, and it is now stated as a **property** (spread per input below the derived
    bound) rather than a pinned ceiling.
  - Header prose rewritten; it now says "No inline frontier cardinality is pinned here, deliberately."
- **Failure evidence.** Mutation: `demo/midgard-core/src/consensus-profile.ts`,
  `maxSpendInputsPreimageBytes` reduced so the derived admissible cardinality fell below 296. Failing
  assertion: "derives the admissible spend-input cardinality from the consensus profile" and then the
  296-shape admission check. A second mutation in
  `demo/midgard-fault-proofs/src/validation-dispute/submit.ts` forcing tier-1 carriage failed
  `expect(carriageTiers[binding]).toBe("RawUtxo")`. Reverted; `cmp` identical; re-ran green.
  **Gap (headline item 3):** the flatness property itself has no mutation. Extra-output mutations (up
  to 296 unique outputs) and metadata padding did not move `ne-submit-step-02` execution memory at all;
  an off-chain mutation cannot inject a genuine per-item on-chain cost, and an on-chain change needs an
  Aiken edit, which is outside this batch. (Side trap recorded: lucid rejects a metadata string over 64
  chars, so the padding attempt had to use `"x".repeat(((preimageBytes) % 60) + 1)`.)
  An earlier attempt to state the flatness bound as a hard `15n` per input was wrong — the real spread
  is ~111/input — and was replaced by the derived band above rather than by loosening a literal.
- **Verification.** tsc/eslint/prettier OK; `npx vitest run` 3/3 green (~5.8 s for the merged
  four-journey test in the combined run).

## 9. tests/native-script-decoding-envelope.test.ts

- **Contract.** The native-script-decoding step redeemers fit the 16,384-byte L1 envelope at
  adversarial MPF depth; the step-02 worst forced-leaf instance keeps > 1 KiB of margin and an
  exhaustion depth above the 2^128 work-reachable branch level.
- **What changed.** Nothing by me. A prior pass on this working tree had already deleted
  `EXPECTED_UNAPPLIED_SIZES_BYTES` and the `expect(deepBytes.publishedChunk).toBe(601)` pin, replacing
  the latter with the load-bearing ratio claim
  `expect(deepBytes.publishedChunk).toBeLessThan(deepBytes.redeemerCarried / 2)` — which is what the
  row's `by_construction` asks for. Verified against the row and re-ran rather than churning.
- **Failure evidence.** Mutation: in `demo/midgard-core/src/codec/native-tx-field-access.ts`, inflated
  the encoded field-access width. Failing assertions: the envelope fit in "proves Q3 by arithmetic and
  fits every applied step in the publication host" and the step-02 margin check. Reverted; `cmp`
  identical; re-ran green.
- **Skipped and why.** `STEP_TX_OVERHEAD_ALLOWANCE_BYTES = 2048` is still an asserted allowance rather
  than a value measured from a real emulator step transaction. Turning it into a measurement means
  threading a measured overhead out of the emulator journeys in another file, which is outside this
  batch's file list. Flagged.

---

## Combined verification (final run, all nine files)

From `demo/midgard-fault-proofs`:

```
npx tsc --noEmit                                   # OK
npx eslint <the 9 test files> --max-warnings=0     # OK
npx prettier --check <the 9 test files> tests/fixtures/pre-option-b-resolver-proof-fit-sweep-baseline-v1.json   # OK
npx vitest run <the 9 test files>                  # Test Files 1 failed | 8 passed (9)
                                                   # Tests      1 failed | 33 passed (34)
```

Log: `<scratchpad>/final-run.log`.

The single failure is the pre-existing `direct-frontier-exact` refusal red named in COMMON.

Note on eslint: `tests/fixtures/pre-option-b-resolver-proof-fit-sweep-baseline-v1.json` produces
`warning File ignored because no matching configuration was supplied` — the package's eslint flat
config has no `.json` block. That is a config gap, not a lint failure; the fixture is covered by
`prettier --check`. I did not modify the eslint config (outside the batch).

## Files changed

- `demo/midgard-fault-proofs/tests/missing-native-script-tx-envelope.test.ts`
- `demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch-publication-fit.test.ts`
- `demo/midgard-fault-proofs/tests/state-queue-yield-publication-admission.test.ts`
- `demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-direct-frontier-reserve.test.ts`
- `demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-reference-frontier.test.ts`
- `demo/midgard-fault-proofs/tests/submit-init-emulator-option-b-direct-frontier-exact.test.ts` (gate + title/comment only)
- `demo/midgard-fault-proofs/tests/submit-init-emulator-spend-input-cardinality.test.ts`
- `demo/midgard-fault-proofs/tests/fixtures/pre-option-b-resolver-proof-fit-sweep-baseline-v1.json` (new)

Verified-only, no edit: `tests/inspect-contracts.test.ts`, `tests/native-script-decoding-envelope.test.ts`.

Nothing committed, stashed, reset or pushed. No `.hs` file touched.
