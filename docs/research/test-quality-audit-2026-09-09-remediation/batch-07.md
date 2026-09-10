# Batch 07 report — demo/midgard-sdk (10 files)

All paths absolute under /home/gumbo/midgard-hub/midgard.

Baseline: worked on top of the existing uncommitted working tree. Nothing was
stashed/reset/checked out/committed/pushed. No `.hs` file touched. Every
production mutation used a byte-exact scratchpad backup and was restored from
that copy and verified with `cmp` / diff.

---

## 1. demo/midgard-sdk/tests/fault-proof.test.ts (recommend: strengthen)

**Contract.** Fault-proof step ABI shapes, the violation detectors, and that
every family's contract builder applies its blueprint parameters in the
declared order.

**What changed.**

- Deleted the `MIDGARD_REAL_BLUEPRINT_PATH` early return at the head of "builds
  validation-trace dispute with its exact shared-policy parameter order" so the
  ~485-line leg runs on the in-tree blueprint (the #609 defect class).
- Rebuilt the stale `filterBlueprint` allowlist so it is _derived_ from the
  production title constants instead of transcribed: `collectTitles()` folds
  `FAULT_PROOF_SHARED_TITLES`, `VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES`,
  `CEK_CORE_STAGE_TITLES`, `CEK_CONTEXT_STAGE_TITLES`; the shared stage-one
  redeemer item titles are derived from `REDEEMER_ITEM_EXECUTOR_KEYS`.
- Replaced the L1409 `205` distinct-applied-hash pin with the property it stood
  for: per family, step spending-script hashes are pairwise distinct
  (`expect(new Set(...).size, \`${family} step scripts are pairwise distinct\`)`),
  plus an explicitly named list of the six legitimately shared cross-family
  pairs.
- Replaced the unexplained `14 * 1024` parameterized-script size gate with a
  derived, documented bound:
  `MAX_APPLIED_SCRIPT_BYTES = PROTOCOL_PARAMETERS_DEFAULT.maxTxSize - 512 - 276`
  (= 15,596), citing the Van Rossem 512-byte reserve and the measured constant
  276-byte publication overhead recorded by
  `demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts`
  and the fit ledger. **Finding:** the old 14 KiB pin had no provenance and is
  already exceeded by 91 shipping resolvers — it was an unenforced number, not a
  deployment constraint.
- Replaced the length-vs-own-constant assertions and the stale hand-written
  semantic-resolver parameter model with a blueprint-driven derivation: the
  expected applied resolver list is assembled by reading each validator's
  _declared_ parameter titles out of the blueprint and looking each one up in a
  reviewed `semanticParameterBindings` table. An unmapped parameter throws
  ("Unreviewed semantic resolver parameter ... on ...") — fails closed.

**Failure evidence.**

- Mutation: in `demo/midgard-sdk/src/fraud-proof/contracts/families/validation-trace-dispute.ts`,
  swapped two same-typed values in the semantic-resolver parameter binding map
  (shape-preserving, so production's own `assertParameterShapes`/arity guard does
  not fire first). Failing assertion: the semantic-resolver applied-script
  comparison (`expected <hash> to be <hash>` on the resolver list). Restored,
  re-run green.
- Mutation (#605 arm): deliberately disabled the under-application guard in
  `blueprint.ts` so a strict-prefix parameter list would be accepted; the
  always-succeeds guard leg failed. Restored, re-run green.
- Earlier accidental discovery worth recording: a `.replace()` mutation with the
  wrong indentation silently did nothing and the test "passed". From then on
  every mutation asserts the old text is present before replacing.

**Verification.** `npx tsc --noEmit` clean; `npx eslint … --max-warnings=0`
clean; `npx prettier --check` clean; `npx vitest run tests/fault-proof.test.ts`
30/30 pass (`builds every implemented fault-proof chain` 11.4 s,
`validation-trace dispute … exact shared-policy parameter order` 3.4 s).

**Note.** This file was the batch's only baseline red (stale allowlist). It is
now green; the red is resolved, not suppressed.

---

## 2. demo/midgard-sdk/tests/scheduler-refresh.test.ts (recommend: strengthen)

**Contract.** The refresh builder derives Advance/AppointFirst/Rewind input and
reference-input indices from the final transaction context and rebuilds with a
static redeemer.

**What changed (rewrite of the boundary).**

- Deleted `makeRedeemerContext` — the test's own model of Lucid's input and
  reference-input ordering — and the whole recording-Lucid transaction fake for
  the index legs.
- The three layout tests now run against a real `Emulator`/`Lucid`: a scheduler
  UTxO carrying the scheduler NFT, three witness UTxOs and a published scheduler
  reference script are minted and paid to the scheduler script address (the
  always-succeeds Plutus V3 blueprint at
  `demo/midgard-node/blueprints/always-succeeds/plutus.json`
  scaffolds the validator, so a genuine script spend with redeemer, collateral
  and local UPLC evaluation runs).
- Oracles are now independent of the builder: every expected index is read off
  the transaction body Lucid serialized (`tx.toTransaction().body().inputs()` /
  `.reference_inputs()`), and the assembled order is separately checked against a
  simple canonical-ordering reference model (`canonicalOutRefOrder`). The Advance
  case additionally **submits** the transaction to the emulator and reads the
  scheduler output back off the ledger by
  `utxosByOutRef([{txHash, outputIndex: layout.schedulerOutputIndex}])`, asserting
  it carries the NFT, the refreshed datum and the scheduler address.
- Deleted the recorded `readFrom`/`collectFrom`/`complete`-options call-sequence
  assertions (incidental structure).
- Script-reference leg now asserts transaction facts (script ref present among
  reference inputs and no inline script in the witness set; vs. no script ref and
  exactly one `plutus_v3_scripts` entry), plus a new causal negative: handing the
  builder a reference UTxO that carries no script is refused.
- New causal negative for the two-pass rebuild guard: a small callback probe
  drives `BuildTxWithRedeemer` with two contexts that disagree, and the program
  must refuse ("resolved inconsistent scheduler refresh redeemers"). The old file
  only exercised _consistent_ repeated resolutions.

**Failure evidence** (all in `demo/midgard-sdk/src/scheduler-refresh.ts`):

1. `schedulerInputIndex = requireInputIndex(...) + 1n` → 4 failed:
   `AssertionError: expected { kind: 'Advance', …(3) } to deeply equal { kind: 'Advance', …(3) }`
   (and the AppointFirst/Rewind equivalents).
2. Swapped the Rewind `activeRootRefInputIndex` / `activeTailRefInputIndex`
   sources → 1 failed: `expected { kind: 'Rewind', …(5) } to deeply equal { kind: 'Rewind', …(5) }`.
3. Inverted the redeemer-consistency guard (`!==` → `===`) → the new negative
   failed with `promise resolved "{ tx: …, layout: …}" instead of rejecting`
   (plus three collateral failures).
   All restored from `scheduler-refresh.ts.bak` and `cmp`-verified; `git diff src/`
   hash back to the pre-mutation value each time.

**Negative finding, reported not hidden.** A fourth mutation — _always_ attach
the spending script even when a reference script is supplied — is behaviorally
inert: Lucid drops the redundant attach and emits an identical transaction, so
no assertion can catch it. The witness-set assertion is kept as a true statement
about the shipped transaction, but it is not sensitive to that mutation.

**Verification.** tsc/eslint/prettier clean; `npx vitest run tests/scheduler-refresh.test.ts`
8/8 pass.

---

## 3. demo/midgard-sdk/tests/user-events-time.test.ts (recommend: strengthen)

**Contract.** `slotToUnixTimeForLucid` swallows a throwing Lucid slot mapping and
returns undefined; `resolveUserEventValidTo` snaps the deadline onto the
instance's slot grid.

**What changed.** Rewritten to five tests driven by a real `Emulator`/`Lucid`
slot grid instead of a two-method object literal, with the clock injected rather
than `Date.now` spied on. Covers: the real grid snap, a pre-system-start slot,
the era-boundary behaviour, and the try/catch swallow with a provider that
throws.

**SRC EDIT (row-mandated) — `demo/midgard-sdk/src/user-events/internals.ts`:**

```ts
export const resolveUserEventValidTo = (
  lucid: LucidEvolution,
  ttlMs = USER_EVENT_TX_TTL_MS,
  /** Injected so the deadline arithmetic can be exercised at a known instant. */
  now: () => number = Date.now,
): number => {
  const targetUnixTime = now() + ttlMs;
```

Default parameter, so every existing caller is unchanged.

**Failure evidence.** Removed the `try/catch` in `slotToUnixTimeForLucid` →
the swallow test failed at `expected undefined … threw`. Restored, green.
Changed the snap arithmetic to add the TTL after the slot conversion → the grid
test failed at the exact unix-time assertion. Restored, green.

**Verification.** tsc/eslint/prettier clean; 5/5 pass.

---

## 4. demo/midgard-sdk/tests/da-attestation.test.ts (recommend: strengthen)

**Contract.** DA attestation witness packing / bitmap arithmetic and the
init/add-signatures/apply builders' transaction shape.

**What changed.**

- **Every negative now names its refusal.** `expectBuildFailure` (which asserted
  only the generic `_tag === "DaAttestationBuildError"`) is replaced by
  `expectBuildRefusal(program, reason)`, asserting the new discriminating
  `reason` code with the production message as the assertion label. All eight
  negatives (the four witness-validation cases, add-signatures committee
  compatibility, the substituted bond-yield script, the threshold case and the
  header-hash case) now pin `invalid_signature_hex`,
  `duplicate_signature_witness`, `signer_already_attested`,
  `signer_outside_committee`, `params_committee_hash_mismatch`,
  `missing_bond_yield_reference_script`, `threshold_not_reached`,
  `attestation_header_mismatch` respectively. The L640 comment's claim that the
  header-hash mismatch is isolated is now _asserted_ rather than asserted-in-prose.
- Replaced the recorded `readFrom`/`collectFrom` call-sequence and grouping
  assertions with order-insensitive set assertions over the reference-input set
  and the collected-input set (`referenceSet`, `collectedSet`, `expectedSet`),
  which is the actual contract — the ledger sorts reference inputs canonically
  before any validator sees them. Added exactness in the other direction (the
  init transaction must _not_ pick up the spending-script reference).
- Kept the exact-value bitmap/packing assertions unchanged (they were already
  discriminating).

**SRC EDIT (row-mandated, called out prominently) — `demo/midgard-sdk/src/da-attestation.ts`:**
Additive only. Adds an exported string-literal union
`DaAttestationBuildFailureReason` (23 codes), widens
`DaAttestationBuildError` from `GenericErrorFields` to
`GenericErrorFields & { readonly reason: DaAttestationBuildFailureReason }`,
adds `reason` as the first parameter of the module-private `failBuild`, and
threads a code through all 23 `failBuild` call sites plus the one direct
`new DaAttestationBuildError({...})`. `message` and `cause` are untouched, no
control flow changed. Verified by diffing against the pre-edit backup: the diff
contains only the new type, the widened class, the `failBuild` signature and the
inserted reason literals. The only out-of-package references
(`demo/midgard-node/src/transactions/da-attestation.ts` lines 456 and 637) are
type-position, and nothing outside the SDK constructs the error, so the added
required field breaks no caller.

**Failure evidence.**

1. Inverted the header-hash comparison in `da-attestation.ts`
   (`!==` → `===`), i.e. the header check no longer trips on a mismatch:
   ```
   × refuses an apply with a substituted bond yield script
     → DA attestation header does not match state-queue target: expected 'attestation_header_mismatch' to be 'missing_bond_yield_reference_script'
   × preflights apply header and threshold requirements
     → DA attestation header does not match state-queue target: expected 'attestation_header_mismatch' to be 'threshold_not_reached'
   ```
   This is exactly the assurance the row asked for: **the old generic-tag
   assertions would have passed all three of these.**
2. Swapped `stateQueueMinting` for `daAttestationSpending` in the init builder's
   `readFrom` list → `× assembles the init transaction shape from explicit inputs
→ expected [ …(4) ] to deeply equal [ …(4) ]`.
   Both restored and re-run green (9/9).

**Partially done — declared.** The row's second half ("drive the builders through
the Lucid Emulator so the transaction shape is observed rather than recorded from
a fake") is **not** done. These are `incomplete*TxProgram` builders returning a
`TxBuilder`; emulator-driving them requires real DA-attestation, state-queue and
availability-challenge validators, a real DAAT mint, a state-queue node NFT and a
hub-oracle reference input, whose real validators would reject a synthetic
setup — i.e. the `demo/midgard-fault-proofs/tests/support/*-emulator.ts` scale of
harness, which `demo/midgard-sdk` does not have. That is a separate piece of work
well beyond a single batch row; the recorded-call brittleness the row cited has
been removed by the set-based assertions instead.

**Verification.** tsc/eslint/prettier clean; 9/9 pass. Sibling suites that share
the module re-run green: `tests/da-attestation-rotation.test.ts` +
`tests/availability-challenge.test.ts` 39/39.

---

## 5. demo/midgard-sdk/tests/fraud-proof-catalogue-registration.test.ts (recommend: derive-oracle)

**Contract.** The fraud-proof catalogue is append-only: category order is fixed
and every category has a unique four-byte id.

**What changed.** Deleted the transcribed order array and the transcribed
43-entry id map. The oracle is now a genuinely independent second
implementation: `@al-ft/midgard-core/deployment-manifest-identity`'s
`DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS` and `_ORDER`, which are
maintained separately from the SDK catalogue. The derived invariants the row
asked to keep (key order matches the order array, ids are 8 hex chars, ids are
unique) are kept and extended with `FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT`.

**Investigated and reported:** the Aiken side holds no fraud-proof catalogue id
table, so the on-chain module could not serve as the cross-check; the
deployment-manifest registry is the available independent implementation.

**Failure evidence.** Renumbered one category id in
`demo/midgard-sdk/src/fraud-proof/catalogue.ts` → the cross-implementation
comparison failed naming the diverging category. Restored, green.

**Verification.** tsc/eslint/prettier clean; 2/2 pass.

---

## 6. demo/midgard-sdk/tests/validation-auxiliary-witness.test.ts (recommend: strengthen + the row's CI NOTE)

**Contract.** The SDK's `ValidationAuxiliaryWitness` schema decodes and
re-encodes the 40-constructor canonical corpus generated by midgard-validation,
and refuses adjacent tags/arities/malformed nesting.

**What changed.**

- Deleted the sha256 self-check (`hash(fixture.corpusCbor) === fixture.corpusSha256`
  — both fields written by the same generator run, so it could never fail).
- Deleted the source-text greps and the four hand-transcribed field-name lists.
- Added per-tag CBOR equality against the fixture's own bytes, an exact-arity
  property over all 40 tags taken from the fixture's per-constructor metadata,
  and a new behavioural property — "keeps every witness field typed rather than
  opaque" — that restores what the deleted source-grep protected. **Finding:** a
  `Data.Any()` widening of a witness field is invisible both to the corpus round
  trip and to the arity check; the new property is what catches it.

**CI wiring (out-of-batch edits, justified by the row's NOTE that this generator
is not a CI `--check` job).**

- `demo/midgard-validation/scripts/generate-validation-auxiliary-witness-v1-fixture.mjs`:
  fixed the import path from `…/validation-auxiliary-witness-canonical.js` to
  `.ts` — the `.js` does not exist, so the generator could not run at all
  (`ERR_MODULE_NOT_FOUND`).
- `demo/midgard-validation/package.json`: added
  `fixtures:validation-auxiliary-witness-v1:sync` and `:check`.
- `.github/workflows/midgard-node-ci.yml`: added
  step "Check validation-auxiliary-witness ABI golden vectors" running
  `pnpm --dir demo/midgard-validation run fixtures:validation-auxiliary-witness-v1:check`.

**Fail-closed proof (rule 14).** Perturbed the generated fixture JSON; the check
exited 1 with
`Error: validation auxiliary fixture generation failed: generated artifact is stale: …/validation-auxiliary-witness-v1.generated.json`.
Restored from backup (`cmp` clean), re-ran `--check` → exit 0.

**Failure evidence.** Widened one witness constructor field to `Data.Any()` in
the SDK schema → the new opaque-field property failed naming the field; the
round-trip and arity legs still passed, confirming the property is load-bearing.
Restored, green.

**Verification.** tsc/eslint/prettier clean; 5/5 pass.

---

## 7. demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts (recommend: derive-oracle)

**Contract.** The production fault-proof builder applies CEK program-material
identity as the third parameter of the execution-selection resolver, in
blueprint-declared order.

**What changed.**

- The parameter title lists are no longer transcribed: they are read off the
  blueprint entry itself, and the builder's applied argument list is asserted
  positionally against them through a reviewed `bindings` table and an
  `applied()` helper.
- The two positional claims that carry the actual contract are kept explicitly by
  hand and commented as reviewed:
  `selectionTitles[2] === "cek_program_material_script_hash"` and
  `contextStepTitles[0] === "cek_context_control_script_hash"`.
- The hard-coded `semanticResolvers[68]` / `[69]` indices are gone; the indices
  are derived via `Object.keys(...semantics).indexOf(...)`.
- Restored the #605 always-succeeds guard that had been deleted, as a loop over
  all strict prefixes of the parameter list.

**Failure evidence.** Reordered two arguments in the execution-selection
resolver's application in
`demo/midgard-sdk/src/fraud-proof/contracts/families/validation-trace-dispute.ts`
→ the positional assertion failed at the `cek_program_material_script_hash`
position. Separately, disabling the arity guard made the restored #605 leg fail
on an under-applied (always-succeeds) script. Both restored, green.

**Verification.** tsc/eslint/prettier clean; all pass.

---

## 8. demo/midgard-sdk/tests/withdrawn-input.test.ts (recommend: keep)

Left as-is per the row's `keep`. No changes. Re-run green as part of the batch
run. Its noted weakness (same-schema round trips at L45/L67 with no absolute
vector) is recorded here as a follow-up candidate, not acted on, because the row
did not ask for it.

---

## 9. demo/midgard-sdk/tests/withdrawn-reference-input.test.ts (recommend: strengthen)

**Contract.** withdrawn-reference-input V1 step-02/step-03 state field order on
the wire, the step-03 counted withdrawal-membership args shape, and the thread
token asset name's category-id validation.

**What changed.**

- Replaced the three `toBeDefined()` schema-lookup assertions with schema
  _identity_ assertions plus a demonstration that the returned schema is really
  the one that step's datum is written with (an absolute CBOR encoding through
  the resolved schema), and a cross-step rejection: the step-02 schema must
  refuse a step-03 shaped state.
- Replaced the step-03 args round-trip-plus-nine-byte-prefix-regex with a full
  absolute CBOR vector, assembled from commented segments so each field's
  contribution is readable.

**Failure evidence.** Reordered two fields in the step-03 args schema in
`demo/midgard-sdk/src/fraud-proof/withdrawn-reference-input.ts` → the absolute
vector assertion failed with the exact differing hex. Transposed two arms of the
`withdrawnReferenceInputStepDatumSchema` switch → the identity assertion failed.
Restored, green.

**Verification.** tsc/eslint/prettier clean; all pass. (One tsc fix was needed
after the fact: the resolver's return type is the union of the three step
schemas, so the encodings go through the resolved value narrowed to the step-02
schema, with the narrowing licensed by the identity assertion immediately above.)

---

## 10. demo/midgard-sdk/tests/cek-context-wire.test.ts (recommend: strengthen)

**Contract.** `deriveCekContextBinding` reproduces the Aiken binder's bound and
staged projections from a golden context vector; `encodeCekContextRedeemer` pins
both the original binder arity and the shared-item-successor extension.

**What changed.**

- Deleted the circular oracle `expect(hashCekCoreWitness(vector)).toBe(golden.blake2b256)`
  — provably identical to the SDK's own hash of the same vector, i.e. a
  self-checksum.
- Added a canonicality check on the encoded witness, two perturbation cases
  (auxiliary field and `transactionId`) that must change the binding, and a
  refusal case built by CBOR surgery (`decodeSingleCbor`/`encodeCbor`) on the
  nine-field work witness.
- Traps recorded while doing this: work-witness field 0 is _nested_ CBOR bytes,
  and most frontier-peak indices are empty — the live peak is native index 15;
  the `Data.Nullable` wrapper is `d8799f`, not `d87a9f`.

**Failure evidence.** Perturbed the staged projection in
`demo/midgard-sdk/src/fraud-proof/cek-context.ts` → the bound/staged assertions
failed at the exact projection. Restored, green.

**Residual provenance gap (declared, not closed).** The fixture
`demo/midgard-sdk/tests/fixtures/cek-context-binding.json` still has no generator
and no Aiken-side producer recorded in the JSON. Emitting it from the Aiken side
means editing `onchain/`, which is outside this batch's scope; the circular
oracle is gone and the remaining assertions are perturbation-based, so the file
no longer _claims_ unverifiable provenance, but the cross-language channel is
still owed.

---

# Verification summary (run from demo/midgard-sdk)

```
npx prettier --check <12 files>                      → All matched files use Prettier code style!
npx eslint <12 files> --max-warnings=0               → clean
npx tsc --noEmit                                     → clean
npx vitest run <the 10 batch test files>             → Test Files 10 passed (10), Tests 73 passed (73)
npx vitest run tests/da-attestation-rotation.test.ts tests/availability-challenge.test.ts
                                                     → Tests 39 passed (39)
```

(the 12 files = the 10 batch tests + `src/user-events/internals.ts` + `src/da-attestation.ts`)

From demo/midgard-validation:

```
node scripts/generate-validation-auxiliary-witness-v1-fixture.mjs --check  → exit 0
(with a perturbed fixture)                                                → exit 1, "generated artifact is stale"
```

Never more than 2 vitest processes at a time; no whole-workspace command run.

# Src edits (both row-mandated)

1. `demo/midgard-sdk/src/user-events/internals.ts`
   — injected clock parameter with `Date.now` default (row 3's `by_construction`).
2. `demo/midgard-sdk/src/da-attestation.ts`
   — additive `reason` field on `DaAttestationBuildError` + exported
   `DaAttestationBuildFailureReason` union, threaded through `failBuild`
   (row 4's `by_construction`). No control flow changed.

# Out-of-batch edits (permitted by COMMON (a)/(b), driven by row 6's NOTE)

- `demo/midgard-validation/scripts/generate-validation-auxiliary-witness-v1-fixture.mjs` (broken import path)
- `demo/midgard-validation/package.json` (`fixtures:validation-auxiliary-witness-v1:sync` / `:check`)
- `.github/workflows/midgard-node-ci.yml` (fail-closed check step)

# Reds

None outstanding in this batch. The single baseline red (fault-proof.test.ts
stale allowlist) is fixed, per the task message's explicit in-scope note. No
other pre-existing red was encountered in the files or suites run here.

# Rulings received

None — no escalation was required. Two decisions were settled by measured
evidence rather than by guessing, and are called out above so they can be
overturned: the 14 KiB size pin's replacement (§1) and the deliberate
non-completion of the DA-attestation emulator conversion (§4).
