# Batch 03 report — demo/midgard-fault-proofs (9 files)

Repo: /home/gumbo/midgard-hub/midgard, branch colll78/canonical-v1-watcher-l1-source-checkpoint.
Package dir for all commands: demo/midgard-fault-proofs

All 9 files done. No production `src/` change was kept. One new test-support helper added.

## Files changed

- demo/midgard-fault-proofs/tests/resolved-output-non-canonical-publication-fit.test.ts
- demo/midgard-fault-proofs/tests/submit-init-emulator-observers-forbidden-publication.test.ts
- demo/midgard-fault-proofs/tests/zero-input-wrongful-rejection-publication-fit.test.ts
- demo/midgard-fault-proofs/tests/support/emulator/catalogue-registration.test.ts
- demo/midgard-fault-proofs/tests/resolved-output-non-canonical-workflow.test.ts
- demo/midgard-fault-proofs/tests/bin.test.ts
- demo/midgard-fault-proofs/tests/missing-signature-envelope.test.ts
- demo/midgard-fault-proofs/tests/structural-na-event-window-variants.test.ts
- demo/midgard-fault-proofs/tests/remove-fraudulent-block.test.ts
- NEW: demo/midgard-fault-proofs/tests/support/blueprint-abi.ts (fail-closed reader for the compiled blueprint's `definitions` section; owned by this batch only)

---

## 1. tests/resolved-output-non-canonical-publication-fit.test.ts (strengthen)

**Contract.** Every applied resolvedOutputNonCanonical script publishes as a reference-script UTxO under the 15,872-byte reliability reserve.

**Changed.**

- Removed `describe.runIf(hasFamily)` (rule 14: a dropped/renamed validator family made the whole suite vanish and report green).
- Added a fail-closed assertion inside the test: the blueprint must declare every `RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES` entry (asserted as `missingTitles == []`, so the failure names which title is gone).
- Added `steps.length === TITLES.length` so a shrunken family cannot pass with a short loop.
- Added a per-step assertion message identifying the step index.

**Failure evidence.** Shared with file 3 (same construction, same helper, same claim) — see §3.

## 2. tests/submit-init-emulator-observers-forbidden-publication.test.ts (strengthen)

Same contract/shape for `OBSERVERS_FORBIDDEN_BLUEPRINT_TITLES`; same three edits (runIf removed, fail-closed title assertion, step-count assertion). Failure evidence shared with §3.

## 3. tests/zero-input-wrongful-rejection-publication-fit.test.ts (strengthen)

Same three edits for `ZERO_INPUT_BLUEPRINT_TITLES`.

**Failure evidence (controlled fault).** Built a doctored blueprint at
`<scratch>/plutus-missing-zero-input-step02.json` = the real `onchain/aiken/plutus.json` with the validator `fraud_proofs/zero_input/step_02.main.spend` removed, then:

```
MIDGARD_REAL_BLUEPRINT_PATH=<scratch>/plutus-missing-zero-input-step02.json \
  npx vitest run tests/zero-input-wrongful-rejection-publication-fit.test.ts
```

New version FAILS at the intended check:
`the blueprint must declare every zeroInput validator title: expected [ Array(1) ] to deeply equal []`

Baseline proof that this was previously undetected: I wrote HEAD's version of the same file to a temporary path and ran it against the same doctored blueprint:
`git show HEAD:demo/midgard-fault-proofs/tests/zero-input-wrongful-rejection-publication-fit.test.ts > tests/zzz-headversion-evidence.test.ts`
→ `Test Files 1 skipped (1) / Tests 1 skipped (1)` — no failure. Temp file deleted afterwards (`rm -f`, absence verified).

No src change. The doctored blueprint lives only in the scratchpad; `onchain/aiken/plutus.json` was never written.

## 4. tests/support/emulator/catalogue-registration.test.ts (strengthen, brittle)

**Contract.** The emulator harness registers every appended fraud-proof category at its canonical category id and its family's first-step script hash, and publishes a membership proof that actually witnesses that pair under the published catalogue root.

**Changed.**

- The hand-maintained `expect(APPENDED_CATEGORY_NAMES).toHaveLength(43)` pin was already deleted in the working tree before I started (uncommitted wave; `git show HEAD:...` still has it). That left the loop able to run vacuously. Replaced with a derived split (`FOUNDATIONAL_CATEGORY_COUNT = 11`, named and commented) plus a non-vacuity guard `APPENDED_CATEGORY_NAMES.length > 0`. Adding a new fraud-proof category no longer fails this test.
- Removed `expect(category.membershipProofCbor).not.toBe("")` (rule 4 presence check).
- New test `publishes a membership proof that reproves each pair under the published root`: rebuilds the catalogue trie with the third-party `@aiken-lang/merkle-patricia-forestry` `Trie` from the declared `(categoryId, scriptHash)` pairs, then requires
  (a) the rebuilt root == `harness.catalogue.root`,
  (b) each rebuilt proof `.verify(true)` recomputes that root, and
  (c) each published `membershipProofCbor` equals the rebuilt proof's CBOR, per category.

**Failure evidence (controlled fault).** In `tests/support/emulator/catalogue.ts`, the trie-insert loop was changed to register `minAda` under a wrong hash:
`encodeCatalogueValue(name === "minAda" ? "99".repeat(28) : category.scriptHash)`.

Result: the new test FAILS at the root comparison —
`expected '14029f3dfc602104b9d3bef079cf370f41b63…' to be '25afd99426b623d664dc3937cf708a1fab51f…'`
while the three pre-existing `registers the selected real … first step` cases stayed green, confirming the added protection is new. Mutation reverted with `git checkout --`; `git diff --stat tests/support/emulator/catalogue.ts` empty afterwards.

## 5. tests/resolved-output-non-canonical-workflow.test.ts (strengthen)

**Contract.** `nextResolvedOutputAction` maps every durable workflow stage to the next submitter action.

**Changed.** Rewritten from five sampled states to:

- an assertion that `RESOLVED_OUTPUT_STAGES` is exactly the nine documented stages (a dropped/added stage fails);
- an exhaustive `it.each(RESOLVED_OUTPUT_STAGES)` against a hand-written `EXPECTED_NEXT_ACTION` table stated from the family's documented step order, not read back out of the production switch (rule 2). `step01`, `step02` and `step04`-adjacent stages that were never exercised are now covered;
- `terminates only after removal or cancellation` — `done` maps exactly to `["removed","cancelled"]`, so an always-`done` implementation cannot pass;
- `owes a distinct submission at every non-terminal stage` — no two stages may collapse onto the same action.

**Failure evidence (targeted mutation).** In `src/resolved-output-non-canonical/workflow.ts`, `case "step02": return "submitStep02"` → `return "submitStep03"`. Two tests failed at the intended checks:

- `owes step02 its documented action` → `expected 'submitStep03' to be 'submitStep02'`
- `owes a distinct submission at every non-terminal stage` → `expected 6 to be 7`
  Reverted; `git diff --stat src/resolved-output-non-canonical/workflow.ts` empty.

**Not done (noted, no src edit).** `nextResolvedOutputAction` returns `undefined` for a stage outside the union rather than throwing. Making that fail closed is a production change the row did not call for, so it is reported rather than made.

## 6. tests/bin.test.ts (strengthen)

**Contract.** The CLI parses each documented verb's flags into its parsed shape and refuses retired flags, unknown categories, duplicate reference inputs, missing required arguments, and legacy diagnostic inputs on authenticated lanes.

**Changed.**

- **Circular oracle removed (was L410-412).** The unknown-category test built its expected message by re-running the production formatting expression over `SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`. Replaced with `refuses an uncatalogued category and enumerates the catalogue in order`, which captures the thrown message and decodes it with an independently written test-side parser (strip the `--fraud-category must be one of ` prefix and the trailing `.`, split on `", "`, require each entry quoted) and then compares the decoded list with the SDK catalogue order. The production encoder is no longer the oracle for its own output. It also fails explicitly if `parseArgs` accepts the bad category at all.
- **Accept/reject pairing added (rule 5).** `it.each(SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)` asserts every catalogued category parses on `submit-init` — an always-rejecting parser now fails 54 cases.
- **Filler consolidated (rule 15, no scenario lost).** Removed the standalone `accepts the zeroInput fault-proof category` and the four/two redundant single-category echo blocks inside `parses invalid-range …` and `parses non-existent-input …` (transitionTrace, validationTraceDispute, nonExistentInputNoIndex, nonExistentInput, invalidRange) — all now covered exhaustively by the table. The surviving `submit-init` case was upgraded from a single `fraudCategory` echo to a `toMatchObject` over command + blueprint + deployment-info + fraudulent-block out-ref + category, so it protects the subject binding rather than one flag rename. Two test titles renamed to match what they now claim.
- Prepare/submit flag-shape cases for the input-no-idx, validation-dispute and non-existent-input verbs were left intact: they exercise distinct verbs and distinct flag sets (rule 9), and their refusal partners (`--validation-dispute-role`, retired `--allow-incompatible-output`, duplicate `--reference-input`, missing `--transition-fault-proof`, missing `--expected-transactions-root`, missing `--fraudulent-header-hash`) already exist in the file.

**Failure evidence (two targeted mutations on `src/bin.ts`).**

1. Message truncated: `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` → `.slice(0, 5)` in the refusal message. FAILED at
   `refuses an uncatalogued category and enumerates the catalogue in order → expected [ 'doubleSpend', …(4) ] to deeply equal [ 'doubleSpend', …(53) ]`.
2. Refusal weakened: the category lookup changed to `candidate.toLowerCase() === value.toLowerCase().replace(/-/gu, "")`, i.e. it accepts `invalid-range`. FAILED at
   `parseArgs accepted the uncatalogued category "invalid-range".`
   Both reverted; `git diff --stat src/bin.ts` empty afterwards.

Result: 71 tests pass (was 60-ish before consolidation; the net change is fewer echo tests and one exhaustive table).

## 7. tests/missing-signature-envelope.test.ts (strengthen, brittle)

**Contract.** The missing-signature family fits the L1 transaction envelope and the release ExUnits basis at the deepest inclusion proof, the first automatic tier-2 witness frontier, and the maximum admissible field-7 vector; field-4/7 carriage tiers change at the exact documented crossings.

**Changed.**

- `EXPECTED_UNAPPLIED_BYTES` (the seven-byte-count churn pin the row wanted deleted) had already been removed in the working tree before I started (`git show HEAD:` still has it). What was left behind was a stale test title (`pins all seven unapplied sizes…`) and a weakened first test. Fixed:
  - retitled to `applies four parameter-distinct steps that fit reference-script deployment`;
  - added `expect(chain).toHaveLength(MISSING_SIGNATURE_STEP_COUNT /* 4 */)` so the distinctness check cannot pass on a truncated chain;
  - added a **new discriminating check**: rebuilding the chain with a different `fraudProofPolicyId` must move _every_ step hash (`no step may survive a parameter change unchanged`). This is the protection the deleted byte pin was standing in for — that the applied parameters are load-bearing — expressed as a property rather than a compiler-churn pin.
- Named the repeated release ExUnits constants `RELEASE_MAX_EXECUTION_MEMORY = 13_200_000n` / `RELEASE_MAX_EXECUTION_STEPS = 8_000_000_000n` with a provenance comment (rule 12: a pin must say what it protects).
- The exact prover-journey transaction counts (9 and 14) are **kept** as exact counts — an extra submission is a real cost/liveness regression and a missing one means a skipped step — but are now named `TIER2_FRONTIER_JOURNEY_TX_COUNT` / `MAX_FIELD07_JOURNEY_TX_COUNT` with a comment stating their composition (proof steps + init + tier-forced carriage publications) and why they are exact rather than upper bounds. The row's `by_construction` addressed only `EXPECTED_UNAPPLIED_BYTES`, so I did not delete them.

**Failure evidence (controlled fault).** In `tests/support/emulator/contracts.ts`, `buildMissingSignatureChain` was made to ignore the `fraudProofPolicyId` argument for step-04 (`"22".repeat(28)` hard-coded). FAILED at the intended check:
`no step may survive a parameter change unchanged: expected [ …(3) ] to deeply equal []`.
Reverted; `git diff --stat tests/support/emulator/contracts.ts` empty.

## 8. tests/structural-na-event-window-variants.test.ts (strengthen)

**Contract.** The SDK's `TransitionFault` encoding for the six Q47 omitted / out-of-window event-window variants matches the Aiken constructor indices, witness indices and field arities, and root domain plus event id remain load-bearing discriminators.

**Changed.**

- **Oracle de-transcribed (the row's main finding).** The hand-transcribed constants `OMITTED_DUE_L1_EVENT_INDEX = 5n`, `OUT_OF_WINDOW_SOURCE_EVENT_INDEX = 7n` and `DEPOSIT/WITHDRAWAL/FORCED_VARIANT_INDEX` are gone. All constructor indices, field orders and arities are now read out of the compiled blueprint's `definitions` section via the new `tests/support/blueprint-abi.ts`, keyed by Aiken title:
  - `midgard/fraud_proofs/transition_trace/proof/TransitionFault`
  - `…/OmittedDueL1EventWitness`, `…/OutOfWindowSourceEventWitness`
  - `midgard/transition_trace/RootDomain`, `midgard/rejection_reason_v1/OperatorVerdictV1`, `midgard/ledger_state/WithdrawalValidity`, `midgard/ledger_state/EventKey`, `midgard/transition_trace/Root{Non,}MembershipProof<…>`
    An Aiken-side reorder is now caught without a human re-transcribing. The helper fails closed (throws) on a missing blueprint, a missing `definitions` section, a missing definition, or an untitled/unindexed constructor.
- **`roundTrips()` is no longer the compatibility oracle** (rule 4: a round trip alone does not establish wire-format compatibility). Each arm now asserts, through the new `expectArmMatchesBlueprint`: the blueprint-declared witness field-title order matches the stated expectation; the outer and witness constructor indices match the blueprint; the encoded field arity matches; the value the SDK placed at each declared scalar position (`event_ref_input_index`, `event_asset_name`, `validity_override`) is the expected one, with `validity_override` rendered from the blueprint's own enum index rather than re-encoded through the SDK; and the trailing `source_(non_)membership` field is a constructor at the blueprint-declared index and arity whose `domain` leaf sits at the blueprint's `RootDomain` index for that event kind. The round trip is retained only as a secondary check, labelled as such.
- `EVENT_REF_INPUT_INDEX` changed from `0n` to `3n` so a transposition of the two leading scalar fields cannot encode identically.
- **Discriminator tests strengthened.** `expect(a).not.toBe(b)` + equal-length (row: "assert only that two encodings differ and share a length") replaced with: decode both proofs, compare field-by-field against the blueprint's declared `RootNonMembershipProof` field list, and require the differing set to be exactly `["domain"]` resp. exactly `["key"]`. The `EventKey` block now also requires each of the three event-kind constructors to sit at the index the compiled ABI declares.

**Failure evidence (two targeted mutations on `demo/midgard-sdk/src/fraud-proof/transition-trace.ts`).**

1. `OmittedDueL1EventWitnessSchema`: the `OmittedDueDeposit` and `OmittedDueWithdrawal` arms swapped (an Aiken/SDK-side constructor reorder). Two tests FAILED at the intended checks:
   `OmittedDueDeposit constructor index: expected 1 to be +0` and `OmittedDueWithdrawal constructor index: expected +0 to be 1`.
2. `OmittedDueForcedTransaction`: `validity_override` moved ahead of `event_asset_name` (a field reorder within an arm). FAILED at
   `OmittedDueForcedTransaction.event_asset_name at field 1: expected 'C0()' to be 'B01'`.
   Both reverted; `git diff --stat demo/midgard-sdk/src/fraud-proof/transition-trace.ts` empty afterwards.

**Note on the second half of `by_construction`.** The row also suggested pinning expected CBOR bytes per arm as a cross-implementation vector. I did not do that: the only available source for such bytes today is the SDK encoder under test, so a byte pin would be a baseline copied from current output (rule 12) — it would freeze behavior without independently proving it. The blueprint-derived structural assertions above give the cross-implementation property the vector was wanted for. If a genuine external vector (Aiken-side generated) is later produced, it should be added as a real vector then.

**Note on the blueprint.** `onchain/aiken/plutus.json` is gitignored and built locally (`MIDGARD_REAL_BLUEPRINT_PATH` overrides it). This test now needs it; that is the same precondition the other emulator suites in this package already have, and the helper throws rather than skipping if it is absent.

## 9. tests/remove-fraudulent-block.test.ts (strengthen)

**Contract.** Q53 fraud-slash economics resolve to exact bond tranches and reject illegal bonds or F04-violating manifests; the HTTP state-queue mutation lease coordinator posts fenced acquire/renew/release/fail actions and refuses a substituted coordinator source.

**Changed (lease-coordinator half only; the Q53 economics half was left as-is — it already uses independently stated tranche values, boundary neighbours, and causal negatives).**

- The old `vi.stubGlobal` responder invented its response bodies as `` `${action}ed` `` — i.e. it answered `"releaseed"` and `"acquireed"`, shapes midgard-node never returns (rule 8: an invented third-party behavior). Replaced with `leaseEndpointResponse`, a fake whose bodies mirror the real handler `resolveStateQueueMutationLeaseRequest` in `demo/midgard-node/src/commands/listen-router.ts`: `{status:"acquired", token}` at 200, `{status:"busy", activeLease}` at 409, `{status:"renewed"|"released"|"failed"}` at 200, and the documented 400 for a missing action. The block carries a comment naming the source of truth and stating explicitly that this is a fake, not a conformance harness, and why (see ESCALATION below).
- Added causal negatives that exercise response shapes the real server can actually return (rule 5):
  - `fails a busy acquire, naming the action, the status and the held lease` — the real 409 busy body has **no** `error` field, so this exercises the coordinator's `responseError` fallback and pins the operator-facing sentence including the serialized held lease.
  - `refuses an acquire response that is not an acquired lease` — a 200 carrying `{status:"busy", token, activeLease:null}` (server drift) must fail closed rather than fence a mutation on a lease that was never granted.
  - `names the failing action rather than a constant sentence` — a 500 on `renew` after a successful `acquire`, proving the pinned sentence at the old L219-238 is composed from the actual action and status rather than being a constant.
- Strengthened the happy-path case: also asserts every request is `POST` with `content-type: application/json` (previously only URL/admin-header/body).
- Strengthened the resume case: `expect(calls).toEqual([])` after the substituted-source refusal, i.e. the refusal must happen **before** any request reaches the node (rule 5: a refusal contract that prohibits side effects must be checked for the absence of the side effect).

**Failure evidence (three targeted mutations on `src/remove-fraudulent-block.ts`).**

1. Acquire guard weakened to also accept `status === "busy"`. FAILED at
   `refuses an acquire response that is not an acquired lease → promise resolved "{ token: 'lease-token', …(4) }" instead of rejecting`.
   (A first attempt at this mutation produced a downstream `Cannot read properties of undefined` crash rather than the intended check; that is not valid evidence per rule 3, so the test fixture was given a token-shaped field and the mutation redone to fail cleanly at the intended assertion.)
2. The action interpolated into the refusal message replaced by the constant `acquire`. FAILED at
   `names the failing action rather than a constant sentence → expected … 'POST /stateQueueMutationLease renew f…' but got 'POST /stateQueueMutationLease acquire…'`.
3. `resume` made to `post({action:"renew", token})` before the source check. FAILED at
   `resumes only the exact journaled coordinator source and fencing token → expected [ { …(4) } ] to deeply equal []`.
   All three reverted; `git diff --stat src/remove-fraudulent-block.ts` empty afterwards.

### ESCALATION — cross-package conformance for /stateQueueMutationLease

**Decision needed.** The row's `why_low` asks for conformance against midgard-node's real `/stateQueueMutationLease` handler. That joined test cannot live in `demo/midgard-fault-proofs`: `demo/midgard-node/package.json` already declares `"@al-ft/midgard-fault-proofs": "workspace:*"`, so importing midgard-node from here would create a dependency cycle, and a relative `../../midgard-node/src/commands/listen-router.js` import would pull in the node's database/worker graph (3,217-line module, `Database`, `SqlClient`, fibers) into a pure-unit test file.

**What exists today.** Both halves are tested against fakes and nothing joins them:

- client half: `demo/midgard-fault-proofs/tests/remove-fraudulent-block.test.ts` (now a fake conformant to the documented protocol);
- server half: `demo/midgard-node/tests/state-queue-mutation-lease-endpoint.test.ts` drives the real `resolveStateQueueMutationLeaseRequest` against a fake `StateQueueMutationLeaseEndpointStore`.

**Recommended resolution (not taken — outside this batch's file list and outside this package).** Add one test in `demo/midgard-node/tests/` that wires `createHttpStateQueueMutationLeaseCoordinator` (imported from `@al-ft/midgard-fault-proofs`, which midgard-node already depends on) to `resolveStateQueueMutationLeaseRequest` through a `fetch` shim over the in-memory lease store, and drives acquire → renew → release and acquire → fail plus the busy 409. That is the direction the cycle allows. It needs an owner ruling because it puts a fault-proofs client test in the node package.

Residual risk until then: server-side protocol drift on `/stateQueueMutationLease` (renamed action string, changed success `status` value, changed error envelope) still passes the fault-proofs suite.

---

## Verification (all run from demo/midgard-fault-proofs)

```
npx tsc --noEmit
```

9 errors, all pre-existing and in files this batch did not touch:
`src/workflow/funding-reservation-permit.ts` (4) and `tests/workflow-runtime.test.ts` (5, TS7006 implicit-any on `vi.mockImplementation` callbacks). Both files are part of the uncommitted working-tree wave; none of the 10 files in this batch produced an error.

```
npx eslint tests/resolved-output-non-canonical-publication-fit.test.ts \
  tests/submit-init-emulator-observers-forbidden-publication.test.ts \
  tests/zero-input-wrongful-rejection-publication-fit.test.ts \
  tests/support/emulator/catalogue-registration.test.ts \
  tests/resolved-output-non-canonical-workflow.test.ts \
  tests/bin.test.ts tests/missing-signature-envelope.test.ts \
  tests/structural-na-event-window-variants.test.ts \
  tests/remove-fraudulent-block.test.ts tests/support/blueprint-abi.ts --max-warnings=0
```

→ clean (ESLINT-OK).

```
npx prettier --check <same 10 files>
```

→ `All matched files use Prettier code style!`

```
npx vitest run tests/resolved-output-non-canonical-publication-fit.test.ts \
  tests/submit-init-emulator-observers-forbidden-publication.test.ts \
  tests/zero-input-wrongful-rejection-publication-fit.test.ts \
  tests/support/emulator/catalogue-registration.test.ts \
  tests/resolved-output-non-canonical-workflow.test.ts \
  tests/bin.test.ts tests/missing-signature-envelope.test.ts \
  tests/structural-na-event-window-variants.test.ts \
  tests/remove-fraudulent-block.test.ts
```

→ `Test Files 9 passed (9) / Tests 122 passed (122)`, 18.47s.

No Postgres was needed by these suites (the emulator suites in this package run in-process); no connection error appeared.

## src edits

None retained. Every production-path change was a temporary mutation for failure evidence, reverted with `git checkout --` and confirmed by an empty `git diff --stat` on the exact path:

- `demo/midgard-fault-proofs/src/bin.ts` (2 mutations)
- `demo/midgard-fault-proofs/src/remove-fraudulent-block.ts` (3 mutations)
- `demo/midgard-fault-proofs/src/resolved-output-non-canonical/workflow.ts` (1)
- `demo/midgard-sdk/src/fraud-proof/transition-trace.ts` (2)
- `demo/midgard-fault-proofs/tests/support/emulator/contracts.ts` (1, test support)
- `demo/midgard-fault-proofs/tests/support/emulator/catalogue.ts` (1, test support)

All six paths report an empty `git diff --stat HEAD -- <path>` now, and none of them appears in the working tree's modified set, so no uncommitted wave edits were destroyed. `npx tsc --noEmit` typechecking cleanly against the rest of the wave corroborates this.

## Anything skipped and why

- No `wire-into-ci` row in this batch; all 9 files are matched by the package's `vitest.config.ts` include (`./tests/**/*.test.{ts,tsx}`) and therefore by `pnpm test`, including `tests/support/emulator/catalogue-registration.test.ts`.
- CBOR byte vectors for the Q47 arms — see §8 note (would be a self-derived baseline today).
- Cross-package lease-endpoint conformance — see ESCALATION in §9.
- `nextResolvedOutputAction` returning `undefined` for an out-of-union stage — production hardening not called for by the row; reported, not changed.
