---
name: adding-fault-proof-families
description: Use when adding a new fault-proof (fraud-proof) family or catalogue category to Midgard, completing a partly registered family, or routing a new RejectionReason arm to a family. Covers the Aiken step validators, the catalogue ID and deployment-manifest identity, the SDK contract chain, the family application registry, the emulator lifecycle tests in both polarities, the watcher and devnet journey wiring, and the fault-proof status docs. Triggers - FRAUD_PROOF_CATALOGUE_CATEGORY_IDS, FAMILY_APPLICATION_REGISTRY, TYPED_REASON_DISPOSITIONS, "new fraud proof", "new family", step-01.ak under validators/fraud-proofs.
---

# Adding fault-proof families

A fault-proof family is one catalogue category carried end to end: Aiken step
validators, a catalogue ID, deployment identity, an SDK contract chain, a
family record the watcher installs, emulator lifecycles, and docs. The pieces
live in about ten packages, and most gaps typecheck cleanly. Work from the
checklist script's output, not from memory:

```bash
node .agents/skills/adding-fault-proof-families/scripts/family-checklist.mjs <category>
```

It exits 0 when every gated artifact is present, 1 on gaps (each `GAP` line
names the file), 2 when it could not read or parse a source (no result, fix
the path or the parser), and 64 on bad usage. It checks presence, not
correctness. Test evidence is matched by file name and is labelled heuristic.

Read [AGENTS.md](../../../AGENTS.md), [docs/agents/contracts.md](../../../docs/agents/contracts.md)
and the glossary entries under "Fault-proof workflow assembly" in
[CONTEXT.md](../../../CONTEXT.md) first.

## Eligibility: is this a new family?

Decide before writing code. The authority is decision
[0002 bounded proof threads](../../../docs/fault-proofs/decisions/0002-bounded-proof-threads.md).

| The change                                                                                                                                        | Path                                                                                                                                                                                                                                          |
| ------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| A deterministic fault with its own authenticated subject shape, decisive predicate, resumable state shape or evidence frontier (0002, "Decision") | New family. Follow every step below.                                                                                                                                                                                                          |
| An existing `RejectionReason` arm that no family serves yet, whose predicate an existing family already proves                                    | Extend that family: its rule and scan, and its `TYPED_REASON_DISPOSITIONS` entry. Skip steps 2 and 3. Several arms may share one family (four witness-script arms route to `witnessScriptDecoding`, `reason-disposition.ts:53`).              |
| A registered family missing a record, tests or docs                                                                                               | Completion. Run the script and fill each `GAP`.                                                                                                                                                                                               |
| Reordering, renumbering or removing a catalogue category                                                                                          | Stop. The order and IDs are append-only; deployment identity and thread asset names derive from them (`catalogue.ts:90`, `classification.ts:341`). Ask the owner.                                                                             |
| A second direct family for a reason and source kind that already has one                                                                          | Stop. At most one direct category per source kind (`reason-disposition.ts:20`). Ask the owner.                                                                                                                                                |
| A new `RejectionReason` constructor                                                                                                               | Stop. It changes the forced-verdict ABI owned by `rejection-reason-v1.ak`, `midgard-sdk/src/rejection-reason.ts` and `docs/spec/midgard-tx.md` §13 ([catalogue](../../../docs/fault-proofs/rejection-reason-catalogue-v1.md)). Ask the owner. |
| A fourth family requirement flag beyond `replayContext`, `validationChallenge`, `historicalNativeScriptAuthority`                                 | Stop. Issue #668 capped the set; growing it "needs a new design discussion".                                                                                                                                                                  |
| A family that only fits with raised transaction limits or inline scripts                                                                          | Stop. Split the family instead (0002, "Alternatives and consequences"). The hard rules below apply.                                                                                                                                           |

## Hard rules

Each rule names what enforces it. Where the enforcement has a hole, the hole
is stated next to it.

1. **Reference scripts only.** A fault-proof transaction never attaches a
   script inline; every script resolves from a published reference script
   (owner ruling 2026-08-26, cited in
   `demo/midgard-fault-proofs/tests/submit-init-emulator.test.ts:694`). Call
   `requireReferenceOnlyScriptWitnesses` on every transaction your adapter
   builds. [runtime: requireReferenceOnlyScriptWitnesses]
   Blind spot: it only runs where an adapter calls it
   (`transaction-boundary.ts:104`). As of 2026-09-25, eight fault-proofs
   modules call it; the `mint-item-non-canonical` central journal does not.
   Nothing lints for inline attachment (plan item W4.4 is open).
2. **Every fraud-proof validator is published with a reference-script token.**
   Each contract the family deploys needs a role in
   `DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE` and a token name in
   `DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES`.
   [ci: Midgard Node CI/Build and test Midgard core DA transport]
   The test is `demo/midgard-core/tests/deployment-manifest-identity.test.ts:429`:
   only seven named core validators may lack a role.
3. **Emulator tests in both polarities, refused in the validator.** The real
   fault is proved to completion, and an adversary against an honest
   commitment is refused by the validator at the check that guards it, not by
   the off-chain builder ([contracts.md](../../../docs/agents/contracts.md),
   "Scenario Coverage"; decision 0002, "Authorities and verification").
   Assert negatives with `expectOnchainRefusal`
   (`tests/support/emulator/expect-onchain-refusal.ts`). [review]
   Partial machine help: `expectOnchainRefusal` fails a negative that the
   builder refused (it requires "failed script execution"), but it does not
   identify which check refused. `assertCompleteLifecycleCoverage`
   (`src/testing/complete-lifecycle.ts:35`) fails a lifecycle missing any of
   the seven `COMPLETE_LIFECYCLE_BASE_SCENARIOS`, but only in tests that opt in
   through `createLifecycleCoverageRecorder`: 16 lifecycle test files as of
   2026-09-25. No test maps each validator to its passing and failing
   scenarios (plan item W4.9 is open).
4. **Validators trust deployment parameters.** A validator never re-checks a
   `validator main(...)` parameter's width, cardinality or domain. Apply
   parameters only through `applyBlueprintParams`
   (`demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts:211`), which
   refuses a wrong arity (under-application compiles to an always-succeeds
   script, #609) and a wrong shape (`assertParameterShapes`, line 364).
   [runtime: applyBlueprintParams]
   Blind spot: nothing stops a direct `applyParamsToScript` call elsewhere
   (plan item W4.5 is open), and nothing flags a redundant on-chain parameter
   check.
5. **Compile and test Aiken only with the pinned fork.** Stock v1.1.22 has an
   unsound expect-decoder. Run `node onchain/aiken/scripts/pinned-compiler.mjs`
   before any Aiken command; it exits 0 only on the pin read from
   `.github/workflows/aiken-ci.yml`. Build commands and focused-test selectors
   are in [aiken-contract-build](../aiken-contract-build/SKILL.md).
   [script: onchain/aiken/scripts/pinned-compiler.mjs]
   CI asserts the same pin in "Assert the pinned compiler identity and put it
   on PATH" and "Assert no stock compiler is reachable".
6. **Never hand-edit generated Aiken.** Files headed "Generated by ... Do not
   edit" (10 under `onchain/aiken` as of 2026-09-25, for example
   `lib/midgard/fraud-proofs/canonical-decodability/rule-golden.test.ak`) are
   rewritten by their generator; change the generator and rerun it.
   [ci: Midgard Node CI/Check canonical-decodability golden vectors]
   Blind spot: only generators with a `:check` step in Midgard Node CI are
   caught (canonical-decodability, committed-field-shape, CEK core-step,
   transition-trace ABI, the native-V1 channels). A new family that generates
   Aiken must add its own check step.
7. **Never raise transaction limits to make a family fit.** Emulator tests run
   at `VAN_ROSSEM_TRANSACTION_LIMITS` (16,384 bytes, 16.5M memory, 10G steps,
   `tests/support/emulator/protocol-parameters.ts`). Split the family instead.
   [ci: Midgard Node CI/Build, typecheck, and test fault-proof tooling]
   The test "finds no positive limit escape across the fault-proof TypeScript
   surface" (`tests/wave0-shared-substrate.test.ts:131`) scans only
   `demo/midgard-fault-proofs`; a negative diagnostic needs the
   `MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN/END` markers.

## Steps

Use the newest family of the same shape as your template. Find its full
footprint with one grep. For `mintItemNonCanonical` it matched 82 files as of
2026-09-25, 50 of them outside the family's own directories:

```bash
git grep -l -e mintItemNonCanonical -e MintItemNonCanonical \
  -e mint-item-non-canonical -e mint_item_non_canonical -- demo onchain docs
```

[references/touch-points.md](references/touch-points.md) lists every surface
by package with what enforces it. Read it before step 2, and again whenever a
`GAP` line names an unfamiliar file.

1. **Choose the shape.** Linear (fixed step chain, derived from
   `LINEAR_FAMILY_CATEGORIES` in `linear-family-spec.ts`), cursor
   (`CURSOR_FAMILY_DEFINITIONS` in `family-definitions.ts`), or bespoke (a
   hand-written family record, like `mintItemNonCanonical`). Read
   [workflow-family-assembly.md](../../../docs/fault-proofs/workflow-family-assembly.md)
   and the nearest sibling in
   [family-reference.md](../../../docs/fault-proofs/family-reference.md).
   Done when you can name the template family and the step count.
2. **Write the Aiken.** Library rule and scan modules under
   `onchain/aiken/lib/midgard/fraud-proofs/<kebab>/` with a `rule.test.ak`,
   and step validators `validators/fraud-proofs/<kebab>/step-0N.ak`. Done when
   the module's tests pass with a nonzero collected count under the pinned
   fork, and `aiken fmt` leaves no diff.
3. **Register the catalogue identity.** Append the category to
   `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, give it an unused 8-hex ID in
   `FRAUD_PROOF_CATALOGUE_CATEGORY_IDS` (IDs are sparse; never derive one from
   the array position), add it to the SDK `FraudProofs` type, and mirror all of
   it in `demo/midgard-core/src/deployment-manifest-identity.ts`. Done when the
   script shows `OK` for `catalogue-order`, `catalogue-id`, `core-identity` and
   `reference-scripts`. [review]
4. **Build the SDK contract chain.** `contracts/families/<kebab>.ts` with the
   blueprint titles and `build<Name>Chain`, wired into `contracts/build.ts`,
   `index.ts` and `types.ts`. Done when `sdk-chain` is `OK`.
5. **Wire the node and the DA fixture.** Deployable scripts, the
   `always-succeeds` chain, manifest contract loading, and the node role-map
   mirror. Regenerate the DA fixture (see
   [references/verification.md](references/verification.md)). Done when the
   node typecheck and the manifest tests pass.
6. **Write the fault-proofs family.** `src/<kebab>/` (scan, replay, submit
   steps, cancel, workflow), a `FRAUD_PROOF_CLASSIFICATION_RULES` entry at the
   category's catalogue position, the reason disposition, complete-replay
   wiring, and the family record in `FAMILY_APPLICATION_REGISTRY` (derived for
   linear families). Done when `classification-rule` and `application-record`
   are `OK`.
7. **Prove both polarities in the emulator.** A lifecycle test that proves the
   real fault through the permanent proof token and removal, refuses honest
   and substituted evidence with `expectOnchainRefusal`, and covers cancel and
   resume. Record it with `createLifecycleCoverageRecorder` so
   `assertCompleteLifecycleCoverage` checks the scenario list. Done when every
   base scenario applicable to the family has a passing test and each negative
   fails in the validator.
8. **Wire the watcher and the devnet journeys.** The watcher's
   `CATALOGUE_CATEGORY_TO_CONTRACT` (`src/runtime/deployment-identity.ts:76`),
   any category-keyed funding tables, and a `JOURNEY_FIXTURE_OWNERS` entry
   with the pinned owner counts in `watcher-journeys/catalogue.test.ts`
   updated. Done when `journey-owner` is `OK` and the watcher suite passes.
9. **Update the docs in the same change.** The maintenance rule in
   [docs/fault-proofs/README.md](../../../docs/fault-proofs/README.md) names
   the catalogue status, coverage matrix, testing status, execution plan and
   [public testnet readiness](../../../docs/public_testnet_readiness.md). Add
   a `family-reference.md` section. Done when `catalogue-status-row` is `OK`
   and each named doc states the new count.
10. **Verify.** Run the script until it exits 0, then the suites in
    [references/verification.md](references/verification.md). Report which
    ran and their counts.

## Checklist

Tick an item only when its enforcement passed or, for `[review]` items, when
you checked it by reading.

- [ ] Eligibility row chosen and no "Stop" row applies. [review]
- [ ] Catalogue order and a unique 8-hex ID, mirrored in core.
      [ci: Midgard Node CI/Build, typecheck, and test Midgard SDK]
      (`fraud-proof-catalogue-registration.test.ts`)
- [ ] Every family contract has a reference-script role and token name.
      [ci: Midgard Node CI/Build and test Midgard core DA transport]
- [ ] Node role-map mirror equals core's.
      [ci: Midgard Node CI/Test Midgard node]
      (`deployment-manifest.test.ts:308`)
- [ ] DA deployment fixture regenerated.
      [ci: Midgard Node CI/Test Midgard node]
      (`da-deployment-fixture-generation.test.ts` fails on drift)
- [ ] Family record in `FAMILY_APPLICATION_REGISTRY`, roster names deployed
      contracts. [ci: Midgard Node CI/Build, typecheck, and test fault-proof tooling]
      (typecheck fails on an omitted category; `family-application-registry.test.ts:312`)
- [ ] Classification rule at the catalogue position.
      [runtime: FRAUD_PROOF_CLASSIFICATION_RULES]
      (the module throws at load; `tests/workflow.test.ts` pins the order)
- [ ] Every typed reason the family serves is routed to it.
      [ci: Midgard Node CI/Build, typecheck, and test fault-proof tooling]
      (`typed-reason-disposition.test.ts`; it cannot tell whether a reason
      belongs to your family rather than another)
- [ ] Both polarities, negatives refused in the validator. [review]
- [ ] Lifecycle recorded with `createLifecycleCoverageRecorder`. [review]
      (opt-in; `assertCompleteLifecycleCoverage` checks only recorded tests)
- [ ] Only reference scripts; adapter calls
      `requireReferenceOnlyScriptWitnesses`. [review]
- [ ] Parameters applied through `applyBlueprintParams` only. [review]
- [ ] Aiken compiled and tested under the pinned fork, formatted.
      [ci: Aiken CI/Compile and run the Aiken test suite with the pinned fork]
- [ ] No generated Aiken edited by hand. [review]
- [ ] Watcher deployment identity lists the category.
      [ci: Midgard Node CI/Build, typecheck, format-check, and test Midgard watcher]
      (inferred: its `exactRecord` parse rejects the core-built catalogue
      fixture; not observed failing)
- [ ] Devnet journey owner assigned and owner counts updated.
      [ci: Midgard Node CI/Typecheck, build, and test Midgard node tools]
      (the typecheck enforces the owner entry; the count test in
      `devnet/watcher-journeys/catalogue.test.ts` runs in no CI job)
- [ ] Status docs updated in the same change. [review]
      (`catalogue-status.md` says it "is not machine-checked"; plan item W5.6)
- [ ] `family-checklist.mjs <category>` exits 0.
      [script: .agents/skills/adding-fault-proof-families/scripts/family-checklist.mjs]
