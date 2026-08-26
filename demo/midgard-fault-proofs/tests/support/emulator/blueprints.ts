import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  Data,
  type Network,
} from "@lucid-evolution/lucid";

export const moduleDir = dirname(fileURLToPath(import.meta.url));

export const repoRoot = resolve(moduleDir, "../../../../..");

/**
 * The compiled on-chain artifact as it sits in the tree. These tests read it;
 * they never rebuild it.
 *
 * KNOWN RED, AND WHO OWNS IT. #584 retired `transaction_commitment` from
 * `ledger_state.L2TransactionSourceV1` and `ledger_state.ForcedInclusionTxV1`
 * without regenerating `plutus.json` — the blueprint still declares both
 * constructors with three and four fields respectively, while the encoders in
 * `@al-ft/midgard-sdk` now emit two and three. The stale script destructures a
 * field that is no longer there, so the scenario dies inside the validator with
 * an `EvaluatorError` reading `unexpected empty list`. #587 then retired the
 * counted publication receipt chain, which took `terminal_receipt_reference` out
 * of `ledger_state.TxOrderPayloadV1` — the same staleness, three more rows. #594
 * then gave the tx-order minting policy its own wrapped mint redeemer, a type the
 * frozen blueprint never declared at all — one more row, and the first one red for
 * a *missing* definition rather than a stale one. #596 then added the §12.7
 * canonical-decodability fault family, whose two new wire types the frozen
 * blueprint likewise never declared — two more rows of that same kind. #601 then
 * added the §12.8 committed-field-shape sibling family, which contributes exactly
 * one more of that kind: its step-02 thread state is a new type, while its claim
 * redeemer is §12.7's reused unchanged and is already row 15.
 * Regenerating the blueprint is #579's, and these are the SEVENTEEN tests it has
 * to turn green again.
 *
 * Six of them are emulator scenarios in this directory. All six die at the same
 * `Spend[0] unexpected empty list`:
 *
 *   1. `submit-init-emulator-soundness.test.ts` — "lets a challenger win against
 *      an operator who claimed Accepted over a non-empty claimed ledger delta"
 *   2. `submit-init-emulator-soundness.test.ts` — "rejects the cleared-delta
 *      rejection successor the deleted VM-DEFECT-2 clause required"
 *   3. `submit-init-emulator-soundness.test.ts` — "cannot be defeated when the
 *      operator honestly accepted a valid transaction carrying a non-empty ledger
 *      delta"
 *   4. `submit-init-emulator-transition-trace.test.ts` — "submits and removes a
 *      tail transition-trace fraud proof end to end"
 *   5. `submit-init-emulator-validation-dispute.test.ts` — "opens, bisects,
 *      resolves a fitting complete item by 'direct', and awards a validation
 *      dispute"
 *   6. `submit-init-emulator-validation-dispute.test.ts` — "opens, bisects,
 *      resolves a fitting complete item by 'reference', and awards a validation
 *      dispute"
 *
 * Rows 5 and 6 are the two `it.each` rows of one table, so a runner reports them
 * under one `FAIL` block with two names. The two *other* validation-dispute
 * scenarios — "publishes every authenticated validation-dispute control under the
 * exact L1 envelope" and "publishes and verifies the generated-blueprint CEK
 * semantic-resolver reference scripts" — **pass**: they
 * publish and re-read controls without ever spending an affected leaf, so the
 * stale arity never reaches a script. Running these three files gives
 * `6 failed | 2 passed (8)`.
 *
 * Rows 2 and 3 need reading rather than only re-running: their expected failure
 * *stage* moved (from prepare-selected / semantic-resolution to open), so they
 * present as an assertion about the wrong stage — `expected [Function] to throw
 * error matching /…prepare-sel…/ but got 'emulator lifecycle stage open failed:…'`
 * — rather than as an evaluator error, while being this same stale script
 * underneath. Row 1 surfaces the `EvaluatorError` directly.
 *
 * The remaining eleven are schema-parity rows, not emulator scenarios, and they
 * live in `demo/midgard-node/tests/sdk-aiken-schema-parity.test.ts`. They compare
 * each SDK `Data` schema against the blueprint definition of the same name, so
 * the stale arity shows up directly as a field-count mismatch rather than as an
 * evaluator error. Rows 7-10 are #584's; rows 11-13 are #587's, and the last two
 * of those are red only because they embed the payload. Rows 14-17 differ in kind
 * from those seven: their Aiken types are **absent** from the frozen blueprint
 * rather than stale, so each fails on a missing definition rather than on a
 * field-count mismatch. Row 14 is #594's — the tx-order minting policy's wrapped
 * mint-redeemer type, `user_events.MintRedeemer` carried beside the §8 carriage
 * vector. Rows 15 and 16 are #596's §12.7 canonical-decodability family, whose
 * claim redeemer and step-02 thread state are new types in that round. Row 17 is
 * #601's §12.8 committed-field-shape family, and it is one row rather than two
 * for a reason worth keeping: that family reuses §12.7's claim redeemer unchanged
 * (one accusation, one wire spelling) and declares only its own step-02 state,
 * which is structurally identical to §12.7's and deliberately a separate type
 * because the two verdict code spaces differ:
 *
 *   7. "matches ForcedInclusionTxV1Schema to
 *      midgard/ledger_state/ForcedInclusionTxV1 recursively"
 *   8. "matches L2TransactionSourceV1Schema to
 *      midgard/ledger_state/L2TransactionSourceV1 recursively"
 *   9. "matches ValidationSourceMembershipV1Schema to
 *      midgard/validation_claim_v1/ValidationSourceMembershipV1 recursively"
 *  10. "matches ValidationClaimWitnessV1Schema to
 *      midgard/validation_claim_v1/ValidationClaimWitnessV1 recursively"
 *  11. "matches TxOrderPayloadV1Schema to midgard/ledger_state/TxOrderPayloadV1
 *      recursively"
 *  12. "matches TxOrderEventV1Schema to midgard/ledger_state/TxOrderEventV1
 *      recursively"
 *  13. "matches TxOrderDatumV1Schema to midgard/user_events/tx_order_v1/Datum
 *      recursively"
 *  14. "matches TxOrderMintRedeemerV1Schema to
 *      midgard/user_events/tx_order_v1/MintRedeemer recursively"
 *  15. "matches CommittedFieldClaimV1Schema to
 *      midgard/fraud_proofs/canonical_decodability/rule/CommittedFieldClaimV1
 *      recursively"
 *  16. "matches CanonicalDecodabilityStep02StateSchema to
 *      midgard/fraud_proofs/canonical_decodability/step_02/State recursively"
 *  17. "matches CommittedFieldShapeStep02StateSchema to
 *      midgard/fraud_proofs/committed_field_shape/step_02/State recursively"
 *
 * That file gives `11 failed | 22 passed (33)`. Its total moved with the set four
 * times: #587 deleted the two mappings for the retired receipt datums and the one
 * for the retired receipt mint redeemer, because a mapping to a type that no
 * longer exists on the SDK side would assert a retired surface rather than measure
 * a stale blueprint; #594 then added row 14 for the tx-order minting policy's own
 * redeemer, which wraps `user_events.MintRedeemer` beside the §8 carriage vector;
 * #596 then added rows 15 and 16 for the §12.7 family's claim redeemer and thread
 * state; #601 then added row 17 for the §12.8 family's thread state. #596 also
 * measured what the whole set does against a *regenerated* blueprint rather than
 * only predicting it: pointed at a scratch stock build of its own working tree,
 * that file gave `32 passed (32)` — every row of the handoff set, old and new,
 * clears with the regeneration and none of them is a shape disagreement hiding
 * behind a stale definition. #601 re-took that measurement on its own working
 * tree, with the same method and the same result at the new size:
 * `33 passed (33)`.
 *
 * **#597 leaves both figures exactly where they are, and that is the point of
 * recording it here.** It moved the TypeScript twins of #592's machine wire
 * change (four `ValidationAuxiliaryWitnessV1` constructors onto §8's
 * `FieldCarriageV1`, plus `ValidationProofItemDatumV1`). Both mappings were added
 * to the parity file, run, and removed again: `ValidationAuxiliaryWitnessV1`
 * cannot be normalized there because it reaches a genuinely recursive Aiken
 * definition through its CEK arm, and `ValidationProofItemDatumV1` has no
 * blueprint definition at all — verified against a scratch stock build of the
 * working tree, not only against the frozen file, because it is read as `Data`
 * off an `InlineDatum` and never reaches a declared ABI surface. Both rows would
 * be gates that cannot pass. **So the largest wire change in this lane is
 * invisible to the parity gate by construction, and regeneration will not reveal
 * it.** What covers it is the cross-language producer vector
 * `typescript_generated_field_chunk_auxiliary_is_exact` in
 * `onchain/aiken/lib/midgard/validation-one-step-cross-language.test.ak`.
 *
 * #597 does add one suite to the frozen-blueprint family, and it is **not** part
 * of the seventeen because it is not a handoff row of this set — it is a whole
 * suite that applies the committed validators:
 * `demo/midgard-validation/tests/complete-item-proof-fit-emulator-v1.test.ts`
 * moved from `1 failed | 5 passed (6)` to `5 failed | 1 passed (6)`. Its
 * redeemers are #592's four-field `Verify` while the frozen compiled validator
 * still expects five, so each row fails inside the script with
 * `failed script execution Spend[1] unexpected empty list`. It clears with the
 * same regeneration and is recorded in that file's own header.
 *
 * SEVENTEEN with those names is the handoff figure, and it is written down here
 * rather than left in a review thread so a later reviewer diffing #579 against it
 * is diffing against the real set. Nothing outside these seventeen is expected red
 * for this reason. Both figures were re-measured on 2026-08-12 after #596 — the
 * parity figure moved to ten red rows, the emulator figure held at the six it
 * has shown since #587 — re-measured again on 2026-08-12 after #597 with neither
 * moving, and re-measured again on 2026-08-13 after #601, where the parity figure
 * moved to eleven red rows of thirty-three and the emulator figure again held at
 * six of eight (#601 adds no emulator load site, which is why only one of the two
 * figures moved):
 * `vitest run tests/submit-init-emulator-soundness.test.ts
 * tests/submit-init-emulator-transition-trace.test.ts
 * tests/submit-init-emulator-validation-dispute.test.ts --pool=forks
 * --no-file-parallelism` and `vitest run tests/sdk-aiken-schema-parity.test.ts`.
 * Re-measure before editing them; a figure in a comment that nobody re-ran is the
 * defect this paragraph exists to prevent (#586).
 *
 * Both figures are for the blueprint **in the tree**. This file and the parity
 * file both honour `MIDGARD_REAL_BLUEPRINT_PATH`, so pointing that variable at an
 * already-regenerated blueprint is how #579 checks its work — and it is also the
 * one way to see a different red set than the one enumerated above.
 */
export const realBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(repoRoot, "onchain/aiken/plutus.json");

export const alwaysSucceedsBlueprintPath = resolve(
  repoRoot,
  "demo/midgard-node/blueprints/always-succeeds/plutus.json",
);

export const network: Network = "Preprod";

export type BlueprintParameter = {
  readonly title: string;
};

export type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
  /**
   * The blueprint's declared parameter list, carried so the loaders below can
   * check it against what the caller actually applies (#610). Absent means the
   * validator declares none — that is the compiler's encoding for a nullary
   * validator, never "unknown, skip the check".
   */
  readonly parameters?: readonly BlueprintParameter[];
};

export type Blueprint = {
  readonly validators: readonly BlueprintValidator[];
};

export const readBlueprint = (path: string): Blueprint =>
  JSON.parse(readFileSync(path, "utf8")) as Blueprint;

export const cloneBlueprint = (blueprint: Blueprint): Blueprint =>
  JSON.parse(JSON.stringify(blueprint)) as Blueprint;

/** Absent `parameters` is the compiler's encoding of "declares none" (#610). */
const declaredParametersOf = (
  validator: BlueprintValidator,
): readonly BlueprintParameter[] => validator.parameters ?? [];

const describeDeclaredParameters = (
  declaredParameters: readonly BlueprintParameter[],
): string =>
  declaredParameters.length === 0
    ? "none"
    : declaredParameters.map((parameter) => parameter.title).join(", ");

const requireBlueprintValidator = (
  blueprint: Blueprint,
  title: string,
): BlueprintValidator => {
  const found = blueprint.validators.find(
    (validator) => validator.title === title,
  );
  if (found === undefined) {
    throw new Error(`Validator with title "${title}" not found`);
  }
  return found;
};

/**
 * The bare-load door (#610): returns `compiledCode` with nothing applied, so it
 * is only sound while the validator declares no parameters.
 *
 * A declared parameter deployed unapplied is the #605 under-application shape —
 * the unapplied `validator main(...)` parameters stay as lambdas, the ledger's
 * single Plutus V3 script-context application reduces to a lambda VALUE rather
 * than running the validator body, evaluation ends without error, and the
 * ledger reads "no error" as SUCCESS. In this harness that produces an
 * always-succeeds script standing in for an authenticated one, which is a test
 * that cannot fail. Refuse at the load boundary instead: before this check the
 * mismatch surfaced only as an opaque `→ undefined` evaluation failure a few
 * hundred milliseconds into the emulated submission.
 */
export const getCompiledScript = (
  blueprint: Blueprint,
  title: string,
): string => {
  const found = requireBlueprintValidator(blueprint, title);
  const declaredParameters = declaredParametersOf(found);
  if (declaredParameters.length !== 0) {
    throw new Error(
      `${title} declares ${declaredParameters.length} parameter(s) but this loader deploys compiledCode bare — declared: ${describeDeclaredParameters(declaredParameters)}. An unapplied declared parameter deploys an always-succeeds script; load it with applyCompiledScript instead of widening this zero-arity door (#610).`,
    );
  }
  return found.compiledCode;
};

/**
 * The parameter-applying door (#610), and the only permitted caller of
 * `applyParamsToScript` in this harness.
 *
 * `applyParamsToScript` applies whatever list it is handed, positionally, and
 * never checks it against the script's own declared arity: too few terms is the
 * silent always-succeeds shape described above, too many is a well-formed
 * script with a hash that matches nothing. Both are refused here, against the
 * blueprint's own declaration, for every validator this harness deploys.
 */
export const applyCompiledScript = (
  blueprint: Blueprint,
  title: string,
  params: readonly Data[],
): string => {
  const found = requireBlueprintValidator(blueprint, title);
  const declaredParameters = declaredParametersOf(found);
  if (declaredParameters.length !== params.length) {
    throw new Error(
      `${title} declares ${declaredParameters.length} parameter(s) but ${params.length} were applied — declared: ${describeDeclaredParameters(declaredParameters)}. Under-application deploys an always-succeeds script and over-application deploys an unusable script hash; apply exactly the declared parameters (#610).`,
    );
  }
  const cacheKey = appliedScriptCacheKey(found.compiledCode, params);
  const cached = appliedScriptCache.get(cacheKey);
  if (cached !== undefined) {
    return cached;
  }
  const applied = applyParamsToScript(found.compiledCode, [...params]);
  appliedScriptCache.set(cacheKey, applied);
  return applied;
};

/**
 * `applyParamsToScript` is pure — the applied script is a function of nothing
 * but the compiled code and the CBOR of the parameters — and it is the
 * dominant fixed cost of every emulator journey (~12–14 s of contract build
 * per test). Memoizing on the exact inputs cannot change a deployed byte: a
 * cache hit is a proof the inputs were identical. The #610 arity guard above
 * runs before the lookup on every call, so under-/over-application still
 * fails closed. The cache is per-process, and this suite runs one fresh
 * process per test file, so entries never outlive a file.
 */
const appliedScriptCache = new Map<string, string>();

const appliedScriptCacheKey = (
  compiledCode: string,
  params: readonly Data[],
): string =>
  createHash("sha256")
    .update(
      `${compiledCode}|${params.map((param) => Data.to(param)).join("|")}`,
    )
    .digest("hex");
