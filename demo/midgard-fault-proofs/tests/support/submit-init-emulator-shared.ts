import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationTraceTree,
  compareOutRefs,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
  findOutRefIndex,
  hashMidgardValidationLedgerDeltaV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  hashMidgardValidationWorkWitnessV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  MIDGARD_PROTOCOL_V1_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardNativeTxFullV1,
  outRefLabel,
} from "@al-ft/midgard-core";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorDatum,
  ActiveOperatorMintRedeemer,
  ActiveOperatorSpendRedeemer,
  AddressData,
  addressDataFromBech32,
  type AuthenticatedValidator,
  buildDaHashPreimageFaultProofContracts,
  buildDoubleSpendFaultProofContracts,
  buildFabricatedDepositFaultProofContracts,
  buildFabricatedWithdrawalFaultProofContracts,
  buildInputNoIdxFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildInvalidSignatureFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildNoReferenceInputFaultProofContracts,
  buildPhasMembershipRewardRegistrationTxProgram,
  buildReferenceInputNoIdxFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  completeReferenceScriptPublicationTxProgram,
  ConfirmedState,
  createReferenceScriptAuthPolicy,
  EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
  EMPTY_MERKLE_TREE_ROOT,
  encodeLinkedListNodeView,
  EventKeySchema,
  EventToStepValueSchema,
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
  ForcedInclusionTxV1Schema,
  FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueCategoryDeploymentInfo,
  FraudProofCatalogueDatum,
  type FraudProofCatalogueDeploymentInfo,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  type FraudProofs,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  getHeaderV1FromStateQueueDatum,
  hashBlockHeaderV1,
  HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  HubOracleDatum,
  incompleteEmulatorCommitBlockHeaderTxProgram,
  makeHubOracleDatum,
  type MidgardValidators,
  type MintingValidator,
  MPF_CHUNKED_VERIFY_WITHDRAW_TITLE,
  OutputReference,
  outputReferenceFromUTxO,
  parseFaultProofBlueprint,
  parsePhasMembershipBlueprint,
  phasMembershipWithdrawalScriptFromBlueprint,
  Proof,
  referenceScriptAuthPolicyDeploymentInfo,
  referenceScriptPublicationFundingTarget,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  RegisteredOperatorMintRedeemer,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorMintRedeemer,
  ROOT_DOMAINS,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerMintRedeemer,
  SchedulerSpendRedeemer,
  ScriptHashSchema,
  selectReferenceScriptFundingUtxos,
  type SpendingValidator as SdkSpendingValidator,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  TransitionStepSchema,
  utxoToStateQueueUTxO,
  type ValidationClaimWitnessV1,
  validationMachineStateDataFromCore,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
  type WithdrawalValidator as SdkWithdrawalValidator,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundleV1,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  outputCborMeetsMinAdaV1,
  RejectCodes,
} from "@al-ft/midgard-validation";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  type BuildTxWithRedeemer,
  CML,
  Constr,
  type CostModels,
  credentialToAddress,
  Data,
  Emulator,
  type EmulatorAccount,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type MintingPolicy,
  mintingPolicyToId,
  type Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  type ProtocolParameters,
  type Script,
  scriptHashToCredential,
  type SpendingValidator,
  toUnit,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
  type WithdrawalValidator,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildCountedRoot,
  encodeData,
  encodePhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  keyValuePhasProof,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
  submitRemoveFraudulentBlock,
  submitValidationDisputeAward,
  submitValidationDisputeEnterResolution,
  submitValidationDisputeOpen,
  submitValidationDisputePrepareResolution,
  submitValidationDisputePrepareSelected,
  submitValidationDisputeReveal,
  submitValidationDisputeSemanticResolution,
  submitValidationDisputeVerifySource,
  VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX_V1,
  validationDisputeValidityRange,
  validationSemanticResolverGlobalIndexV1,
  validationValueAndMintSemanticReferenceScriptDeploymentEntryV1,
} from "../../src/index.js";
import type { FabricatedDepositContractsV1 } from "../../src/submit-fabricated-deposit-step-01.js";
import type { FabricatedWithdrawalContractsV1 } from "../../src/submit-fabricated-withdrawal-step-01.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { submitInit } from "./legacy-submit-emulator.js";

export const moduleDir = dirname(fileURLToPath(import.meta.url));

export const repoRoot = resolve(moduleDir, "../../../..");

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

export type DiagnosticCardanoParameterOverrides = Pick<
  ProtocolParameters,
  | "minFeeA"
  | "minFeeB"
  | "maxValSize"
  | "maxTxExMem"
  | "maxTxExSteps"
  | "priceMem"
  | "priceStep"
  | "coinsPerUtxoByte"
  | "collateralPercentage"
  | "maxCollateralInputs"
  | "minFeeRefScriptCostPerByte"
  | "costModels"
>;

export const requireJsonRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object`);
  }
  return value as Record<string, unknown>;
};

export const requireFiniteNumber = (
  record: Record<string, unknown>,
  key: string,
): number => {
  const value = record[key];
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new Error(`Diagnostic Cardano parameter ${key} must be finite`);
  }
  return value;
};

export const requireNonNegativeInteger = (
  record: Record<string, unknown>,
  key: string,
): number => {
  const value = requireFiniteNumber(record, key);
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `Diagnostic Cardano parameter ${key} must be a non-negative safe integer`,
    );
  }
  return value;
};

export const requireBigIntParameter = (
  record: Record<string, unknown>,
  key: string,
): bigint => {
  const value = record[key];
  if (
    (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) &&
    (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
  ) {
    throw new Error(
      `Diagnostic Cardano parameter ${key} must be a non-negative integer`,
    );
  }
  return BigInt(value);
};

export const requireCostModel = (
  costModels: Record<string, unknown>,
  version: keyof CostModels,
): number[] => {
  const value = costModels[version];
  if (
    !Array.isArray(value) ||
    value.some((entry) => typeof entry !== "number" || !Number.isFinite(entry))
  ) {
    throw new Error(
      `Diagnostic Cardano parameter cost_models.${version} must be a finite-number array`,
    );
  }
  return [...value];
};

export const loadDiagnosticCardanoParameterOverrides =
  (): Partial<DiagnosticCardanoParameterOverrides> => {
    const parameterPath =
      process.env.MIDGARD_DIAGNOSTIC_CARDANO_PARAMETERS?.trim();
    if (parameterPath == null || parameterPath.length === 0) {
      return {};
    }
    const parsed = JSON.parse(readFileSync(parameterPath, "utf8")) as unknown;
    if (!Array.isArray(parsed) || parsed.length !== 1) {
      throw new Error(
        "Diagnostic Cardano parameter snapshot must contain exactly one epoch",
      );
    }
    const parameters = requireJsonRecord(
      parsed[0],
      "Diagnostic Cardano parameter snapshot entry",
    );
    const maxTxSize = requireNonNegativeInteger(parameters, "max_tx_size");
    if (maxTxSize !== PROTOCOL_PARAMETERS_DEFAULT.maxTxSize) {
      throw new Error(
        `Diagnostic target max_tx_size must be ${PROTOCOL_PARAMETERS_DEFAULT.maxTxSize.toString()}, found ${maxTxSize.toString()}`,
      );
    }
    const costModelsJson = requireJsonRecord(
      parameters.cost_models,
      "Diagnostic Cardano parameter cost_models",
    );
    const costModels: CostModels = {
      PlutusV1: requireCostModel(costModelsJson, "PlutusV1"),
      PlutusV2: requireCostModel(costModelsJson, "PlutusV2"),
      PlutusV3: requireCostModel(costModelsJson, "PlutusV3"),
    };
    return {
      minFeeA: requireNonNegativeInteger(parameters, "min_fee_a"),
      minFeeB: requireNonNegativeInteger(parameters, "min_fee_b"),
      maxValSize: requireNonNegativeInteger(parameters, "max_val_size"),
      maxTxExMem: requireBigIntParameter(parameters, "max_tx_ex_mem"),
      maxTxExSteps: requireBigIntParameter(parameters, "max_tx_ex_steps"),
      priceMem: requireFiniteNumber(parameters, "price_mem"),
      priceStep: requireFiniteNumber(parameters, "price_step"),
      coinsPerUtxoByte: requireBigIntParameter(
        parameters,
        "coins_per_utxo_size",
      ),
      collateralPercentage: requireNonNegativeInteger(
        parameters,
        "collateral_percent",
      ),
      maxCollateralInputs: requireNonNegativeInteger(
        parameters,
        "max_collateral_inputs",
      ),
      minFeeRefScriptCostPerByte: requireFiniteNumber(
        parameters,
        "min_fee_ref_script_cost_per_byte",
      ),
      costModels,
    };
  };

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  ...loadDiagnosticCardanoParameterOverrides(),
  maxTxSize: 65_536,
  maxCollateralInputs: 3,
} as const;

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
  return applyParamsToScript(found.compiledCode, [...params]);
};

export const makeMintingValidator = (
  mintingScriptCBOR: string,
): MintingValidator => {
  const mintingScript: MintingPolicy = {
    type: "PlutusV3",
    script: mintingScriptCBOR,
  };
  return {
    mintingScriptCBOR,
    mintingScript,
    policyId: mintingPolicyToId(mintingScript),
  };
};

export const makeSpendingValidator = (
  spendingScriptCBOR: string,
): SdkSpendingValidator => {
  const spendingScript: SpendingValidator = {
    type: "PlutusV3",
    script: spendingScriptCBOR,
  };
  return {
    spendingScriptCBOR,
    spendingScript,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
    spendingScriptHash: validatorToScriptHash(spendingScript),
  };
};

export const makeWithdrawalValidator = (
  withdrawalScriptCBOR: string,
): SdkWithdrawalValidator => {
  const withdrawalScript: WithdrawalValidator = {
    type: "PlutusV3",
    script: withdrawalScriptCBOR,
  };
  return {
    withdrawalScriptCBOR,
    withdrawalScript,
    withdrawalScriptHash: validatorToScriptHash(withdrawalScript),
  };
};

export const makeAuthenticatedValidator = (
  mintingScriptCBOR: string,
  spendingScriptCBOR: string,
): AuthenticatedValidator => ({
  ...makeMintingValidator(mintingScriptCBOR),
  ...makeSpendingValidator(spendingScriptCBOR),
});

export const alwaysTitle = (
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  category === "midgard"
    ? `${category}.${baseName}_${purpose}.else`
    : `${category}.${baseName}.else`;

export const alwaysScript = (
  blueprint: Blueprint,
  category: "midgard" | "fraud_proofs",
  baseName: string,
  purpose: "spend" | "mint" | "withdraw",
): string =>
  applyDoubleCborEncoding(
    getCompiledScript(blueprint, alwaysTitle(category, baseName, purpose)),
  );

export const alwaysAuthenticated = (
  blueprint: Blueprint,
  baseName: string,
): AuthenticatedValidator =>
  makeAuthenticatedValidator(
    alwaysScript(blueprint, "midgard", baseName, "mint"),
    alwaysScript(blueprint, "midgard", baseName, "spend"),
  );

export const makeAlwaysSucceedsContracts = (
  blueprint: Blueprint,
): MidgardValidators => {
  const reserve = {
    ...makeSpendingValidator(
      alwaysScript(blueprint, "midgard", "reserve", "spend"),
    ),
    ...makeWithdrawalValidator(
      alwaysScript(blueprint, "midgard", "reserve", "withdraw"),
    ),
  };
  const alwaysValidationTraceDispute = makeSpendingValidator(
    alwaysScript(blueprint, "fraud_proofs", "transition_trace", "spend"),
  );
  const fraudProofs: FraudProofs = {
    doubleSpend: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "double_spend", "spend"),
    ),
    nonExistentInput: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "non_existent_input", "spend"),
    ),
    nonExistentInputNoIndex: makeSpendingValidator(
      alwaysScript(
        blueprint,
        "fraud_proofs",
        "non_existent_input_no_index",
        "spend",
      ),
    ),
    invalidRange: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "invalid_range", "spend"),
    ),
    transitionTrace: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "transition_trace", "spend"),
    ),
    zeroInput: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "zero_input", "spend"),
    ),
    validationTraceDispute: {
      ...alwaysValidationTraceDispute,
      source: alwaysValidationTraceDispute,
      game: alwaysValidationTraceDispute,
      boundary: alwaysValidationTraceDispute,
      timeout: alwaysValidationTraceDispute,
      award: alwaysValidationTraceDispute,
    },
    // The always-succeeds devnet blueprint has no dedicated
    // `da_hash_preimage` stub, so Q44 reuses the `zero_input` stub, mirroring
    // how `validationTraceDispute` reuses `transition_trace`.
    daHashPreimage: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "zero_input", "spend"),
    ),
    // Same aliasing for the three families registered by #547: the
    // always-succeeds devnet blueprint carries no `no_reference_input`,
    // `reference_input_no_idx`, or `invalid_signature` stub, so each reuses
    // the stub of the family it mirrors on-chain.
    noReferenceInput: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "non_existent_input", "spend"),
    ),
    referenceInputNoIdx: makeSpendingValidator(
      alwaysScript(
        blueprint,
        "fraud_proofs",
        "non_existent_input_no_index",
        "spend",
      ),
    ),
    invalidSignature: makeSpendingValidator(
      alwaysScript(blueprint, "fraud_proofs", "invalid_range", "spend"),
    ),
  };
  const fieldPreimageV1 = makeSpendingValidator(
    alwaysScript(blueprint, "midgard", "state_queue", "spend"),
  );
  // #579 ruling A: this always-succeeds spend+mint pair used to stand in for the
  // retired `tx_field_receipt_v1` family. It now stands in for the §8.6
  // field-preimage certificate, which is the role the emulator set actually has
  // to fill — the tx-order mint is parameterized by the certificate policy id.
  const fieldPreimageCertificateV1 = {
    ...fieldPreimageV1,
    ...makeMintingValidator(
      alwaysScript(blueprint, "midgard", "state_queue", "mint"),
    ),
  };

  return {
    referenceScriptAuth: makeMintingValidator(
      alwaysScript(blueprint, "midgard", "state_queue", "mint"),
    ),
    hubOracle: {
      ...makeMintingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
      ...makeSpendingValidator(
        alwaysScript(blueprint, "midgard", "hub_oracle", "mint"),
      ),
    },
    daParamsGovernor: alwaysAuthenticated(blueprint, "state_queue"),
    daAttestation: alwaysAuthenticated(blueprint, "state_queue"),
    stateQueue: alwaysAuthenticated(blueprint, "state_queue"),
    scheduler: alwaysAuthenticated(blueprint, "scheduler"),
    registeredOperators: alwaysAuthenticated(blueprint, "registered_operators"),
    activeOperators: alwaysAuthenticated(blueprint, "active_operators"),
    retiredOperators: alwaysAuthenticated(blueprint, "retired_operators"),
    escapeHatch: alwaysAuthenticated(blueprint, "escape_hatch"),
    fraudProofCatalogue: alwaysAuthenticated(
      blueprint,
      "fraud_proof_catalogue",
    ),
    fraudProof: alwaysAuthenticated(blueprint, "fraud_proof"),
    deposit: alwaysAuthenticated(blueprint, "deposit"),
    withdrawal: alwaysAuthenticated(blueprint, "withdrawal"),
    txOrder: alwaysAuthenticated(blueprint, "tx_order"),
    fieldPreimageCertificate: fieldPreimageCertificateV1,
    cekProgramMaterial: fieldPreimageV1,
    settlement: alwaysAuthenticated(blueprint, "settlement"),
    reserve,
    payout: alwaysAuthenticated(blueprint, "payout"),
    fraudProofs,
  };
};

export const buildMinimalFaultProofContracts = async (
  realBlueprint: Blueprint,
  alwaysBlueprint: Blueprint,
  nonceUtxo: UTxO,
  {
    realNonExistentInput = false,
    realInvalidRange = false,
    realTransitionTrace = false,
    realZeroInput = false,
    realDaHashPreimage = false,
    realFabricatedDeposit = false,
    realFabricatedWithdrawal = false,
    realInputNoIdx = false,
    realNoReferenceInput = false,
    realReferenceInputNoIdx = false,
    realInvalidSignature = false,
    realValidationTraceDispute = false,
    alwaysFraudProofCatalogue = false,
  }: {
    readonly realNonExistentInput?: boolean;
    readonly realInvalidRange?: boolean;
    readonly realTransitionTrace?: boolean;
    readonly realZeroInput?: boolean;
    readonly realDaHashPreimage?: boolean;
    readonly realFabricatedDeposit?: boolean;
    readonly realFabricatedWithdrawal?: boolean;
    readonly realInputNoIdx?: boolean;
    readonly realNoReferenceInput?: boolean;
    readonly realReferenceInputNoIdx?: boolean;
    readonly realInvalidSignature?: boolean;
    readonly realValidationTraceDispute?: boolean;
    readonly alwaysFraudProofCatalogue?: boolean;
  } = {},
): Promise<
  MidgardValidators & {
    readonly fabricatedDeposit?: FabricatedDepositContractsV1;
    readonly fabricatedWithdrawal?: FabricatedWithdrawalContractsV1;
  }
> => {
  // This integration test proves the real active-operators slashing and
  // scheduler removal path. Registered/retired operator setup remains
  // scaffolded only where needed to support the focused removal flow.
  const base = makeAlwaysSucceedsContracts(alwaysBlueprint);
  const hubOracle = makeMintingValidator(
    applyCompiledScript(realBlueprint, "hub_oracle.mint.mint", [
      new Constr(0, [
        nonceUtxo.txHash.toLowerCase(),
        BigInt(nonceUtxo.outputIndex),
      ]),
      HUB_ORACLE_ASSET_NAME,
    ]),
  );
  const hubOracleAuth: AuthenticatedValidator = {
    ...hubOracle,
    spendingScriptCBOR: hubOracle.mintingScriptCBOR,
    spendingScript: hubOracle.mintingScript as SpendingValidator,
    spendingScriptHash: hubOracle.policyId,
    spendingScriptAddress: credentialToAddress(
      network,
      scriptHashToCredential(hubOracle.policyId),
    ),
  };
  const withHubOracle = {
    ...base,
    hubOracle: hubOracleAuth,
  };

  const fraudProofCatalogue = alwaysFraudProofCatalogue
    ? withHubOracle.fraudProofCatalogue
    : makeAuthenticatedValidator(
        applyCompiledScript(realBlueprint, "fraud_proof_catalogue.mint.mint", [
          hubOracle.policyId,
        ]),
        getCompiledScript(realBlueprint, "fraud_proof_catalogue.spend.else"),
      );
  const withCatalogue = {
    ...withHubOracle,
    fraudProofCatalogue,
  };

  const activeOperatorsMinting = makeMintingValidator(
    applyCompiledScript(
      realBlueprint,
      "operator_directory/active_operators.mint.mint",
      [
        hubOracle.policyId,
        withCatalogue.registeredOperators.policyId,
        withCatalogue.retiredOperators.policyId,
      ],
    ),
  );
  const activeOperators: AuthenticatedValidator = {
    ...activeOperatorsMinting,
    ...makeSpendingValidator(
      applyCompiledScript(
        realBlueprint,
        "operator_directory/active_operators.spend.spend",
        [activeOperatorsMinting.policyId, hubOracle.policyId],
      ),
    ),
  };
  const withActiveOperators = {
    ...withCatalogue,
    activeOperators,
  };

  const doubleSpendContracts = await Effect.runPromise(
    buildDoubleSpendFaultProofContracts({
      blueprint: parseFaultProofBlueprint(realBlueprint),
      network,
      hubOraclePolicyId: hubOracle.policyId,
      fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
    }),
  );
  const nonExistentInputContracts = realNonExistentInput
    ? await Effect.runPromise(
        buildNonExistentInputFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (nonExistentInputContracts !== undefined) {
    expect(nonExistentInputContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const invalidRangeContracts = realInvalidRange
    ? await Effect.runPromise(
        buildInvalidRangeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (invalidRangeContracts !== undefined) {
    expect(invalidRangeContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const transitionTraceContracts = realTransitionTrace
    ? await Effect.runPromise(
        buildTransitionTraceFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (transitionTraceContracts !== undefined) {
    expect(transitionTraceContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const validationTraceDisputeContracts = realValidationTraceDispute
    ? await Effect.runPromise(
        buildValidationTraceDisputeFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  const zeroInputContracts = realZeroInput
    ? await Effect.runPromise(
        buildZeroInputFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (validationTraceDisputeContracts !== undefined) {
    expect(validationTraceDisputeContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  if (zeroInputContracts !== undefined) {
    expect(zeroInputContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const daHashPreimageContracts = realDaHashPreimage
    ? await Effect.runPromise(
        buildDaHashPreimageFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (daHashPreimageContracts !== undefined) {
    expect(daHashPreimageContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const fabricatedDepositContracts = realFabricatedDeposit
    ? await Effect.runPromise(
        buildFabricatedDepositFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (fabricatedDepositContracts !== undefined) {
    expect(fabricatedDepositContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const fabricatedWithdrawalContracts = realFabricatedWithdrawal
    ? await Effect.runPromise(
        buildFabricatedWithdrawalFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (fabricatedWithdrawalContracts !== undefined) {
    expect(fabricatedWithdrawalContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const inputNoIdxContracts = realInputNoIdx
    ? await Effect.runPromise(
        buildInputNoIdxFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (inputNoIdxContracts !== undefined) {
    expect(inputNoIdxContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const noReferenceInputContracts = realNoReferenceInput
    ? await Effect.runPromise(
        buildNoReferenceInputFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (noReferenceInputContracts !== undefined) {
    expect(noReferenceInputContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const referenceInputNoIdxContracts = realReferenceInputNoIdx
    ? await Effect.runPromise(
        buildReferenceInputNoIdxFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (referenceInputNoIdxContracts !== undefined) {
    expect(referenceInputNoIdxContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const invalidSignatureContracts = realInvalidSignature
    ? await Effect.runPromise(
        buildInvalidSignatureFaultProofContracts({
          blueprint: parseFaultProofBlueprint(cloneBlueprint(realBlueprint)),
          network,
          hubOraclePolicyId: hubOracle.policyId,
          fraudProofCataloguePolicyId: fraudProofCatalogue.policyId,
        }),
      )
    : undefined;
  if (invalidSignatureContracts !== undefined) {
    expect(invalidSignatureContracts.fraudProof.policyId).toBe(
      doubleSpendContracts.fraudProof.policyId,
    );
  }
  const activeOperatorsAddressData = await Effect.runPromise(
    addressDataFromBech32(
      withActiveOperators.activeOperators.spendingScriptAddress,
    ).pipe(
      Effect.map((addressData) => Data.from(Data.to(addressData, AddressData))),
    ),
  );
  const schedulerMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "scheduler.mint.mint", [
      hubOracle.policyId,
    ]),
  );
  const scheduler: AuthenticatedValidator = {
    ...schedulerMinting,
    ...makeSpendingValidator(
      applyCompiledScript(realBlueprint, "scheduler.spend.spend", [
        withActiveOperators.registeredOperators.policyId,
        activeOperatorsAddressData,
        withActiveOperators.activeOperators.policyId,
        schedulerMinting.policyId,
        hubOracle.policyId,
      ]),
    ),
  };
  const withScheduler = {
    ...withActiveOperators,
    scheduler,
  };
  const stateQueueMinting = makeMintingValidator(
    applyCompiledScript(realBlueprint, "state_queue.mint.mint", [
      hubOracle.policyId,
      withScheduler.activeOperators.policyId,
      activeOperatorsAddressData,
      withScheduler.retiredOperators.policyId,
      withScheduler.scheduler.policyId,
      doubleSpendContracts.fraudProof.policyId,
      withScheduler.settlement.policyId,
      withScheduler.daAttestation.policyId,
    ]),
  );
  const stateQueueSpending = makeSpendingValidator(
    applyCompiledScript(realBlueprint, "state_queue.spend.spend", [
      stateQueueMinting.policyId,
      withScheduler.daAttestation.policyId,
    ]),
  );

  // The two Q39/Q40 families predate their catalogue registration: production
  // deployment resolution cannot build them yet (parent-owned integration
  // work), so their submitters take an explicit contracts record. Assemble it
  // here, from the same parameterized chains whose step-01 hashes the tests
  // register as extra catalogue categories.
  const fabricatedDeposit: FabricatedDepositContractsV1 | undefined =
    fabricatedDepositContracts === undefined
      ? undefined
      : {
          steps: fabricatedDepositContracts.fabricatedDeposit.steps,
          computationThread: fabricatedDepositContracts.computationThread,
          fraudProof: fabricatedDepositContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          categoryId: FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
        };
  const fabricatedWithdrawal: FabricatedWithdrawalContractsV1 | undefined =
    fabricatedWithdrawalContracts === undefined
      ? undefined
      : {
          steps: fabricatedWithdrawalContracts.fabricatedWithdrawal.steps,
          computationThread: fabricatedWithdrawalContracts.computationThread,
          fraudProof: fabricatedWithdrawalContracts.fraudProof,
          hubOraclePolicyId: hubOracle.policyId,
          stateQueuePolicyId: stateQueueMinting.policyId,
          categoryId: FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
        };

  return {
    ...withScheduler,
    ...(fabricatedDeposit === undefined ? {} : { fabricatedDeposit }),
    ...(fabricatedWithdrawal === undefined ? {} : { fabricatedWithdrawal }),
    cekProgramMaterial:
      validationTraceDisputeContracts?.validationTraceDispute
        .cekProgramMaterial ?? withScheduler.cekProgramMaterial,
    stateQueue: {
      ...stateQueueMinting,
      ...stateQueueSpending,
    },
    fraudProof: {
      ...doubleSpendContracts.fraudProof,
      policyId: doubleSpendContracts.fraudProof.policyId,
      mintingScript: doubleSpendContracts.fraudProof.mintingScript,
      mintingScriptCBOR: doubleSpendContracts.fraudProof.mintingScriptCBOR,
    },
    fraudProofs: {
      ...withActiveOperators.fraudProofs,
      doubleSpend: doubleSpendContracts.doubleSpend.firstStep,
      nonExistentInput:
        nonExistentInputContracts?.nonExistentInput.firstStep ??
        withActiveOperators.fraudProofs.nonExistentInput,
      invalidRange:
        invalidRangeContracts?.invalidRange.firstStep ??
        withActiveOperators.fraudProofs.invalidRange,
      transitionTrace:
        transitionTraceContracts?.transitionTrace.firstStep ??
        withActiveOperators.fraudProofs.transitionTrace,
      zeroInput:
        zeroInputContracts?.zeroInput.firstStep ??
        withActiveOperators.fraudProofs.zeroInput,
      daHashPreimage:
        daHashPreimageContracts?.daHashPreimage.firstStep ??
        withActiveOperators.fraudProofs.daHashPreimage,
      nonExistentInputNoIndex:
        inputNoIdxContracts?.nonExistentInputNoIndex.firstStep ??
        withActiveOperators.fraudProofs.nonExistentInputNoIndex,
      noReferenceInput:
        noReferenceInputContracts?.noReferenceInput.firstStep ??
        withActiveOperators.fraudProofs.noReferenceInput,
      referenceInputNoIdx:
        referenceInputNoIdxContracts?.referenceInputNoIdx.firstStep ??
        withActiveOperators.fraudProofs.referenceInputNoIdx,
      invalidSignature:
        invalidSignatureContracts?.invalidSignature.firstStep ??
        withActiveOperators.fraudProofs.invalidSignature,
      validationTraceDispute:
        validationTraceDisputeContracts === undefined
          ? withActiveOperators.fraudProofs.validationTraceDispute
          : {
              ...validationTraceDisputeContracts.validationTraceDispute
                .firstStep,
              source:
                validationTraceDisputeContracts.validationTraceDispute.source,
              game: validationTraceDisputeContracts.validationTraceDispute.game,
              boundary:
                validationTraceDisputeContracts.validationTraceDispute.boundary,
              timeout:
                validationTraceDisputeContracts.validationTraceDispute.timeout,
              award:
                validationTraceDisputeContracts.validationTraceDispute.award,
            },
    },
  };
};

export const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});

export type LucidDataSchema = Parameters<typeof Data.to>[1];

export const categoryId = (index: number): string => {
  const buf = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  buf.writeUInt32BE(index);
  return buf.toString("hex");
};

export const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(
    Data.to(id, categoryIdSchema as unknown as LucidDataSchema),
    "hex",
  );

export const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(
    Data.to(scriptHash, ScriptHashSchema as unknown as LucidDataSchema),
    "hex",
  );

export const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

export const ledgerOrderedIndex = (
  candidates: readonly UTxO[],
  target: UTxO,
  label: string,
): bigint => {
  const index = findOutRefIndex([...candidates].sort(compareOutRefs), target);
  if (index === undefined) {
    throw new Error(`Missing ${label} in candidate set`);
  }
  return BigInt(index);
};

export const alignUnixTimeToEmulatorSlotBoundary = (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  unixTime: number,
): number => lucid.slotToUnixTime(lucid.unixTimeToSlot(unixTime));

export const firstWalletUtxo = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  label: string,
): Promise<UTxO> => {
  const [utxo] = await lucid.wallet().getUtxos();
  if (utxo === undefined) {
    throw new Error(`Expected wallet UTxO for ${label}`);
  }
  return utxo;
};

export const expectSingleUtxoWithUnit = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  address: string,
  unit: string,
): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  expect(utxos).toHaveLength(1);
  return utxos[0]!;
};

export const SETUP_OUTPUT_INDEX = {
  stateQueueRoot: 2n,
  activeOperatorsRoot: 3n,
  retiredOperatorsRoot: 4n,
} as const;

export const ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX = {
  root: 0n,
  insertedNode: 1n,
} as const;

export const SCHEDULER_APPOINTMENT_OUTPUT_INDEX = {
  scheduler: 0n,
} as const;

export const h32 = (byte: string): string => byte.repeat(32);

export const deploymentManifest = (
  contracts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown> = {},
) => ({
  referenceScriptAuthPolicy,
  contracts,
});

export const makeNativeTx = ({
  spendInputCbors,
  fee,
  referenceByte,
  outputByte,
  outputCbor,
  witnessByte,
  addrTxWitsPreimageCbor,
  validityIntervalStart = MIDGARD_POSIX_TIME_NONE,
  validityIntervalEnd = MIDGARD_POSIX_TIME_NONE,
}: {
  readonly spendInputCbors: readonly Buffer[];
  readonly fee: bigint;
  readonly referenceByte?: string;
  readonly outputByte?: string;
  readonly outputCbor?: Buffer;
  readonly witnessByte?: string;
  readonly addrTxWitsPreimageCbor?: Buffer;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
}): MidgardNativeTxFullV1 =>
  materializeMidgardNativeTxFromCanonicalV1({
    version: MIDGARD_NATIVE_TX_V1_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: encodeCbor(spendInputCbors),
      referenceInputsPreimageCbor:
        referenceByte === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor([Buffer.from(h32(referenceByte), "hex")]),
      outputsPreimageCbor:
        outputCbor !== undefined
          ? encodeCbor([outputCbor])
          : outputByte === undefined
            ? EMPTY_CBOR_LIST
            : encodeCbor([Buffer.from(h32(outputByte), "hex")]),
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      fee,
      validityIntervalStart,
      validityIntervalEnd,
      networkId: 0n,
    },
    witnessSet: {
      addrTxWitsPreimageCbor:
        addrTxWitsPreimageCbor ??
        (witnessByte === undefined
          ? EMPTY_CBOR_LIST
          : encodeCbor([Buffer.from(h32(witnessByte), "hex")])),
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  });

/**
 * A catalogue category registered on top of the canonical eleven — the Q39/Q40
 * fabricated families, whose production registration is parent-owned. With no
 * extras the emitted root and every base proof are byte-identical to the
 * two-argument behaviour, so no measured fixture moves.
 */
export type CatalogueExtraCategoryV1 = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const buildCatalogueDeploymentInfo = async (
  fraudProofs: FraudProofs,
  extraCategories: Readonly<
    Record<string, { readonly categoryId: string; readonly scriptHash: string }>
  > = {},
): Promise<
  FraudProofCatalogueDeploymentInfo & {
    readonly extraCategories: Readonly<
      Record<string, CatalogueExtraCategoryV1>
    >;
  }
> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name, index) => [
      name,
      {
        categoryId: categoryId(index),
        scriptHash: fraudProofs[name].spendingScriptHash,
        membershipProofCbor: "",
      },
    ]),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    await trie.insert(
      encodeCatalogueKey(category.categoryId),
      encodeCatalogueValue(category.scriptHash),
    );
  }
  for (const extra of Object.values(extraCategories)) {
    await trie.insert(
      encodeCatalogueKey(extra.categoryId),
      encodeCatalogueValue(extra.scriptHash),
    );
  }

  const categoriesWithProofs = { ...categories };
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
    categoriesWithProofs[name] = {
      ...category,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }
  const extraCategoriesWithProofs: Record<string, CatalogueExtraCategoryV1> =
    {};
  for (const [name, extra] of Object.entries(extraCategories)) {
    const proof = await trie.prove(encodeCatalogueKey(extra.categoryId));
    extraCategoriesWithProofs[name] = {
      ...extra,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }

  return {
    root: trieRootHex(trie),
    categories: categoriesWithProofs,
    extraCategories: extraCategoriesWithProofs,
  };
};

/**
 * Registers the reward account of the merkelized published-chunk verifier
 * (issue #545). A step on the chunked route withdraws zero from it, which is
 * how the verifier is invoked, so the account must exist first — exactly as the
 *  membership account must for the redeemer-carried route.
 */
export const registerChunkedVerifyRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const script: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, MPF_CHUNKED_VERIFY_WITHDRAW_TITLE),
  };
  const built = await Effect.runPromise(
    buildPhasMembershipRewardRegistrationTxProgram(lucid, { script }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

export const registerPhasMembershipRewardAccount = async (
  lucid: Awaited<ReturnType<typeof Lucid>>,
  realBlueprint: Blueprint,
): Promise<void> => {
  const phasMembershipScript = phasMembershipWithdrawalScriptFromBlueprint(
    parsePhasMembershipBlueprint(realBlueprint),
  );
  const built = await Effect.runPromise(
    buildPhasMembershipRewardRegistrationTxProgram(lucid, {
      script: phasMembershipScript,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

export const makeHeader = (
  operatorVkey: string,
  now: number,
  transactionsRoot = EMPTY_MERKLE_TREE_ROOT,
  l2TransactionCount = 0n,
): HeaderV1 => {
  const hasL2Transactions = l2TransactionCount > 0n;
  const eventCommitmentRoot = hasL2Transactions
    ? transactionsRoot
    : EMPTY_MERKLE_TREE_ROOT;
  return {
    prevUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    utxosRoot: EMPTY_MERKLE_TREE_ROOT,
    withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
    ...EMPTY_HEADER_TRANSITION_COMMITMENTS_V1,
    l2TransactionCount,
    totalEventCount: l2TransactionCount,
    transitionStepCount: l2TransactionCount,
    validationTraceCount: l2TransactionCount,
    transactionsRoot,
    transitionTraceRoot: eventCommitmentRoot,
    eventToStepRoot: eventCommitmentRoot,
    validationTracesRoot: eventCommitmentRoot,
    depositsRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: BigInt(now),
    endTime: BigInt(now + 1_000),
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: GENESIS_HEADER_HASH,
    operatorVkey,
    protocolVersion: BigInt(MIDGARD_PROTOCOL_V1_VERSION),
  };
};

export const transitionTraceOutRef = (byte: string): OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

export const transitionTraceDaEntry = <K, V>({
  key,
  keySchema,
  value,
  valueSchema,
}: {
  readonly key: K;
  readonly keySchema: Parameters<typeof Data.Nullable>[0];
  readonly value: V;
  readonly valueSchema: Parameters<typeof Data.Nullable>[0];
}): [string, string] => [
  encodeData(key, keySchema).toString("hex"),
  encodeData(value, valueSchema).toString("hex"),
];

export type ForcedValidationSourceEntryV1 = NonNullable<
  ValidationClaimWitnessV1["source_membership"] extends infer Source
    ? Source extends {
        ForcedValidationSource: { membership: { value: infer V } };
      }
      ? V
      : never
    : never
>;

/**
 * Builds the block-owned roots, forced-source membership, committed claim, and
 * header for a single-forced-transaction validation-trace block from an
 * operator trace. Shared by every forced validation-dispute fixture so the
 * committed-claim shape stays identical across scenarios and only the traces
 * and transition roots vary.
 */
export const buildForcedValidationDisputeCommitments = async ({
  operatorVkey,
  now,
  txOrderId,
  eventKey,
  forcedTransaction,
  operatorTrace,
  preUtxosRoot,
  postUtxosRoot,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly txOrderId: OutputReference;
  readonly eventKey: {
    readonly ForcedTransactionEventKey: {
      readonly tx_order_id: OutputReference;
    };
  };
  readonly forcedTransaction: ForcedValidationSourceEntryV1;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly preUtxosRoot: string;
  readonly postUtxosRoot: string;
}): Promise<{
  readonly header: HeaderV1;
  readonly claim: ValidationClaimWitnessV1;
}> => {
  const step = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction" as const,
    pre_utxos_root: preUtxosRoot,
    post_utxos_root: postUtxosRoot,
  };
  const eventToStepValue = {
    step_index: 0n,
    phase: "ForcedTransaction" as const,
  };
  const operatorDescriptor = validationTraceDescriptorDataFromCore(
    operatorTrace.tree.descriptor,
  );
  const forcedEntry = transitionTraceDaEntry({
    key: txOrderId,
    keySchema: OutputReference as never,
    value: forcedTransaction,
    valueSchema: ForcedInclusionTxV1Schema,
  });
  const transitionEntry = transitionTraceDaEntry({
    key: step.step_index,
    keySchema: Data.Integer() as never,
    value: step,
    valueSchema: TransitionStepSchema,
  });
  const eventToStepEntry = transitionTraceDaEntry({
    key: eventKey,
    keySchema: EventKeySchema,
    value: eventToStepValue,
    valueSchema: EventToStepValueSchema,
  });
  const descriptorEntry = transitionTraceDaEntry({
    key: eventKey,
    keySchema: EventKeySchema,
    value: operatorDescriptor,
    valueSchema: ValidationTraceDescriptorV1Schema,
  });
  const forcedRoot = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    {
      key: Buffer.from(forcedEntry[0], "hex"),
      value: Buffer.from(forcedEntry[1], "hex"),
    },
  ]);
  const transitionRoot = await buildCountedRoot(ROOT_DOMAINS.transitionTrace, [
    {
      key: Buffer.from(transitionEntry[0], "hex"),
      value: Buffer.from(transitionEntry[1], "hex"),
    },
  ]);
  const eventToStepRoot = await buildCountedRoot(ROOT_DOMAINS.eventToStep, [
    {
      key: Buffer.from(eventToStepEntry[0], "hex"),
      value: Buffer.from(eventToStepEntry[1], "hex"),
    },
  ]);
  const descriptorRoot = await buildCountedRoot(ROOT_DOMAINS.validationTraces, [
    {
      key: Buffer.from(descriptorEntry[0], "hex"),
      value: Buffer.from(descriptorEntry[1], "hex"),
    },
  ]);
  const membership = async (
    root: typeof forcedRoot,
    entry: readonly [string, string],
  ) => ({
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    proof: await keyValuePhasProof(
      {
        root: root.phasRoot,
        count: root.count,
        entries: root.entries,
      },
      Buffer.from(entry[0], "hex"),
      Buffer.from(entry[1], "hex"),
    ),
  });
  const claim: ValidationClaimWitnessV1 = {
    version: 1n,
    descriptor_membership: {
      ...(await membership(descriptorRoot, descriptorEntry)),
      key: eventKey,
      value: operatorDescriptor,
    },
    transition_step_membership: {
      ...(await membership(transitionRoot, transitionEntry)),
      key: 0n,
      value: step,
    },
    event_to_step_membership: {
      ...(await membership(eventToStepRoot, eventToStepEntry)),
      key: eventKey,
      value: eventToStepValue,
    },
    source_membership: {
      ForcedValidationSource: {
        membership: {
          ...(await membership(forcedRoot, forcedEntry)),
          key: txOrderId,
          value: forcedTransaction,
        },
      },
    },
    validation_context_cbor:
      operatorTrace.validationContextCbor.toString("hex"),
    initial_state: validationMachineStateDataFromCore(operatorTrace.states[0]!),
    terminal_state: validationMachineStateDataFromCore(
      operatorTrace.states.at(-1)!,
    ),
    initial_state_proof: validationTraceProofDataFromCore(
      operatorTrace.tree.proofs[0]!,
    ),
    terminal_state_proof: validationTraceProofDataFromCore(
      operatorTrace.tree.proofs.at(-1)!,
    ),
  };
  const header: HeaderV1 = {
    ...makeHeader(operatorVkey, now),
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: transitionRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: descriptorRoot.root,
    forcedTransactionCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return { header, claim };
};

/**
 * The empty claimed-delta commitment, i.e. `frontier_commitment(0, [])` on the
 * Aiken side. Every pre-VM-DEFECT-2 rejection fixture pinned the machine
 * state's `ledger_delta_root` to exactly this value, which is the one
 * pre-state in which the deleted `rejected_successor_is_exact` clause was
 * satisfiable.
 */
export const EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1 =
  hashMidgardValidationLedgerDeltaV1([]);

export const outRefCbor = (byte: number, index = 0n): Buffer =>
  encodeMidgardSpendInputItemV1({
    txId: Buffer.alloc(32, byte),
    outputIndex: Number(index),
  });

export const plainOutputCbor = (lovelace: bigint): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace, assets: new Map() },
  });

/**
 * Produces a genuine non-empty claimed ledger-delta commitment through exactly
 * the reference-builder pipeline an accepted transaction uses
 * (`hashMidgardValidationLedgerDeltaV1` over authenticated delete/insert
 * operations carrying real MPF proof descriptors). The value is deliberately
 * *not* synthesised: it is the commitment a real one-input/one-output L2
 * transaction claims.
 */
export const buildNonEmptyClaimedLedgerDeltaRootV1 =
  async (): Promise<Buffer> => {
    const spent = outRefCbor(0x9c);
    const produced = outRefCbor(0x9d);
    const spentOutput = plainOutputCbor(10_000_000n);
    const producedOutput = plainOutputCbor(9_000_000n);
    const mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: [
        { type: "delete", key: spent },
        buildValidationMachineLedgerInsertOpV1({
          key: produced,
          outputCbor: producedOutput,
        }),
      ],
    });
    return hashMidgardValidationLedgerDeltaV1(
      mutationSteps.map(({ operation, proofFoldTrace }) => ({
        ...operation,
        proofDescriptor: proofFoldTrace.descriptor,
      })),
    );
  };

export const restampTraceLedgerDeltaRoot = (
  trace: DeterministicValidationMachineTrace,
  ledgerDeltaRoot: Buffer,
): DeterministicValidationMachineTrace => {
  const states = trace.states.map((state) => ({ ...state, ledgerDeltaRoot }));
  return {
    ...trace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineStateV1),
      trace.verdict,
      states.at(-1)!.rejectionCodeHash,
    ),
  };
};

export const replaceTerminalState = (
  trace: DeterministicValidationMachineTrace,
  {
    terminal,
    verdict,
    rejectionCode,
    rejectionCodeHash,
  }: {
    readonly terminal: DeterministicValidationMachineTrace["states"][number];
    readonly verdict: "accepted" | "rejected";
    readonly rejectionCode: DeterministicValidationMachineTrace["rejectionCode"];
    readonly rejectionCodeHash: Buffer;
  },
): DeterministicValidationMachineTrace => {
  const states = trace.states.map((state, index) =>
    index === trace.states.length - 1 ? terminal : state,
  );
  return {
    ...trace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineStateV1),
      verdict,
      rejectionCodeHash,
    ),
    verdict,
    rejectionCode,
  };
};

/**
 * Work root of the exact rejecting-terminal witness the canonical V1 machine
 * requires of a rejection successor, i.e. the Aiken
 * `hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root))`.
 */
export const rejectingTerminalWorkRootV1 = ({
  programCounter,
  rejectionCode,
  priorLedgerRoot,
}: {
  readonly programCounter: number;
  readonly rejectionCode: string;
  readonly priorLedgerRoot: Buffer;
}): Buffer =>
  Buffer.from(
    hashMidgardValidationWorkWitnessV1({
      phase: "terminal",
      programCounter,
      witnessCbor: encodeCbor([
        2n,
        Buffer.from(rejectionCode, "ascii"),
        priorLedgerRoot,
        Buffer.from("80", "hex"),
      ]),
    }),
  );

export type ForcedValidationDisputeFixture = {
  readonly header: HeaderV1;
  readonly claim: ValidationClaimWitnessV1;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly challengerDescriptor: ReturnType<
    typeof validationTraceDescriptorDataFromCore
  >;
  readonly evidence: ReturnType<typeof buildValidationDisputeEvidenceBundleV1>;
  readonly claimedLedgerDeltaRoot: Buffer;
};

/**
 * VM-DEFECT-2 regression fixture — the adversarial case the pre-fix rejection
 * surface could not express.
 *
 * The forced source carries `verdict: ForcedTxValid`, which
 * `validation-claim-v1.ak` (`forced_verdict_matches`, and the exactly
 * analogous `descriptor.verdict == Accepted` clause every *normal* L2 source
 * is held to) forces into an `Accepted` committed descriptor. The transaction
 * itself has no spend inputs, so the deterministic machine rejects it with
 * `E_EMPTY_INPUTS` at the `inputSets` instruction. The claimed ledger delta is
 * a real non-empty commitment, exactly as a real transaction's would be.
 *
 * The reference TypeScript builder refuses to emit a rejected trace with a
 * non-empty delta (`validation-machine.ts`: "a rejected transaction must
 * commit an exact ledger no-op"), so the operator's chosen claimed-delta
 * commitment is re-stamped onto every state afterwards. That is faithful:
 * `ledger_delta_root` is immutable context chosen by the operator and pinned
 * pre == post by `immutable_context_matches`, and nothing before the
 * `ledgerDelta` phase reads it.
 */
export const buildAcceptedClaimOverRejectingTransactionFixture = async ({
  operatorVkey,
  now,
  claimedLedgerDeltaRoot,
  clearChallengerTerminalDelta = false,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly claimedLedgerDeltaRoot: Buffer;
  readonly clearChallengerTerminalDelta?: boolean;
}): Promise<ForcedValidationDisputeFixture> => {
  const txOrderId = transitionTraceOutRef("e2");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: plainOutputCbor(100_000_000n),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(forcedNativeTx);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  const honestTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      ledgerWitnessEntries: [],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.EmptyInputs,
      // The challenger replays the operator's ACCEPTED leaf to a rejection;
      // its states must still bind the committed (ForcedTxValid) source.
      committedForcedVerdict: "accepted",
    }),
  );
  const restamped = restampTraceLedgerDeltaRoot(
    honestTrace,
    claimedLedgerDeltaRoot,
  );
  // Counterfactual variant: the successor shape the deleted
  // `post.ledger_delta_root == frontier_commitment(0, [])` clause demanded.
  const challengerTrace = clearChallengerTerminalDelta
    ? replaceTerminalState(restamped, {
        terminal: {
          ...restamped.states.at(-1)!,
          ledgerDeltaRoot: EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1,
        },
        verdict: "rejected",
        rejectionCode: restamped.rejectionCode,
        rejectionCodeHash: restamped.states.at(-1)!.rejectionCodeHash,
      })
    : restamped;
  const operatorTrace = replaceTerminalState(restamped, {
    terminal: {
      ...restamped.states.at(-1)!,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      workRoot: Buffer.alloc(32, 0x7e),
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot,
  };
};

/**
 * Lovelace carried by both sides of the min-Ada journey transaction below.
 * `coins_per_utxo_byte * (160 + |canonical output|)` for an output of that
 * shape is on the order of a million lovelace, so this is a decisive miss,
 * and the fixture asserts the miss rather than trusting the arithmetic.
 */
const MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1 = 100_000n;

/**
 * R8 of decision 0005 (#618) / the #627 ruling: the end-to-end journey for the
 * `E_MIN_ADA` wiring in the ValueAndMint output ladder.
 *
 * The forced source carries `verdict: ForcedTxValid`, which
 * `validation-claim-v1.ak` forces into an `Accepted` committed descriptor. The
 * transaction is otherwise impeccable -- one resolved spend input, a real
 * key witness, zero fee, and the produced output carries exactly the lovelace
 * the input did, so value is preserved and nothing before stage 3 of
 * ValueAndMint has anything to say about it. The one rule it breaks is the
 * produced output's minimum-Ada floor, which the machine convicts on at the
 * output-descriptor step of stage 3 (`E_MIN_ADA`).
 *
 * The operator commits the honest trace with only its terminal replaced by an
 * `Accepted` one, so the bisection lands on the last step -- the ValueAndMint
 * output-descriptor instruction whose successor is the rejecting terminal --
 * and the challenger proves it through `value_and_mint_v1` and
 * `value_and_mint_output_descriptor_semantic_v1`. That is the only route on
 * which the new `rejected_successor_is_exact(pre, post, reject_min_ada)`
 * conjunct executes on L1.
 *
 * A rejected transaction commits an exact ledger no-op, so the block's prior
 * and post UTxO roots are both the root of the honest pre-state ledger and
 * there are no mutation steps.
 */
export const buildAcceptedClaimOverMinAdaRejectingTransactionFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}): Promise<
  ForcedValidationDisputeFixture & { readonly disputedLowIndex: number }
> => {
  const txOrderId = transitionTraceOutRef("e6");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const spendingKey = CML.PrivateKey.generate_ed25519();
  const spendingAddress = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const spentOutRef = outRefCbor(0x8b);
  const spentOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1, assets: new Map() },
  });
  const producedOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1, assets: new Map() },
  });
  // Measured, not assumed: this fixture only means anything if the produced
  // output really is below the floor the wiring convicts on.
  expect(
    outputCborMeetsMinAdaV1(producedOutput, MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1),
  ).toBe(false);
  const unsignedTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
  });
  const transactionId = computeMidgardNativeTxIdV1(unsignedTx);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
    addrTxWitsPreimageCbor: encodeCbor([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(transactionId),
          spendingKey,
        ).to_cbor_bytes(),
      ),
    ]),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  // The probe deletion is only a way to read the root of the honest pre-state
  // ledger trie; none of its steps reach the machine, which is given an exact
  // no-op as a rejected transaction requires.
  const ledgerRootProbe = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spentOutRef, output: spentOutput }],
    operations: [{ type: "delete", key: spentOutRef }],
  });
  const utxosRoot = ledgerRootProbe[0]!.preRoot.toString("hex");
  const challengerTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: utxosRoot,
      postUtxosRoot: utxosRoot,
      ledgerWitnessEntries: [{ outRef: spentOutRef, output: spentOutput }],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.MinAda,
      // The challenger replays the operator's ACCEPTED leaf to a rejection;
      // its states must still bind the committed (ForcedTxValid) source.
      committedForcedVerdict: "accepted",
    }),
  );
  const operatorTrace = replaceTerminalState(challengerTrace, {
    terminal: {
      ...challengerTrace.states.at(-1)!,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      workRoot: Buffer.alloc(32, 0x7e),
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot: utxosRoot,
    postUtxosRoot: utxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: challengerTrace.states[0]!.ledgerDeltaRoot,
    disputedLowIndex: challengerTrace.states.length - 2,
  };
};

/**
 * Mirror control for VM-DEFECT-2 (GOAL_SPEC §3 invariant 9 -- soundness is
 * symmetric). Same block layout, same disputed instruction, same rejection
 * code and same *genuinely non-empty* claimed ledger delta as the
 * challenger-wins fixture; the only difference is that the transaction is
 * actually valid and the operator's committed `Accepted` verdict is honest.
 *
 * The dishonest challenger commits the strongest forgery available: a
 * rejecting terminal whose immutable context, program counter, execution
 * budget and work root are all exactly what `rejected_successor_is_exact`
 * demands (`hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(code, pre.prior_ledger_root))`). The one
 * thing it cannot supply is a genuine rejection at the `inputSets`
 * instruction, so the challenger must lose. Removing the delta-clearing clause
 * must not have made honest blocks challengeable.
 */
/**
 * One honest, valid, signed native transaction (one spend, one output) and
 * its accepted deterministic trace, shared by the honest-operator mirror
 * fixture below and the forged-operator-successor fixtures that dispute one
 * of its steps. `txOrderSeed` keeps the forced-event keys of the fixtures
 * distinct.
 */
const buildHonestAcceptedNativeTransactionTraceV1 = async ({
  now,
  txOrderSeed,
}: {
  readonly now: number;
  readonly txOrderSeed: string;
}) => {
  const txOrderId = transitionTraceOutRef(txOrderSeed);
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const spendingKey = CML.PrivateKey.generate_ed25519();
  const spendingAddress = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const spentOutRef = outRefCbor(0x8a);
  const spentOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: 10_000_000n, assets: new Map() },
  });
  const producedOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: 10_000_000n, assets: new Map() },
  });
  const unsignedTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
  });
  const transactionId = computeMidgardNativeTxIdV1(unsignedTx);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
    addrTxWitsPreimageCbor: encodeCbor([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(transactionId),
          spendingKey,
        ).to_cbor_bytes(),
      ),
    ]),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  const producedOutRef = encodeMidgardSpendInputItemV1({
    txId: transactionId,
    outputIndex: 0,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spentOutRef },
    buildValidationMachineLedgerInsertOpV1({
      key: producedOutRef,
      outputCbor: producedOutput,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spentOutRef, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  const preUtxosRoot = ledgerMutationSteps[0]!.preRoot.toString("hex");
  const postUtxosRoot = ledgerMutationSteps.at(-1)!.postRoot.toString("hex");
  const honestTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: preUtxosRoot,
      postUtxosRoot,
      ledgerWitnessEntries: [{ outRef: spentOutRef, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
  return {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace,
    preUtxosRoot,
    postUtxosRoot,
  };
};

/**
 * Mirror control for VM-DEFECT-2 (GOAL_SPEC §3 invariant 9 -- soundness is
 * symmetric). Same block layout, same disputed instruction, same rejection
 * code and same *genuinely non-empty* claimed ledger delta as the
 * challenger-wins fixture; the only difference is that the transaction is
 * actually valid and the operator's committed `Accepted` verdict is honest.
 *
 * The dishonest challenger commits the strongest forgery available: a
 * rejecting terminal whose immutable context, program counter, execution
 * budget and work root are all exactly what `rejected_successor_is_exact`
 * demands (`hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(code, pre.prior_ledger_root))`). The one
 * thing it cannot supply is a genuine rejection at the `inputSets`
 * instruction, so the challenger must lose. Removing the delta-clearing clause
 * must not have made honest blocks challengeable.
 */
export const buildHonestAcceptedValidationDisputeFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}): Promise<
  ForcedValidationDisputeFixture & { readonly disputedPhase: string }
> => {
  const {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace: operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  } = await buildHonestAcceptedNativeTransactionTraceV1({
    now,
    txOrderSeed: "e3",
  });
  const disputedLowIndex = operatorTrace.states.findIndex(
    (state) => state.phase === "inputSets",
  );
  if (disputedLowIndex < 0) {
    throw new Error(
      "honest accepted validation trace is missing its inputSets instruction",
    );
  }
  const preState = operatorTrace.states[disputedLowIndex]!;
  const forgedRejectionCode = RejectCodes.EmptyInputs;
  const forgedTerminal = {
    ...preState,
    phase: "terminal" as const,
    programCounter: preState.programCounter + 1,
    workRoot: rejectingTerminalWorkRootV1({
      programCounter: preState.programCounter + 1,
      rejectionCode: forgedRejectionCode,
      priorLedgerRoot: preState.priorLedgerRoot,
    }),
    verdict: "rejected" as const,
    rejectionCodeHash: Buffer.from(
      hashMidgardValidationRejectionCodeV1(forgedRejectionCode),
    ),
  };
  const challengerStates = operatorTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forgedTerminal,
  );
  const challengerTrace: DeterministicValidationMachineTrace = {
    ...operatorTrace,
    states: challengerStates,
    tree: buildMidgardValidationTraceTree(
      challengerStates.map(hashMidgardValidationMachineStateV1),
      "rejected",
      forgedTerminal.rejectionCodeHash,
    ),
    verdict: "rejected",
    rejectionCode: forgedRejectionCode,
  };
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase: preState.phase,
  };
};

/**
 * R5 item 1 (#617) journey fixture for the cek and ValueAndMint prepare +
 * semantic decomposition. The transaction is the honest, valid, signed
 * native transaction above and the challenger's trace is its honest
 * accepted trace; the operator commits the same trace up to the first state
 * of `disputedPhase` and a forged successor from there on (the honest
 * `Accepted` terminal with a fabricated work root, repeated to the honest
 * length so the descriptor and every midpoint after the boundary disagree,
 * which pins the bisection at exactly that boundary). The one-step the challenger then
 * proves on L1 is the honest trace's first step of that phase:
 *
 * - `cek`: this pure key-witness spend has `execution_count == 0` (no script
 *   execution of any language), so the cek phase is the single stand-alone
 *   ValueAndMint hand-off (`cek_v1` prepare,
 *   then `cek_finish_semantic_v1`, resolver 11 / semantic 0);
 * - `valueAndMint`: the stage-0 `begin` step (`value_and_mint_v1` prepare,
 *   then `value_and_mint_begin_semantic_v1`, resolver 12 / semantic 0).
 */
export const buildForgedOperatorSuccessorValidationDisputeFixture = async ({
  operatorVkey,
  now,
  disputedPhase,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly disputedPhase: "cek" | "valueAndMint";
}): Promise<
  ForcedValidationDisputeFixture & {
    readonly disputedPhase: "cek" | "valueAndMint";
    readonly disputedLowIndex: number;
  }
> => {
  const {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace: challengerTrace,
    preUtxosRoot,
    postUtxosRoot,
  } = await buildHonestAcceptedNativeTransactionTraceV1({
    now,
    txOrderSeed: disputedPhase === "cek" ? "e4" : "e5",
  });
  const disputedLowIndex = challengerTrace.states.findIndex(
    (state) => state.phase === disputedPhase,
  );
  if (disputedLowIndex < 0) {
    throw new Error(
      `honest accepted validation trace is missing its ${disputedPhase} phase`,
    );
  }
  const honestTerminal = challengerTrace.states.at(-1)!;
  if (honestTerminal.phase !== "terminal") {
    throw new Error(
      "honest accepted validation trace does not end in a terminal state",
    );
  }
  // The honest terminal with only its work root fabricated: every endpoint
  // check the source validator applies to the operator's claim (terminal
  // phase, program counter == step count, verdict, rejection code, immutable
  // context, ledger delta root) still holds, so the dispute opens and the
  // bisection -- not the source stage -- is what exposes the forgery.
  const forgedTerminal = {
    ...honestTerminal,
    workRoot: Buffer.alloc(32, 0x7e),
  };
  const operatorStates = challengerTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forgedTerminal,
  );
  const operatorTrace: DeterministicValidationMachineTrace = {
    ...challengerTrace,
    states: operatorStates,
    tree: buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineStateV1),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ),
  };
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase,
    disputedLowIndex,
  };
};

export const runEmulatorLifecycleStage = async <T>(
  stage: string,
  operation: () => Promise<T>,
): Promise<T> => {
  try {
    return await operation();
  } catch (cause) {
    const serializedCause =
      typeof cause === "object" && cause !== null
        ? JSON.stringify(
            cause,
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
            2,
          )
        : undefined;
    const detail = [
      cause instanceof Error ? (cause.stack ?? cause.message) : String(cause),
      serializedCause,
    ]
      .filter((value) => value !== undefined && value.length > 0)
      .join("\n");
    throw new Error(`emulator lifecycle stage ${stage} failed: ${detail}`);
  }
};

export type CompleteSignedTransactionMeasurement = {
  readonly completeSignedBytes: number;
  readonly l1ByteMargin: number;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
  readonly inputCount: number;
  readonly referenceInputCount: number;
  readonly outputCount: number;
  readonly vkeyWitnessCount: number;
  readonly nativeScriptCount: number;
  readonly redeemerCount: number;
  readonly datumCount: number;
  readonly plutusV1ScriptCount: number;
  readonly plutusV2ScriptCount: number;
  readonly plutusV3ScriptCount: number;
};

export const measureCompleteSignedTransaction = (
  transactionCbor: string,
): CompleteSignedTransactionMeasurement => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const redeemers = witnessSet.redeemers()?.to_flat_format();
  let executionMemory = 0n;
  let executionSteps = 0n;
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const exUnits = redeemers!.get(index).ex_units();
    executionMemory += exUnits.mem();
    executionSteps += exUnits.steps();
  }
  const completeSignedBytes = transactionCbor.length / 2;
  return {
    completeSignedBytes,
    l1ByteMargin: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize - completeSignedBytes,
    executionMemory,
    executionSteps,
    inputCount: body.inputs().len(),
    referenceInputCount: body.reference_inputs()?.len() ?? 0,
    outputCount: body.outputs().len(),
    vkeyWitnessCount: witnessSet.vkeywitnesses()?.len() ?? 0,
    nativeScriptCount: witnessSet.native_scripts()?.len() ?? 0,
    redeemerCount: redeemers?.len() ?? 0,
    datumCount: witnessSet.plutus_datums()?.len() ?? 0,
    plutusV1ScriptCount: witnessSet.plutus_v1_scripts()?.len() ?? 0,
    plutusV2ScriptCount: witnessSet.plutus_v2_scripts()?.len() ?? 0,
    plutusV3ScriptCount: witnessSet.plutus_v3_scripts()?.len() ?? 0,
  };
};

// Byte attribution for a complete signed transaction: splits the envelope
// into body/witness-set contributors and names every attached script by hash
// so an oversize transaction can be attributed to specific contributors
// rather than guessed at.
export const midgardScriptHashNames = (
  contracts: MidgardValidators,
): ReadonlyMap<string, string> =>
  new Map<string, string>([
    [contracts.stateQueue.policyId, "stateQueueMint"],
    [contracts.stateQueue.spendingScriptHash, "stateQueueSpend"],
    [contracts.activeOperators.policyId, "activeOperatorsMint"],
    [contracts.activeOperators.spendingScriptHash, "activeOperatorsSpend"],
    [contracts.retiredOperators.policyId, "retiredOperatorsMint"],
    [contracts.retiredOperators.spendingScriptHash, "retiredOperatorsSpend"],
    [contracts.registeredOperators.policyId, "registeredOperatorsMint"],
    [
      contracts.registeredOperators.spendingScriptHash,
      "registeredOperatorsSpend",
    ],
    [contracts.scheduler.policyId, "schedulerMint"],
    [contracts.scheduler.spendingScriptHash, "schedulerSpend"],
    [contracts.hubOracle.policyId, "hubOracleMint"],
    [contracts.hubOracle.spendingScriptHash, "hubOracleSpend"],
    [contracts.fraudProof.policyId, "fraudProofMint"],
    [contracts.fraudProof.spendingScriptHash, "fraudProofSpend"],
    [contracts.fraudProofCatalogue.policyId, "fraudProofCatalogueMint"],
    [
      contracts.fraudProofCatalogue.spendingScriptHash,
      "fraudProofCatalogueSpend",
    ],
    [
      contracts.fraudProofs.validationTraceDispute.spendingScriptHash,
      "validationTraceDispute",
    ],
  ]);

export const attributeTransactionBytes = (
  label: string,
  transactionCbor: string,
  scriptNames: ReadonlyMap<string, string>,
): void => {
  const transaction = CML.Transaction.from_cbor_hex(transactionCbor);
  const body = transaction.body();
  const witnessSet = transaction.witness_set();
  const total = transactionCbor.length / 2;
  const lines: string[] = [
    `[tx-attribution] ${label}`,
    `  total signed bytes = ${total.toString()} (L1 limit ${PROTOCOL_PARAMETERS_DEFAULT.maxTxSize.toString()})`,
    `  body bytes         = ${body.to_cbor_bytes().length.toString()}`,
    `  witness set bytes  = ${witnessSet.to_cbor_bytes().length.toString()}`,
  ];
  // CML list wrappers expose no `to_cbor_bytes`, so list contributions are the
  // sum of their element encodings (the enclosing array header is a few bytes).
  const listBytes = <T extends { to_cbor_bytes: () => Uint8Array }>(list: {
    len: () => number;
    get: (index: number) => T;
  }): number => {
    let bytes = 0;
    for (let index = 0; index < list.len(); index += 1) {
      bytes += list.get(index).to_cbor_bytes().length;
    }
    return bytes;
  };
  const inputs = body.inputs();
  lines.push(
    `  body.inputs           n=${inputs.len().toString()} bytes=${listBytes(inputs).toString()}`,
  );
  const referenceInputs = body.reference_inputs();
  lines.push(
    `  body.reference_inputs n=${(referenceInputs?.len() ?? 0).toString()} bytes=${(referenceInputs === undefined ? 0 : listBytes(referenceInputs)).toString()}`,
  );
  const outputs = body.outputs();
  lines.push(
    `  body.outputs          n=${outputs.len().toString()} bytes=${listBytes(outputs).toString()}`,
  );
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = outputs.get(index);
    const datum = output.datum();
    const scriptRef = output.script_ref();
    lines.push(
      `    output[${index.toString()}] bytes=${output.to_cbor_bytes().length.toString()} datum=${(datum?.to_cbor_bytes().length ?? 0).toString()} scriptRef=${(scriptRef?.to_cbor_bytes().length ?? 0).toString()}`,
    );
  }
  const mint = body.mint();
  lines.push(
    `  body.mint             policies=${(mint?.policy_count() ?? 0).toString()}`,
  );
  const vkeyWitnesses = witnessSet.vkeywitnesses();
  lines.push(
    `  witness.vkeys         n=${(vkeyWitnesses?.len() ?? 0).toString()} bytes=${(vkeyWitnesses === undefined ? 0 : listBytes(vkeyWitnesses)).toString()}`,
  );
  const redeemers = witnessSet.redeemers();
  lines.push(
    `  witness.redeemers     bytes=${(redeemers?.to_cbor_bytes().length ?? 0).toString()}`,
  );
  const flatRedeemers = redeemers?.to_flat_format();
  for (let index = 0; index < (flatRedeemers?.len() ?? 0); index += 1) {
    const redeemer = flatRedeemers!.get(index);
    lines.push(
      `    redeemer[${index.toString()}] tag=${redeemer.tag().toString()} index=${redeemer.index().toString()} dataBytes=${redeemer.data().to_cbor_bytes().length.toString()}`,
    );
  }
  const datums = witnessSet.plutus_datums();
  lines.push(
    `  witness.datums        n=${(datums?.len() ?? 0).toString()} bytes=${(datums === undefined ? 0 : listBytes(datums)).toString()}`,
  );
  const nativeScripts = witnessSet.native_scripts();
  lines.push(
    `  witness.nativeScripts n=${(nativeScripts?.len() ?? 0).toString()} bytes=${(nativeScripts === undefined ? 0 : listBytes(nativeScripts)).toString()}`,
  );
  let attachedScriptBytes = 0;
  const v3Scripts = witnessSet.plutus_v3_scripts();
  for (let index = 0; index < (v3Scripts?.len() ?? 0); index += 1) {
    const script = v3Scripts!.get(index);
    const hash = script.hash().to_hex();
    const bytes = script.to_cbor_bytes().length;
    attachedScriptBytes += bytes;
    lines.push(
      `    plutusV3[${index.toString()}] ${scriptNames.get(hash) ?? "unknown"} hash=${hash} bytes=${bytes.toString()}`,
    );
  }
  const v2Scripts = witnessSet.plutus_v2_scripts();
  for (let index = 0; index < (v2Scripts?.len() ?? 0); index += 1) {
    const script = v2Scripts!.get(index);
    const bytes = script.to_cbor_bytes().length;
    attachedScriptBytes += bytes;
    lines.push(
      `    plutusV2[${index.toString()}] hash=${script.hash().to_hex()} bytes=${bytes.toString()}`,
    );
  }
  lines.push(
    `  attached script bytes total = ${attachedScriptBytes.toString()}`,
    `  non-script bytes            = ${(total - attachedScriptBytes).toString()}`,
  );
  console.info(lines.join("\n"));
};

export const captureEmulatorSubmission = async <T>(
  emulator: Emulator,
  operation: () => Promise<T>,
): Promise<{
  readonly result: T;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly measurements: readonly CompleteSignedTransactionMeasurement[];
  readonly transactionCbors: readonly string[];
}> => {
  const submit = emulator.submitTx.bind(emulator);
  let measurement: CompleteSignedTransactionMeasurement | undefined;
  const measurements: CompleteSignedTransactionMeasurement[] = [];
  const transactionCbors: string[] = [];
  emulator.submitTx = async (transaction) => {
    measurement = measureCompleteSignedTransaction(transaction);
    measurements.push(measurement);
    transactionCbors.push(transaction);
    return submit(transaction);
  };
  try {
    const result = await operation();
    if (measurement === undefined) {
      throw new Error("Expected emulator operation to submit a transaction");
    }
    return { result, measurement, measurements, transactionCbors };
  } finally {
    emulator.submitTx = submit;
  }
};

export const submitSetupTx = async ({
  lucid,
  contracts,
  nonceUtxo,
  catalogue,
  header,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly nonceUtxo: UTxO;
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly header: HeaderV1;
}): Promise<{
  readonly fraudulentBlockOutRef: string;
  readonly headerHash: string;
  readonly stateQueueBlockUnit: string;
  readonly stateQueueRootUnit: string;
  readonly hubOracle: UTxO;
  readonly scheduler: UTxO;
  readonly activeOperatorsRoot: UTxO;
  readonly activeOperatorsRootUnit: string;
  readonly retiredOperatorsRoot: UTxO;
  readonly retiredOperatorsRootUnit: string;
  readonly activeOperatorNode: UTxO;
  readonly activeOperatorNodeUnit: string;
  readonly registeredOperatorsRoot: UTxO;
}> => {
  const hubOracleDatum = await Effect.runPromise(makeHubOracleDatum(contracts));
  const headerHash = await Effect.runPromise(hashBlockHeaderV1(header));
  const hubOracleUnit = toUnit(
    contracts.hubOracle.policyId,
    HUB_ORACLE_ASSET_NAME,
  );
  const fraudProofCatalogueUnit = toUnit(
    contracts.fraudProofCatalogue.policyId,
    FRAUD_PROOF_CATALOGUE_ASSET_NAME,
  );
  const stateQueueBlockUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
  );
  const stateQueueRootUnit = toUnit(
    contracts.stateQueue.policyId,
    STATE_QUEUE_ROOT_ASSET_NAME,
  );
  const schedulerUnit = toUnit(
    contracts.scheduler.policyId,
    SCHEDULER_ASSET_NAME,
  );
  const activeOperatorsRootUnit = toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  );
  const retiredOperatorsRootUnit = toUnit(
    contracts.retiredOperators.policyId,
    RETIRED_OPERATORS_ROOT_ASSET_NAME,
  );
  const activeOperatorNodeUnit = toUnit(
    contracts.activeOperators.policyId,
    ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + header.operatorVkey,
  );
  const registeredOperatorsRootUnit = toUnit(
    contracts.registeredOperators.policyId,
    REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  );
  const confirmedState = {
    headerHash: GENESIS_HEADER_HASH,
    prevHeaderHash: GENESIS_HEADER_HASH,
    utxoRoot: EMPTY_MERKLE_TREE_ROOT,
    startTime: header.startTime,
    endTime: header.startTime,
    protocolVersion: GENESIS_PROTOCOL_VERSION,
  };
  const unsigned = await lucid
    .newTx()
    .validFrom(Number(header.startTime - 120_000n))
    .validTo(Number(header.startTime + 1n))
    .collectFrom([nonceUtxo])
    .mintAssets({ [hubOracleUnit]: 1n }, Data.void())
    .pay.ToAddressWithData(
      credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOracle.policyId),
      ),
      {
        kind: "inline",
        value: Data.to(hubOracleDatum, HubOracleDatum),
      },
      { [hubOracleUnit]: 1n },
    )
    .mintAssets({ [schedulerUnit]: 1n }, Data.to("Init", SchedulerMintRedeemer))
    .pay.ToContract(
      contracts.scheduler.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to("NoActiveOperators", SchedulerDatum),
      },
      { [schedulerUnit]: 1n },
    )
    // Fixed by the authored setup output order: hub oracle, scheduler,
    // state-queue root, active-operators root, retired-operators root, then
    // registered-operators root.
    .mintAssets(
      { [stateQueueRootUnit]: 1n },
      Data.to(
        { InitV1: { output_index: SETUP_OUTPUT_INDEX.stateQueueRoot } },
        StateQueueRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.stateQueue.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: Data.castTo(confirmedState, ConfirmedState),
        }),
      },
      { [stateQueueRootUnit]: 1n },
    )
    .mintAssets(
      { [activeOperatorsRootUnit]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.activeOperatorsRoot } },
        ActiveOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [activeOperatorsRootUnit]: 1n },
    )
    .mintAssets(
      { [retiredOperatorsRootUnit]: 1n },
      Data.to(
        { Init: { output_index: SETUP_OUTPUT_INDEX.retiredOperatorsRoot } },
        RetiredOperatorMintRedeemer,
      ),
    )
    .pay.ToContract(
      contracts.retiredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [retiredOperatorsRootUnit]: 1n },
    )
    .mintAssets({ [registeredOperatorsRootUnit]: 1n }, Data.void())
    .pay.ToContract(
      contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView({
          key: "Empty",
          next: "Empty",
          data: "",
        }),
      },
      { [registeredOperatorsRootUnit]: 1n },
    )
    .mintAssets({ [fraudProofCatalogueUnit]: 1n }, Data.void())
    .pay.ToAddressWithData(
      contracts.fraudProofCatalogue.spendingScriptAddress,
      {
        kind: "inline",
        value: Data.to(catalogue.root, FraudProofCatalogueDatum),
      },
      { [fraudProofCatalogueUnit]: 1n },
    )
    .attach.MintingPolicy(contracts.hubOracle.mintingScript)
    .attach.MintingPolicy(contracts.fraudProofCatalogue.mintingScript)
    .attach.MintingPolicy(contracts.scheduler.mintingScript)
    .attach.MintingPolicy(contracts.stateQueue.mintingScript)
    .attach.MintingPolicy(contracts.activeOperators.mintingScript)
    .attach.MintingPolicy(contracts.retiredOperators.mintingScript)
    .attach.MintingPolicy(contracts.registeredOperators.mintingScript)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.initial", async () =>
    lucid.awaitTx(await signed.submit()),
  );

  const [initialActiveOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorsRootUnit,
  );
  if (initialActiveOperatorsRoot === undefined) {
    throw new Error("Setup transaction did not produce active-operators root");
  }
  const registeredOperatorActivationUnit = toUnit(
    contracts.registeredOperators.policyId,
    "00",
  );
  const activeRootWithOperatorDatum = encodeLinkedListNodeView({
    key: "Empty",
    next: { Key: { key: header.operatorVkey } },
    data: "",
  });
  const activeOperatorInitialDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      { bond_unlock_time: null, inactivity_strikes: 0n },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorsActivateRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.activeOperators.policyId,
      "test active-operators activation mint",
    );
    return Data.to(
      {
        ActivateOperator: {
          new_active_operator_key: header.operatorVkey,
          active_operator_anchor_element_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
          active_operator_inserted_node_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.insertedNode,
          registered_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.registeredOperators.policyId,
            "test registered-operators activation mint",
          ),
          active_operators_set_was_empty: true,
        },
      },
      ActiveOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const registeredOperatorsActivateRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.registeredOperators.policyId,
      "test registered-operators activation mint",
    );
    return Data.to(
      {
        ActivateOperator: {
          activating_operator: header.operatorVkey,
          anchor_element_input_outref: outputReferenceFromUTxO(
            initialActiveOperatorsRoot,
          ),
          anchor_element_output_index:
            ACTIVE_OPERATOR_ACTIVATION_OUTPUT_INDEX.root,
          hub_oracle_ref_input_index: 0n,
          retired_operators_element_ref_input_index: 0n,
          active_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.activeOperators.policyId,
            "test active-operators activation mint",
          ),
        },
      },
      RegisteredOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const activationUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-activation.complete",
    () =>
      lucid
        .newTx()
        .collectFrom(
          [initialActiveOperatorsRoot],
          Data.to("ListStateTransition", ActiveOperatorSpendRedeemer),
        )
        .mintAssets(
          { [activeOperatorNodeUnit]: 1n },
          activeOperatorsActivateRedeemer,
        )
        .mintAssets(
          { [registeredOperatorActivationUnit]: 1n },
          registeredOperatorsActivateRedeemer,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          { kind: "inline", value: activeRootWithOperatorDatum },
          initialActiveOperatorsRoot.assets,
        )
        .pay.ToContract(
          contracts.activeOperators.spendingScriptAddress,
          { kind: "inline", value: activeOperatorInitialDatum },
          { lovelace: 20_000_000n, [activeOperatorNodeUnit]: 1n },
        )
        .attach.MintingPolicy(contracts.activeOperators.mintingScript)
        .attach.Script(contracts.activeOperators.spendingScript)
        .attach.MintingPolicy(contracts.registeredOperators.mintingScript)
        .complete({ localUPLCEval: true }),
  );
  const activationSigned = await activationUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-activation", async () =>
    lucid.awaitTx(await activationSigned.submit()),
  );

  const [hubOracleUtxo] = await lucid.utxosAtWithUnit(
    credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOracle.policyId),
    ),
    hubOracleUnit,
  );
  const [stateQueueRootUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueRootUnit,
  );
  const [schedulerUtxo] = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    schedulerUnit,
  );
  const [activeOperatorNode] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit,
  );
  const [activeOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorsRootUnit,
  );
  const [retiredOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.retiredOperators.spendingScriptAddress,
    retiredOperatorsRootUnit,
  );
  const [registeredOperatorsRoot] = await lucid.utxosAtWithUnit(
    contracts.registeredOperators.spendingScriptAddress,
    registeredOperatorsRootUnit,
  );
  if (
    hubOracleUtxo === undefined ||
    stateQueueRootUtxo === undefined ||
    schedulerUtxo === undefined ||
    activeOperatorNode === undefined ||
    activeOperatorsRoot === undefined ||
    retiredOperatorsRoot === undefined ||
    registeredOperatorsRoot === undefined
  ) {
    throw new Error(
      "Setup transaction did not produce all state-queue dependencies",
    );
  }

  const stateQueueRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(stateQueueRootUtxo, contracts.stateQueue.policyId),
  );
  const schedulerAppointmentFeeInput = await firstWalletUtxo(
    lucid,
    "scheduler appointment fee input",
  );
  const appointmentInputs = [schedulerAppointmentFeeInput, schedulerUtxo];
  const appointmentRefs = [activeOperatorNode, registeredOperatorsRoot];
  const schedulerAppointmentRedeemer: SchedulerSpendRedeemer = {
    scheduler_input_index: ledgerOrderedIndex(
      appointmentInputs,
      schedulerUtxo,
      "scheduler appointment input",
    ),
    scheduler_output_index: SCHEDULER_APPOINTMENT_OUTPUT_INDEX.scheduler,
    advancing_approach: {
      AppointFirstOperator: {
        new_shifts_operator_node_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          activeOperatorNode,
          "active-operator node appointment reference input",
        ),
        registered_element_ref_input_index: ledgerOrderedIndex(
          appointmentRefs,
          registeredOperatorsRoot,
          "registered-operators root appointment reference input",
        ),
      },
    },
  };
  const appointmentUnsigned = await runEmulatorLifecycleStage(
    "setup.operator-appointment.complete",
    () =>
      lucid
        .newTx()
        .collectFrom([schedulerAppointmentFeeInput])
        .collectFrom(
          [schedulerUtxo],
          Data.to(schedulerAppointmentRedeemer, SchedulerSpendRedeemer),
        )
        .readFrom(appointmentRefs)
        .pay.ToContract(
          contracts.scheduler.spendingScriptAddress,
          {
            kind: "inline",
            value: Data.to(
              {
                ActiveOperator: {
                  operator: header.operatorVkey,
                  start_time: header.startTime,
                },
              },
              SchedulerDatum,
            ),
          },
          schedulerUtxo.assets,
        )
        .attach.Script(contracts.scheduler.spendingScript)
        .validFrom(Number(header.startTime - 120_000n))
        .validTo(Number(header.startTime + 1n))
        .complete({ localUPLCEval: true }),
  );
  const appointmentSigned = await appointmentUnsigned.sign
    .withWallet()
    .complete();
  await runEmulatorLifecycleStage("setup.operator-appointment", async () =>
    lucid.awaitTx(await appointmentSigned.submit()),
  );

  const [appointedSchedulerUtxo] = await lucid.utxosAtWithUnit(
    contracts.scheduler.spendingScriptAddress,
    schedulerUnit,
  );
  if (appointedSchedulerUtxo === undefined) {
    throw new Error(
      "Scheduler appointment transaction did not preserve scheduler",
    );
  }
  expect(Data.from(appointedSchedulerUtxo.datum!, SchedulerDatum)).toEqual({
    ActiveOperator: {
      operator: header.operatorVkey,
      start_time: header.startTime,
    },
  });

  const commitFeeInput = await firstWalletUtxo(lucid, "commit fee input");
  const commitValidFrom = header.startTime - 60_000n;
  const commitValidTo = header.endTime + 1n;
  const continuedActiveOperatorDatum = encodeLinkedListNodeView({
    key: { Key: { key: header.operatorVkey } },
    next: "Empty",
    data: Data.castTo(
      {
        bond_unlock_time:
          commitValidTo -
          1n +
          BigInt(MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs),
        inactivity_strikes: 0n,
      },
      ActiveOperatorDatum,
    ),
  });
  const activeOperatorCommitRedeemer = ((ctx) =>
    Data.to(
      {
        UpdateBondHoldNewState: {
          active_operator: header.operatorVkey,
          active_node_input_index: requireInputIndex(
            ctx,
            activeOperatorNode,
            "commit active-operator input",
          ),
          active_node_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              output.address ===
                contracts.activeOperators.spendingScriptAddress &&
              (output.assets[activeOperatorNodeUnit] ?? 0n) === 1n,
            "commit active-operator output",
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            "commit hub-oracle reference input",
          ),
          state_queue_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.stateQueue.policyId,
            "commit state-queue mint redeemer",
          ),
        },
      } satisfies ActiveOperatorSpendRedeemer,
      ActiveOperatorSpendRedeemer,
    )) satisfies BuildTxWithRedeemer;
  const commitTx = await Effect.runPromise(
    incompleteEmulatorCommitBlockHeaderTxProgram(
      lucid,
      {
        stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
        stateQueuePolicyId: contracts.stateQueue.policyId,
      },
      {
        anchorUTxO: stateQueueRoot,
        newHeader: header,
        additionalInputs: [commitFeeInput],
        validFrom: commitValidFrom,
        validTo: commitValidTo,
        schedulerRefInput: appointedSchedulerUtxo,
        additionalRefInputs: [hubOracleUtxo],
        activeOperatorInput: activeOperatorNode,
        activeOperatorSpendRedeemer: activeOperatorCommitRedeemer,
        activeOperatorSpendingScript: contracts.activeOperators.spendingScript,
        continuedActiveOperatorOutput: {
          address: contracts.activeOperators.spendingScriptAddress,
          datum: continuedActiveOperatorDatum,
          assets: activeOperatorNode.assets,
        },
        stateQueueSpendingScript: contracts.stateQueue.spendingScript,
        stateQueueMintingScript: contracts.stateQueue.mintingScript,
      },
    ),
  );
  const commitUnsigned = await runEmulatorLifecycleStage(
    "setup.header-commit.complete",
    () => commitTx.complete({ localUPLCEval: true }),
  );
  const commitSigned = await commitUnsigned.sign.withWallet().complete();
  await runEmulatorLifecycleStage("setup.header-commit", async () =>
    lucid.awaitTx(await commitSigned.submit()),
  );

  const [fraudulentBlockUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueBlockUnit,
  );
  const [continuedRootUtxo] = await lucid.utxosAtWithUnit(
    contracts.stateQueue.spendingScriptAddress,
    stateQueueRootUnit,
  );
  const [continuedActiveOperatorNode] = await lucid.utxosAtWithUnit(
    contracts.activeOperators.spendingScriptAddress,
    activeOperatorNodeUnit,
  );
  if (
    fraudulentBlockUtxo === undefined ||
    continuedRootUtxo === undefined ||
    continuedActiveOperatorNode === undefined
  ) {
    throw new Error(
      "Commit transaction did not produce the expected queue nodes",
    );
  }
  const committedBlock = await Effect.runPromise(
    utxoToStateQueueUTxO(fraudulentBlockUtxo, contracts.stateQueue.policyId),
  );
  const committedHeader = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(committedBlock.datum),
  );
  expect(committedHeader.transactionsRoot).toBe(header.transactionsRoot);
  const continuedRoot = await Effect.runPromise(
    utxoToStateQueueUTxO(continuedRootUtxo, contracts.stateQueue.policyId),
  );
  expect(continuedRoot.datum.next).toEqual({ Key: { key: headerHash } });

  return {
    fraudulentBlockOutRef: `${fraudulentBlockUtxo.txHash}#${fraudulentBlockUtxo.outputIndex.toString()}`,
    headerHash,
    stateQueueBlockUnit,
    stateQueueRootUnit,
    hubOracle: hubOracleUtxo,
    scheduler: appointedSchedulerUtxo,
    activeOperatorsRoot,
    activeOperatorsRootUnit,
    retiredOperatorsRoot,
    retiredOperatorsRootUnit,
    activeOperatorNode: continuedActiveOperatorNode,
    activeOperatorNodeUnit,
    registeredOperatorsRoot,
  };
};

export const VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE =
  "V1 validation-trace dispute";

export const validationDisputeControlPublicationTargets = (
  contracts: MidgardValidators,
) =>
  [
    {
      control: "dispute",
      name: VALIDATION_DISPUTE_REFERENCE_SCRIPT_ROLE,
      script: contracts.fraudProofs.validationTraceDispute.spendingScript,
    },
    {
      control: "source",
      name: "V1 validation-trace source",
      script:
        contracts.fraudProofs.validationTraceDispute.source.spendingScript,
    },
    {
      control: "game",
      name: "V1 validation-trace game",
      script: contracts.fraudProofs.validationTraceDispute.game.spendingScript,
    },
    {
      control: "boundary",
      name: "V1 validation-trace boundary",
      script:
        contracts.fraudProofs.validationTraceDispute.boundary.spendingScript,
    },
    {
      control: "timeout",
      name: "V1 validation-trace timeout",
      script:
        contracts.fraudProofs.validationTraceDispute.timeout.spendingScript,
    },
    {
      control: "award",
      name: "V1 validation-trace award",
      script: contracts.fraudProofs.validationTraceDispute.award.spendingScript,
    },
  ] as const;

export type ValidationDisputeControlPublicationTarget = ReturnType<
  typeof validationDisputeControlPublicationTargets
>[number];

export const publishAuthenticatedValidationDisputeControl = async ({
  lucid,
  target,
  authPolicy,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly target: ValidationDisputeControlPublicationTarget;
  readonly authPolicy: ReturnType<typeof createReferenceScriptAuthPolicy>;
}) => {
  const selectedFundingInputs = selectReferenceScriptFundingUtxos(
    await lucid.wallet().getUtxos(),
    referenceScriptPublicationFundingTarget(1),
  );
  if (selectedFundingInputs.length === 0) {
    throw new Error(
      `Expected a plain-Ada input for authenticated validation-dispute ${target.control} reference-script publication`,
    );
  }
  const referenceScriptsAddress = await lucid.wallet().address();
  const { tx, layout } = await Effect.runPromise(
    completeReferenceScriptPublicationTxProgram({
      lucid,
      selectedFundingInputs,
      walletAddress: referenceScriptsAddress,
      referenceScriptsAddress,
      missingTargets: [target],
      authPolicy,
    }),
  );
  const localOutput = layout.localReferenceOutputs.get(target.name);
  if (localOutput === undefined) {
    throw new Error(
      `Authenticated publication transaction omitted the validation-dispute ${target.control} reference-script output`,
    );
  }
  const signed = await tx.sign.withWallet().complete();
  const publicationMeasurement = measureCompleteSignedTransaction(
    signed.toCBOR(),
  );
  if (publicationMeasurement.l1ByteMargin <= 0) {
    throw new Error(
      `Authenticated validation-dispute ${target.control} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and does not fit the 16,384-byte L1 envelope`,
    );
  }
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const outRef = {
    txHash,
    outputIndex: localOutput.outputIndex,
  };
  const published = await lucid.utxosByOutRef([outRef]);
  if (published.length !== 1) {
    throw new Error(
      `Expected one live validation-dispute ${target.control} reference-script UTxO at ${txHash}#${localOutput.outputIndex.toString()}, found ${published.length.toString()}`,
    );
  }
  return {
    authPolicyDeploymentInfo:
      referenceScriptAuthPolicyDeploymentInfo(authPolicy),
    publicationMeasurement,
    utxo: published[0]!,
  };
};

export const publishValidationDisputeReferenceScript = async ({
  lucid,
  contracts,
  now,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
  readonly now: number;
}) => {
  const target = validationDisputeControlPublicationTargets(contracts)[0];
  return publishAuthenticatedValidationDisputeControl({
    lucid,
    target,
    authPolicy: createReferenceScriptAuthPolicy(lucid, now),
  });
};

// Publishes a deployed validator as a plain reference-script UTxO at the
// publisher wallet address, following the hash-checked deployment
// consumption pattern (`requireDeploymentReferenceScript`); the consuming
// submit path re-derives the applied script hash and requires the published
// scriptRef to match it exactly.
export const publishPlainReferenceScriptUtxo = async ({
  lucid,
  script,
  label,
  oversized = false,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly script: Script;
  readonly label: string;
  /**
   * The applied CEK execution-selection / context-step / core-step semantic
   * resolvers (R5 item 1) exceed the 16,384-byte L1 proof envelope, so their
   * deployment-time publication cannot fit it: the emulator must host them
   * under a raised `maxTxSize`, the output must reach the script-ref min-Ada
   * for a ~45–94 KiB reference script, and the measurement is returned
   * unasserted so callers pin the honest publication size while the consuming
   * semantic-resolution transaction stays inside the envelope via `readFrom`.
   */
  readonly oversized?: boolean;
}): Promise<{
  readonly utxo: UTxO;
  readonly publicationMeasurement: CompleteSignedTransactionMeasurement;
}> => {
  // Park the reference script at an unspendable script credential so no
  // later wallet coin selection can consume the published UTxO mid-flow.
  const parkAddress = credentialToAddress(
    network,
    scriptHashToCredential("2f".repeat(28)),
  );
  const lovelace = oversized
    ? BigInt(script.script.length / 2) * 8_620n + 100_000_000n
    : 20_000_000n;
  const unsigned = await lucid
    .newTx()
    .pay.ToAddressWithData(parkAddress, undefined, { lovelace }, script)
    .complete();
  const signed = await unsigned.sign.withWallet().complete();
  const signedCbor = signed.toCBOR();
  const publicationMeasurement = measureCompleteSignedTransaction(signedCbor);
  if (!oversized && publicationMeasurement.l1ByteMargin <= 0) {
    throw new Error(
      `${label} reference-script publication is ${publicationMeasurement.completeSignedBytes.toString()} bytes and does not fit the 16,384-byte L1 envelope`,
    );
  }
  const outputs = CML.Transaction.from_cbor_hex(signedCbor).body().outputs();
  let scriptRefOutputIndex = -1;
  for (let index = 0; index < outputs.len(); index += 1) {
    if (outputs.get(index).script_ref() !== undefined) {
      scriptRefOutputIndex = index;
      break;
    }
  }
  if (scriptRefOutputIndex < 0) {
    throw new Error(`${label} publication omitted its script-ref output`);
  }
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const published = await lucid.utxosByOutRef([
    { txHash, outputIndex: scriptRefOutputIndex },
  ]);
  if (published.length !== 1 || published[0]!.scriptRef == null) {
    throw new Error(
      `Expected one live ${label} reference-script UTxO at ${txHash}#${scriptRefOutputIndex.toString()}`,
    );
  }
  return { utxo: published[0]!, publicationMeasurement };
};

// The validators `remove-fraudulent-block` needs, in the same roster order as
// `REFERENCE_SCRIPT_NAMES` in `src/remove-fraudulent-block.ts`. Every one of
// these is also a production reference-script publication target (see
// `midgard-node/src/transactions/reference-scripts.ts`), so sourcing them from
// reference inputs is the deployed shape, not a test-only shortcut.
export type RemovalReferenceScriptName =
  | "stateQueueSpend"
  | "stateQueueMint"
  | "activeOperatorsSpend"
  | "activeOperatorsMint"
  | "retiredOperatorsSpend"
  | "retiredOperatorsMint"
  | "schedulerSpend";

export type RemovalReferenceScriptPublications = Readonly<
  Record<RemovalReferenceScriptName, UTxO>
>;

export type RemovalReferenceScriptMeasurements = Readonly<
  Record<RemovalReferenceScriptName, CompleteSignedTransactionMeasurement>
>;

export const publishRemovalReferenceScripts = async ({
  lucid,
  contracts,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly contracts: MidgardValidators;
}): Promise<{
  readonly published: RemovalReferenceScriptPublications;
  readonly measurements: RemovalReferenceScriptMeasurements;
}> => {
  // Sequential: each publication consumes wallet UTxOs the next one selects
  // from.
  const publish = (name: RemovalReferenceScriptName, script: Script) =>
    publishPlainReferenceScriptUtxo({
      lucid,
      script,
      label: `state-queue removal ${name}`,
    });
  const stateQueueSpend = await publish(
    "stateQueueSpend",
    contracts.stateQueue.spendingScript,
  );
  const stateQueueMint = await publish(
    "stateQueueMint",
    contracts.stateQueue.mintingScript,
  );
  const activeOperatorsSpend = await publish(
    "activeOperatorsSpend",
    contracts.activeOperators.spendingScript,
  );
  const activeOperatorsMint = await publish(
    "activeOperatorsMint",
    contracts.activeOperators.mintingScript,
  );
  const retiredOperatorsSpend = await publish(
    "retiredOperatorsSpend",
    contracts.retiredOperators.spendingScript,
  );
  const retiredOperatorsMint = await publish(
    "retiredOperatorsMint",
    contracts.retiredOperators.mintingScript,
  );
  const schedulerSpend = await publish(
    "schedulerSpend",
    contracts.scheduler.spendingScript,
  );
  return {
    published: {
      stateQueueSpend: stateQueueSpend.utxo,
      stateQueueMint: stateQueueMint.utxo,
      activeOperatorsSpend: activeOperatorsSpend.utxo,
      activeOperatorsMint: activeOperatorsMint.utxo,
      retiredOperatorsSpend: retiredOperatorsSpend.utxo,
      retiredOperatorsMint: retiredOperatorsMint.utxo,
      schedulerSpend: schedulerSpend.utxo,
    },
    measurements: {
      stateQueueSpend: stateQueueSpend.publicationMeasurement,
      stateQueueMint: stateQueueMint.publicationMeasurement,
      activeOperatorsSpend: activeOperatorsSpend.publicationMeasurement,
      activeOperatorsMint: activeOperatorsMint.publicationMeasurement,
      retiredOperatorsSpend: retiredOperatorsSpend.publicationMeasurement,
      retiredOperatorsMint: retiredOperatorsMint.publicationMeasurement,
      schedulerSpend: schedulerSpend.publicationMeasurement,
    },
  };
};

export const buildRemovalDeploymentInfo = (
  contracts: MidgardValidators,
  catalogue: FraudProofCatalogueDeploymentInfo,
  validationDisputePublication?: Awaited<
    ReturnType<typeof publishValidationDisputeReferenceScript>
  >,
  validationItemSemanticReference?: {
    readonly scriptHash: string;
    readonly utxo: UTxO;
  },
  validationItemObserveReference?: {
    readonly scriptHash: string;
    readonly utxo: UTxO;
  },
  validationCanonicalDecodePrepareReference?: {
    readonly scriptHash: string;
    readonly utxo: UTxO;
  },
  removalReferenceScripts?: RemovalReferenceScriptPublications,
  /**
   * #634. Published ValueAndMint semantic-resolver reference scripts, keyed by
   * the ValueAndMint-local semantic index (0..10). Splices in the same shape
   * as the item-semantic / canonical-decode-prepare entries above, so a
   * journey that publishes one resolver adds exactly one entry.
   */
  validationValueAndMintSemanticReferences?: readonly {
    readonly semanticResolverIndex: number;
    readonly scriptHash: string;
    readonly utxo: UTxO;
  }[],
) => {
  const valueAndMintSemanticEntries = Object.fromEntries(
    (validationValueAndMintSemanticReferences ?? []).map(
      ({ semanticResolverIndex, scriptHash, utxo }) => {
        const entryName =
          validationValueAndMintSemanticReferenceScriptDeploymentEntryV1(
            semanticResolverIndex,
          );
        if (entryName === undefined) {
          throw new Error(
            `ValueAndMint semantic resolver ${semanticResolverIndex.toString()} has no reference-script deployment entry`,
          );
        }
        return [
          entryName,
          {
            scriptHash,
            refScriptUTxO: {
              txHash: utxo.txHash,
              outputIndex: utxo.outputIndex,
            },
          },
        ] as const;
      },
    ),
  );
  const deploymentEntry = (
    scriptHash: string,
    script: Script,
    referenceName?: RemovalReferenceScriptName,
  ) => {
    const published =
      referenceName === undefined
        ? undefined
        : removalReferenceScripts?.[referenceName];
    return {
      scriptHash,
      refScriptUTxO:
        published === undefined
          ? null
          : {
              txHash: published.txHash,
              outputIndex: published.outputIndex,
            },
      contract: {
        type: script.type,
        cborHex: script.script,
      },
    };
  };
  return deploymentManifest(
    {
      ...valueAndMintSemanticEntries,
      ...(validationItemSemanticReference === undefined
        ? {}
        : {
            [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
              scriptHash: validationItemSemanticReference.scriptHash,
              refScriptUTxO: {
                txHash: validationItemSemanticReference.utxo.txHash,
                outputIndex: validationItemSemanticReference.utxo.outputIndex,
              },
            },
          }),
      ...(validationItemObserveReference === undefined
        ? {}
        : {
            [VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
              scriptHash: validationItemObserveReference.scriptHash,
              refScriptUTxO: {
                txHash: validationItemObserveReference.utxo.txHash,
                outputIndex: validationItemObserveReference.utxo.outputIndex,
              },
            },
          }),
      ...(validationCanonicalDecodePrepareReference === undefined
        ? {}
        : {
            [VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]:
              {
                scriptHash:
                  validationCanonicalDecodePrepareReference.scriptHash,
                refScriptUTxO: {
                  txHash: validationCanonicalDecodePrepareReference.utxo.txHash,
                  outputIndex:
                    validationCanonicalDecodePrepareReference.utxo.outputIndex,
                },
              },
          }),
      hubOracleMint: { scriptHash: contracts.hubOracle.policyId },
      fraudProofCatalogueMint: {
        scriptHash: contracts.fraudProofCatalogue.policyId,
        fraudProofCatalogue: catalogue,
      },
      fraudProofCatalogueSpend: {
        scriptHash: contracts.fraudProofCatalogue.spendingScriptHash,
      },
      fraudProofMint: { scriptHash: contracts.fraudProof.policyId },
      fraudProofSpend: {
        scriptHash: contracts.fraudProof.spendingScriptHash,
      },
      fraudProofDoubleSpend: {
        scriptHash: contracts.fraudProofs.doubleSpend.spendingScriptHash,
      },
      fraudProofNonExistentInput: {
        scriptHash: contracts.fraudProofs.nonExistentInput.spendingScriptHash,
      },
      fraudProofInvalidRange: {
        scriptHash: contracts.fraudProofs.invalidRange.spendingScriptHash,
      },
      fraudProofTransitionTrace: {
        scriptHash: contracts.fraudProofs.transitionTrace.spendingScriptHash,
      },
      fraudProofZeroInput: {
        scriptHash: contracts.fraudProofs.zeroInput.spendingScriptHash,
      },
      fraudProofDaHashPreimage: {
        scriptHash: contracts.fraudProofs.daHashPreimage.spendingScriptHash,
      },
      fraudProofNoReferenceInput: {
        scriptHash: contracts.fraudProofs.noReferenceInput.spendingScriptHash,
      },
      fraudProofReferenceInputNoIdx: {
        scriptHash:
          contracts.fraudProofs.referenceInputNoIdx.spendingScriptHash,
      },
      fraudProofInvalidSignature: {
        scriptHash: contracts.fraudProofs.invalidSignature.spendingScriptHash,
      },
      fraudProofNonExistentInputNoIndex: {
        scriptHash:
          contracts.fraudProofs.nonExistentInputNoIndex.spendingScriptHash,
        contract: {
          type: contracts.fraudProofs.nonExistentInputNoIndex.spendingScript
            .type,
          cborHex:
            contracts.fraudProofs.nonExistentInputNoIndex.spendingScript.script,
        },
      },
      validationTraceDispute: {
        scriptHash:
          contracts.fraudProofs.validationTraceDispute.spendingScriptHash,
        refScriptUTxO:
          validationDisputePublication === undefined
            ? null
            : {
                txHash: validationDisputePublication.utxo.txHash,
                outputIndex: validationDisputePublication.utxo.outputIndex,
              },
        contract: {
          type: contracts.fraudProofs.validationTraceDispute.spendingScript
            .type,
          cborHex:
            contracts.fraudProofs.validationTraceDispute.spendingScript.script,
        },
      },
      cekProgramMaterialSpend: {
        scriptHash: contracts.cekProgramMaterial.spendingScriptHash,
        contract: {
          type: contracts.cekProgramMaterial.spendingScript.type,
          cborHex: contracts.cekProgramMaterial.spendingScript.script,
        },
      },
      stateQueueMint: deploymentEntry(
        contracts.stateQueue.policyId,
        contracts.stateQueue.mintingScript,
        "stateQueueMint",
      ),
      stateQueueSpend: deploymentEntry(
        contracts.stateQueue.spendingScriptHash,
        contracts.stateQueue.spendingScript,
        "stateQueueSpend",
      ),
      retiredOperatorsMint: deploymentEntry(
        contracts.retiredOperators.policyId,
        contracts.retiredOperators.mintingScript,
        "retiredOperatorsMint",
      ),
      retiredOperatorsSpend: deploymentEntry(
        contracts.retiredOperators.spendingScriptHash,
        contracts.retiredOperators.spendingScript,
        "retiredOperatorsSpend",
      ),
      registeredOperatorsMint: {
        scriptHash: contracts.registeredOperators.policyId,
      },
      registeredOperatorsSpend: deploymentEntry(
        contracts.registeredOperators.spendingScriptHash,
        contracts.registeredOperators.spendingScript,
      ),
      activeOperatorsMint: deploymentEntry(
        contracts.activeOperators.policyId,
        contracts.activeOperators.mintingScript,
        "activeOperatorsMint",
      ),
      activeOperatorsSpend: deploymentEntry(
        contracts.activeOperators.spendingScriptHash,
        contracts.activeOperators.spendingScript,
        "activeOperatorsSpend",
      ),
      schedulerMint: { scriptHash: contracts.scheduler.policyId },
      schedulerSpend: deploymentEntry(
        contracts.scheduler.spendingScriptHash,
        contracts.scheduler.spendingScript,
        "schedulerSpend",
      ),
      settlementMint: { scriptHash: contracts.settlement.policyId },
    },
    validationDisputePublication?.authPolicyDeploymentInfo,
  );
};

/**
 * VM-DEFECT-2 dispute-level regression
 * (`docs/exec-plans/evidence/vm-defect-decision-memo.md` §2).
 *
 * The shipped defect made `rejected_successor_is_exact` demand that the
 * rejecting terminal *write* `ledger_delta_root = frontier_commitment(0, [])`
 * while `immutable_context_matches` pins that same field pre == post on every
 * transition. The two are jointly unsatisfiable from any pre-state whose
 * claimed delta is non-empty -- which is every adversarially interesting
 * pre-state, because the challenger is the party who must exhibit a
 * one-step-valid rejection successor to win
 * (`validation-resolver-v1.ak` -> `challenger_wins_with_valid_successor`) and
 * a real invalid transaction always claims a non-empty delta.
 *
 * It shipped because no test ever drove a challenger to an actual win: every
 * rejection fixture pinned the claimed delta to the empty commitment, the one
 * pre-state in which the contradiction vanishes. These tests close that gap
 * end to end on the emulator, against the compiled validators, in both
 * directions (GOAL_SPEC §3 invariant 9 -- soundness is symmetric).
 */
export const runForcedValidationDisputeScenario = async (
  buildFixture: (input: {
    readonly operatorVkey: string;
    readonly now: number;
  }) => Promise<ForcedValidationDisputeFixture>,
  {
    stopAfter,
    onRemovalReferenceScriptPublicationAttempt,
  }: {
    readonly stopAfter?:
      | "prepare-resolution"
      | "prepare-selected"
      | "semantic-resolution";
    readonly onRemovalReferenceScriptPublicationAttempt?: () => void;
  } = {},
) => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const operator = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const challenger = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const feeUtxoCount = 12;
  const feeUtxoLovelace = 100_000_000n;
  const emulator = new Emulator(
    [
      {
        ...operator,
        assets: {
          lovelace:
            operator.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...operator,
        assets: { lovelace: feeUtxoLovelace },
      })),
      {
        ...challenger,
        assets: {
          lovelace:
            challenger.assets.lovelace - BigInt(feeUtxoCount) * feeUtxoLovelace,
        },
      },
      ...Array.from({ length: feeUtxoCount }, () => ({
        ...challenger,
        assets: { lovelace: feeUtxoLovelace },
      })),
    ],
    EMULATOR_PROTOCOL_PARAMETERS,
  );
  const operatorLucid = await Lucid(emulator, "Custom");
  const challengerLucid = await Lucid(emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  challengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const operatorSigner = resolveProverSigner({
    network,
    walletSeedPhrase: operator.seedPhrase,
  });
  const challengerSigner = resolveProverSigner({
    network,
    walletSeedPhrase: challenger.seedPhrase,
  });
  const validityRange = () => validationDisputeValidityRange(emulator.now());

  await registerPhasMembershipRewardAccount(operatorLucid, realBlueprint);
  const nonceUtxo = (await operatorLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected operator wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    {
      realValidationTraceDispute: true,
      alwaysFraudProofCatalogue: true,
    },
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs);
  const operatorPaymentCredential = getAddressDetails(
    await operatorLucid.wallet().address(),
  ).paymentCredential;
  if (
    operatorPaymentCredential === undefined ||
    operatorPaymentCredential.type !== "Key"
  ) {
    throw new Error("Expected operator wallet to expose a payment key hash");
  }
  const headerStartTime =
    alignUnixTimeToEmulatorSlotBoundary(
      operatorLucid,
      emulator.now() + 120_000,
    ) - 1;
  const fixture = await buildFixture({
    operatorVkey: operatorPaymentCredential.hash,
    now: headerStartTime,
  });
  const setup = await runEmulatorLifecycleStage("setup", () =>
    submitSetupTx({
      lucid: operatorLucid,
      contracts,
      nonceUtxo,
      catalogue,
      header: fixture.header,
    }),
  );
  const publicationSlotConfig = operatorLucid.config().slotConfig;
  if (publicationSlotConfig === undefined) {
    throw new Error(
      "Expected reference-script publisher Lucid to expose its Custom slot config",
    );
  }
  const setupProtocolParameters = emulator.protocolParameters;
  emulator.protocolParameters = {
    ...setupProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const referenceScriptPublisherLucid = await Lucid(emulator, "Custom", {
    slotConfig: publicationSlotConfig,
  });
  referenceScriptPublisherLucid.selectWallet.fromSeed(operator.seedPhrase);
  const validationDisputePublication = await runEmulatorLifecycleStage(
    "reference-script.publish-authenticated",
    async () => {
      try {
        return await publishValidationDisputeReferenceScript({
          lucid: referenceScriptPublisherLucid,
          contracts,
          now: emulator.now(),
        });
      } finally {
        emulator.protocolParameters = setupProtocolParameters;
      }
    },
  );
  const deploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    validationDisputePublication,
  );
  const initResult = await runEmulatorLifecycleStage("init", () =>
    submitInit({
      lucid: challengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      fraudCategory: "validationTraceDispute",
      fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
      awaitConfirmation: true,
    }),
  );
  const functionalProtocolParameters = emulator.protocolParameters;
  const functionalSlotConfig = challengerLucid.config().slotConfig;
  if (functionalSlotConfig === undefined) {
    throw new Error(
      "Expected functional emulator Lucid to expose its Custom slot config",
    );
  }
  emulator.protocolParameters = {
    ...functionalProtocolParameters,
    maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  };
  const targetOperatorLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  const targetChallengerLucid = await Lucid(emulator, "Custom", {
    slotConfig: functionalSlotConfig,
  });
  targetOperatorLucid.selectWallet.fromSeed(operator.seedPhrase);
  targetChallengerLucid.selectWallet.fromSeed(challenger.seedPhrase);
  const firstStepUtxo = await expectSingleUtxoWithUnit(
    targetChallengerLucid,
    initResult.firstStepAddress,
    initResult.computationThreadUnit,
  );
  const openResult = await runEmulatorLifecycleStage("open", () =>
    submitValidationDisputeOpen({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: outRefLabel(firstStepUtxo),
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      claim: fixture.claim,
      challengerDescriptor: fixture.challengerDescriptor,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  const sourceResult = await runEmulatorLifecycleStage("source", () =>
    submitValidationDisputeVerifySource({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: openResult.nextThreadOutRef,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );

  let threadOutRef = sourceResult.nextThreadOutRef;
  for (const move of fixture.evidence.moves) {
    const revealResult = await runEmulatorLifecycleStage(
      `reveal.${move.role}`,
      () =>
        submitValidationDisputeReveal({
          lucid:
            move.role === "operator"
              ? targetOperatorLucid
              : targetChallengerLucid,
          blueprint: realBlueprint,
          deploymentInfo,
          network,
          signer: move.role === "operator" ? operatorSigner : challengerSigner,
          threadOutRef,
          role: move.role,
          proof: move.proof,
          validityRange: validityRange(),
          awaitConfirmation: true,
        }),
    );
    threadOutRef = revealResult.nextThreadOutRef;
  }

  const resolutionResult = await runEmulatorLifecycleStage(
    "enter-resolution",
    () =>
      submitValidationDisputeEnterResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  const { lowIndex, highIndex } = fixture.evidence.finalDispute;
  const prepareResult = await runEmulatorLifecycleStage(
    "prepare-resolution",
    () =>
      submitValidationDisputePrepareResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: resolutionResult.nextThreadOutRef,
        preState: validationMachineStateDataFromCore(
          fixture.operatorTrace.states[lowIndex]!,
        ),
        operatorPost: validationTraceProofDataFromCore(
          fixture.operatorTrace.tree.proofs[highIndex]!,
        ),
        challengerPost: validationTraceProofDataFromCore(
          fixture.challengerTrace.tree.proofs[highIndex]!,
        ),
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  if (stopAfter === "prepare-resolution") {
    return { fixture, contracts, initResult, lowIndex, highIndex };
  }
  const selectedResult = await runEmulatorLifecycleStage(
    "prepare-selected",
    () =>
      submitValidationDisputePrepareSelected({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: prepareResult.nextThreadOutRef,
        oneStepArgument: fixture.evidence.oneStepArgument,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
  );
  if (stopAfter === "prepare-selected") {
    return { fixture, contracts, initResult, lowIndex, highIndex };
  }
  // #634. The ValueAndMint semantic resolvers now hold the same
  // reference-script deployment role the CEK ones do. Publish exactly the
  // resolver this fixture's one-step argument routes to, and only when its
  // applied body cannot ride inside the literal 16,384-byte L1 proof envelope
  // — eight of the eleven cannot, so without this the resolution transaction
  // overflows (#634 measured 21,576 bytes for the output-descriptor journey).
  // The publication itself is necessarily oversized, exactly as the CEK ones
  // are, so it runs under the emulator's raised deployment-time maxTxSize on
  // its own publisher Lucid; the consuming resolution stays on
  // `targetChallengerLucid`, which is pinned to the real L1 limit.
  const stagedResolverIndex = fixture.evidence.oneStepArgument.resolverIndex;
  const stagedSemanticIndex =
    fixture.evidence.oneStepArgument.semanticResolverIndex;
  // Resolved through the very helper the submit path uses, so the published
  // body is byte-identical to the one the resolution will hash-check.
  const valueAndMintSemanticContract =
    stagedResolverIndex === VALIDATION_VALUE_AND_MINT_RESOLVER_INDEX_V1
      ? (
          await resolveValidationTraceDisputeDeploymentContracts({
            blueprint: realBlueprint,
            deploymentInfo,
            network,
          })
        ).contracts.validationTraceDispute.semanticResolvers[
          validationSemanticResolverGlobalIndexV1(
            stagedResolverIndex,
            stagedSemanticIndex,
          )
        ]
      : undefined;
  const valueAndMintSemanticEntryName =
    valueAndMintSemanticContract === undefined
      ? undefined
      : validationValueAndMintSemanticReferenceScriptDeploymentEntryV1(
          stagedSemanticIndex,
        );
  const valueAndMintSemanticPublication =
    valueAndMintSemanticContract !== undefined &&
    valueAndMintSemanticEntryName !== undefined &&
    valueAndMintSemanticContract.spendingScript.script.length / 2 >
      PROTOCOL_PARAMETERS_DEFAULT.maxTxSize
      ? await runEmulatorLifecycleStage(
          `reference-script.publish.${valueAndMintSemanticEntryName}`,
          async () => {
            const prePublicationProtocolParameters =
              emulator.protocolParameters;
            emulator.protocolParameters = functionalProtocolParameters;
            try {
              const oversizedPublisherLucid = await Lucid(emulator, "Custom", {
                slotConfig: functionalSlotConfig,
              });
              oversizedPublisherLucid.selectWallet.fromSeed(
                operator.seedPhrase,
              );
              return await publishPlainReferenceScriptUtxo({
                lucid: oversizedPublisherLucid,
                script: valueAndMintSemanticContract.spendingScript,
                label: valueAndMintSemanticEntryName,
                oversized: true,
              });
            } finally {
              emulator.protocolParameters = prePublicationProtocolParameters;
            }
          },
        )
      : undefined;
  const semanticDeploymentInfo =
    valueAndMintSemanticPublication === undefined
      ? deploymentInfo
      : buildRemovalDeploymentInfo(
          contracts,
          catalogue,
          validationDisputePublication,
          undefined,
          undefined,
          undefined,
          undefined,
          [
            {
              semanticResolverIndex: stagedSemanticIndex,
              scriptHash: valueAndMintSemanticContract!.spendingScriptHash,
              utxo: valueAndMintSemanticPublication.utxo,
            },
          ],
        );
  const semanticCapture = await captureEmulatorSubmission(emulator, () =>
    runEmulatorLifecycleStage("semantic-resolution", () =>
      submitValidationDisputeSemanticResolution({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo: semanticDeploymentInfo,
        network,
        signer: challengerSigner,
        threadOutRef: selectedResult.nextThreadOutRef,
        oneStepArgument: fixture.evidence.oneStepArgument,
        validityRange: validityRange(),
        awaitConfirmation: true,
      }),
    ),
  );
  const semanticResult = semanticCapture.result;
  if (stopAfter === "semantic-resolution") {
    return {
      fixture,
      contracts,
      initResult,
      lowIndex,
      highIndex,
      semanticResult,
      semanticMeasurement: semanticCapture.measurement,
      valueAndMintSemanticReferencePublication:
        valueAndMintSemanticPublication === undefined ||
        valueAndMintSemanticContract === undefined ||
        valueAndMintSemanticEntryName === undefined
          ? undefined
          : {
              entryName: valueAndMintSemanticEntryName,
              appliedResolverBytes:
                valueAndMintSemanticContract.spendingScript.script.length / 2,
              appliedResolverHash:
                valueAndMintSemanticContract.spendingScriptHash,
              utxo: valueAndMintSemanticPublication.utxo,
              publicationMeasurement:
                valueAndMintSemanticPublication.publicationMeasurement,
            },
    };
  }
  const awardResult = await runEmulatorLifecycleStage("award", () =>
    submitValidationDisputeAward({
      lucid: targetChallengerLucid,
      blueprint: realBlueprint,
      deploymentInfo,
      network,
      signer: challengerSigner,
      threadOutRef: semanticResult.nextThreadOutRef,
      validityRange: validityRange(),
      awaitConfirmation: true,
    }),
  );
  // Block removal needs the state-queue, operator-directory and scheduler
  // validators. Publishing them as reference-script UTxOs is what the deployed
  // node does; `publishPlainReferenceScriptUtxo` refuses any publication that
  // does not itself fit the literal 16,384-byte L1 envelope, so this also
  // proves each of these validators is publishable on L1. Defer the seven
  // submissions until the route has actually reached removal so validation-only
  // and negative scenarios do not mutate the emulator first.
  const removalReferenceScriptPublications = await runEmulatorLifecycleStage(
    "reference-script.publish-removal",
    async () => {
      onRemovalReferenceScriptPublicationAttempt?.();
      const prePublicationProtocolParameters = emulator.protocolParameters;
      emulator.protocolParameters = {
        ...prePublicationProtocolParameters,
        maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      };
      try {
        return await publishRemovalReferenceScripts({
          lucid: referenceScriptPublisherLucid,
          contracts,
        });
      } finally {
        emulator.protocolParameters = prePublicationProtocolParameters;
      }
    },
  );
  const removalDeploymentInfo = buildRemovalDeploymentInfo(
    contracts,
    catalogue,
    validationDisputePublication,
    undefined,
    undefined,
    undefined,
    removalReferenceScriptPublications.published,
  );
  const removeNow = BigInt(emulator.now());
  // Block removal runs under the same literal 16,384-byte L1 envelope as every
  // dispute transaction above: `targetChallengerLucid` was constructed with
  // `maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize`, and every validator the
  // correction needs is sourced from a published reference-script UTxO instead
  // of being attached inline. Attaching them instead costs 35,634 bytes of
  // witness set and puts the correction 2.3x over the limit.
  const removalCapture = await captureEmulatorSubmission(emulator, () =>
    runEmulatorLifecycleStage("remove-fraudulent-block", () =>
      submitRemoveFraudulentBlock({
        lucid: targetChallengerLucid,
        blueprint: realBlueprint,
        deploymentInfo: removalDeploymentInfo,
        network,
        signer: challengerSigner,
        fraudCategory: "validationTraceDispute",
        fraudulentHeaderHash: setup.headerHash,
        awaitConfirmation: true,
        requireReferenceScripts: true,
        validFrom: removeNow > 120_000n ? removeNow - 120_000n : 0n,
        validTo: removeNow + 300_000n,
      }),
    ),
  );
  const removal = removalCapture.result;
  if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
    const removalScriptNames = midgardScriptHashNames(contracts);
    removalCapture.transactionCbors.forEach((cbor, index) => {
      attributeTransactionBytes(
        `remove-fraudulent-block tx[${index.toString()}]`,
        cbor,
        removalScriptNames,
      );
    });
  }
  return {
    fixture,
    contracts,
    initResult,
    lowIndex,
    highIndex,
    awardResult,
    removal,
    removalMeasurements: removalCapture.measurements,
    removalReferenceScriptMeasurements:
      removalReferenceScriptPublications.measurements,
    challengerLucid: targetChallengerLucid,
    setup,
  };
};

/**
 * GOAL_SPEC.md 3.3 proof-fit thresholds, shared by every emulator suite that
 * measures a complete correction path.
 *
 * 1. Byte fit: `l1ByteMargin` is computed against the real
 *    `PROTOCOL_PARAMETERS_DEFAULT.maxTxSize` (16,384), not the emulator's
 *    relaxed 65,536 ceiling, so a non-negative margin is exactly the 3.3
 *    item-1 check.
 * 2. Execution fit: memory and CPU are at or below the deployment's measured
 *    limits with at least a 20% reserve. A path that fits the raw limit but
 *    not the reserve is a FAILING result, not a smaller margin.
 */
export const EXECUTION_RESERVE_FRACTION = 20n;

export const expectProofFitV1 = ({
  stage,
  measurement,
  maxTxExMem,
  maxTxExSteps,
}: {
  readonly stage: string;
  readonly measurement: CompleteSignedTransactionMeasurement;
  readonly maxTxExMem: bigint;
  readonly maxTxExSteps: bigint;
}): void => {
  // 3.3 item 1 - byte fit against the real L1 envelope.
  expect(
    measurement.l1ByteMargin,
    `${stage} exceeds the 16,384-byte L1 envelope`,
  ).toBeGreaterThanOrEqual(0);
  // 3.3 item 2 - execution fit with a 20% reserve.
  const memoryCeiling =
    (maxTxExMem * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  const stepCeiling =
    (maxTxExSteps * (100n - EXECUTION_RESERVE_FRACTION)) / 100n;
  expect(
    measurement.executionMemory <= memoryCeiling,
    `${stage} execution memory ${measurement.executionMemory.toString()} exceeds the 20%-reserve ceiling ${memoryCeiling.toString()}`,
  ).toBe(true);
  expect(
    measurement.executionSteps <= stepCeiling,
    `${stage} execution steps ${measurement.executionSteps.toString()} exceeds the 20%-reserve ceiling ${stepCeiling.toString()}`,
  ).toBe(true);
};

/**
 * Debug-only proof-fit dump, gated on `MIDGARD_PRINT_PROOF_FIT=1`. `headline`
 * is the caller's own label text, so each suite prints exactly the line it
 * printed before; `extra` is merged into the same object the stage map
 * produces, and `includeReferenceInputs` adds the reference-input count the
 * published-chunk carriage suite reports.
 */
export const printProofFitV1 = ({
  headline,
  stages,
  extra,
  includeReferenceInputs = false,
}: {
  readonly headline: string;
  readonly stages: Record<string, CompleteSignedTransactionMeasurement>;
  readonly extra?: Record<string, unknown>;
  readonly includeReferenceInputs?: boolean;
}): void => {
  if (process.env["MIDGARD_PRINT_PROOF_FIT"] !== "1") {
    return;
  }
  const stageEntries = Object.fromEntries(
    Object.entries(stages).map(([stage, measurement]) => [
      stage,
      {
        bytes: measurement.completeSignedBytes,
        l1ByteMargin: measurement.l1ByteMargin,
        memory: measurement.executionMemory.toString(),
        steps: measurement.executionSteps.toString(),
        ...(includeReferenceInputs
          ? { referenceInputs: measurement.referenceInputCount }
          : {}),
      },
    ]),
  );
  console.log(
    `${headline}: ${JSON.stringify(
      extra === undefined ? stageEntries : { ...stageEntries, ...extra },
      null,
      2,
    )}`,
  );
};

/**
 * Two funded emulator wallets and their Lucid instances: the funder that
 * publishes the fraudulent block and the prover that drives the correction
 * path.
 */
export const newEmulatorParty = async () => {
  const funder = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  proverLucid.selectWallet.fromSeed(prover.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });
  return { emulator, funderLucid, proverLucid, proverSigner };
};

export const funderPaymentKeyHash = async (
  funderLucid: Awaited<ReturnType<typeof Lucid>>,
): Promise<string> => {
  const credential = getAddressDetails(
    await funderLucid.wallet().address(),
  ).paymentCredential;
  if (credential === undefined || credential.type !== "Key") {
    throw new Error("Expected funder wallet to expose a payment key hash");
  }
  return credential.hash;
};

const encodeCatalogueMembershipRedeemer = ({
  root,
  categoryId: id,
  categoryScriptHash,
  membershipProofCbor,
}: {
  readonly root: string;
  readonly categoryId: string;
  readonly categoryScriptHash: string;
  readonly membershipProofCbor: string;
}): string =>
  encodePhasMembershipProofRedeemer({
    root,
    keyCbor: Data.to(id, categoryIdSchema as unknown as LucidDataSchema),
    valueCbor: Data.to(
      categoryScriptHash,
      ScriptHashSchema as unknown as LucidDataSchema,
    ),
    membershipProofCbor,
  });

/**
 * Init transaction for a fabricated family (Q39/Q40): mints the computation
 * thread under the family's extra catalogue category and locks it at step-01.
 *
 * This mirrors the generic tail of `src/submit-init.ts` exactly — catalogue,
 * hub-oracle and fraudulent-block reference inputs, the PHAS membership
 * withdrawal carrying the category proof, and the `Init` mint redeemer — but
 * lives here because the production `submitInit` category union is parent-owned
 * and does not register these families yet.
 */
export const submitFabricatedFamilyInitV1 = async ({
  lucid,
  realBlueprint,
  contracts,
  catalogueRoot,
  category,
  family,
  familyLabel,
  signer,
  fraudulentBlockOutRef,
}: {
  readonly lucid: Awaited<ReturnType<typeof Lucid>>;
  readonly realBlueprint: Blueprint;
  readonly contracts: Pick<
    MidgardValidators,
    "fraudProofCatalogue" | "hubOracle"
  >;
  readonly catalogueRoot: string;
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly family:
    | FabricatedDepositContractsV1
    | FabricatedWithdrawalContractsV1;
  readonly familyLabel: string;
  readonly signer: ReturnType<typeof resolveProverSigner>;
  readonly fraudulentBlockOutRef: string;
}): Promise<{
  readonly txHash: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly threadOutRef: string;
}> => {
  // The deployed step-01 must be the very script the catalogue category
  // registers; a divergence would mint a thread the family cannot spend.
  expect(category.categoryId).toBe(family.categoryId);
  expect(category.scriptHash).toBe(family.steps[0].spendingScriptHash);

  const [catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo] = await Promise.all(
    [
      requireSingletonUtxo({
        lucid,
        address: contracts.fraudProofCatalogue.spendingScriptAddress,
        unit: toUnit(
          contracts.fraudProofCatalogue.policyId,
          FRAUD_PROOF_CATALOGUE_ASSET_NAME,
        ),
        label: `${familyLabel} init fraud-proof catalogue`,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOracle.policyId),
        ),
        unit: toUnit(contracts.hubOracle.policyId, HUB_ORACLE_ASSET_NAME),
        label: `${familyLabel} init hub oracle`,
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          fraudulentBlockOutRef,
          `${familyLabel} fraudulent block out-ref`,
        ),
        label: `${familyLabel} fraudulent block UTxO`,
      }),
    ],
  );
  const fraudulentHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: family.stateQueuePolicyId,
    fraudulentBlockUtxo,
  });
  const computationThreadAssetName = `${family.categoryId}${fraudulentHeaderHash}`;
  const computationThreadUnit = toUnit(
    family.computationThread.policyId,
    computationThreadAssetName,
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(realBlueprint, "phas.membership.withdraw"),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const firstStepAddress = family.steps[0].spendingScriptAddress;
  const firstStepDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const firstStepOutputMatches = computationThreadOutputPredicate({
    address: firstStepAddress,
    datum: firstStepDatum,
    unit: computationThreadUnit,
  });
  let firstStepOutputIndex: bigint | undefined;
  const computationThreadMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      family.computationThread.policyId,
      `${familyLabel} init computation-thread mint`,
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      firstStepOutputMatches,
      `${familyLabel} init first step`,
    );
    firstStepOutputIndex = outputIndex;
    return Data.to(
      {
        Init: {
          first_step_output_index: outputIndex,
          fraud_category_id: category.categoryId,
          fraud_category: category.scriptHash,
          fraud_category_membership_proof: Data.from(
            category.membershipProofCbor,
            Proof,
          ),
          fraud_proof_catalogue_ref_input_index: requireReferenceInputIndex(
            ctx,
            catalogueUtxo,
            `${familyLabel} init fraud-proof catalogue`,
          ),
          inclusion_proof_script_redeemer_index: requireWithdrawalRedeemerIndex(
            ctx,
            phasRewardAddress,
            `${familyLabel} init PHAS membership`,
          ),
          hub_oracle_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${familyLabel} init hub oracle`,
          ),
          fraudulent_block_ref_input_index: requireReferenceInputIndex(
            ctx,
            fraudulentBlockUtxo,
            `${familyLabel} init fraudulent block`,
          ),
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const unsigned = await lucid
    .newTx()
    .readFrom([catalogueUtxo, hubOracleUtxo, fraudulentBlockUtxo])
    .withdraw(
      phasRewardAddress,
      0n,
      encodeCatalogueMembershipRedeemer({
        root: catalogueRoot,
        categoryId: category.categoryId,
        categoryScriptHash: category.scriptHash,
        membershipProofCbor: category.membershipProofCbor,
      }),
    )
    .mintAssets({ [computationThreadUnit]: 1n }, computationThreadMintRedeemer)
    .pay.ToContract(
      firstStepAddress,
      { kind: "inline", value: firstStepDatum },
      { [computationThreadUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(family.computationThread.mintingScript)
    .attach.WithdrawalValidator(phasMembershipScript)
    .complete({ localUPLCEval: true });
  if (firstStepOutputIndex === undefined) {
    throw new Error(
      `BuildTxWithRedeemer did not resolve ${familyLabel} init output index.`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);

  return {
    txHash,
    fraudulentHeaderHash,
    computationThreadAssetName,
    computationThreadUnit,
    firstStepAddress,
    threadOutRef: `${txHash}#${firstStepOutputIndex.toString()}`,
  };
};

export type FaultProofEmulatorHarnessV1 = {
  readonly realBlueprint: Blueprint;
  readonly alwaysBlueprint: Blueprint;
  readonly emulator: Emulator;
  readonly funderLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverLucid: Awaited<ReturnType<typeof Lucid>>;
  readonly proverSigner: ReturnType<typeof resolveProverSigner>;
  readonly nonceUtxo: UTxO;
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  >;
  readonly catalogue: Awaited<ReturnType<typeof buildCatalogueDeploymentInfo>>;
};

/**
 * The journey preamble every fault-proof emulator suite opens with, in the
 * exact order the suites performed it: read both blueprints, stand up the
 * funder/prover party, register the PHAS membership reward account (then any
 * family-specific reward accounts the caller registers, in the caller's own
 * order), take the funder's first UTxO as the parameterizing nonce, build the
 * minimal contract set for the family under test, then derive the catalogue
 * deployment info.
 *
 * Reference-script publication is deliberately NOT part of this helper: the
 * suites publish at different points in the timeline, and the emulator clock
 * they sample afterwards is what their measured byte counts are anchored to.
 */
export const makeFaultProofEmulatorHarnessV1 = async ({
  contractOptions = {},
  accounts,
  emulatorTimeMs,
  registerAdditionalRewardAccounts,
}: {
  readonly contractOptions?: Parameters<
    typeof buildMinimalFaultProofContracts
  >[3];
  readonly accounts?: {
    readonly funder: EmulatorAccount;
    readonly prover: EmulatorAccount;
  };
  readonly emulatorTimeMs?: number;
  readonly registerAdditionalRewardAccounts?: (
    funderLucid: Awaited<ReturnType<typeof Lucid>>,
    realBlueprint: Blueprint,
  ) => Promise<void>;
} = {}): Promise<FaultProofEmulatorHarnessV1> => {
  const realBlueprint = readBlueprint(realBlueprintPath);
  const alwaysBlueprint = readBlueprint(alwaysSucceedsBlueprintPath);
  const funder =
    accounts?.funder ?? generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover =
    accounts?.prover ?? generateEmulatorAccount({ lovelace: 20_000_000_000n });
  const emulator = new Emulator([funder, prover], EMULATOR_PROTOCOL_PARAMETERS);
  if (emulatorTimeMs !== undefined) {
    emulator.time = emulatorTimeMs;
  }
  const funderLucid = await Lucid(emulator, "Custom");
  const proverLucid = await Lucid(emulator, "Custom");
  funderLucid.selectWallet.fromSeed(funder.seedPhrase);
  proverLucid.selectWallet.fromSeed(prover.seedPhrase);
  const proverSigner = resolveProverSigner({
    network,
    walletSeedPhrase: prover.seedPhrase,
  });

  await registerPhasMembershipRewardAccount(funderLucid, realBlueprint);
  if (registerAdditionalRewardAccounts !== undefined) {
    await registerAdditionalRewardAccounts(funderLucid, realBlueprint);
  }
  const nonceUtxo = (await funderLucid.wallet().getUtxos())[0];
  if (nonceUtxo === undefined) {
    throw new Error("Expected funder wallet to expose a nonce UTxO");
  }
  const contracts = await buildMinimalFaultProofContracts(
    realBlueprint,
    alwaysBlueprint,
    nonceUtxo,
    contractOptions,
  );
  const catalogue = await buildCatalogueDeploymentInfo(contracts.fraudProofs, {
    ...(contracts.fabricatedDeposit === undefined
      ? {}
      : {
          fabricatedDeposit: {
            categoryId: contracts.fabricatedDeposit.categoryId,
            scriptHash: contracts.fabricatedDeposit.steps[0].spendingScriptHash,
          },
        }),
    ...(contracts.fabricatedWithdrawal === undefined
      ? {}
      : {
          fabricatedWithdrawal: {
            categoryId: contracts.fabricatedWithdrawal.categoryId,
            scriptHash:
              contracts.fabricatedWithdrawal.steps[0].spendingScriptHash,
          },
        }),
  });
  return {
    realBlueprint,
    alwaysBlueprint,
    emulator,
    funderLucid,
    proverLucid,
    proverSigner,
    nonceUtxo,
    contracts,
    catalogue,
  };
};
