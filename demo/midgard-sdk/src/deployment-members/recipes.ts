import type {
  DEPLOYMENT_MEMBERS,
  DeploymentContract,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { canonicalDecodabilityRecipes } from "./canonical-decodability.js";
import { committedFieldShapeRecipes } from "./committed-field-shape.js";
import { daHashPreimageRecipes } from "./da-hash-preimage.js";
import { distinctAssetAccumulationLimitRecipes } from "./distinct-asset-accumulation-limit.js";
import { doubleSpendRecipes } from "./double-spend.js";
import { doubleWithdrawRecipes } from "./double-withdraw.js";
import { executionNativeScriptInvalidRecipes } from "./execution-native-script-invalid.js";
import { executionSourceScriptDecodingRecipes } from "./execution-source-script-decoding.js";
import { fabricatedDepositRecipes } from "./fabricated-deposit.js";
import { fabricatedWithdrawalRecipes } from "./fabricated-withdrawal.js";
import { fieldItemWidthIllegalRecipes } from "./field-item-width-illegal.js";
import { fieldPreimageLengthMismatchRecipes } from "./field-preimage-length-mismatch.js";
import { inputSetUniquenessRecipes } from "./input-set-uniqueness.js";
import { invalidRangeRecipes } from "./invalid-range.js";
import { invalidSignatureRecipes } from "./invalid-signature.js";
import { l2TxMistagRecipes } from "./l2-tx-mistag.js";
import { minAdaRecipes } from "./min-ada.js";
import { minFeeRecipes } from "./min-fee.js";
import { mintAuthorizationRecipes } from "./mint-authorization.js";
import { mintDeclaredAssetLimitRecipes } from "./mint-declared-asset-limit.js";
import { mintItemNonCanonicalRecipes } from "./mint-item-non-canonical.js";
import { missingRedeemerRecipes } from "./missing-redeemer.js";
import { missingScriptSourceRecipes } from "./missing-script-source.js";
import { missingSignatureRecipes } from "./missing-signature.js";
import { nativeScriptDecodingRecipes } from "./native-script-decoding.js";
import { nativeScriptInvalidRecipes } from "./native-script-invalid.js";
import { networkIdRecipes } from "./network-id.js";
import { noReferenceInputRecipes } from "./no-reference-input.js";
import { nonExistentInputRecipes } from "./non-existent-input.js";
import { nonExistentInputNoIndexRecipes } from "./non-existent-input-no-index.js";
import { observerOrderInvalidRecipes } from "./observer-order-invalid.js";
import { observersForbiddenOnUntaggedNetworkRecipes } from "./observers-forbidden-on-untagged-network.js";
import { outputReferenceScriptDecodingRecipes } from "./output-reference-script-decoding.js";
import { protectedOutputSignerMissingRecipes } from "./protected-output-signer-missing.js";
import { protocolRecipes } from "./protocol.js";
import { receivePurposeLanguageRecipes } from "./receive-purpose-language.js";
import type { DeploymentRecipe } from "./recipes.types.js";
import { redeemerCanonicityRecipes } from "./redeemer-canonicity.js";
import { referenceInputNoIdxRecipes } from "./reference-input-no-idx.js";
import { resolvedOutputNonCanonicalRecipes } from "./resolved-output-non-canonical.js";
import { scriptIntegrityHashMismatchRecipes } from "./script-integrity-hash-mismatch.js";
import { scriptIntegrityHashMissingRecipes } from "./script-integrity-hash-missing.js";
import { spendInputSignerMissingRecipes } from "./spend-input-signer-missing.js";
import { transactionOutputNonCanonicalRecipes } from "./transaction-output-non-canonical.js";
import { transitionTraceRecipes } from "./transition-trace.js";
import { unusedRedeemerRecipes } from "./unused-redeemer.js";
import { unusedScriptWitnessRecipes } from "./unused-script-witness.js";
import { validationTraceCanonicalRecipes } from "./validation-trace-canonical.js";
import { validationTraceCekContextRecipes } from "./validation-trace-cek-context.js";
import { validationTraceCekContextItemsRecipes } from "./validation-trace-cek-context-items.js";
import { validationTraceCekCoreRecipes } from "./validation-trace-cek-core.js";
import { validationTraceControlRecipes } from "./validation-trace-control.js";
import { validationTraceLedgerOutputYieldsRecipes } from "./validation-trace-ledger-output-yields.js";
import { validationTraceMachineYieldsRecipes } from "./validation-trace-machine-yields.js";
import { validationTracePhaseASemanticsRecipes } from "./validation-trace-phase-a-semantics.js";
import { validationTraceRedeemerItemRecipes } from "./validation-trace-redeemer-item.js";
import { validationTraceResolveInputsSemanticsRecipes } from "./validation-trace-resolve-inputs-semantics.js";
import { validationTraceScriptSourcesSemanticsRecipes } from "./validation-trace-script-sources-semantics.js";
import { validationTraceScriptSourcesYieldsRecipes } from "./validation-trace-script-sources-yields.js";
import { valueNotPreservedRecipes } from "./value-not-preserved.js";
import { withdrawalMistagRecipes } from "./withdrawal-mistag.js";
import { withdrawnInputRecipes } from "./withdrawn-input.js";
import { withdrawnReferenceInputRecipes } from "./withdrawn-reference-input.js";
import { witnessScriptDecodingRecipes } from "./witness-script-decoding.js";
import { zeroInputRecipes } from "./zero-input.js";

/** SDK owns selection/application recipes; identity and order live in core. */
export const DEPLOYMENT_RECIPES = Object.freeze({
  ...validationTraceControlRecipes,
  ...validationTraceResolveInputsSemanticsRecipes,
  ...validationTraceScriptSourcesSemanticsRecipes,
  ...validationTraceRedeemerItemRecipes,
  ...validationTraceScriptSourcesYieldsRecipes,
  ...validationTraceLedgerOutputYieldsRecipes,
  ...validationTraceCanonicalRecipes,
  ...validationTracePhaseASemanticsRecipes,
  ...validationTraceCekContextRecipes,
  ...validationTraceCekContextItemsRecipes,
  ...validationTraceCekCoreRecipes,
  ...validationTraceMachineYieldsRecipes,
  ...protocolRecipes,
  ...doubleSpendRecipes,
  ...nonExistentInputRecipes,
  ...nonExistentInputNoIndexRecipes,
  ...invalidRangeRecipes,
  ...zeroInputRecipes,
  ...daHashPreimageRecipes,
  ...noReferenceInputRecipes,
  ...referenceInputNoIdxRecipes,
  ...invalidSignatureRecipes,
  ...fabricatedDepositRecipes,
  ...fabricatedWithdrawalRecipes,
  ...nativeScriptDecodingRecipes,
  ...missingSignatureRecipes,
  ...withdrawnReferenceInputRecipes,
  ...canonicalDecodabilityRecipes,
  ...committedFieldShapeRecipes,
  ...minFeeRecipes,
  ...withdrawalMistagRecipes,
  ...doubleWithdrawRecipes,
  ...l2TxMistagRecipes,
  ...withdrawnInputRecipes,
  ...valueNotPreservedRecipes,
  ...inputSetUniquenessRecipes,
  ...mintAuthorizationRecipes,
  ...networkIdRecipes,
  ...nativeScriptInvalidRecipes,
  ...minAdaRecipes,
  ...fieldPreimageLengthMismatchRecipes,
  ...fieldItemWidthIllegalRecipes,
  ...witnessScriptDecodingRecipes,
  ...scriptIntegrityHashMissingRecipes,
  ...transactionOutputNonCanonicalRecipes,
  ...mintItemNonCanonicalRecipes,
  ...resolvedOutputNonCanonicalRecipes,
  ...mintDeclaredAssetLimitRecipes,
  ...spendInputSignerMissingRecipes,
  ...protectedOutputSignerMissingRecipes,
  ...observersForbiddenOnUntaggedNetworkRecipes,
  ...observerOrderInvalidRecipes,
  ...redeemerCanonicityRecipes,
  ...outputReferenceScriptDecodingRecipes,
  ...executionSourceScriptDecodingRecipes,
  ...receivePurposeLanguageRecipes,
  ...unusedScriptWitnessRecipes,
  ...unusedRedeemerRecipes,
  ...executionNativeScriptInvalidRecipes,
  ...scriptIntegrityHashMismatchRecipes,
  ...distinctAssetAccumulationLimitRecipes,
  ...missingScriptSourceRecipes,
  ...missingRedeemerRecipes,
  ...transitionTraceRecipes,
}) satisfies {
  readonly [Contract in DeploymentContract]: DeploymentRecipe<
    (typeof DEPLOYMENT_MEMBERS)[Contract]["purpose"]
  >;
};
