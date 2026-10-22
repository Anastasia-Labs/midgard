import type { DeploymentContract, DeploymentMember } from "./members.types.js";
import { canonicalDecodabilityMembers } from "./members/canonical-decodability.js";
import { committedFieldShapeMembers } from "./members/committed-field-shape.js";
import { daHashPreimageMembers } from "./members/da-hash-preimage.js";
import { distinctAssetAccumulationLimitMembers } from "./members/distinct-asset-accumulation-limit.js";
import { doubleSpendMembers } from "./members/double-spend.js";
import { doubleWithdrawMembers } from "./members/double-withdraw.js";
import { executionNativeScriptInvalidMembers } from "./members/execution-native-script-invalid.js";
import { executionSourceScriptDecodingMembers } from "./members/execution-source-script-decoding.js";
import { fabricatedDepositMembers } from "./members/fabricated-deposit.js";
import { fabricatedWithdrawalMembers } from "./members/fabricated-withdrawal.js";
import { fieldItemWidthIllegalMembers } from "./members/field-item-width-illegal.js";
import { fieldPreimageLengthMismatchMembers } from "./members/field-preimage-length-mismatch.js";
import { inputSetUniquenessMembers } from "./members/input-set-uniqueness.js";
import { invalidRangeMembers } from "./members/invalid-range.js";
import { invalidSignatureMembers } from "./members/invalid-signature.js";
import { l2TxMistagMembers } from "./members/l2-tx-mistag.js";
import { minAdaMembers } from "./members/min-ada.js";
import { minFeeMembers } from "./members/min-fee.js";
import { mintAuthorizationMembers } from "./members/mint-authorization.js";
import { mintDeclaredAssetLimitMembers } from "./members/mint-declared-asset-limit.js";
import { mintItemNonCanonicalMembers } from "./members/mint-item-non-canonical.js";
import { missingRedeemerMembers } from "./members/missing-redeemer.js";
import { missingScriptSourceMembers } from "./members/missing-script-source.js";
import { missingSignatureMembers } from "./members/missing-signature.js";
import { nativeScriptDecodingMembers } from "./members/native-script-decoding.js";
import { nativeScriptInvalidMembers } from "./members/native-script-invalid.js";
import { networkIdMembers } from "./members/network-id.js";
import { noReferenceInputMembers } from "./members/no-reference-input.js";
import { nonExistentInputMembers } from "./members/non-existent-input.js";
import { nonExistentInputNoIndexMembers } from "./members/non-existent-input-no-index.js";
import { observerOrderInvalidMembers } from "./members/observer-order-invalid.js";
import { observersForbiddenOnUntaggedNetworkMembers } from "./members/observers-forbidden-on-untagged-network.js";
import { outputReferenceScriptDecodingMembers } from "./members/output-reference-script-decoding.js";
import { protectedOutputSignerMissingMembers } from "./members/protected-output-signer-missing.js";
import { protocolAuthorityMembers } from "./members/protocol-authority.js";
import { protocolProofSupportMembers } from "./members/protocol-proof-support.js";
import { protocolUserEventsMembers } from "./members/protocol-user-events.js";
import { receivePurposeLanguageMembers } from "./members/receive-purpose-language.js";
import { redeemerCanonicityMembers } from "./members/redeemer-canonicity.js";
import { referenceInputNoIdxMembers } from "./members/reference-input-no-idx.js";
import { resolvedOutputNonCanonicalMembers } from "./members/resolved-output-non-canonical.js";
import { scriptIntegrityHashMismatchMembers } from "./members/script-integrity-hash-mismatch.js";
import { scriptIntegrityHashMissingMembers } from "./members/script-integrity-hash-missing.js";
import { spendInputSignerMissingMembers } from "./members/spend-input-signer-missing.js";
import { transactionOutputNonCanonicalMembers } from "./members/transaction-output-non-canonical.js";
import { transitionTraceMembers } from "./members/transition-trace.js";
import { unusedRedeemerMembers } from "./members/unused-redeemer.js";
import { unusedScriptWitnessMembers } from "./members/unused-script-witness.js";
import { validationTraceCanonicalMembers } from "./members/validation-trace-canonical.js";
import { validationTraceCekContextMembers } from "./members/validation-trace-cek-context.js";
import { validationTraceCekContextItemsMembers } from "./members/validation-trace-cek-context-items.js";
import { validationTraceCekCoreMembers } from "./members/validation-trace-cek-core.js";
import { validationTraceCekResolversMembers } from "./members/validation-trace-cek-resolvers.js";
import { validationTraceControlMembers } from "./members/validation-trace-control.js";
import { validationTraceLedgerOutputYieldsMembers } from "./members/validation-trace-ledger-output-yields.js";
import { validationTraceMachineYieldsMembers } from "./members/validation-trace-machine-yields.js";
import { validationTracePhaseASemanticsMembers } from "./members/validation-trace-phase-a-semantics.js";
import { validationTracePhaseResolversMembers } from "./members/validation-trace-phase-resolvers.js";
import { validationTraceRedeemerItemMembers } from "./members/validation-trace-redeemer-item.js";
import { validationTraceResolveInputsSemanticsMembers } from "./members/validation-trace-resolve-inputs-semantics.js";
import { validationTraceScriptSourcesSemanticsMembers } from "./members/validation-trace-script-sources-semantics.js";
import { validationTraceScriptSourcesYieldsMembers } from "./members/validation-trace-script-sources-yields.js";
import { valueNotPreservedMembers } from "./members/value-not-preserved.js";
import { withdrawalMistagMembers } from "./members/withdrawal-mistag.js";
import { withdrawnInputMembers } from "./members/withdrawn-input.js";
import { withdrawnReferenceInputMembers } from "./members/withdrawn-reference-input.js";
import { witnessScriptDecodingMembers } from "./members/witness-script-decoding.js";
import { zeroInputMembers } from "./members/zero-input.js";

/** Complete deployment identity; ordered views are projections of these positions. */
export const DEPLOYMENT_MEMBERS = Object.freeze({
  ...validationTraceControlMembers,
  ...validationTraceResolveInputsSemanticsMembers,
  ...validationTraceScriptSourcesSemanticsMembers,
  ...validationTraceRedeemerItemMembers,
  ...validationTraceScriptSourcesYieldsMembers,
  ...validationTraceLedgerOutputYieldsMembers,
  ...validationTraceCanonicalMembers,
  ...validationTracePhaseASemanticsMembers,
  ...validationTraceCekContextMembers,
  ...validationTraceCekContextItemsMembers,
  ...validationTraceCekCoreMembers,
  ...validationTraceCekResolversMembers,
  ...validationTracePhaseResolversMembers,
  ...validationTraceMachineYieldsMembers,
  ...protocolAuthorityMembers,
  ...protocolUserEventsMembers,
  ...protocolProofSupportMembers,
  ...doubleSpendMembers,
  ...nonExistentInputMembers,
  ...nonExistentInputNoIndexMembers,
  ...invalidRangeMembers,
  ...zeroInputMembers,
  ...daHashPreimageMembers,
  ...noReferenceInputMembers,
  ...referenceInputNoIdxMembers,
  ...invalidSignatureMembers,
  ...fabricatedDepositMembers,
  ...fabricatedWithdrawalMembers,
  ...nativeScriptDecodingMembers,
  ...missingSignatureMembers,
  ...withdrawnReferenceInputMembers,
  ...canonicalDecodabilityMembers,
  ...committedFieldShapeMembers,
  ...minFeeMembers,
  ...withdrawalMistagMembers,
  ...doubleWithdrawMembers,
  ...l2TxMistagMembers,
  ...withdrawnInputMembers,
  ...valueNotPreservedMembers,
  ...inputSetUniquenessMembers,
  ...mintAuthorizationMembers,
  ...networkIdMembers,
  ...nativeScriptInvalidMembers,
  ...minAdaMembers,
  ...fieldPreimageLengthMismatchMembers,
  ...fieldItemWidthIllegalMembers,
  ...witnessScriptDecodingMembers,
  ...scriptIntegrityHashMissingMembers,
  ...transactionOutputNonCanonicalMembers,
  ...mintItemNonCanonicalMembers,
  ...resolvedOutputNonCanonicalMembers,
  ...mintDeclaredAssetLimitMembers,
  ...spendInputSignerMissingMembers,
  ...protectedOutputSignerMissingMembers,
  ...observersForbiddenOnUntaggedNetworkMembers,
  ...observerOrderInvalidMembers,
  ...redeemerCanonicityMembers,
  ...outputReferenceScriptDecodingMembers,
  ...executionSourceScriptDecodingMembers,
  ...receivePurposeLanguageMembers,
  ...unusedScriptWitnessMembers,
  ...unusedRedeemerMembers,
  ...executionNativeScriptInvalidMembers,
  ...scriptIntegrityHashMismatchMembers,
  ...distinctAssetAccumulationLimitMembers,
  ...missingScriptSourceMembers,
  ...missingRedeemerMembers,
  ...transitionTraceMembers,
}) satisfies Readonly<Record<DeploymentContract, DeploymentMember>>;
