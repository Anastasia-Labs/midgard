/**
 * Building the full FaultProofContracts set from a blueprint and mapping chains to first steps.
 */

import { Effect } from "effect";

import { type FraudProofs } from "../../common.js";
import { buildCanonicalDecodabilityChain } from "./families/canonical-decodability.js";
import { buildCommittedFieldShapeChain } from "./families/committed-field-shape.js";
import { buildCrossBlockDuplicateEventChain } from "./families/cross-block-duplicate-event.js";
import { buildDaHashPreimageChain } from "./families/da-hash-preimage.js";
import { buildDistinctAssetAccumulationLimitChain } from "./families/distinct-asset-accumulation-limit.js";
import { buildDoubleSpendChain } from "./families/double-spend.js";
import { buildDoubleWithdrawChain } from "./families/double-withdraw.js";
import { buildExecutionNativeScriptInvalidChain } from "./families/execution-native-script-invalid.js";
import { buildExecutionSourceScriptDecodingChain } from "./families/execution-source-script-decoding.js";
import { buildFabricatedDepositChain } from "./families/fabricated-deposit.js";
import { buildFabricatedWithdrawalChain } from "./families/fabricated-withdrawal.js";
import { buildFieldItemWidthIllegalChain } from "./families/field-item-width-illegal.js";
import { buildFieldPreimageLengthMismatchChain } from "./families/field-preimage-length-mismatch.js";
import { buildInputNoIdxChain } from "./families/input-no-idx.js";
import { buildInputSetUniquenessChain } from "./families/input-set-uniqueness.js";
import { buildInvalidRangeChain } from "./families/invalid-range.js";
import { buildInvalidSignatureChain } from "./families/invalid-signature.js";
import { buildL2TxMistagChain } from "./families/l2-tx-mistag.js";
import {
  buildMinAdaChain,
  type BuildMinAdaFaultProofContractsParams,
} from "./families/min-ada.js";
import { buildMinFeeChain } from "./families/min-fee.js";
import { buildMintAuthorizationChain } from "./families/mint-authorization.js";
import { buildMintDeclaredAssetLimitChain } from "./families/mint-declared-asset-limit.js";
import { buildMissingNativeScriptTxChain } from "./families/missing-native-script-tx.js";
import { buildMissingNativeScriptUtxoChain } from "./families/missing-native-script-utxo.js";
import { buildMissingRedeemerChain } from "./families/missing-redeemer.js";
import { buildMissingScriptSourceChain } from "./families/missing-script-source.js";
import { buildMissingSignatureChain } from "./families/missing-signature.js";
import { buildNativeScriptDecodingChain } from "./families/native-script-decoding.js";
import { buildNativeScriptInvalidChain } from "./families/native-script-invalid.js";
import { buildNetworkIdChain } from "./families/network-id.js";
import { buildNoReferenceInputChain } from "./families/no-reference-input.js";
import { buildNonExistentInputChain } from "./families/non-existent-input.js";
import { buildObserverOrderInvalidChain } from "./families/observer-order-invalid.js";
import { buildObserversForbiddenOnUntaggedNetworkChain } from "./families/observers-forbidden-on-untagged-network.js";
import { buildOutputReferenceScriptDecodingChain } from "./families/output-reference-script-decoding.js";
import { buildProtectedOutputSignerMissingChain } from "./families/protected-output-signer-missing.js";
import { buildReceivePurposeLanguageChain } from "./families/receive-purpose-language.js";
import { buildRedeemerCanonicityChain } from "./families/redeemer-canonicity.js";
import { buildReferenceInputNoIdxChain } from "./families/reference-input-no-idx.js";
import { buildResolvedOutputNonCanonicalChain } from "./families/resolved-output-non-canonical.js";
import { buildScriptIntegrityHashMismatchChain } from "./families/script-integrity-hash-mismatch.js";
import { buildScriptIntegrityHashMissingChain } from "./families/script-integrity-hash-missing.js";
import { buildSpendInputSignerMissingChain } from "./families/spend-input-signer-missing.js";
import { buildTransactionOutputNonCanonicalChain } from "./families/transaction-output-non-canonical.js";
import { buildTransitionTraceChain } from "./families/transition-trace.js";
import { buildUnusedRedeemerChain } from "./families/unused-redeemer.js";
import { buildUnusedScriptWitnessChain } from "./families/unused-script-witness.js";
import { buildValidationTraceDisputeChain } from "./families/validation-trace-dispute.js";
import { buildValueNotPreservedChain } from "./families/value-not-preserved.js";
import { buildWithdrawalMistagChain } from "./families/withdrawal-mistag.js";
import { buildWithdrawnInputChain } from "./families/withdrawn-input.js";
import { buildWithdrawnReferenceInputChain } from "./families/withdrawn-reference-input.js";
import { buildWitnessScriptDecodingChain } from "./families/witness-script-decoding.js";
import { buildZeroInputChain } from "./families/zero-input.js";
import { buildSharedFaultProofContracts } from "./shared.js";
import {
  type FaultProofContractChains,
  type FaultProofContracts,
} from "./types.js";

export const buildFaultProofContracts = (
  params: BuildMinAdaFaultProofContractsParams,
): Effect.Effect<FaultProofContracts, Error> =>
  Effect.gen(function* () {
    const shared = yield* buildSharedFaultProofContracts(params);
    const doubleSpend = yield* buildDoubleSpendChain({
      ...params,
      ...shared,
    });
    const nonExistentInput = yield* buildNonExistentInputChain({
      ...params,
      ...shared,
    });
    const noReferenceInput = yield* buildNoReferenceInputChain({
      ...params,
      ...shared,
    });
    const invalidRange = yield* buildInvalidRangeChain({
      ...params,
      ...shared,
    });
    const invalidSignature = yield* buildInvalidSignatureChain({
      ...params,
      ...shared,
    });
    const zeroInput = yield* buildZeroInputChain({
      ...params,
      ...shared,
    });
    const transitionTrace = yield* buildTransitionTraceChain({
      ...params,
      ...shared,
    });
    const validationTraceDispute = yield* buildValidationTraceDisputeChain({
      ...params,
      ...shared,
    });
    const daHashPreimage = yield* buildDaHashPreimageChain({
      ...params,
      ...shared,
    });
    const nonExistentInputNoIndex = yield* buildInputNoIdxChain({
      ...params,
      ...shared,
    });
    const referenceInputNoIdx = yield* buildReferenceInputNoIdxChain({
      ...params,
      ...shared,
    });
    const fabricatedDeposit = yield* buildFabricatedDepositChain({
      ...params,
      ...shared,
    });
    const fabricatedWithdrawal = yield* buildFabricatedWithdrawalChain({
      ...params,
      ...shared,
    });
    const nativeScriptDecoding = yield* buildNativeScriptDecodingChain({
      ...params,
      ...shared,
    });
    const missingSignature = yield* buildMissingSignatureChain({
      ...params,
      ...shared,
    });
    const missingNativeScriptTx = yield* buildMissingNativeScriptTxChain({
      ...params,
      ...shared,
    });
    const withdrawnReferenceInput = yield* buildWithdrawnReferenceInputChain({
      ...params,
      ...shared,
    });
    const canonicalDecodability = yield* buildCanonicalDecodabilityChain({
      ...params,
      ...shared,
    });
    const committedFieldShape = yield* buildCommittedFieldShapeChain({
      ...params,
      ...shared,
    });
    const minFee = yield* buildMinFeeChain({ ...params, ...shared });
    const withdrawalMistag = yield* buildWithdrawalMistagChain({
      ...params,
      ...shared,
    });
    const doubleWithdraw = yield* buildDoubleWithdrawChain({
      ...params,
      ...shared,
    });
    const crossBlockDuplicateEvent = yield* buildCrossBlockDuplicateEventChain({
      ...params,
      ...shared,
    });
    const l2TxMistag = yield* buildL2TxMistagChain({ ...params, ...shared });
    const withdrawnInput = yield* buildWithdrawnInputChain({
      ...params,
      ...shared,
    });
    const valueNotPreserved = yield* buildValueNotPreservedChain({
      ...params,
      ...shared,
    });
    const inputSetUniqueness = yield* buildInputSetUniquenessChain({
      ...params,
      ...shared,
    });
    const mintAuthorization = yield* buildMintAuthorizationChain({
      ...params,
      ...shared,
    });
    const networkId = yield* buildNetworkIdChain({
      ...params,
      ...shared,
    });
    const missingNativeScriptUtxo = yield* buildMissingNativeScriptUtxoChain({
      ...params,
      ...shared,
    });
    const nativeScriptInvalid = yield* buildNativeScriptInvalidChain({
      ...params,
      ...shared,
    });
    const minAda = yield* buildMinAdaChain({ ...params, ...shared });
    const fieldPreimageLengthMismatch =
      yield* buildFieldPreimageLengthMismatchChain({ ...params, ...shared });
    const fieldItemWidthIllegal = yield* buildFieldItemWidthIllegalChain({
      ...params,
      ...shared,
    });
    const witnessScriptDecoding = yield* buildWitnessScriptDecodingChain({
      ...params,
      ...shared,
    });
    const scriptIntegrityHashMissing =
      yield* buildScriptIntegrityHashMissingChain({ ...params, ...shared });
    const transactionOutputNonCanonical =
      yield* buildTransactionOutputNonCanonicalChain({ ...params, ...shared });
    const resolvedOutputNonCanonical =
      yield* buildResolvedOutputNonCanonicalChain({ ...params, ...shared });
    const mintDeclaredAssetLimit = yield* buildMintDeclaredAssetLimitChain({
      ...params,
      ...shared,
    });
    const spendInputSignerMissing = yield* buildSpendInputSignerMissingChain({
      ...params,
      ...shared,
    });
    const protectedOutputSignerMissing =
      yield* buildProtectedOutputSignerMissingChain({ ...params, ...shared });
    const observersForbiddenOnUntaggedNetwork =
      yield* buildObserversForbiddenOnUntaggedNetworkChain({
        ...params,
        ...shared,
      });
    const outputReferenceScriptDecoding =
      yield* buildOutputReferenceScriptDecodingChain({ ...params, ...shared });
    const executionSourceScriptDecoding =
      yield* buildExecutionSourceScriptDecodingChain({ ...params, ...shared });
    const executionNativeScriptInvalid =
      yield* buildExecutionNativeScriptInvalidChain({ ...params, ...shared });
    const observerOrderInvalid = yield* buildObserverOrderInvalidChain({
      ...params,
      ...shared,
    });
    const redeemerCanonicity = yield* buildRedeemerCanonicityChain({
      ...params,
      ...shared,
    });
    const receivePurposeLanguage = yield* buildReceivePurposeLanguageChain({
      ...params,
      ...shared,
    });
    const unusedScriptWitness = yield* buildUnusedScriptWitnessChain({
      ...params,
      ...shared,
    });
    const missingScriptSource = yield* buildMissingScriptSourceChain({
      ...params,
      ...shared,
    });
    const missingRedeemer = yield* buildMissingRedeemerChain({
      ...params,
      ...shared,
    });
    const unusedRedeemer = yield* buildUnusedRedeemerChain({
      ...params,
      ...shared,
    });
    const scriptIntegrityHashMismatch =
      yield* buildScriptIntegrityHashMismatchChain({ ...params, ...shared });
    const distinctAssetAccumulationLimit =
      yield* buildDistinctAssetAccumulationLimitChain({
        ...params,
        ...shared,
      });

    return {
      computationThread: shared.computationThread,
      fraudProof: shared.fraudProof,
      fieldPreimageCertificate: shared.fieldPreimageCertificate,
      doubleSpend,
      nonExistentInput,
      noReferenceInput,
      invalidRange,
      invalidSignature,
      zeroInput,
      transitionTrace,
      validationTraceDispute,
      daHashPreimage,
      nonExistentInputNoIndex,
      referenceInputNoIdx,
      fabricatedDeposit,
      fabricatedWithdrawal,
      nativeScriptDecoding,
      missingSignature,
      missingNativeScriptTx,
      withdrawnReferenceInput,
      canonicalDecodability,
      committedFieldShape,
      minFee,
      withdrawalMistag,
      doubleWithdraw,
      crossBlockDuplicateEvent,
      l2TxMistag,
      withdrawnInput,
      valueNotPreserved,
      inputSetUniqueness,
      mintAuthorization,
      networkId,
      missingNativeScriptUtxo,
      nativeScriptInvalid,
      minAda,
      fieldPreimageLengthMismatch,
      fieldItemWidthIllegal,
      witnessScriptDecoding,
      scriptIntegrityHashMissing,
      transactionOutputNonCanonical,
      resolvedOutputNonCanonical,
      mintDeclaredAssetLimit,
      spendInputSignerMissing,
      protectedOutputSignerMissing,
      observersForbiddenOnUntaggedNetwork,
      outputReferenceScriptDecoding,
      executionSourceScriptDecoding,
      executionNativeScriptInvalid,
      observerOrderInvalid,
      redeemerCanonicity,
      receivePurposeLanguage,
      unusedScriptWitness,
      missingScriptSource,
      missingRedeemer,
      unusedRedeemer,
      scriptIntegrityHashMismatch,
      distinctAssetAccumulationLimit,
    };
  });

/** Project the full deployment registry to the first-step catalogue leaves. */
export const fraudProofContractsToFirstSteps = (
  contracts: FaultProofContractChains,
): FraudProofs => ({
  doubleSpend: contracts.doubleSpend.firstStep,
  nonExistentInput: contracts.nonExistentInput.firstStep,
  nonExistentInputNoIndex: contracts.nonExistentInputNoIndex.firstStep,
  invalidRange: contracts.invalidRange.firstStep,
  transitionTrace: contracts.transitionTrace.firstStep,
  zeroInput: contracts.zeroInput.firstStep,
  validationTraceDispute: {
    ...contracts.validationTraceDispute.firstStep,
    source: contracts.validationTraceDispute.source,
    game: contracts.validationTraceDispute.game,
    boundary: contracts.validationTraceDispute.boundary,
    timeout: contracts.validationTraceDispute.timeout,
    award: contracts.validationTraceDispute.award,
  },
  daHashPreimage: contracts.daHashPreimage.firstStep,
  noReferenceInput: contracts.noReferenceInput.firstStep,
  referenceInputNoIdx: contracts.referenceInputNoIdx.firstStep,
  invalidSignature: contracts.invalidSignature.firstStep,
  fabricatedDeposit: contracts.fabricatedDeposit.firstStep,
  fabricatedWithdrawal: contracts.fabricatedWithdrawal.firstStep,
  nativeScriptDecoding: contracts.nativeScriptDecoding.firstStep,
  missingSignature: contracts.missingSignature.firstStep,
  missingNativeScriptTx: contracts.missingNativeScriptTx.firstStep,
  withdrawnReferenceInput: contracts.withdrawnReferenceInput.firstStep,
  canonicalDecodability: contracts.canonicalDecodability.firstStep,
  committedFieldShape: contracts.committedFieldShape.firstStep,
  minFee: contracts.minFee.firstStep,
  withdrawalMistag: contracts.withdrawalMistag.firstStep,
  doubleWithdraw: contracts.doubleWithdraw.firstStep,
  crossBlockDuplicateEvent: contracts.crossBlockDuplicateEvent.firstStep,
  l2TxMistag: contracts.l2TxMistag.firstStep,
  withdrawnInput: contracts.withdrawnInput.firstStep,
  valueNotPreserved: contracts.valueNotPreserved.firstStep,
  inputSetUniqueness: contracts.inputSetUniqueness.firstStep,
  mintAuthorization: contracts.mintAuthorization.firstStep,
  networkId: contracts.networkId.firstStep,
  missingNativeScriptUtxo: contracts.missingNativeScriptUtxo.firstStep,
  nativeScriptInvalid: contracts.nativeScriptInvalid.firstStep,
  minAda: contracts.minAda.firstStep,
  fieldPreimageLengthMismatch: contracts.fieldPreimageLengthMismatch.firstStep,
  fieldItemWidthIllegal: contracts.fieldItemWidthIllegal.firstStep,
  witnessScriptDecoding: contracts.witnessScriptDecoding.firstStep,
  scriptIntegrityHashMissing: contracts.scriptIntegrityHashMissing.firstStep,
  transactionOutputNonCanonical:
    contracts.transactionOutputNonCanonical.firstStep,
  resolvedOutputNonCanonical: contracts.resolvedOutputNonCanonical.firstStep,
  mintDeclaredAssetLimit: contracts.mintDeclaredAssetLimit.firstStep,
  spendInputSignerMissing: contracts.spendInputSignerMissing.firstStep,
  protectedOutputSignerMissing:
    contracts.protectedOutputSignerMissing.firstStep,
  observersForbiddenOnUntaggedNetwork:
    contracts.observersForbiddenOnUntaggedNetwork.firstStep,
  outputReferenceScriptDecoding:
    contracts.outputReferenceScriptDecoding.firstStep,
  executionSourceScriptDecoding:
    contracts.executionSourceScriptDecoding.firstStep,
  executionNativeScriptInvalid:
    contracts.executionNativeScriptInvalid.firstStep,
  observerOrderInvalid: contracts.observerOrderInvalid.firstStep,
  redeemerCanonicity: contracts.redeemerCanonicity.firstStep,
  receivePurposeLanguage: contracts.receivePurposeLanguage.firstStep,
  unusedScriptWitness: contracts.unusedScriptWitness.firstStep,
  missingScriptSource: contracts.missingScriptSource.firstStep,
  missingRedeemer: contracts.missingRedeemer.firstStep,
  unusedRedeemer: contracts.unusedRedeemer.firstStep,
  scriptIntegrityHashMismatch: contracts.scriptIntegrityHashMismatch.firstStep,
  distinctAssetAccumulationLimit:
    contracts.distinctAssetAccumulationLimit.firstStep,
});
