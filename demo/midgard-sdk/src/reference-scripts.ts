import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  type Assets,
  CML,
  coreToTxOutput,
  Data,
  fromText,
  type LucidEvolution,
  mintingPolicyToId,
  type Script,
  toUnit,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { MintingValidator } from "./common.js";
import { LucidError } from "./common.js";
import { StateQueueError } from "./state-queue.js";
import { isPlainPositiveAdaOnlyUtxo } from "./tx-output-utils.js";

export const REFERENCE_SCRIPT_AUTH_TIMELOCK_MS = 4 * 60 * 60 * 1000;
export const REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS = 90 * 60 * 1000;

export const REFERENCE_SCRIPT_AUTH_TOKEN_NAMES = {
  "reference-script-auth minting": "ReferenceScriptAuthMint",
  "hub-oracle minting": "HubOracleMint",
  "da-params-governor spending": "DaParamsGovernorSpend",
  "da-params-governor minting": "DaParamsGovernorMint",
  "da-attestation spending": "DaAttestationSpend",
  "da-attestation minting": "DaAttestationMint",
  "scheduler spending": "SchedulerSpend",
  "scheduler minting": "SchedulerMint",
  "state-queue spending": "StateQueueSpend",
  "state-queue minting": "StateQueueMint",
  "state-queue commit withdrawal": "StateQueueCommitYield",
  "state-queue unattested-timeout withdrawal": "StateQueueUnattestedYield",
  "state-queue unavailable-timeout withdrawal": "StateQueueUnavailableYield",
  "state-queue fraud-removal withdrawal": "StateQueueFraudRemovalYield",
  "state-queue merge withdrawal": "StateQueueMergeYield",
  "registered-operators spending": "RegisteredOperatorsSpend",
  "registered-operators minting": "RegisteredOperatorsMint",
  "active-operators spending": "ActiveOperatorsSpend",
  "active-operators minting": "ActiveOperatorsMint",
  "retired-operators spending": "RetiredOperatorsSpend",
  "retired-operators minting": "RetiredOperatorsMint",
  "fraud-proof-catalogue minting": "FraudProofCatalogueMint",
  "deposit minting": "DepositMint",
  "deposit spending": "DepositSpend",
  "withdrawal minting": "WithdrawalMint",
  "withdrawal spending": "WithdrawalSpend",
  "settlement minting": "SettlementMint",
  "membership proof withdrawal": "MembershipProofWithdraw",
  "reserve spending": "ReserveSpend",
  "reserve observer": "ReserveObserver",
  "payout spending": "PayoutSpend",
  "payout minting": "PayoutMint",
  // #579. Mirrors `midgard-core`'s token-name removal; ABI-04 fails closed on
  // divergence.
  "V1 field-preimage certificate": "V1FieldPreimageCertSpend",
  "V1 field-preimage certificate minting": "V1FieldPreimageCertMint",
  "V1 immutable CEK program-material publication": "V1CekProgramMaterialSpend",
  "V1 validation-trace dispute": "V1ValidationTraceDispute",
  "V1 validation-trace source": "V1ValidationTraceSource",
  "V1 validation-trace game": "V1ValidationTraceGame",
  "V1 validation-trace boundary": "V1ValidationTraceBoundary",
  "V1 validation-trace timeout": "V1ValidationTraceTimeout",
  "V1 validation-trace award": "V1ValidationTraceAward",
  "V1 validation-trace CEK direct resolver": "V1ValidationTraceCekResolver0",
  "V1 fraud-proof transition-trace route": "V1FpTransitionTraceRoute",
  "V1 fraud-proof transition-trace final-0": "V1FpTransitionTraceFinal0",
  "V1 fraud-proof transition-trace final-1": "V1FpTransitionTraceFinal1",
  "V1 fraud-proof transition-trace final-2": "V1FpTransitionTraceFinal2",
  "V1 fraud-proof transition-trace final-3": "V1FpTransitionTraceFinal3",
  "V1 fraud-proof transition-trace final-4": "V1FpTransitionTraceFinal4",
  "V1 fraud-proof transition-trace final-5": "V1FpTransitionTraceFinal5",
  "V1 fraud-proof transition-trace final-6": "V1FpTransitionTraceFinal6",
  "V1 fraud-proof transition-trace final-7": "V1FpTransitionTraceFinal7",
  "V1 fraud-proof fabricated-deposit step-01": "V1FpFabricatedDepositS01",
  "V1 fraud-proof fabricated-deposit step-02": "V1FpFabricatedDepositS02",
  "V1 fraud-proof fabricated-deposit step-03": "V1FpFabricatedDepositS03",
  "V1 fraud-proof fabricated-deposit step-04": "V1FpFabricatedDepositS04",
  "V1 fraud-proof fabricated-withdrawal step-01": "V1FpFabricatedWithdrawalS01",
  "V1 fraud-proof fabricated-withdrawal step-02": "V1FpFabricatedWithdrawalS02",
  "V1 fraud-proof fabricated-withdrawal step-03": "V1FpFabricatedWithdrawalS03",
  "V1 fraud-proof fabricated-withdrawal step-04": "V1FpFabricatedWithdrawalS04",
  "V1 fraud-proof native-script-decoding step-01":
    "V1FpNativeScriptDecodingS01",
  "V1 fraud-proof native-script-decoding step-02":
    "V1FpNativeScriptDecodingS02",
  "V1 fraud-proof native-script-decoding step-03 open-subject":
    "V1FpNativeScriptDecodingS03Open",
  "V1 fraud-proof native-script-decoding step-03 bind-descriptor":
    "V1FpNativeScriptDecodingS03Bind",
  "V1 fraud-proof native-script-decoding step-03 advance-or-close":
    "V1FpNativeScriptDecodingS03Scan",
  "V1 fraud-proof native-script-decoding step-04":
    "V1FpNativeScriptDecodingS04",
  "V1 fraud-proof missing-signature step-01": "V1FpMissingSignatureS01",
  "V1 fraud-proof missing-signature step-02": "V1FpMissingSignatureS02",
  "V1 fraud-proof missing-signature step-03": "V1FpMissingSignatureS03",
  "V1 fraud-proof missing-signature step-04": "V1FpMissingSignatureS04",
  "V1 fraud-proof missing-native-script-tx step-01":
    "V1FpMissingNativeScriptTxS01",
  "V1 fraud-proof missing-native-script-tx step-02":
    "V1FpMissingNativeScriptTxS02",
  "V1 fraud-proof missing-native-script-tx step-03":
    "V1FpMissingNativeScriptTxS03",
  "V1 fraud-proof missing-native-script-tx step-04":
    "V1FpMissingNativeScriptTxS04",
  "V1 fraud-proof missing-native-script-tx step-05":
    "V1FpMissingNativeScriptTxS05",
  "V1 fraud-proof missing-native-script-tx step-06":
    "V1FpMissingNativeScriptTxS06",
  "V1 fraud-proof withdrawn-reference-input step-01":
    "V1FpWithdrawnReferenceInputS01",
  "V1 fraud-proof withdrawn-reference-input step-02":
    "V1FpWithdrawnReferenceInputS02",
  "V1 fraud-proof withdrawn-reference-input step-03":
    "V1FpWithdrawnReferenceInputS03",
  "V1 fraud-proof canonical-decodability step-01":
    "V1FpCanonicalDecodabilityS01",
  "V1 fraud-proof canonical-decodability step-02":
    "V1FpCanonicalDecodabilityS02",
  "V1 fraud-proof committed-field-shape step-01": "V1FpCommittedFieldShapeS01",
  "V1 fraud-proof committed-field-shape step-02": "V1FpCommittedFieldShapeS02",
  "V1 fraud-proof min-fee step-01": "V1FpMinFeeS01",
  "V1 fraud-proof min-fee step-02": "V1FpMinFeeS02",
  "V1 fraud-proof withdrawal-mistag step-01": "V1FpWithdrawalMistagS01",
  "V1 fraud-proof withdrawal-mistag step-02": "V1FpWithdrawalMistagS02",
  "V1 fraud-proof withdrawal-mistag step-03": "V1FpWithdrawalMistagS03",
  "V1 fraud-proof withdrawal-mistag step-04": "V1FpWithdrawalMistagS04",
  "V1 fraud-proof withdrawal-mistag step-05": "V1FpWithdrawalMistagS05",
  "V1 fraud-proof double-withdraw step-01": "V1FpDoubleWithdrawS01",
  "V1 fraud-proof double-withdraw step-02": "V1FpDoubleWithdrawS02",
  "V1 fraud-proof cross-block-duplicate-event step-01":
    "V1FpCrossBlockDuplicateEventS01",
  "V1 fraud-proof cross-block-duplicate-event step-02":
    "V1FpCrossBlockDuplicateEventS02",
  "V1 fraud-proof l2-tx-mistag step-01": "V1FpL2TxMistagS01",
  "V1 fraud-proof l2-tx-mistag step-02": "V1FpL2TxMistagS02",
  "V1 fraud-proof withdrawn-input step-01": "V1FpWithdrawnInputS01",
  "V1 fraud-proof withdrawn-input step-02": "V1FpWithdrawnInputS02",
  "V1 fraud-proof withdrawn-input step-03": "V1FpWithdrawnInputS03",
  "V1 fraud-proof value-not-preserved step-01": "V1FpValueNotPreservedS01",
  "V1 fraud-proof value-not-preserved step-02": "V1FpValueNotPreservedS02",
  "V1 fraud-proof value-not-preserved step-03": "V1FpValueNotPreservedS03",
  "V1 fraud-proof value-not-preserved step-04": "V1FpValueNotPreservedS04",
  "V1 fraud-proof input-set-uniqueness step-01": "V1FpInputSetUniquenessS01",
  "V1 fraud-proof input-set-uniqueness step-02": "V1FpInputSetUniquenessS02",
  "V1 fraud-proof input-set-uniqueness step-03": "V1FpInputSetUniquenessS03",
  "V1 fraud-proof input-set-uniqueness step-04": "V1FpInputSetUniquenessS04",
  "V1 fraud-proof mint-authorization step-01": "V1FpMintAuthorizationS01",
  "V1 fraud-proof mint-authorization step-02": "V1FpMintAuthorizationS02",
  "V1 fraud-proof mint-authorization step-03": "V1FpMintAuthorizationS03",
  "V1 fraud-proof mint-authorization step-04": "V1FpMintAuthorizationS04",
  "V1 fraud-proof mint-authorization step-05": "V1FpMintAuthorizationS05",
  "V1 fraud-proof network-id step-01": "V1FpNetworkIdS01",
  "V1 fraud-proof network-id step-02": "V1FpNetworkIdS02",
  "V1 fraud-proof computation-thread minting": "V1FpComputationThreadMint",
  "V1 fraud-proof token minting": "V1FpTokenMint",
  "V1 MPF chunked-verify withdrawal": "V1MpfChunkedVerifyWithdraw",
  "V1 MPF pexcludes withdrawal": "V1MpfPexcludesWithdraw",
  "V1 fraud-proof double-spend step-01": "V1FpDoubleSpendS01",
  "V1 fraud-proof double-spend step-02": "V1FpDoubleSpendS02",
  "V1 fraud-proof double-spend step-03": "V1FpDoubleSpendS03",
  "V1 fraud-proof double-spend step-04": "V1FpDoubleSpendS04",
  "V1 fraud-proof non-existent-input step-01": "V1FpNonExistentInputS01",
  "V1 fraud-proof non-existent-input step-02": "V1FpNonExistentInputS02",
  "V1 fraud-proof non-existent-input step-03": "V1FpNonExistentInputS03",
  "V1 fraud-proof non-existent-input step-04": "V1FpNonExistentInputS04",
  "V1 fraud-proof non-existent-input-no-index step-01":
    "V1FpNonExistentInputNoIdxS01",
  "V1 fraud-proof non-existent-input-no-index step-02":
    "V1FpNonExistentInputNoIdxS02",
  "V1 fraud-proof non-existent-input-no-index step-03":
    "V1FpNonExistentInputNoIdxS03",
  "V1 fraud-proof non-existent-input-no-index step-04":
    "V1FpNonExistentInputNoIdxS04",
  "V1 fraud-proof invalid-range step-01": "V1FpInvalidRangeS01",
  "V1 fraud-proof invalid-range step-02": "V1FpInvalidRangeS02",
  "V1 fraud-proof zero-input step-01": "V1FpZeroInputS01",
  "V1 fraud-proof zero-input step-02": "V1FpZeroInputS02",
  "V1 fraud-proof da-hash-preimage step-01": "V1FpDaHashPreimageS01",
  "V1 fraud-proof da-hash-preimage step-02": "V1FpDaHashPreimageS02",
  "V1 fraud-proof no-reference-input step-01": "V1FpNoReferenceInputS01",
  "V1 fraud-proof no-reference-input step-02": "V1FpNoReferenceInputS02",
  "V1 fraud-proof no-reference-input step-03": "V1FpNoReferenceInputS03",
  "V1 fraud-proof no-reference-input step-04": "V1FpNoReferenceInputS04",
  "V1 fraud-proof reference-input-no-idx step-01": "V1FpReferenceInputNoIdxS01",
  "V1 fraud-proof reference-input-no-idx step-02": "V1FpReferenceInputNoIdxS02",
  "V1 fraud-proof reference-input-no-idx step-03": "V1FpReferenceInputNoIdxS03",
  "V1 fraud-proof reference-input-no-idx step-04": "V1FpReferenceInputNoIdxS04",
  "V1 fraud-proof invalid-signature step-01": "V1FpInvalidSignatureS01",
  "V1 fraud-proof invalid-signature step-02": "V1FpInvalidSignatureS02",
  "V1 fraud-proof missing-native-script-tx step-07":
    "V1FpMissingNativeScriptTxS07",
  "V1 fraud-proof missing-native-script-tx step-08":
    "V1FpMissingNativeScriptTxS08",
  "V1 fraud-proof missing-native-script-utxo step-01":
    "V1FpMissingNativeScriptUtxoS01",
  "V1 fraud-proof missing-native-script-utxo step-02":
    "V1FpMissingNativeScriptUtxoS02",
  "V1 fraud-proof missing-native-script-utxo step-03":
    "V1FpMissingNativeScriptUtxoS03",
  "V1 fraud-proof missing-native-script-utxo step-04":
    "V1FpMissingNativeScriptUtxoS04",
  "V1 fraud-proof missing-native-script-utxo step-05":
    "V1FpMissingNativeScriptUtxoS05",
  "V1 fraud-proof native-script-invalid step-01": "V1FpNativeScriptInvalidS01",
  "V1 fraud-proof native-script-invalid step-02": "V1FpNativeScriptInvalidS02",
  "V1 fraud-proof native-script-invalid step-03": "V1FpNativeScriptInvalidS03",
  "V1 fraud-proof min-ada step-01": "V1FpMinAdaS01",
  "V1 fraud-proof min-ada step-02": "V1FpMinAdaS02",
  "V1 fraud-proof min-ada step-02 tx yield": "V1FpMinAdaS02TxYield",
  "V1 fraud-proof min-ada step-02 UTxO yield": "V1FpMinAdaS02UtxoYield",
  "correction-lock spending": "CorrectionLockSpend",
  "V1 fraud-proof min-ada step-03": "V1FpMinAdaS03",
  "V1 fraud-proof min-ada step-04": "V1FpMinAdaS04",
  "V1 fraud-proof min-ada step-05": "V1FpMinAdaS05",
  "V1 fraud-proof field-preimage-length-mismatch step-01": "V1FpFieldLenS01",
  "V1 fraud-proof field-preimage-length-mismatch step-02 accepted":
    "V1FpFieldLenS02Accepted",
  "V1 fraud-proof field-preimage-length-mismatch step-02 forced":
    "V1FpFieldLenS02Forced",
  "V1 fraud-proof field-preimage-length-mismatch step-03": "V1FpFieldLenS03",
  "V1 fraud-proof field-item-width-illegal step-01": "V1FpItemWidthS01",
  "V1 fraud-proof field-item-width-illegal step-02": "V1FpItemWidthS02",
  "V1 fraud-proof field-item-width-illegal step-03": "V1FpItemWidthS03",
  "V1 fraud-proof witness-script-decoding step-01": "V1FpWitnessDecodeS01",
  "V1 fraud-proof witness-script-decoding step-02": "V1FpWitnessDecodeS02",
  "V1 fraud-proof witness-script-decoding step-03": "V1FpWitnessDecodeS03",
  "V1 fraud-proof witness-script-decoding step-04": "V1FpWitnessDecodeS04",
  "V1 fraud-proof script-integrity-hash-missing step-01":
    "V1FpIntegrityMissingS01",
  "V1 fraud-proof script-integrity-hash-missing step-02":
    "V1FpIntegrityMissingS02",
  "V1 fraud-proof script-integrity-hash-missing step-03":
    "V1FpIntegrityMissingS03",
  "V1 fraud-proof script-integrity-hash-missing script-grammar":
    "V1FpIntegrityMissingGrammar",
  "V1 fraud-proof script-integrity-hash-missing script-scan":
    "V1FpIntegrityMissingScan",
  "V1 fraud-proof script-integrity-hash-missing redeemer-grammar":
    "V1FpIntegrityMissingRedeemer",
  "V1 fraud-proof script-integrity-hash-missing step-04":
    "V1FpIntegrityMissingS04",
  "V1 fraud-proof transaction-output-non-canonical step-01":
    "V1FpTxOutputCanonicalS01",
  "V1 fraud-proof transaction-output-non-canonical step-02":
    "V1FpTxOutputCanonicalS02",
  "V1 fraud-proof transaction-output-non-canonical step-03":
    "V1FpTxOutputCanonicalS03",
  "V1 fraud-proof transaction-output-non-canonical step-04":
    "V1FpTxOutputCanonicalS04",
  "V1 fraud-proof resolved-output-non-canonical step-01":
    "V1FpResolvedOutputS01",
  "V1 fraud-proof resolved-output-non-canonical step-02":
    "V1FpResolvedOutputS02",
  "V1 fraud-proof resolved-output-non-canonical step-03":
    "V1FpResolvedOutputS03",
  "V1 fraud-proof resolved-output-non-canonical step-04":
    "V1FpResolvedOutputS04",
  "V1 fraud-proof resolved-output-non-canonical step-05":
    "V1FpResolvedOutputS05",
  "V1 fraud-proof mint-declared-asset-limit step-01": "V1FpMintAssetLimitS01",
  "V1 fraud-proof mint-declared-asset-limit step-02": "V1FpMintAssetLimitS02",
  "V1 fraud-proof mint-declared-asset-limit step-03": "V1FpMintAssetLimitS03",
  "V1 fraud-proof mint-declared-asset-limit step-04": "V1FpMintAssetLimitS04",
  "V1 fraud-proof spend-input-signer-missing step-01": "V1FpSpendSignerS01",
  "V1 fraud-proof spend-input-signer-missing step-02": "V1FpSpendSignerS02",
  "V1 fraud-proof spend-input-signer-missing step-03": "V1FpSpendSignerS03",
  "V1 fraud-proof spend-input-signer-missing step-04": "V1FpSpendSignerS04",
  "V1 fraud-proof spend-input-signer-missing step-05": "V1FpSpendSignerS05",
  "V1 fraud-proof protected-output-signer-missing step-01":
    "V1FpProtectedSignerS01",
  "V1 fraud-proof protected-output-signer-missing step-02":
    "V1FpProtectedSignerS02",
  "V1 fraud-proof protected-output-signer-missing step-03":
    "V1FpProtectedSignerS03",
  "V1 fraud-proof protected-output-signer-missing step-04":
    "V1FpProtectedSignerS04",
  "V1 fraud-proof protected-output-signer-missing step-05":
    "V1FpProtectedSignerS05",
  "V1 fraud-proof observers-forbidden-on-untagged-network step-01":
    "V1FpObserversForbiddenS01",
  "V1 fraud-proof observers-forbidden-on-untagged-network step-02":
    "V1FpObserversForbiddenS02",
  "V1 fraud-proof output-reference-script-decoding step-01":
    "V1FpOutputRefDecodeS01",
  "V1 fraud-proof output-reference-script-decoding step-02":
    "V1FpOutputRefDecodeS02",
  "V1 fraud-proof output-reference-script-decoding step-03":
    "V1FpOutputRefDecodeS03",
  "V1 fraud-proof output-reference-script-decoding step-04":
    "V1FpOutputRefDecodeS04",
  "V1 fraud-proof output-reference-script-decoding step-05":
    "V1FpOutputRefDecodeS05",
  "V1 fraud-proof output-reference-script-decoding step-06":
    "V1FpOutputRefDecodeS06",
  "V1 fraud-proof execution-source-script-decoding step-01":
    "V1FpExecSourceDecodeS01",
  "V1 fraud-proof execution-source-script-decoding step-02":
    "V1FpExecSourceDecodeS02",
  "V1 fraud-proof execution-source-script-decoding step-03":
    "V1FpExecSourceDecodeS03",
  "V1 fraud-proof execution-source-script-decoding step-04":
    "V1FpExecSourceDecodeS04",
  "V1 fraud-proof execution-source-script-decoding step-05":
    "V1FpExecSourceDecodeS05",
  "V1 fraud-proof observer-order-invalid step-01": "V1FpObserverOrderS01",
  "V1 fraud-proof observer-order-invalid step-02": "V1FpObserverOrderS02",
  "V1 fraud-proof observer-order-invalid step-03": "V1FpObserverOrderS03",
  "V1 fraud-proof observer-order-invalid step-04": "V1FpObserverOrderS04",
  "V1 fraud-proof redeemer-canonicity step-01": "V1FpRedeemerCanonS01",
  "V1 fraud-proof redeemer-canonicity step-02": "V1FpRedeemerCanonS02",
  "V1 fraud-proof redeemer-canonicity step-03": "V1FpRedeemerCanonS03",
  "V1 fraud-proof receive-purpose-language step-01": "V1FpReceivePurposeS01",
  "V1 fraud-proof receive-purpose-language step-02": "V1FpReceivePurposeS02",
  "V1 fraud-proof receive-purpose-language step-03": "V1FpReceivePurposeS03",
  "V1 fraud-proof unused-script-witness step-01": "V1FpUnusedScriptWitnessS01",
  "V1 fraud-proof unused-script-witness step-02": "V1FpUnusedScriptWitnessS02",
  "V1 fraud-proof unused-script-witness step-03": "V1FpUnusedScriptWitnessS03",
  "V1 fraud-proof unused-script-witness step-04": "V1FpUnusedScriptWitnessS04",
  "V1 fraud-proof unused-script-witness step-05": "V1FpUnusedScriptWitnessS05",
  "V1 fraud-proof unused-script-witness step-06": "V1FpUnusedScriptWitnessS06",
  "V1 fraud-proof missing-script-source step-01": "V1FpMissingScriptSourceS01",
  "V1 fraud-proof missing-script-source step-02": "V1FpMissingScriptSourceS02",
  "V1 fraud-proof missing-script-source step-03": "V1FpMissingScriptSourceS03",
  "V1 fraud-proof missing-script-source step-04": "V1FpMissingScriptSourceS04",
  "V1 fraud-proof missing-script-source step-05": "V1FpMissingScriptSourceS05",
  "V1 fraud-proof missing-script-source step-06": "V1FpMissingScriptSourceS06",
  "V1 fraud-proof missing-redeemer step-01": "V1FpMissingRedeemerS01",
  "V1 fraud-proof missing-redeemer step-02": "V1FpMissingRedeemerS02",
  "V1 fraud-proof missing-redeemer step-02a": "V1FpMissingRedeemerS02a",
  "V1 fraud-proof missing-redeemer step-02b": "V1FpMissingRedeemerS02b",
  "V1 fraud-proof missing-redeemer step-03": "V1FpMissingRedeemerS03",
  "V1 fraud-proof missing-redeemer step-04": "V1FpMissingRedeemerS04",
  "V1 fraud-proof missing-redeemer step-05": "V1FpMissingRedeemerS05",
  "V1 fraud-proof unused-redeemer step-01": "V1FpUnusedRedeemerS01",
  "V1 fraud-proof unused-redeemer step-02": "V1FpUnusedRedeemerS02",
  "V1 fraud-proof unused-redeemer step-02a": "V1FpUnusedRedeemerS02a",
  "V1 fraud-proof unused-redeemer step-02b": "V1FpUnusedRedeemerS02b",
  "V1 fraud-proof unused-redeemer step-02c": "V1FpUnusedRedeemerS02c",
  "V1 fraud-proof unused-redeemer step-03": "V1FpUnusedRedeemerS03",
  "V1 fraud-proof unused-redeemer step-04": "V1FpUnusedRedeemerS04",
  "V1 fraud-proof unused-redeemer step-05": "V1FpUnusedRedeemerS05",
  "V1 fraud-proof unused-redeemer step-06": "V1FpUnusedRedeemerS06",
  "V1 fraud-proof execution-native-script-invalid step-01":
    "V1FpExecNativeInvalidS01",
  "V1 fraud-proof execution-native-script-invalid step-02":
    "V1FpExecNativeInvalidS02",
  "V1 fraud-proof execution-native-script-invalid step-03":
    "V1FpExecNativeInvalidS03",
  "V1 fraud-proof execution-native-script-invalid step-04":
    "V1FpExecNativeInvalidS04",
  "V1 fraud-proof execution-native-script-invalid step-05":
    "V1FpExecNativeInvalidS05",
  "V1 fraud-proof execution-native-script-invalid step-06":
    "V1FpExecNativeInvalidS06",
  "V1 fraud-proof execution-native-script-invalid accepted-reconstruction-init":
    "V1FpExecNativeInvAccInit",
  "V1 fraud-proof execution-native-script-invalid accepted-spend-prefix":
    "V1FpExecNativeInvAccSpend",
  "V1 fraud-proof execution-native-script-invalid accepted-mint-prefix":
    "V1FpExecNativeInvAccMint",
  "V1 fraud-proof execution-native-script-invalid accepted-observer-prefix":
    "V1FpExecNativeInvAccObserver",
  "V1 fraud-proof execution-native-script-invalid accepted-receive-prefix":
    "V1FpExecNativeInvAccReceive",
  "V1 fraud-proof execution-native-script-invalid accepted-inline-source":
    "V1FpExecNativeInvAccInline",
  "V1 fraud-proof execution-native-script-invalid accepted-reference-source":
    "V1FpExecNativeInvAccRef",
  "V1 fraud-proof script-integrity-hash-mismatch step-01":
    "V1FpIntegrityMismatchS01",
  "V1 fraud-proof script-integrity-hash-mismatch step-02":
    "V1FpIntegrityMismatchS02",
  "V1 fraud-proof script-integrity-hash-mismatch step-03":
    "V1FpIntegrityMismatchS03",
  "V1 fraud-proof script-integrity-hash-mismatch step-04":
    "V1FpIntegrityMismatchS04",
  "V1 fraud-proof script-integrity-hash-mismatch step-05":
    "V1FpIntegrityMismatchS05",
  "V1 fraud-proof distinct-asset-accumulation-limit step-01":
    "V1FpDistinctAssetLimitS01",
  "V1 fraud-proof distinct-asset-accumulation-limit step-02":
    "V1FpDistinctAssetLimitS02",
  "V1 fraud-proof distinct-asset-accumulation-limit step-03":
    "V1FpDistinctAssetLimitS03",
  "V1 fraud-proof distinct-asset-accumulation-limit step-04":
    "V1FpDistinctAssetLimitS04",
  "V1 fraud-proof distinct-asset-accumulation-limit step-05":
    "V1FpDistinctAssetLimitS05",
  "V1 fraud-proof distinct-asset-accumulation-limit step-06":
    "V1FpDistinctAssetLimitS06",
  // Mirrors `midgard-core`'s
  // `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES` role for role: the
  // node's manifest verifier compares a manifest's
  // `referenceScriptAuthPolicy.tokenNames` against THIS vocabulary while
  // `midgard-core`'s compares it against its own, so a role present in one and
  // absent from the other makes every manifest unverifiable by one of the two.
  "availability-challenge spending": "AvailabilityChallengeSpend",
  "availability-challenge minting": "AvailabilityChallengeMint",
} as const;

export type ReferenceScriptAuthTokenTarget =
  keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES;

export type ReferenceScriptAuthPolicy = MintingValidator & {
  readonly expiresAtSlot: number;
  readonly expiresAtUnixTime: number;
  readonly timelockDurationMs: number;
};

export type ReferenceScriptAuthPolicyRef = Pick<
  ReferenceScriptAuthPolicy,
  "policyId"
>;

export type ReferenceScriptAuthMintingPolicy = MintingValidator & {
  readonly expiresAtUnixTime?: number;
};

export type ReferenceScriptAuthDeadlineDiagnostic = {
  readonly scopeName: string;
  readonly targetNames: readonly string[];
  readonly nowMs: number;
  readonly expiresAtUnixTime?: number;
  readonly remainingMs?: number;
  readonly minRemainingMs: number;
};

export type ReferenceScriptAuthPolicyDeploymentInfo = {
  readonly policyId: string;
  readonly nativeScript: {
    readonly type: "Native";
    readonly cborHex: string;
    readonly expiresAtSlot: number;
    readonly expiresAtUnixTime: number;
    readonly timelockDurationMs: number;
  };
  readonly tokenNames: Readonly<Record<ReferenceScriptAuthTokenTarget, string>>;
  readonly postTimelockAudit: {
    readonly required: true;
    readonly rule: string;
  };
};

export type ReferenceScriptTarget = {
  readonly name: string;
  readonly script: Script;
};

/**
 * Release-bound Cardano transaction envelope used for reference-script
 * publication. A script body at or above this size cannot possibly fit once
 * the output, funding input, auth mint and signature are added.
 */
export const REFERENCE_SCRIPT_PUBLICATION_L1_MAX_TX_BYTES_V1 = 16_384;

/**
 * Fail-fast lower-bound admission for production reference-script
 * publication. This deliberately does not claim that a smaller raw body fits:
 * the completed, signed transaction remains the authoritative fit check.
 */
export const assertReferenceScriptRawBodiesFitL1EnvelopeV1 = (
  targets: readonly ReferenceScriptTarget[],
  maxTxBytes = REFERENCE_SCRIPT_PUBLICATION_L1_MAX_TX_BYTES_V1,
): void => {
  for (const target of targets) {
    const rawScriptBytes = target.script.script.length / 2;
    if (rawScriptBytes >= maxTxBytes) {
      throw new StateQueueError({
        message:
          `${target.name} raw script is ${rawScriptBytes.toString()} bytes, ` +
          `exceeding the ${maxTxBytes.toString()}-byte L1 transaction envelope ` +
          `by at least ${(rawScriptBytes - maxTxBytes).toString()} bytes`,
        cause: "reference_script_raw_body_exceeds_l1_envelope_v1",
      });
    }
  }
};

export type ReferenceScriptResolved = {
  readonly name: string;
  readonly utxo: UTxO;
};

export type ReferenceScriptWalletReplenishmentTxParams = {
  readonly lucid: LucidEvolution;
  readonly selectedFundingInputs: readonly UTxO[];
  readonly referenceScriptAddress: string;
  readonly topUpAmount: bigint;
};

export type ReferenceScriptPublicationTxParams = {
  readonly lucid: LucidEvolution;
  readonly selectedFundingInputs: readonly UTxO[];
  readonly walletAddress: string;
  readonly referenceScriptsAddress: string;
  readonly missingTargets: readonly ReferenceScriptTarget[];
  readonly authPolicy: ReferenceScriptAuthMintingPolicy;
};

export type ReferenceScriptPublicationLayout = {
  readonly localReferenceOutputs: ReadonlyMap<string, Omit<UTxO, "txHash">>;
  readonly walletOutputs: readonly Omit<UTxO, "txHash">[];
};

export type BuiltReferenceScriptPublicationTx = {
  readonly tx: TxSignBuilder;
  readonly layout: ReferenceScriptPublicationLayout;
};

type TxCompleteOptions = NonNullable<Parameters<TxBuilder["complete"]>[0]>;

export const SCRIPT_REF_OUTPUT_LOVELACE = 4_000_000n;
export const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE = 10_000_000n;

export const referenceScriptAuthTokenNameText = (
  targetName: string,
): string => {
  const tokenName =
    REFERENCE_SCRIPT_AUTH_TOKEN_NAMES[
      targetName as ReferenceScriptAuthTokenTarget
    ];
  if (tokenName === undefined) {
    throw new Error(`Missing reference-script auth token name: ${targetName}`);
  }
  return tokenName;
};

export const referenceScriptAuthTokenName = (targetName: string): string =>
  fromText(referenceScriptAuthTokenNameText(targetName));

export const referenceScriptAuthUnit = (
  policyId: string,
  targetName: string,
): string => toUnit(policyId, referenceScriptAuthTokenName(targetName));

export const createReferenceScriptAuthPolicy = (
  lucid: LucidEvolution,
  nowMs: number = Date.now(),
  timelockDurationMs: number = REFERENCE_SCRIPT_AUTH_TIMELOCK_MS,
): ReferenceScriptAuthPolicy => {
  const expiresAtUnixTime = nowMs + timelockDurationMs;
  const expiresAtSlot = lucid.unixTimeToSlot(expiresAtUnixTime);
  const nativeScript = CML.NativeScript.new_script_invalid_hereafter(
    BigInt(expiresAtSlot),
  );
  const mintingScript: Script = {
    type: "Native",
    script: nativeScript.to_cbor_hex(),
  };
  return {
    mintingScriptCBOR: mintingScript.script,
    policyId: mintingPolicyToId(mintingScript),
    mintingScript,
    expiresAtSlot,
    expiresAtUnixTime,
    timelockDurationMs,
  };
};

export const referenceScriptAuthPolicyFromDeploymentInfo = (
  deploymentInfo: ReferenceScriptAuthPolicyDeploymentInfo,
): ReferenceScriptAuthPolicy => ({
  mintingScriptCBOR: deploymentInfo.nativeScript.cborHex,
  mintingScript: {
    type: "Native",
    script: deploymentInfo.nativeScript.cborHex,
  },
  policyId: deploymentInfo.policyId,
  expiresAtSlot: deploymentInfo.nativeScript.expiresAtSlot,
  expiresAtUnixTime: deploymentInfo.nativeScript.expiresAtUnixTime,
  timelockDurationMs: deploymentInfo.nativeScript.timelockDurationMs,
});

export const referenceScriptAuthRemainingMs = (
  policy: ReferenceScriptAuthMintingPolicy,
  nowMs: number,
): number | undefined =>
  policy.expiresAtUnixTime === undefined
    ? undefined
    : policy.expiresAtUnixTime - nowMs;

export class ReferenceScriptAuthDeadlineError extends Error {
  readonly diagnostic: ReferenceScriptAuthDeadlineDiagnostic;

  constructor(diagnostic: ReferenceScriptAuthDeadlineDiagnostic) {
    const remaining =
      diagnostic.remainingMs === undefined
        ? "missing"
        : diagnostic.remainingMs.toString();
    super(
      [
        `Reference-script auth policy is not safe to use for ${diagnostic.scopeName}`,
        `now_ms=${diagnostic.nowMs.toString()}`,
        `expires_at_unix_time=${
          diagnostic.expiresAtUnixTime === undefined
            ? "missing"
            : diagnostic.expiresAtUnixTime.toString()
        }`,
        `remaining_ms=${remaining}`,
        `min_remaining_ms=${diagnostic.minRemainingMs.toString()}`,
        `targets=${diagnostic.targetNames.join(",")}`,
      ].join("; "),
    );
    this.name = "ReferenceScriptAuthDeadlineError";
    this.diagnostic = diagnostic;
  }
}

export const assertReferenceScriptAuthMinimumRemaining = ({
  policy,
  nowMs,
  minRemainingMs,
  scopeName,
  targetNames,
}: {
  readonly policy: ReferenceScriptAuthMintingPolicy;
  readonly nowMs: number;
  readonly minRemainingMs: number;
  readonly scopeName: string;
  readonly targetNames: readonly string[];
}): void => {
  if (!Number.isSafeInteger(minRemainingMs) || minRemainingMs <= 0) {
    throw new Error(
      "REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS must be a positive safe integer",
    );
  }
  const remainingMs = referenceScriptAuthRemainingMs(policy, nowMs);
  if (remainingMs === undefined || remainingMs <= minRemainingMs) {
    throw new ReferenceScriptAuthDeadlineError({
      scopeName,
      targetNames,
      nowMs,
      expiresAtUnixTime: policy.expiresAtUnixTime,
      remainingMs,
      minRemainingMs,
    });
  }
};

export const referenceScriptAuthPolicyDeploymentInfo = (
  policy: ReferenceScriptAuthPolicy,
): ReferenceScriptAuthPolicyDeploymentInfo => {
  if (policy.mintingScript.type !== "Native") {
    throw new Error("Reference-script auth policy must be a native script");
  }
  return {
    policyId: policy.policyId,
    nativeScript: {
      type: "Native",
      cborHex: policy.mintingScript.script,
      expiresAtSlot: policy.expiresAtSlot,
      expiresAtUnixTime: policy.expiresAtUnixTime,
      timelockDurationMs: policy.timelockDurationMs,
    },
    tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    postTimelockAudit: {
      required: true,
      rule: "After the timelock expires, verify there is exactly one role token under this policy for every listed token name before treating the deployment as production-ready.",
    },
  };
};

export const referenceScriptRoleAssets = (
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): Assets => ({
  lovelace: SCRIPT_REF_OUTPUT_LOVELACE,
  [referenceScriptAuthUnit(authPolicy.policyId, target.name)]: 1n,
});

export const isSameScriptRef = (
  left: Script | null | undefined,
  right: Script,
): boolean => {
  if (left === undefined || left === null || left.type !== right.type) {
    return false;
  }
  try {
    return validatorToScriptHash(left) === validatorToScriptHash(right);
  } catch {
    return false;
  }
};

export const hasReferenceScriptAuthRole = (
  utxo: UTxO,
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): boolean =>
  utxo.assets[referenceScriptAuthUnit(authPolicy.policyId, target.name)] === 1n;

export const referenceScriptPublicationFundingTarget = (
  missingTargetCount: number,
): bigint =>
  SCRIPT_REF_OUTPUT_LOVELACE * (BigInt(missingTargetCount) + 1n) +
  SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;

const lovelaceOf = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

const isPlainAdaOnlyUtxo = isPlainPositiveAdaOnlyUtxo;

export const orderReferenceScriptFundingUtxos = (
  utxos: readonly UTxO[],
): readonly UTxO[] =>
  [...utxos].sort((left, right) => {
    const leftIsPlain = isPlainAdaOnlyUtxo(left);
    const rightIsPlain = isPlainAdaOnlyUtxo(right);
    if (leftIsPlain !== rightIsPlain) {
      return leftIsPlain ? -1 : 1;
    }
    const leftLovelace = lovelaceOf(left);
    const rightLovelace = lovelaceOf(right);
    if (leftLovelace === rightLovelace) {
      return compareOutRefs(left, right);
    }
    return leftLovelace > rightLovelace ? -1 : 1;
  });

export const selectReferenceScriptFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] => {
  if (targetLovelace <= 0n) {
    return [];
  }
  const selected: UTxO[] = [];
  let covered = 0n;
  for (const utxo of orderReferenceScriptFundingUtxos(utxos)) {
    if (!isPlainAdaOnlyUtxo(utxo)) {
      continue;
    }
    selected.push(utxo);
    covered += lovelaceOf(utxo);
    if (covered >= targetLovelace) {
      return selected;
    }
  }
  return [];
};

const completeWithSelectedFundingInputs = (
  selectedFundingInputs: readonly UTxO[],
): TxCompleteOptions => ({
  coinSelection: false,
  localUPLCEval: true,
  presetWalletInputs: [...selectedFundingInputs],
});

export const incompleteReferenceScriptWalletReplenishmentTxProgram = ({
  lucid,
  selectedFundingInputs,
  referenceScriptAddress,
  topUpAmount,
}: ReferenceScriptWalletReplenishmentTxParams): Effect.Effect<
  TxBuilder,
  LucidError
> =>
  Effect.try({
    try: () =>
      lucid
        .newTx()
        .collectFrom([...selectedFundingInputs])
        .pay.ToAddress(referenceScriptAddress, { lovelace: topUpAmount }),
    catch: (cause) =>
      new LucidError({
        message: `Failed to build reference-script wallet replenishment transaction: ${String(cause)}`,
        cause,
      }),
  });

export const completeReferenceScriptWalletReplenishmentTxProgram = (
  params: ReferenceScriptWalletReplenishmentTxParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const tx =
      yield* incompleteReferenceScriptWalletReplenishmentTxProgram(params);
    return yield* Effect.tryPromise({
      try: () =>
        tx.complete(
          completeWithSelectedFundingInputs(params.selectedFundingInputs),
        ),
      catch: (cause) =>
        new LucidError({
          message: `Failed to complete reference-script wallet replenishment transaction: ${String(cause)}`,
          cause,
        }),
    });
  });

export const incompleteReferenceScriptPublicationTxProgram = ({
  lucid,
  selectedFundingInputs,
  walletAddress,
  referenceScriptsAddress,
  missingTargets,
  authPolicy,
}: ReferenceScriptPublicationTxParams): Effect.Effect<TxBuilder, LucidError> =>
  Effect.try({
    try: () => {
      const roleMintAssets: Assets = {};
      for (const target of missingTargets) {
        roleMintAssets[
          referenceScriptAuthUnit(authPolicy.policyId, target.name)
        ] = 1n;
      }
      let tx = lucid.newTx().collectFrom([...selectedFundingInputs]);
      tx =
        authPolicy.mintingScript.type === "Native"
          ? tx.mintAssets(roleMintAssets)
          : tx.mintAssets(roleMintAssets, Data.void());
      tx = tx.attach.MintingPolicy(authPolicy.mintingScript);
      if (authPolicy.expiresAtUnixTime !== undefined) {
        tx = tx.validTo(authPolicy.expiresAtUnixTime - 1);
      }
      tx = tx.pay.ToAddressWithData(walletAddress, undefined, {
        lovelace: SCRIPT_REF_OUTPUT_LOVELACE,
      });
      for (const target of missingTargets) {
        tx = tx.pay.ToAddressWithData(
          referenceScriptsAddress,
          undefined,
          referenceScriptRoleAssets(target, authPolicy),
          target.script,
        );
      }
      return tx;
    },
    catch: (cause) =>
      new LucidError({
        message: `Failed to build reference-script publication transaction for ${missingTargets
          .map(({ name }) => name)
          .join(", ")}: ${String(cause)}`,
        cause,
      }),
  });

export const resolveReferenceScriptPublicationLayout = (
  tx: TxSignBuilder,
  params: Pick<
    ReferenceScriptPublicationTxParams,
    | "walletAddress"
    | "referenceScriptsAddress"
    | "missingTargets"
    | "authPolicy"
  >,
): Effect.Effect<ReferenceScriptPublicationLayout, StateQueueError> =>
  Effect.try({
    try: () => {
      const publicationOutputs = tx.toTransaction().body().outputs();
      const localReferenceOutputs = new Map<string, Omit<UTxO, "txHash">>();
      const walletOutputs: Omit<UTxO, "txHash">[] = [];
      for (
        let outputIndex = 0;
        outputIndex < publicationOutputs.len();
        outputIndex += 1
      ) {
        const output = coreToTxOutput(publicationOutputs.get(outputIndex));
        if (output.address === params.walletAddress) {
          walletOutputs.push({
            outputIndex,
            address: output.address,
            assets: output.assets,
            datum: output.datum ?? undefined,
            datumHash: output.datumHash ?? undefined,
            scriptRef: output.scriptRef ?? undefined,
          });
        }
        if (output.address !== params.referenceScriptsAddress) {
          continue;
        }
        if (output.scriptRef === undefined) {
          continue;
        }
        const matchingTarget = params.missingTargets.find(
          (target) =>
            !localReferenceOutputs.has(target.name) &&
            isSameScriptRef(output.scriptRef, target.script) &&
            output.assets[
              referenceScriptAuthUnit(params.authPolicy.policyId, target.name)
            ] === 1n,
        );
        if (matchingTarget === undefined) {
          continue;
        }
        localReferenceOutputs.set(matchingTarget.name, {
          outputIndex,
          address: output.address,
          assets: output.assets,
          datum: output.datum ?? undefined,
          datumHash: output.datumHash ?? undefined,
          scriptRef: output.scriptRef,
        });
      }
      return {
        localReferenceOutputs,
        walletOutputs,
      };
    },
    catch: (cause) =>
      new StateQueueError({
        message: "Failed to resolve reference-script publication layout",
        cause,
      }),
  });

export const completeReferenceScriptPublicationTxProgram = (
  params: ReferenceScriptPublicationTxParams,
): Effect.Effect<
  BuiltReferenceScriptPublicationTx,
  LucidError | StateQueueError
> =>
  Effect.gen(function* () {
    const tx = yield* incompleteReferenceScriptPublicationTxProgram(params);
    const unsigned = yield* Effect.tryPromise({
      try: () =>
        tx.complete(
          completeWithSelectedFundingInputs(params.selectedFundingInputs),
        ),
      catch: (cause) =>
        new LucidError({
          message: `Failed to complete reference-script publication transaction for ${params.missingTargets
            .map(({ name }) => name)
            .join(", ")}: ${String(cause)}`,
          cause,
        }),
    });
    const layout = yield* resolveReferenceScriptPublicationLayout(
      unsigned,
      params,
    );
    return { tx: unsigned, layout };
  });
