import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  MIDGARD_RELEASE_EVIDENCE_DIGEST,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestId as computeDeploymentManifestV1Id,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  type DeploymentManifestAvailabilityChallenge,
  type DeploymentManifestCardanoProtocolParameters,
  type DeploymentManifestEconomics,
  type DeploymentManifestFraudProofCatalogueCategory,
  type DeploymentManifestFraudProofCatalogueCategoryIdentity,
  type DeploymentManifestJsonValue,
  type DeploymentManifestL1Finality,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestEconomics,
  verifyDeploymentManifestFraudProofCatalogueIdentity,
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  hashHexWithBlake2b,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

export {
  computeDeploymentManifestJsonDigest,
  normalizeDeploymentManifestJsonValue,
};
export type { DeploymentManifestJsonValue };

export const DEPLOYMENT_MANIFEST_SCHEMA_VERSION =
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION;

export const REQUIRED_TRANSACTION_ORDER_CONTRACTS = Object.freeze([
  "txOrderSpend",
  "txOrderMint",
  // #579: all three retired tx-field names are removed here too. This vector
  // gates what a transaction-order deployment is REQUIRED to carry, so keeping a
  // name the regenerated blueprint cannot resolve would make every deployment
  // fail a requirement it has no way to satisfy.
  "cekProgramMaterialSpend",
  "validationTraceDispute",
  "validationTraceDisputeSource",
  "validationTraceDisputeGame",
  "validationTraceDisputeBoundary",
  "validationTraceDisputeTimeout",
  "validationTraceDisputeAward",
] as const);

export const DEPLOYMENT_MANIFEST_CONTRACT_NAMES = Object.freeze([
  "referenceScriptAuthMint",
  "hubOracleMint",
  "daParamsGovernorSpend",
  "daParamsGovernorMint",
  "daAttestationSpend",
  "daAttestationMint",
  "stateQueueSpend",
  "stateQueueMint",
  "schedulerSpend",
  "schedulerMint",
  "registeredOperatorsSpend",
  "registeredOperatorsMint",
  "activeOperatorsSpend",
  "activeOperatorsMint",
  "retiredOperatorsSpend",
  "retiredOperatorsMint",
  "escapeHatchSpend",
  "escapeHatchMint",
  "fraudProofCatalogueSpend",
  "fraudProofCatalogueMint",
  "fraudProofSpend",
  "fraudProofMint",
  "depositSpend",
  "depositMint",
  "withdrawalSpend",
  "withdrawalMint",
  "txOrderSpend",
  "txOrderMint",
  // #579. Mirrors `midgard-core`'s three-name removal element for element — see
  // the cause recorded there. ABI-01 fails closed if the two ever diverge.
  "cekProgramMaterialSpend",
  "settlementSpend",
  "settlementMint",
  "payoutSpend",
  "payoutMint",
  "reserveSpend",
  "reserveWithdraw",
  "phasMembershipWithdraw",
  "fraudProofDoubleSpend",
  "fraudProofNonExistentInput",
  "fraudProofNonExistentInputNoIndex",
  "fraudProofInvalidRange",
  "fraudProofTransitionTrace",
  "fraudProofZeroInput",
  "validationTraceDispute",
  "validationTraceDisputeSource",
  "validationTraceDisputeGame",
  "validationTraceDisputeBoundary",
  "validationTraceDisputeTimeout",
  "validationTraceDisputeAward",
  "fraudProofDaHashPreimage",
  "fraudProofNoReferenceInput",
  "fraudProofReferenceInputNoIdx",
  "fraudProofInvalidSignature",
  // #579. Mirrors `midgard-core`'s vector element for element; ABI-01 fails
  // closed if these two declarations ever diverge. Appended, not inserted:
  // this vector is positional.
  "fieldPreimageCertificateSpend",
  "fieldPreimageCertificateMint",
  "fraudProofFabricatedDeposit",
  "fraudProofFabricatedDepositStep02",
  "fraudProofFabricatedDepositStep03",
  "fraudProofFabricatedDepositStep04",
  "fraudProofFabricatedWithdrawal",
  "fraudProofFabricatedWithdrawalStep02",
  "fraudProofFabricatedWithdrawalStep03",
  "fraudProofFabricatedWithdrawalStep04",
  "fraudProofNativeScriptDecoding",
  "fraudProofNativeScriptDecodingStep02",
  "fraudProofNativeScriptDecodingStep03OpenSubject",
  "fraudProofNativeScriptDecodingStep03BindDescriptor",
  "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
  "fraudProofNativeScriptDecodingStep04",
  "fraudProofMissingSignature",
  "fraudProofMissingSignatureStep02",
  "fraudProofMissingSignatureStep03",
  "fraudProofMissingSignatureStep04",
  "fraudProofMissingNativeScriptTx",
  "fraudProofMissingNativeScriptTxStep02",
  "fraudProofMissingNativeScriptTxStep03",
  "fraudProofMissingNativeScriptTxStep04",
  "fraudProofMissingNativeScriptTxStep05",
  "fraudProofMissingNativeScriptTxStep06",
  "fraudProofWithdrawnReferenceInput",
  "fraudProofWithdrawnReferenceInputStep02",
  "fraudProofWithdrawnReferenceInputStep03",
  "fraudProofCanonicalDecodability",
  "fraudProofCanonicalDecodabilityStep02",
  "fraudProofCommittedFieldShape",
  "fraudProofCommittedFieldShapeStep02",
  "fraudProofMinFee",
  "fraudProofMinFeeStep02",
  "fraudProofWithdrawalMistag",
  "fraudProofWithdrawalMistagStep02",
  "fraudProofWithdrawalMistagStep03",
  "fraudProofWithdrawalMistagStep04",
  "fraudProofWithdrawalMistagStep05",
  "fraudProofDoubleWithdraw",
  "fraudProofDoubleWithdrawStep02",
  "fraudProofCrossBlockDuplicateEvent",
  "fraudProofCrossBlockDuplicateEventStep02",
  "fraudProofL2TxMistag",
  "fraudProofL2TxMistagStep02",
  "fraudProofWithdrawnInput",
  "fraudProofWithdrawnInputStep02",
  "fraudProofWithdrawnInputStep03",
  "fraudProofValueNotPreserved",
  "fraudProofValueNotPreservedStep02",
  "fraudProofValueNotPreservedStep03",
  "fraudProofValueNotPreservedStep04",
  "fraudProofInputSetUniqueness",
  "fraudProofInputSetUniquenessStep02",
  "fraudProofInputSetUniquenessStep03",
  "fraudProofInputSetUniquenessStep04",
  "fraudProofMintAuthorization",
  "fraudProofMintAuthorizationStep02",
  "fraudProofMintAuthorizationStep03",
  "fraudProofMintAuthorizationStep04",
  "fraudProofMintAuthorizationStep05",
  "fraudProofTransitionTraceControl",
  "fraudProofTransitionTraceSource",
  "fraudProofTransitionTraceWithdrawal",
  "fraudProofTransitionTraceForced",
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
  "fraudProofTransitionTraceL1Event",
  "fraudProofTransitionTraceDuplicate",
  "fraudProofNetworkId",
  "fraudProofNetworkIdStep02",
  "computationThreadMint",
  "chunkedVerifyWithdraw",
  "pexcludesWithdraw",
  "fraudProofDoubleSpendStep02",
  "fraudProofDoubleSpendStep03",
  "fraudProofDoubleSpendStep04",
  "fraudProofNonExistentInputStep02",
  "fraudProofNonExistentInputStep03",
  "fraudProofNonExistentInputStep04",
  "fraudProofNonExistentInputNoIndexStep02",
  "fraudProofNonExistentInputNoIndexStep03",
  "fraudProofNonExistentInputNoIndexStep04",
  "fraudProofInvalidRangeStep02",
  "fraudProofZeroInputStep02",
  "fraudProofDaHashPreimageStep02",
  "fraudProofNoReferenceInputStep02",
  "fraudProofNoReferenceInputStep03",
  "fraudProofNoReferenceInputStep04",
  "fraudProofReferenceInputNoIdxStep02",
  "fraudProofReferenceInputNoIdxStep03",
  "fraudProofReferenceInputNoIdxStep04",
  "fraudProofInvalidSignatureStep02",
  "fraudProofMissingNativeScriptTxStep07",
  "fraudProofMissingNativeScriptTxStep08",
  "fraudProofMissingNativeScriptUtxo",
  "fraudProofMissingNativeScriptUtxoStep02",
  "fraudProofMissingNativeScriptUtxoStep03",
  "fraudProofMissingNativeScriptUtxoStep04",
  "fraudProofMissingNativeScriptUtxoStep05",
  "fraudProofNativeScriptInvalid",
  "fraudProofNativeScriptInvalidStep02",
  "fraudProofNativeScriptInvalidStep03",
  "fraudProofMinAda",
  "fraudProofMinAdaStep02",
  "fraudProofMinAdaStep02TxWithdraw",
  "fraudProofMinAdaStep02UtxoWithdraw",
  "correctionLockSpend",
  "fraudProofMinAdaStep03",
  "fraudProofMinAdaStep04",
  "fraudProofMinAdaStep05",
  "fraudProofFieldPreimageLengthMismatch",
  "fraudProofFieldPreimageLengthMismatchStep02Accepted",
  "fraudProofFieldPreimageLengthMismatchStep02Forced",
  "fraudProofFieldPreimageLengthMismatchStep03",
  "fraudProofFieldItemWidthIllegal",
  "fraudProofFieldItemWidthIllegalStep02",
  "fraudProofFieldItemWidthIllegalStep03",
  "fraudProofWitnessScriptDecoding",
  "fraudProofWitnessScriptDecodingStep02",
  "fraudProofWitnessScriptDecodingStep03",
  "fraudProofWitnessScriptDecodingStep04",
  "fraudProofScriptIntegrityHashMissing",
  "fraudProofScriptIntegrityHashMissingStep02",
  "fraudProofScriptIntegrityHashMissingStep03",
  "fraudProofScriptIntegrityHashMissingScriptGrammar",
  "fraudProofScriptIntegrityHashMissingScriptScan",
  "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
  "fraudProofScriptIntegrityHashMissingStep04",
  "fraudProofTransactionOutputNonCanonical",
  "fraudProofTransactionOutputNonCanonicalStep02",
  "fraudProofTransactionOutputNonCanonicalStep03",
  "fraudProofTransactionOutputNonCanonicalStep04",
  "fraudProofResolvedOutputNonCanonical",
  "fraudProofResolvedOutputNonCanonicalStep02",
  "fraudProofResolvedOutputNonCanonicalStep03",
  "fraudProofResolvedOutputNonCanonicalStep04",
  "fraudProofResolvedOutputNonCanonicalStep05",
  "fraudProofMintDeclaredAssetLimit",
  "fraudProofMintDeclaredAssetLimitStep02",
  "fraudProofMintDeclaredAssetLimitStep03",
  "fraudProofMintDeclaredAssetLimitStep04",
  "fraudProofSpendInputSignerMissing",
  "fraudProofSpendInputSignerMissingStep02",
  "fraudProofSpendInputSignerMissingStep03",
  "fraudProofSpendInputSignerMissingStep04",
  "fraudProofSpendInputSignerMissingStep05",
  "fraudProofProtectedOutputSignerMissing",
  "fraudProofProtectedOutputSignerMissingStep02",
  "fraudProofProtectedOutputSignerMissingStep03",
  "fraudProofProtectedOutputSignerMissingStep04",
  "fraudProofProtectedOutputSignerMissingStep05",
  "fraudProofObserversForbiddenOnUntaggedNetwork",
  "fraudProofObserversForbiddenOnUntaggedNetworkStep02",
  "fraudProofOutputReferenceScriptDecoding",
  "fraudProofOutputReferenceScriptDecodingStep02",
  "fraudProofOutputReferenceScriptDecodingStep03",
  "fraudProofOutputReferenceScriptDecodingStep04",
  "fraudProofOutputReferenceScriptDecodingStep05",
  "fraudProofOutputReferenceScriptDecodingStep06",
  "fraudProofExecutionSourceScriptDecoding",
  "fraudProofExecutionSourceScriptDecodingStep02",
  "fraudProofExecutionSourceScriptDecodingStep03",
  "fraudProofExecutionSourceScriptDecodingStep04",
  "fraudProofExecutionSourceScriptDecodingStep05",
  "fraudProofObserverOrderInvalid",
  "fraudProofObserverOrderInvalidStep02",
  "fraudProofObserverOrderInvalidStep03",
  "fraudProofObserverOrderInvalidStep04",
  "fraudProofRedeemerCanonicity",
  "fraudProofRedeemerCanonicityStep02",
  "fraudProofRedeemerCanonicityStep03",
  "fraudProofReceivePurposeLanguage",
  "fraudProofReceivePurposeLanguageStep02",
  "fraudProofReceivePurposeLanguageStep03",
  "fraudProofUnusedScriptWitness",
  "fraudProofUnusedScriptWitnessStep02",
  "fraudProofUnusedScriptWitnessStep03",
  "fraudProofUnusedScriptWitnessStep04",
  "fraudProofUnusedScriptWitnessStep05",
  "fraudProofUnusedScriptWitnessStep06",
  "fraudProofMissingScriptSource",
  "fraudProofMissingScriptSourceStep02",
  "fraudProofMissingScriptSourceStep03",
  "fraudProofMissingScriptSourceStep04",
  "fraudProofMissingScriptSourceStep05",
  "fraudProofMissingScriptSourceStep06",
  "fraudProofMissingRedeemer",
  "fraudProofMissingRedeemerStep02",
  "fraudProofMissingRedeemerStep02a",
  "fraudProofMissingRedeemerStep02b",
  "fraudProofMissingRedeemerStep03",
  "fraudProofMissingRedeemerStep04",
  "fraudProofMissingRedeemerStep05",
  "fraudProofUnusedRedeemer",
  "fraudProofUnusedRedeemerStep02",
  "fraudProofUnusedRedeemerStep02a",
  "fraudProofUnusedRedeemerStep02b",
  "fraudProofUnusedRedeemerStep02c",
  "fraudProofUnusedRedeemerStep03",
  "fraudProofUnusedRedeemerStep04",
  "fraudProofUnusedRedeemerStep05",
  "fraudProofUnusedRedeemerStep06",
  "fraudProofExecutionNativeScriptInvalid",
  "fraudProofExecutionNativeScriptInvalidStep02",
  "fraudProofExecutionNativeScriptInvalidStep03",
  "fraudProofExecutionNativeScriptInvalidStep04",
  "fraudProofExecutionNativeScriptInvalidStep05",
  "fraudProofExecutionNativeScriptInvalidStep06",
  "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
  "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
  "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
  "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
  "fraudProofScriptIntegrityHashMismatch",
  "fraudProofScriptIntegrityHashMismatchStep02",
  "fraudProofScriptIntegrityHashMismatchStep03",
  "fraudProofScriptIntegrityHashMismatchStep04",
  "fraudProofScriptIntegrityHashMismatchStep05",
  "fraudProofDistinctAssetAccumulationLimit",
  "fraudProofDistinctAssetAccumulationLimitStep02",
  "fraudProofDistinctAssetAccumulationLimitStep03",
  "fraudProofDistinctAssetAccumulationLimitStep04",
  "fraudProofDistinctAssetAccumulationLimitStep05",
  "fraudProofDistinctAssetAccumulationLimitStep06",
  "availabilityChallengeSpend",
  "availabilityChallengeMint",
  "stateQueueCommitWithdraw",
  "stateQueueUnattestedTimeoutWithdraw",
  "stateQueueUnavailableTimeoutWithdraw",
  "stateQueueFraudRemovalWithdraw",
  "stateQueueMergeWithdraw",
] as const);

export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze({
    "reference-script-auth minting": "referenceScriptAuthMint",
    "hub-oracle minting": "hubOracleMint",
    "da-params-governor spending": "daParamsGovernorSpend",
    "da-params-governor minting": "daParamsGovernorMint",
    "da-attestation spending": "daAttestationSpend",
    "da-attestation minting": "daAttestationMint",
    "state-queue spending": "stateQueueSpend",
    "state-queue minting": "stateQueueMint",
    "state-queue commit withdrawal": "stateQueueCommitWithdraw",
    "state-queue unattested-timeout withdrawal":
      "stateQueueUnattestedTimeoutWithdraw",
    "state-queue unavailable-timeout withdrawal":
      "stateQueueUnavailableTimeoutWithdraw",
    "state-queue fraud-removal withdrawal": "stateQueueFraudRemovalWithdraw",
    "state-queue merge withdrawal": "stateQueueMergeWithdraw",
    "scheduler spending": "schedulerSpend",
    "scheduler minting": "schedulerMint",
    "registered-operators spending": "registeredOperatorsSpend",
    "registered-operators minting": "registeredOperatorsMint",
    "active-operators spending": "activeOperatorsSpend",
    "active-operators minting": "activeOperatorsMint",
    "retired-operators spending": "retiredOperatorsSpend",
    "retired-operators minting": "retiredOperatorsMint",
    "fraud-proof-catalogue minting": "fraudProofCatalogueMint",
    "deposit spending": "depositSpend",
    "deposit minting": "depositMint",
    "withdrawal spending": "withdrawalSpend",
    "withdrawal minting": "withdrawalMint",
    "settlement minting": "settlementMint",
    "payout spending": "payoutSpend",
    "payout minting": "payoutMint",
    "reserve spending": "reserveSpend",
    "reserve observer": "reserveWithdraw",
    "membership proof withdrawal": "phasMembershipWithdraw",
    // #579. Mirrors `midgard-core`'s role-map removal; ABI-03 fails closed on
    // divergence.
    "V1 field-preimage certificate": "fieldPreimageCertificateSpend",
    "V1 field-preimage certificate minting": "fieldPreimageCertificateMint",
    "V1 immutable CEK program-material publication": "cekProgramMaterialSpend",
    "V1 validation-trace dispute": "validationTraceDispute",
    "V1 validation-trace source": "validationTraceDisputeSource",
    "V1 validation-trace game": "validationTraceDisputeGame",
    "V1 validation-trace boundary": "validationTraceDisputeBoundary",
    "V1 validation-trace timeout": "validationTraceDisputeTimeout",
    "V1 validation-trace award": "validationTraceDisputeAward",
    "V1 fraud-proof fabricated-deposit step-01": "fraudProofFabricatedDeposit",
    "V1 fraud-proof fabricated-deposit step-02":
      "fraudProofFabricatedDepositStep02",
    "V1 fraud-proof fabricated-deposit step-03":
      "fraudProofFabricatedDepositStep03",
    "V1 fraud-proof fabricated-deposit step-04":
      "fraudProofFabricatedDepositStep04",
    "V1 fraud-proof fabricated-withdrawal step-01":
      "fraudProofFabricatedWithdrawal",
    "V1 fraud-proof fabricated-withdrawal step-02":
      "fraudProofFabricatedWithdrawalStep02",
    "V1 fraud-proof fabricated-withdrawal step-03":
      "fraudProofFabricatedWithdrawalStep03",
    "V1 fraud-proof fabricated-withdrawal step-04":
      "fraudProofFabricatedWithdrawalStep04",
    "V1 fraud-proof native-script-decoding step-01":
      "fraudProofNativeScriptDecoding",
    "V1 fraud-proof native-script-decoding step-02":
      "fraudProofNativeScriptDecodingStep02",
    "V1 fraud-proof native-script-decoding step-03 open-subject":
      "fraudProofNativeScriptDecodingStep03OpenSubject",
    "V1 fraud-proof native-script-decoding step-03 bind-descriptor":
      "fraudProofNativeScriptDecodingStep03BindDescriptor",
    "V1 fraud-proof native-script-decoding step-03 advance-or-close":
      "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
    "V1 fraud-proof native-script-decoding step-04":
      "fraudProofNativeScriptDecodingStep04",
    "V1 fraud-proof missing-signature step-01": "fraudProofMissingSignature",
    "V1 fraud-proof missing-signature step-02":
      "fraudProofMissingSignatureStep02",
    "V1 fraud-proof missing-signature step-03":
      "fraudProofMissingSignatureStep03",
    "V1 fraud-proof missing-signature step-04":
      "fraudProofMissingSignatureStep04",
    "V1 fraud-proof missing-native-script-tx step-01":
      "fraudProofMissingNativeScriptTx",
    "V1 fraud-proof missing-native-script-tx step-02":
      "fraudProofMissingNativeScriptTxStep02",
    "V1 fraud-proof missing-native-script-tx step-03":
      "fraudProofMissingNativeScriptTxStep03",
    "V1 fraud-proof missing-native-script-tx step-04":
      "fraudProofMissingNativeScriptTxStep04",
    "V1 fraud-proof missing-native-script-tx step-05":
      "fraudProofMissingNativeScriptTxStep05",
    "V1 fraud-proof missing-native-script-tx step-06":
      "fraudProofMissingNativeScriptTxStep06",
    "V1 fraud-proof withdrawn-reference-input step-01":
      "fraudProofWithdrawnReferenceInput",
    "V1 fraud-proof withdrawn-reference-input step-02":
      "fraudProofWithdrawnReferenceInputStep02",
    "V1 fraud-proof withdrawn-reference-input step-03":
      "fraudProofWithdrawnReferenceInputStep03",
    "V1 fraud-proof canonical-decodability step-01":
      "fraudProofCanonicalDecodability",
    "V1 fraud-proof canonical-decodability step-02":
      "fraudProofCanonicalDecodabilityStep02",
    "V1 fraud-proof committed-field-shape step-01":
      "fraudProofCommittedFieldShape",
    "V1 fraud-proof committed-field-shape step-02":
      "fraudProofCommittedFieldShapeStep02",
    "V1 fraud-proof min-fee step-01": "fraudProofMinFee",
    "V1 fraud-proof min-fee step-02": "fraudProofMinFeeStep02",
    "V1 fraud-proof withdrawal-mistag step-01": "fraudProofWithdrawalMistag",
    "V1 fraud-proof withdrawal-mistag step-02":
      "fraudProofWithdrawalMistagStep02",
    "V1 fraud-proof withdrawal-mistag step-03":
      "fraudProofWithdrawalMistagStep03",
    "V1 fraud-proof withdrawal-mistag step-04":
      "fraudProofWithdrawalMistagStep04",
    "V1 fraud-proof withdrawal-mistag step-05":
      "fraudProofWithdrawalMistagStep05",
    "V1 fraud-proof double-withdraw step-01": "fraudProofDoubleWithdraw",
    "V1 fraud-proof double-withdraw step-02": "fraudProofDoubleWithdrawStep02",
    "V1 fraud-proof cross-block-duplicate-event step-01":
      "fraudProofCrossBlockDuplicateEvent",
    "V1 fraud-proof cross-block-duplicate-event step-02":
      "fraudProofCrossBlockDuplicateEventStep02",
    "V1 fraud-proof l2-tx-mistag step-01": "fraudProofL2TxMistag",
    "V1 fraud-proof l2-tx-mistag step-02": "fraudProofL2TxMistagStep02",
    "V1 fraud-proof withdrawn-input step-01": "fraudProofWithdrawnInput",
    "V1 fraud-proof withdrawn-input step-02": "fraudProofWithdrawnInputStep02",
    "V1 fraud-proof withdrawn-input step-03": "fraudProofWithdrawnInputStep03",
    "V1 fraud-proof value-not-preserved step-01": "fraudProofValueNotPreserved",
    "V1 fraud-proof value-not-preserved step-02":
      "fraudProofValueNotPreservedStep02",
    "V1 fraud-proof value-not-preserved step-03":
      "fraudProofValueNotPreservedStep03",
    "V1 fraud-proof value-not-preserved step-04":
      "fraudProofValueNotPreservedStep04",
    "V1 fraud-proof input-set-uniqueness step-01":
      "fraudProofInputSetUniqueness",
    "V1 fraud-proof input-set-uniqueness step-02":
      "fraudProofInputSetUniquenessStep02",
    "V1 fraud-proof input-set-uniqueness step-03":
      "fraudProofInputSetUniquenessStep03",
    "V1 fraud-proof input-set-uniqueness step-04":
      "fraudProofInputSetUniquenessStep04",
    "V1 fraud-proof mint-authorization step-01": "fraudProofMintAuthorization",
    "V1 fraud-proof mint-authorization step-02":
      "fraudProofMintAuthorizationStep02",
    "V1 fraud-proof mint-authorization step-03":
      "fraudProofMintAuthorizationStep03",
    "V1 fraud-proof mint-authorization step-04":
      "fraudProofMintAuthorizationStep04",
    "V1 fraud-proof mint-authorization step-05":
      "fraudProofMintAuthorizationStep05",
    "V1 fraud-proof transition-trace route": "fraudProofTransitionTrace",
    "V1 fraud-proof transition-trace final-0":
      "fraudProofTransitionTraceControl",
    "V1 fraud-proof transition-trace final-1":
      "fraudProofTransitionTraceSource",
    "V1 fraud-proof transition-trace final-2":
      "fraudProofTransitionTraceWithdrawal",
    "V1 fraud-proof transition-trace final-3":
      "fraudProofTransitionTraceForced",
    "V1 fraud-proof transition-trace final-4":
      "fraudProofTransitionTraceAcceptedTransaction",
    "V1 fraud-proof transition-trace final-5":
      "fraudProofTransitionTraceDeposit",
    "V1 fraud-proof transition-trace final-6":
      "fraudProofTransitionTraceL1Event",
    "V1 fraud-proof transition-trace final-7":
      "fraudProofTransitionTraceDuplicate",
    "V1 fraud-proof network-id step-01": "fraudProofNetworkId",
    "V1 fraud-proof network-id step-02": "fraudProofNetworkIdStep02",
    "V1 fraud-proof computation-thread minting": "computationThreadMint",
    "V1 fraud-proof token minting": "fraudProofMint",
    "V1 MPF chunked-verify withdrawal": "chunkedVerifyWithdraw",
    "V1 MPF pexcludes withdrawal": "pexcludesWithdraw",
    "V1 fraud-proof double-spend step-01": "fraudProofDoubleSpend",
    "V1 fraud-proof double-spend step-02": "fraudProofDoubleSpendStep02",
    "V1 fraud-proof double-spend step-03": "fraudProofDoubleSpendStep03",
    "V1 fraud-proof double-spend step-04": "fraudProofDoubleSpendStep04",
    "V1 fraud-proof non-existent-input step-01": "fraudProofNonExistentInput",
    "V1 fraud-proof non-existent-input step-02":
      "fraudProofNonExistentInputStep02",
    "V1 fraud-proof non-existent-input step-03":
      "fraudProofNonExistentInputStep03",
    "V1 fraud-proof non-existent-input step-04":
      "fraudProofNonExistentInputStep04",
    "V1 fraud-proof non-existent-input-no-index step-01":
      "fraudProofNonExistentInputNoIndex",
    "V1 fraud-proof non-existent-input-no-index step-02":
      "fraudProofNonExistentInputNoIndexStep02",
    "V1 fraud-proof non-existent-input-no-index step-03":
      "fraudProofNonExistentInputNoIndexStep03",
    "V1 fraud-proof non-existent-input-no-index step-04":
      "fraudProofNonExistentInputNoIndexStep04",
    "V1 fraud-proof invalid-range step-01": "fraudProofInvalidRange",
    "V1 fraud-proof invalid-range step-02": "fraudProofInvalidRangeStep02",
    "V1 fraud-proof zero-input step-01": "fraudProofZeroInput",
    "V1 fraud-proof zero-input step-02": "fraudProofZeroInputStep02",
    "V1 fraud-proof da-hash-preimage step-01": "fraudProofDaHashPreimage",
    "V1 fraud-proof da-hash-preimage step-02": "fraudProofDaHashPreimageStep02",
    "V1 fraud-proof no-reference-input step-01": "fraudProofNoReferenceInput",
    "V1 fraud-proof no-reference-input step-02":
      "fraudProofNoReferenceInputStep02",
    "V1 fraud-proof no-reference-input step-03":
      "fraudProofNoReferenceInputStep03",
    "V1 fraud-proof no-reference-input step-04":
      "fraudProofNoReferenceInputStep04",
    "V1 fraud-proof reference-input-no-idx step-01":
      "fraudProofReferenceInputNoIdx",
    "V1 fraud-proof reference-input-no-idx step-02":
      "fraudProofReferenceInputNoIdxStep02",
    "V1 fraud-proof reference-input-no-idx step-03":
      "fraudProofReferenceInputNoIdxStep03",
    "V1 fraud-proof reference-input-no-idx step-04":
      "fraudProofReferenceInputNoIdxStep04",
    "V1 fraud-proof invalid-signature step-01": "fraudProofInvalidSignature",
    "V1 fraud-proof invalid-signature step-02":
      "fraudProofInvalidSignatureStep02",
    "V1 fraud-proof missing-native-script-tx step-07":
      "fraudProofMissingNativeScriptTxStep07",
    "V1 fraud-proof missing-native-script-tx step-08":
      "fraudProofMissingNativeScriptTxStep08",
    "V1 fraud-proof missing-native-script-utxo step-01":
      "fraudProofMissingNativeScriptUtxo",
    "V1 fraud-proof missing-native-script-utxo step-02":
      "fraudProofMissingNativeScriptUtxoStep02",
    "V1 fraud-proof missing-native-script-utxo step-03":
      "fraudProofMissingNativeScriptUtxoStep03",
    "V1 fraud-proof missing-native-script-utxo step-04":
      "fraudProofMissingNativeScriptUtxoStep04",
    "V1 fraud-proof missing-native-script-utxo step-05":
      "fraudProofMissingNativeScriptUtxoStep05",
    "V1 fraud-proof native-script-invalid step-01":
      "fraudProofNativeScriptInvalid",
    "V1 fraud-proof native-script-invalid step-02":
      "fraudProofNativeScriptInvalidStep02",
    "V1 fraud-proof native-script-invalid step-03":
      "fraudProofNativeScriptInvalidStep03",
    "V1 fraud-proof min-ada step-01": "fraudProofMinAda",
    "V1 fraud-proof min-ada step-02": "fraudProofMinAdaStep02",
    "V1 fraud-proof min-ada step-02 tx yield":
      "fraudProofMinAdaStep02TxWithdraw",
    "V1 fraud-proof min-ada step-02 UTxO yield":
      "fraudProofMinAdaStep02UtxoWithdraw",
    "correction-lock spending": "correctionLockSpend",
    "V1 fraud-proof min-ada step-03": "fraudProofMinAdaStep03",
    "V1 fraud-proof min-ada step-04": "fraudProofMinAdaStep04",
    "V1 fraud-proof min-ada step-05": "fraudProofMinAdaStep05",
    "V1 fraud-proof field-preimage-length-mismatch step-01":
      "fraudProofFieldPreimageLengthMismatch",
    "V1 fraud-proof field-preimage-length-mismatch step-02 accepted":
      "fraudProofFieldPreimageLengthMismatchStep02Accepted",
    "V1 fraud-proof field-preimage-length-mismatch step-02 forced":
      "fraudProofFieldPreimageLengthMismatchStep02Forced",
    "V1 fraud-proof field-preimage-length-mismatch step-03":
      "fraudProofFieldPreimageLengthMismatchStep03",
    "V1 fraud-proof field-item-width-illegal step-01":
      "fraudProofFieldItemWidthIllegal",
    "V1 fraud-proof field-item-width-illegal step-02":
      "fraudProofFieldItemWidthIllegalStep02",
    "V1 fraud-proof field-item-width-illegal step-03":
      "fraudProofFieldItemWidthIllegalStep03",
    "V1 fraud-proof witness-script-decoding step-01":
      "fraudProofWitnessScriptDecoding",
    "V1 fraud-proof witness-script-decoding step-02":
      "fraudProofWitnessScriptDecodingStep02",
    "V1 fraud-proof witness-script-decoding step-03":
      "fraudProofWitnessScriptDecodingStep03",
    "V1 fraud-proof witness-script-decoding step-04":
      "fraudProofWitnessScriptDecodingStep04",
    "V1 fraud-proof script-integrity-hash-missing step-01":
      "fraudProofScriptIntegrityHashMissing",
    "V1 fraud-proof script-integrity-hash-missing step-02":
      "fraudProofScriptIntegrityHashMissingStep02",
    "V1 fraud-proof script-integrity-hash-missing step-03":
      "fraudProofScriptIntegrityHashMissingStep03",
    "V1 fraud-proof script-integrity-hash-missing script-grammar":
      "fraudProofScriptIntegrityHashMissingScriptGrammar",
    "V1 fraud-proof script-integrity-hash-missing script-scan":
      "fraudProofScriptIntegrityHashMissingScriptScan",
    "V1 fraud-proof script-integrity-hash-missing redeemer-grammar":
      "fraudProofScriptIntegrityHashMissingRedeemerGrammar",
    "V1 fraud-proof script-integrity-hash-missing step-04":
      "fraudProofScriptIntegrityHashMissingStep04",
    "V1 fraud-proof transaction-output-non-canonical step-01":
      "fraudProofTransactionOutputNonCanonical",
    "V1 fraud-proof transaction-output-non-canonical step-02":
      "fraudProofTransactionOutputNonCanonicalStep02",
    "V1 fraud-proof transaction-output-non-canonical step-03":
      "fraudProofTransactionOutputNonCanonicalStep03",
    "V1 fraud-proof transaction-output-non-canonical step-04":
      "fraudProofTransactionOutputNonCanonicalStep04",
    "V1 fraud-proof resolved-output-non-canonical step-01":
      "fraudProofResolvedOutputNonCanonical",
    "V1 fraud-proof resolved-output-non-canonical step-02":
      "fraudProofResolvedOutputNonCanonicalStep02",
    "V1 fraud-proof resolved-output-non-canonical step-03":
      "fraudProofResolvedOutputNonCanonicalStep03",
    "V1 fraud-proof resolved-output-non-canonical step-04":
      "fraudProofResolvedOutputNonCanonicalStep04",
    "V1 fraud-proof resolved-output-non-canonical step-05":
      "fraudProofResolvedOutputNonCanonicalStep05",
    "V1 fraud-proof mint-declared-asset-limit step-01":
      "fraudProofMintDeclaredAssetLimit",
    "V1 fraud-proof mint-declared-asset-limit step-02":
      "fraudProofMintDeclaredAssetLimitStep02",
    "V1 fraud-proof mint-declared-asset-limit step-03":
      "fraudProofMintDeclaredAssetLimitStep03",
    "V1 fraud-proof mint-declared-asset-limit step-04":
      "fraudProofMintDeclaredAssetLimitStep04",
    "V1 fraud-proof spend-input-signer-missing step-01":
      "fraudProofSpendInputSignerMissing",
    "V1 fraud-proof spend-input-signer-missing step-02":
      "fraudProofSpendInputSignerMissingStep02",
    "V1 fraud-proof spend-input-signer-missing step-03":
      "fraudProofSpendInputSignerMissingStep03",
    "V1 fraud-proof spend-input-signer-missing step-04":
      "fraudProofSpendInputSignerMissingStep04",
    "V1 fraud-proof spend-input-signer-missing step-05":
      "fraudProofSpendInputSignerMissingStep05",
    "V1 fraud-proof protected-output-signer-missing step-01":
      "fraudProofProtectedOutputSignerMissing",
    "V1 fraud-proof protected-output-signer-missing step-02":
      "fraudProofProtectedOutputSignerMissingStep02",
    "V1 fraud-proof protected-output-signer-missing step-03":
      "fraudProofProtectedOutputSignerMissingStep03",
    "V1 fraud-proof protected-output-signer-missing step-04":
      "fraudProofProtectedOutputSignerMissingStep04",
    "V1 fraud-proof protected-output-signer-missing step-05":
      "fraudProofProtectedOutputSignerMissingStep05",
    "V1 fraud-proof observers-forbidden-on-untagged-network step-01":
      "fraudProofObserversForbiddenOnUntaggedNetwork",
    "V1 fraud-proof observers-forbidden-on-untagged-network step-02":
      "fraudProofObserversForbiddenOnUntaggedNetworkStep02",
    "V1 fraud-proof output-reference-script-decoding step-01":
      "fraudProofOutputReferenceScriptDecoding",
    "V1 fraud-proof output-reference-script-decoding step-02":
      "fraudProofOutputReferenceScriptDecodingStep02",
    "V1 fraud-proof output-reference-script-decoding step-03":
      "fraudProofOutputReferenceScriptDecodingStep03",
    "V1 fraud-proof output-reference-script-decoding step-04":
      "fraudProofOutputReferenceScriptDecodingStep04",
    "V1 fraud-proof output-reference-script-decoding step-05":
      "fraudProofOutputReferenceScriptDecodingStep05",
    "V1 fraud-proof output-reference-script-decoding step-06":
      "fraudProofOutputReferenceScriptDecodingStep06",
    "V1 fraud-proof execution-source-script-decoding step-01":
      "fraudProofExecutionSourceScriptDecoding",
    "V1 fraud-proof execution-source-script-decoding step-02":
      "fraudProofExecutionSourceScriptDecodingStep02",
    "V1 fraud-proof execution-source-script-decoding step-03":
      "fraudProofExecutionSourceScriptDecodingStep03",
    "V1 fraud-proof execution-source-script-decoding step-04":
      "fraudProofExecutionSourceScriptDecodingStep04",
    "V1 fraud-proof execution-source-script-decoding step-05":
      "fraudProofExecutionSourceScriptDecodingStep05",
    "V1 fraud-proof observer-order-invalid step-01":
      "fraudProofObserverOrderInvalid",
    "V1 fraud-proof observer-order-invalid step-02":
      "fraudProofObserverOrderInvalidStep02",
    "V1 fraud-proof observer-order-invalid step-03":
      "fraudProofObserverOrderInvalidStep03",
    "V1 fraud-proof observer-order-invalid step-04":
      "fraudProofObserverOrderInvalidStep04",
    "V1 fraud-proof redeemer-canonicity step-01":
      "fraudProofRedeemerCanonicity",
    "V1 fraud-proof redeemer-canonicity step-02":
      "fraudProofRedeemerCanonicityStep02",
    "V1 fraud-proof redeemer-canonicity step-03":
      "fraudProofRedeemerCanonicityStep03",
    "V1 fraud-proof receive-purpose-language step-01":
      "fraudProofReceivePurposeLanguage",
    "V1 fraud-proof receive-purpose-language step-02":
      "fraudProofReceivePurposeLanguageStep02",
    "V1 fraud-proof receive-purpose-language step-03":
      "fraudProofReceivePurposeLanguageStep03",
    "V1 fraud-proof unused-script-witness step-01":
      "fraudProofUnusedScriptWitness",
    "V1 fraud-proof unused-script-witness step-02":
      "fraudProofUnusedScriptWitnessStep02",
    "V1 fraud-proof unused-script-witness step-03":
      "fraudProofUnusedScriptWitnessStep03",
    "V1 fraud-proof unused-script-witness step-04":
      "fraudProofUnusedScriptWitnessStep04",
    "V1 fraud-proof unused-script-witness step-05":
      "fraudProofUnusedScriptWitnessStep05",
    "V1 fraud-proof unused-script-witness step-06":
      "fraudProofUnusedScriptWitnessStep06",
    "V1 fraud-proof missing-script-source step-01":
      "fraudProofMissingScriptSource",
    "V1 fraud-proof missing-script-source step-02":
      "fraudProofMissingScriptSourceStep02",
    "V1 fraud-proof missing-script-source step-03":
      "fraudProofMissingScriptSourceStep03",
    "V1 fraud-proof missing-script-source step-04":
      "fraudProofMissingScriptSourceStep04",
    "V1 fraud-proof missing-script-source step-05":
      "fraudProofMissingScriptSourceStep05",
    "V1 fraud-proof missing-script-source step-06":
      "fraudProofMissingScriptSourceStep06",
    "V1 fraud-proof missing-redeemer step-01": "fraudProofMissingRedeemer",
    "V1 fraud-proof missing-redeemer step-02":
      "fraudProofMissingRedeemerStep02",
    "V1 fraud-proof missing-redeemer step-02a":
      "fraudProofMissingRedeemerStep02a",
    "V1 fraud-proof missing-redeemer step-02b":
      "fraudProofMissingRedeemerStep02b",
    "V1 fraud-proof missing-redeemer step-03":
      "fraudProofMissingRedeemerStep03",
    "V1 fraud-proof missing-redeemer step-04":
      "fraudProofMissingRedeemerStep04",
    "V1 fraud-proof missing-redeemer step-05":
      "fraudProofMissingRedeemerStep05",
    "V1 fraud-proof unused-redeemer step-01": "fraudProofUnusedRedeemer",
    "V1 fraud-proof unused-redeemer step-02": "fraudProofUnusedRedeemerStep02",
    "V1 fraud-proof unused-redeemer step-02a":
      "fraudProofUnusedRedeemerStep02a",
    "V1 fraud-proof unused-redeemer step-02b":
      "fraudProofUnusedRedeemerStep02b",
    "V1 fraud-proof unused-redeemer step-02c":
      "fraudProofUnusedRedeemerStep02c",
    "V1 fraud-proof unused-redeemer step-03": "fraudProofUnusedRedeemerStep03",
    "V1 fraud-proof unused-redeemer step-04": "fraudProofUnusedRedeemerStep04",
    "V1 fraud-proof unused-redeemer step-05": "fraudProofUnusedRedeemerStep05",
    "V1 fraud-proof unused-redeemer step-06": "fraudProofUnusedRedeemerStep06",
    "V1 fraud-proof execution-native-script-invalid step-01":
      "fraudProofExecutionNativeScriptInvalid",
    "V1 fraud-proof execution-native-script-invalid step-02":
      "fraudProofExecutionNativeScriptInvalidStep02",
    "V1 fraud-proof execution-native-script-invalid step-03":
      "fraudProofExecutionNativeScriptInvalidStep03",
    "V1 fraud-proof execution-native-script-invalid step-04":
      "fraudProofExecutionNativeScriptInvalidStep04",
    "V1 fraud-proof execution-native-script-invalid step-05":
      "fraudProofExecutionNativeScriptInvalidStep05",
    "V1 fraud-proof execution-native-script-invalid step-06":
      "fraudProofExecutionNativeScriptInvalidStep06",
    "V1 fraud-proof execution-native-script-invalid accepted-reconstruction-init":
      "fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit",
    "V1 fraud-proof execution-native-script-invalid accepted-spend-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-mint-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-observer-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-receive-prefix":
      "fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix",
    "V1 fraud-proof execution-native-script-invalid accepted-inline-source":
      "fraudProofExecutionNativeScriptInvalidAcceptedInlineSource",
    "V1 fraud-proof execution-native-script-invalid accepted-reference-source":
      "fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource",
    "V1 fraud-proof script-integrity-hash-mismatch step-01":
      "fraudProofScriptIntegrityHashMismatch",
    "V1 fraud-proof script-integrity-hash-mismatch step-02":
      "fraudProofScriptIntegrityHashMismatchStep02",
    "V1 fraud-proof script-integrity-hash-mismatch step-03":
      "fraudProofScriptIntegrityHashMismatchStep03",
    "V1 fraud-proof script-integrity-hash-mismatch step-04":
      "fraudProofScriptIntegrityHashMismatchStep04",
    "V1 fraud-proof script-integrity-hash-mismatch step-05":
      "fraudProofScriptIntegrityHashMismatchStep05",
    "V1 fraud-proof distinct-asset-accumulation-limit step-01":
      "fraudProofDistinctAssetAccumulationLimit",
    "V1 fraud-proof distinct-asset-accumulation-limit step-02":
      "fraudProofDistinctAssetAccumulationLimitStep02",
    "V1 fraud-proof distinct-asset-accumulation-limit step-03":
      "fraudProofDistinctAssetAccumulationLimitStep03",
    "V1 fraud-proof distinct-asset-accumulation-limit step-04":
      "fraudProofDistinctAssetAccumulationLimitStep04",
    "V1 fraud-proof distinct-asset-accumulation-limit step-05":
      "fraudProofDistinctAssetAccumulationLimitStep05",
    "V1 fraud-proof distinct-asset-accumulation-limit step-06":
      "fraudProofDistinctAssetAccumulationLimitStep06",
    "availability-challenge spending": "availabilityChallengeSpend",
    "availability-challenge minting": "availabilityChallengeMint",
  } as const);

export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES = Object.freeze(
  Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
);

export const DEPLOYMENT_MANIFEST_STEP_NAMES = Object.freeze([
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
  "phasRegistration",
  "operatorRegistration",
  "operatorActivation",
] as const);

const DEPLOYMENT_MANIFEST_STEP_STATUSES = Object.freeze([
  "pending",
  "in_progress",
  "submitted",
  "complete",
  "attached",
  "failed",
  "blocked_requires_fresh_redeploy",
] as const);

const DEPLOYMENT_MANIFEST_SCRIPT_TYPES = Object.freeze([
  "Native",
  "PlutusV1",
  "PlutusV2",
  "PlutusV3",
] as const);

const DEPLOYMENT_MANIFEST_NETWORKS = new Set([
  "Mainnet",
  "Preprod",
  "Preview",
  "Custom",
]);

type DeploymentManifestOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

type DeploymentManifestContractEntry = {
  readonly refScriptUTxO: DeploymentManifestOutRef | null;
  readonly contract: {
    readonly type: (typeof DEPLOYMENT_MANIFEST_SCRIPT_TYPES)[number];
    readonly cborHex: string;
  };
  readonly scriptHash: string;
  readonly fraudProofCatalogue?: {
    readonly root: string;
    readonly categories: Readonly<
      Record<
        (typeof FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number],
        {
          readonly categoryId: string;
          readonly scriptHash: string;
          readonly membershipProofCbor: string;
        }
      >
    >;
  };
};

export type DeploymentManifestValue = {
  readonly schemaVersion: typeof DEPLOYMENT_MANIFEST_SCHEMA_VERSION;
  readonly manifestId: string;
  readonly consensusProfile: MidgardConsensusProfile;
  readonly consensusProfileDigest: string;
  readonly network: string;
  readonly cardanoProtocolParameters: {
    readonly snapshot: DeploymentManifestCardanoProtocolParameters;
    readonly digest: string;
  };
  readonly genesis: {
    readonly headerHash: string;
    readonly utxoSetDigest: string;
  };
  readonly createdAt: string;
  readonly updatedAt: string;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShot: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly outRef: string;
    readonly status: "consumed_by_init";
  };
  readonly referenceScriptAuthPolicy: {
    readonly policyId: string;
    readonly nativeScript: {
      readonly type: "Native";
      readonly cborHex: string;
      readonly expiresAtSlot: number;
      readonly expiresAtUnixTime: number;
      readonly timelockDurationMs: number;
    };
    readonly tokenNames: Readonly<
      Record<keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES, string>
    >;
    readonly postTimelockAudit: {
      readonly required: true;
      readonly rule: string;
    };
  };
  readonly contracts: Readonly<Record<string, DeploymentManifestContractEntry>>;
  readonly referenceScripts: Readonly<
    Record<
      string,
      {
        readonly status: "confirmed";
        readonly roleUnit: string;
        readonly scriptHash: string;
        readonly outRef: string;
      }
    >
  >;
  readonly da: {
    readonly committeeVkeys: readonly string[];
    readonly committeeSignersHash: string;
    readonly threshold: number;
    readonly transportProfile: {
      readonly protocolVersion: typeof DA_TRANSPORT_PROTOCOL_VERSION;
      readonly runtimeManifestSchemaVersion: typeof DA_RUNTIME_MANIFEST_SCHEMA_VERSION;
      readonly envelopeEncoding: "identity" | "zstd";
      readonly zstdLevel: number;
      readonly limits: typeof DA_TRANSPORT_LIMITS;
      readonly retentionDays: number;
    };
  };
  readonly proofEvidence: {
    readonly digest: string | null;
    readonly blueprintHash: string;
  };
  readonly steps: Readonly<
    Record<
      string,
      {
        readonly status: (typeof DEPLOYMENT_MANIFEST_STEP_STATUSES)[number];
        readonly txHash?: string;
      }
    >
  >;
  readonly validationDispute: {
    readonly version: number;
    readonly responseWindowMs: number;
    readonly maxBisectionRounds: number;
    readonly maturityMs: number;
  };
  readonly l1Finality: DeploymentManifestL1Finality;
  readonly economics: DeploymentManifestEconomics;
  readonly availabilityChallenge: DeploymentManifestAvailabilityChallenge;
};

export const computeDeploymentManifestDaCommitteeSignersHash = (
  committeeVkeys: readonly string[],
): string => Effect.runSync(hashHexWithBlake2b(committeeVkeys.join(""), 32));

const deploymentManifestIdentityInput = (
  manifest: Omit<DeploymentManifestValue, "manifestId">,
): unknown => manifest;

export const computeDeploymentManifestId = (
  manifest: Omit<DeploymentManifestValue, "manifestId">,
): string =>
  computeDeploymentManifestV1Id(
    deploymentManifestIdentityInput(manifest) as Record<string, unknown>,
  );

const requireObject = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value === "object" && value !== null && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }
  throw new Error(`Deployment manifest ${field} must be an object`);
};

const requireExactKeys = (
  value: Record<string, unknown>,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[],
  field: string,
): void => {
  const allowed = new Set([...requiredKeys, ...optionalKeys]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) {
      throw new Error(`Deployment manifest ${field}.${key} is unexpected`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.hasOwn(value, key)) {
      throw new Error(`Deployment manifest ${field}.${key} is required`);
    }
  }
};

const requireNonEmptyString = (value: unknown, field: string): string => {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }
  throw new Error(`Deployment manifest ${field} must be a non-empty string`);
};

const requireLowercaseHex = (
  value: unknown,
  bytes: number,
  field: string,
): string => {
  const parsed = requireNonEmptyString(value, field);
  if (!new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u").test(parsed)) {
    throw new Error(
      `Deployment manifest ${field} must be ${bytes.toString()}-byte lowercase hex`,
    );
  }
  return parsed;
};

const requireNonNegativeSafeInteger = (
  value: unknown,
  field: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `Deployment manifest ${field} must be a non-negative safe integer`,
    );
  }
  return value;
};

const requireIsoTimestamp = (value: unknown, field: string): string => {
  const parsed = requireNonEmptyString(value, field);
  const timestamp = new Date(parsed);
  if (
    !Number.isFinite(timestamp.getTime()) ||
    timestamp.toISOString() !== parsed
  ) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical ISO timestamp`,
    );
  }
  return parsed;
};

const requireOutRef = (
  value: unknown,
  field: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const outRef = requireObject(value, field);
  requireExactKeys(outRef, ["txHash", "outputIndex"], [], field);
  return {
    txHash: requireLowercaseHex(outRef.txHash, 32, `${field}.txHash`),
    outputIndex: requireNonNegativeSafeInteger(
      outRef.outputIndex,
      `${field}.outputIndex`,
    ),
  };
};

const requireLowercaseVariableHex = (value: unknown, field: string): string => {
  const parsed = requireNonEmptyString(value, field);
  if (!/^(?:[0-9a-f]{2})+$/u.test(parsed)) {
    throw new Error(
      `Deployment manifest ${field} must be non-empty even-length lowercase hex`,
    );
  }
  return parsed;
};

const requirePositiveSafeInteger = (value: unknown, field: string): number => {
  const parsed = requireNonNegativeSafeInteger(value, field);
  if (parsed === 0) {
    throw new Error(
      `Deployment manifest ${field} must be a positive safe integer`,
    );
  }
  return parsed;
};

const requireOutRefString = (
  value: unknown,
  field: string,
): DeploymentManifestOutRef => {
  const parsed = requireNonEmptyString(value, field);
  const match = /^([0-9a-f]{64})#([0-9]+)$/u.exec(parsed);
  if (match === null) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical lowercase transaction outref`,
    );
  }
  const outputIndex = Number(match[2]);
  if (
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0 ||
    outputIndex.toString() !== match[2]
  ) {
    throw new Error(
      `Deployment manifest ${field} output index must be canonical`,
    );
  }
  return { txHash: match[1], outputIndex };
};

const requireScriptType = (
  value: unknown,
  field: string,
): (typeof DEPLOYMENT_MANIFEST_SCRIPT_TYPES)[number] => {
  if (
    typeof value === "string" &&
    DEPLOYMENT_MANIFEST_SCRIPT_TYPES.some((entry) => entry === value)
  ) {
    return value as (typeof DEPLOYMENT_MANIFEST_SCRIPT_TYPES)[number];
  }
  throw new Error(
    `Deployment manifest ${field} must be Native, PlutusV1, PlutusV2, or PlutusV3`,
  );
};

const validateReferenceScriptAuthPolicy = (
  candidate: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["policyId", "nativeScript", "tokenNames", "postTimelockAudit"],
    [],
    "referenceScriptAuthPolicy",
  );
  const policyId = requireLowercaseHex(
    candidate.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  const nativeScript = requireObject(
    candidate.nativeScript,
    "referenceScriptAuthPolicy.nativeScript",
  );
  requireExactKeys(
    nativeScript,
    [
      "type",
      "cborHex",
      "expiresAtSlot",
      "expiresAtUnixTime",
      "timelockDurationMs",
    ],
    [],
    "referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const nativeScriptCbor = requireLowercaseVariableHex(
    nativeScript.cborHex,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  requireNonNegativeSafeInteger(
    nativeScript.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  requireNonNegativeSafeInteger(
    nativeScript.expiresAtUnixTime,
    "referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime",
  );
  requirePositiveSafeInteger(
    nativeScript.timelockDurationMs,
    "referenceScriptAuthPolicy.nativeScript.timelockDurationMs",
  );
  let derivedPolicyId: string;
  try {
    derivedPolicyId = validatorToScriptHash({
      type: "Native",
      script: nativeScriptCbor,
    });
  } catch (cause) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.nativeScript.cborHex is invalid: ${String(cause)}`,
    );
  }
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }

  const tokenNames = requireObject(
    candidate.tokenNames,
    "referenceScriptAuthPolicy.tokenNames",
  );
  const tokenNameKeys = Object.keys(REFERENCE_SCRIPT_AUTH_TOKEN_NAMES);
  requireExactKeys(
    tokenNames,
    tokenNameKeys,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const [role, expectedTokenName] of Object.entries(
    REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  )) {
    if (tokenNames[role] !== expectedTokenName) {
      throw new Error(
        `Deployment manifest referenceScriptAuthPolicy.tokenNames.${role} must equal ${expectedTokenName}`,
      );
    }
  }

  const postTimelockAudit = requireObject(
    candidate.postTimelockAudit,
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  requireExactKeys(
    postTimelockAudit,
    ["required", "rule"],
    [],
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  if (postTimelockAudit.required !== true) {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit.required must be true",
    );
  }
  requireNonEmptyString(
    postTimelockAudit.rule,
    "referenceScriptAuthPolicy.postTimelockAudit.rule",
  );
};

const validateFraudProofCatalogue = (
  candidate: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  const root = requireLowercaseHex(
    candidate.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const categories = requireObject(
    candidate.categories,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  requireExactKeys(
    categories,
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const contractNameByCategory = {
    doubleSpend: "fraudProofDoubleSpend",
    nonExistentInput: "fraudProofNonExistentInput",
    nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
    invalidRange: "fraudProofInvalidRange",
    transitionTrace: "fraudProofTransitionTrace",
    zeroInput: "fraudProofZeroInput",
    validationTraceDispute: "validationTraceDispute",
    daHashPreimage: "fraudProofDaHashPreimage",
    noReferenceInput: "fraudProofNoReferenceInput",
    referenceInputNoIdx: "fraudProofReferenceInputNoIdx",
    invalidSignature: "fraudProofInvalidSignature",
    fabricatedDeposit: "fraudProofFabricatedDeposit",
    fabricatedWithdrawal: "fraudProofFabricatedWithdrawal",
    nativeScriptDecoding: "fraudProofNativeScriptDecoding",
    missingSignature: "fraudProofMissingSignature",
    missingNativeScriptTx: "fraudProofMissingNativeScriptTx",
    withdrawnReferenceInput: "fraudProofWithdrawnReferenceInput",
    canonicalDecodability: "fraudProofCanonicalDecodability",
    committedFieldShape: "fraudProofCommittedFieldShape",
    minFee: "fraudProofMinFee",
    withdrawalMistag: "fraudProofWithdrawalMistag",
    doubleWithdraw: "fraudProofDoubleWithdraw",
    crossBlockDuplicateEvent: "fraudProofCrossBlockDuplicateEvent",
    l2TxMistag: "fraudProofL2TxMistag",
    withdrawnInput: "fraudProofWithdrawnInput",
    valueNotPreserved: "fraudProofValueNotPreserved",
    inputSetUniqueness: "fraudProofInputSetUniqueness",
    mintAuthorization: "fraudProofMintAuthorization",
    networkId: "fraudProofNetworkId",
    missingNativeScriptUtxo: "fraudProofMissingNativeScriptUtxo",
    nativeScriptInvalid: "fraudProofNativeScriptInvalid",
    minAda: "fraudProofMinAda",
    fieldPreimageLengthMismatch: "fraudProofFieldPreimageLengthMismatch",
    fieldItemWidthIllegal: "fraudProofFieldItemWidthIllegal",
    witnessScriptDecoding: "fraudProofWitnessScriptDecoding",
    scriptIntegrityHashMissing: "fraudProofScriptIntegrityHashMissing",
    transactionOutputNonCanonical: "fraudProofTransactionOutputNonCanonical",
    resolvedOutputNonCanonical: "fraudProofResolvedOutputNonCanonical",
    mintDeclaredAssetLimit: "fraudProofMintDeclaredAssetLimit",
    spendInputSignerMissing: "fraudProofSpendInputSignerMissing",
    protectedOutputSignerMissing: "fraudProofProtectedOutputSignerMissing",
    observersForbiddenOnUntaggedNetwork:
      "fraudProofObserversForbiddenOnUntaggedNetwork",
    outputReferenceScriptDecoding: "fraudProofOutputReferenceScriptDecoding",
    executionSourceScriptDecoding: "fraudProofExecutionSourceScriptDecoding",
    observerOrderInvalid: "fraudProofObserverOrderInvalid",
    redeemerCanonicity: "fraudProofRedeemerCanonicity",
    receivePurposeLanguage: "fraudProofReceivePurposeLanguage",
    unusedScriptWitness: "fraudProofUnusedScriptWitness",
    missingScriptSource: "fraudProofMissingScriptSource",
    missingRedeemer: "fraudProofMissingRedeemer",
    unusedRedeemer: "fraudProofUnusedRedeemer",
    executionNativeScriptInvalid: "fraudProofExecutionNativeScriptInvalid",
    scriptIntegrityHashMismatch: "fraudProofScriptIntegrityHashMismatch",
    distinctAssetAccumulationLimit: "fraudProofDistinctAssetAccumulationLimit",
  } as const;
  const parsedCategories = {} as Record<
    DeploymentManifestFraudProofCatalogueCategory,
    DeploymentManifestFraudProofCatalogueCategoryIdentity
  >;
  for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = requireObject(categories[categoryName], field);
    requireExactKeys(
      category,
      ["categoryId", "scriptHash", "membershipProofCbor"],
      [],
      field,
    );
    const categoryId = requireLowercaseHex(
      category.categoryId,
      4,
      `${field}.categoryId`,
    );
    const scriptHash = requireLowercaseHex(
      category.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    const membershipProofCbor = requireLowercaseVariableHex(
      category.membershipProofCbor,
      `${field}.membershipProofCbor`,
    );
    parsedCategories[categoryName] = {
      categoryId,
      scriptHash,
      membershipProofCbor,
    };
    const expectedContract = requireObject(
      contracts[contractNameByCategory[categoryName]],
      `contracts.${contractNameByCategory[categoryName]}`,
    );
    if (expectedContract.scriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractNameByCategory[categoryName]}.scriptHash`,
      );
    }
  }
  verifyDeploymentManifestFraudProofCatalogueIdentity({
    root,
    categories: parsedCategories,
  });
};

const validateContracts = (contracts: Record<string, unknown>): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    [],
    "contracts",
  );
  for (const contractName of DEPLOYMENT_MANIFEST_CONTRACT_NAMES) {
    const field = `contracts.${contractName}`;
    const entry = requireObject(contracts[contractName], field);
    requireExactKeys(
      entry,
      ["refScriptUTxO", "contract", "scriptHash"],
      contractName === "fraudProofCatalogueMint" ? ["fraudProofCatalogue"] : [],
      field,
    );
    const refScriptUTxO =
      entry.refScriptUTxO === null
        ? null
        : requireOutRef(entry.refScriptUTxO, `${field}.refScriptUTxO`);
    if (
      refScriptUTxO !== null &&
      `${refScriptUTxO.txHash}#${refScriptUTxO.outputIndex.toString()}` !==
        `${(entry.refScriptUTxO as Record<string, unknown>).txHash as string}#${(entry.refScriptUTxO as Record<string, unknown>).outputIndex as number}`
    ) {
      throw new Error(
        `Deployment manifest ${field}.refScriptUTxO must be canonical`,
      );
    }
    const contract = requireObject(entry.contract, `${field}.contract`);
    requireExactKeys(contract, ["type", "cborHex"], [], `${field}.contract`);
    const scriptType = requireScriptType(
      contract.type,
      `${field}.contract.type`,
    );
    const cborHex = requireLowercaseVariableHex(
      contract.cborHex,
      `${field}.contract.cborHex`,
    );
    const scriptHash = requireLowercaseHex(
      entry.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    let derivedScriptHash: string;
    try {
      derivedScriptHash = validatorToScriptHash({
        type: scriptType,
        script: cborHex,
      });
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.contract.cborHex is invalid: ${String(cause)}`,
      );
    }
    if (derivedScriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash mismatch: expected ${derivedScriptHash}`,
      );
    }
  }
  const fraudProofCatalogueMint = requireObject(
    contracts.fraudProofCatalogueMint,
    "contracts.fraudProofCatalogueMint",
  );
  if (fraudProofCatalogueMint.fraudProofCatalogue !== undefined) {
    validateFraudProofCatalogue(
      requireObject(
        fraudProofCatalogueMint.fraudProofCatalogue,
        "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
      ),
      contracts,
    );
  }
};

const validateReferenceScripts = (
  referenceScripts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    referenceScripts,
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES,
    [],
    "referenceScripts",
  );
  const policyId = requireNonEmptyString(
    referenceScriptAuthPolicy.policyId,
    "referenceScriptAuthPolicy.policyId",
  );
  for (const role of DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES) {
    const field = `referenceScripts.${role}`;
    const record = requireObject(referenceScripts[role], field);
    requireExactKeys(
      record,
      ["status", "roleUnit", "scriptHash", "outRef"],
      [],
      field,
    );
    if (record.status !== "confirmed") {
      throw new Error(`Deployment manifest ${field}.status must be confirmed`);
    }
    const expectedRoleUnit = referenceScriptAuthUnit(
      policyId,
      role as keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
    );
    if (record.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const contract = requireObject(
      contracts[contractName],
      `contracts.${contractName}`,
    );
    if (record.scriptHash !== contract.scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const contractOutRef = requireOutRef(
      contract.refScriptUTxO,
      `contracts.${contractName}.refScriptUTxO`,
    );
    const outRef = requireOutRefString(record.outRef, `${field}.outRef`);
    if (
      outRef.txHash !== contractOutRef.txHash ||
      outRef.outputIndex !== contractOutRef.outputIndex
    ) {
      throw new Error(
        `Deployment manifest ${field}.outRef must match contracts.${contractName}.refScriptUTxO`,
      );
    }
  }
};

const validateSteps = (steps: Record<string, unknown>): void => {
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_STEP_NAMES, [], "steps");
  for (const stepName of DEPLOYMENT_MANIFEST_STEP_NAMES) {
    const field = `steps.${stepName}`;
    const step = requireObject(steps[stepName], field);
    requireExactKeys(step, ["status"], ["txHash"], field);
    if (
      typeof step.status !== "string" ||
      !DEPLOYMENT_MANIFEST_STEP_STATUSES.some(
        (status) => status === step.status,
      )
    ) {
      throw new Error(`Deployment manifest ${field}.status is unsupported`);
    }
    if (step.txHash !== undefined) {
      requireLowercaseHex(step.txHash, 32, `${field}.txHash`);
    }
  }
};

const validateDaIdentity = (candidate: Record<string, unknown>): void => {
  requireExactKeys(
    candidate,
    ["committeeVkeys", "committeeSignersHash", "threshold", "transportProfile"],
    [],
    "da",
  );
  if (
    !Array.isArray(candidate.committeeVkeys) ||
    candidate.committeeVkeys.length === 0
  ) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must be a non-empty array",
    );
  }
  const committeeVkeys = candidate.committeeVkeys.map((vkey, index) =>
    requireLowercaseHex(vkey, 32, `da.committeeVkeys[${index.toString()}]`),
  );
  if (new Set(committeeVkeys).size !== committeeVkeys.length) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must not contain duplicates",
    );
  }
  const committeeSignersHash = requireLowercaseHex(
    candidate.committeeSignersHash,
    32,
    "da.committeeSignersHash",
  );
  const expectedCommitteeSignersHash =
    computeDeploymentManifestDaCommitteeSignersHash(committeeVkeys);
  if (committeeSignersHash !== expectedCommitteeSignersHash) {
    throw new Error(
      `Deployment manifest da.committeeSignersHash mismatch: expected ${expectedCommitteeSignersHash}`,
    );
  }
  const threshold = requirePositiveSafeInteger(
    candidate.threshold,
    "da.threshold",
  );
  if (threshold > committeeVkeys.length) {
    throw new Error(
      "Deployment manifest da.threshold must not exceed committee size",
    );
  }
  const transportProfile = requireObject(
    candidate.transportProfile,
    "da.transportProfile",
  );
  requireExactKeys(
    transportProfile,
    [
      "protocolVersion",
      "runtimeManifestSchemaVersion",
      "envelopeEncoding",
      "zstdLevel",
      "limits",
      "retentionDays",
    ],
    [],
    "da.transportProfile",
  );
  if (transportProfile.protocolVersion !== DA_TRANSPORT_PROTOCOL_VERSION) {
    throw new Error(
      `Deployment manifest da.transportProfile.protocolVersion must equal ${DA_TRANSPORT_PROTOCOL_VERSION.toString()}`,
    );
  }
  if (
    transportProfile.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_SCHEMA_VERSION
  ) {
    throw new Error(
      `Deployment manifest da.transportProfile.runtimeManifestSchemaVersion must equal ${DA_RUNTIME_MANIFEST_SCHEMA_VERSION}`,
    );
  }
  if (
    transportProfile.envelopeEncoding !== "identity" &&
    transportProfile.envelopeEncoding !== "zstd"
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.envelopeEncoding must be identity or zstd",
    );
  }
  const zstdLevel = requirePositiveSafeInteger(
    transportProfile.zstdLevel,
    "da.transportProfile.zstdLevel",
  );
  if (zstdLevel > 19) {
    throw new Error(
      "Deployment manifest da.transportProfile.zstdLevel must not exceed 19",
    );
  }
  const limits = requireObject(
    transportProfile.limits,
    "da.transportProfile.limits",
  );
  requireExactKeys(
    limits,
    Object.keys(DA_TRANSPORT_LIMITS),
    [],
    "da.transportProfile.limits",
  );
  for (const [key, expected] of Object.entries(DA_TRANSPORT_LIMITS)) {
    if (limits[key] !== expected) {
      throw new Error(
        "Deployment manifest da.transportProfile.limits must exactly match canonical V1",
      );
    }
  }
  const retentionDays = requirePositiveSafeInteger(
    transportProfile.retentionDays,
    "da.transportProfile.retentionDays",
  );
  if (retentionDays < DA_TRANSPORT_LIMITS.minimumRetentionDays) {
    throw new Error(
      `Deployment manifest da.transportProfile.retentionDays must be at least ${DA_TRANSPORT_LIMITS.minimumRetentionDays.toString()}`,
    );
  }
};

const validateValidationDispute = (
  candidate: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["version", "responseWindowMs", "maxBisectionRounds", "maturityMs"],
    [],
    "validationDispute",
  );
  if (
    candidate.version !== MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion ||
    candidate.responseWindowMs !==
      MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs ||
    candidate.maxBisectionRounds !==
      MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds
  ) {
    throw new Error(
      "Deployment manifest validationDispute must exactly match canonical V1",
    );
  }
  if (
    candidate.maturityMs !== MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs ||
    (candidate.maturityMs as number) <
      MIDGARD_CONSENSUS_PROFILE.limits.minValidationDisputeMaturityMs
  ) {
    throw new Error(
      "Deployment manifest validationDispute.maturityMs must equal the canonical V1 maturity and cover the dispute schedule",
    );
  }
};

const parseDeploymentManifestCommon = (
  candidate: Record<string, unknown>,
): DeploymentManifestValue => {
  const network = requireNonEmptyString(candidate.network, "network");
  if (!DEPLOYMENT_MANIFEST_NETWORKS.has(network)) {
    throw new Error(
      "Deployment manifest network must be Mainnet, Preprod, Preview, or Custom",
    );
  }
  const createdAt = requireIsoTimestamp(candidate.createdAt, "createdAt");
  const updatedAt = requireIsoTimestamp(candidate.updatedAt, "updatedAt");
  if (updatedAt < createdAt) {
    throw new Error("Deployment manifest updatedAt must not precede createdAt");
  }
  requireNonEmptyString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );
  const hubOracleOneShot = requireObject(
    candidate.hubOracleOneShot,
    "hubOracleOneShot",
  );
  requireExactKeys(
    hubOracleOneShot,
    ["txHash", "outputIndex", "outRef", "status"],
    [],
    "hubOracleOneShot",
  );
  const txHash = requireLowercaseHex(
    hubOracleOneShot.txHash,
    32,
    "hubOracleOneShot.txHash",
  );
  const outputIndex = requireNonNegativeSafeInteger(
    hubOracleOneShot.outputIndex,
    "hubOracleOneShot.outputIndex",
  );
  const expectedOutRef = `${txHash}#${outputIndex.toString()}`;
  if (hubOracleOneShot.outRef !== expectedOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef mismatch: expected ${expectedOutRef}`,
    );
  }
  if (hubOracleOneShot.status !== "consumed_by_init") {
    throw new Error(
      "Deployment manifest hubOracleOneShot.status must be consumed_by_init",
    );
  }
  const referenceScriptAuthPolicy = requireObject(
    candidate.referenceScriptAuthPolicy,
    "referenceScriptAuthPolicy",
  );
  validateReferenceScriptAuthPolicy(referenceScriptAuthPolicy);
  const cardanoProtocolParameters = requireObject(
    candidate.cardanoProtocolParameters,
    "cardanoProtocolParameters",
  );
  requireExactKeys(
    cardanoProtocolParameters,
    ["snapshot", "digest"],
    [],
    "cardanoProtocolParameters",
  );
  const cardanoSnapshot = normalizeDeploymentManifestJsonValue(
    cardanoProtocolParameters.snapshot,
    "cardanoProtocolParameters.snapshot",
  );
  const cardanoDigest = requireLowercaseHex(
    cardanoProtocolParameters.digest,
    32,
    "cardanoProtocolParameters.digest",
  );
  const expectedCardanoDigest =
    computeDeploymentManifestJsonDigest(cardanoSnapshot);
  if (cardanoDigest !== expectedCardanoDigest) {
    throw new Error(
      `Deployment manifest cardanoProtocolParameters.digest mismatch: expected ${expectedCardanoDigest}`,
    );
  }
  const genesis = requireObject(candidate.genesis, "genesis");
  requireExactKeys(genesis, ["headerHash", "utxoSetDigest"], [], "genesis");
  requireLowercaseHex(genesis.headerHash, 28, "genesis.headerHash");
  requireLowercaseHex(genesis.utxoSetDigest, 32, "genesis.utxoSetDigest");

  const contracts = requireObject(candidate.contracts, "contracts");
  validateContracts(contracts);
  const referenceScriptAuthContract = requireObject(
    contracts.referenceScriptAuthMint,
    "contracts.referenceScriptAuthMint",
  );
  if (
    referenceScriptAuthContract.scriptHash !==
    referenceScriptAuthPolicy.policyId
  ) {
    throw new Error(
      "Deployment manifest contracts.referenceScriptAuthMint.scriptHash must match referenceScriptAuthPolicy.policyId",
    );
  }
  validateReferenceScripts(
    requireObject(candidate.referenceScripts, "referenceScripts"),
    referenceScriptAuthPolicy,
    contracts,
  );
  validateDaIdentity(requireObject(candidate.da, "da"));
  const proofEvidence = requireObject(candidate.proofEvidence, "proofEvidence");
  requireExactKeys(
    proofEvidence,
    ["digest", "blueprintHash"],
    [],
    "proofEvidence",
  );
  if (
    proofEvidence.digest !== null &&
    !/^[0-9a-f]{64}$/u.test(
      requireNonEmptyString(proofEvidence.digest, "proofEvidence.digest"),
    )
  ) {
    throw new Error(
      "Deployment manifest proofEvidence.digest must be null or lowercase SHA-256 hex",
    );
  }
  if (proofEvidence.digest !== MIDGARD_RELEASE_EVIDENCE_DIGEST) {
    throw new Error(
      "Deployment manifest proofEvidence.digest must exactly match the compiled V1 release evidence",
    );
  }
  requireLowercaseHex(
    proofEvidence.blueprintHash,
    32,
    "proofEvidence.blueprintHash",
  );
  validateSteps(requireObject(candidate.steps, "steps"));
  validateValidationDispute(
    requireObject(candidate.validationDispute, "validationDispute"),
  );
  const l1Finality = requireObject(candidate.l1Finality, "l1Finality");
  requireExactKeys(
    l1Finality,
    ["confirmationDepth", "automaticRecoveryMaxDepth", "deepRollbackPolicy"],
    [],
    "l1Finality",
  );
  for (const [key, expected] of Object.entries(
    DEPLOYMENT_MANIFEST_L1_FINALITY,
  )) {
    if (l1Finality[key] !== expected) {
      throw new Error(
        `Deployment manifest l1Finality.${key} must equal ${String(expected)}`,
      );
    }
  }
  parseDeploymentManifestEconomics(candidate.economics);
  const manifestId = requireNonEmptyString(candidate.manifestId, "manifestId");
  if (!/^[0-9a-f]{64}$/.test(manifestId)) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const parsed = candidate as unknown as DeploymentManifestValue;
  const { manifestId: _manifestId, ...identityInput } = parsed;
  const expectedManifestId = computeDeploymentManifestId(identityInput);
  if (manifestId !== expectedManifestId) {
    throw new Error(
      `Deployment manifest id mismatch: expected ${expectedManifestId}, found ${manifestId}`,
    );
  }
  return parsed;
};

export const parseDeploymentManifestValue = (
  value: unknown,
): DeploymentManifestValue => {
  const candidate = verifyDeploymentManifestIdentity(value);
  requireExactKeys(
    candidate,
    [
      "schemaVersion",
      "manifestId",
      "consensusProfile",
      "consensusProfileDigest",
      "network",
      "cardanoProtocolParameters",
      "genesis",
      "createdAt",
      "updatedAt",
      "referenceScriptDeployAddress",
      "hubOracleOneShot",
      "referenceScriptAuthPolicy",
      "contracts",
      "referenceScripts",
      "da",
      "proofEvidence",
      "steps",
      "validationDispute",
      "l1Finality",
      "economics",
      "availabilityChallenge",
    ],
    [],
    "value",
  );
  if (!isMidgardConsensusProfile(candidate.consensusProfile)) {
    throw new Error(
      "Deployment manifest consensusProfile must exactly match canonical V1",
    );
  }
  if (candidate.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_DIGEST) {
    throw new Error(
      "Deployment manifest consensusProfileDigest must exactly match canonical V1",
    );
  }
  const parsed = parseDeploymentManifestCommon(candidate);
  verifyFinalizedDeploymentManifest(parsed);
  return parsed;
};
