import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";

import { encodeCbor } from "./codec/cbor.js";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  MIDGARD_RELEASE_EVIDENCE_DIGEST,
} from "./consensus-profile-v1.js";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "./da-transport.js";
import {
  buildMidgardMpfProofFoldTrace,
  type MidgardMpfProofStep,
} from "./mpf-proof-fold-v1.js";
import {
  MIDGARD_RETENTION_WINDOW,
  retentionDaysCoverWindow,
} from "./retention-window-v1.js";

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
  // #579, owner rulings of 2026-08-14. THREE names stood here and were all
  // REMOVED together: `txOrderFieldPreimageSpend`, `txOrderFieldReceiptSpend`
  // and `txOrderFieldReceiptMint`.
  //
  // They are one retirement, not three. Commit df53dc6a7 (#587), executing the
  // owner's 2026-08-10 ruling that "the tx-field receipt chain dies before
  // #579's single blueprint regeneration rather than being frozen into it",
  // deleted `tx-field-preimage-v1.ak`, `tx-field-receipt-v1.ak` and its spend
  // twin in a single commit; #594 then replaced the tx-order mint's receipt
  // parameters with the §8.6 certificate policy id. The regenerated blueprint
  // declares none of the three titles, so these names had no script to resolve
  // to and were dead roles holding live ABI positions. Removing them here is
  // what completes off-chain what #587 did on-chain.
  //
  // This is a removal from a POSITIONAL vector, so it renumbers every contract
  // after it — which is why it is an owner-authorized identity movement rather
  // than a tidy-up. The deployment-manifest id, the ABI-01/03/04/07 digests and
  // every position-derived downstream identity (the watcher's synthetic
  // catalogue among them) move with it and are re-pinned with cause in the same
  // batch, the same way the certificate's registration was appended below.
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
  // #579. The §8.6 field-preimage certificate policy. Appended rather than
  // slotted beside the transaction-field entries because this vector is
  // positional — its order IS the ABI — so an insertion would renumber every
  // contract after it. #594 retired the receipt family and gave the tx-order
  // mint and every field-opening step a
  // `field_preimage_certificate_policy_id` parameter; the certificate is the
  // one deployed policy those readers share (§8.6 Consumption,
  // docs/spec/midgard-tx.md), so it needs a deployment role rather than being
  // rederived independently at each load site.
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

export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER =
  Object.freeze([
    "doubleSpend",
    "nonExistentInput",
    "nonExistentInputNoIndex",
    "invalidRange",
    "transitionTrace",
    "zeroInput",
    "validationTraceDispute",
    "daHashPreimage",
    "noReferenceInput",
    "referenceInputNoIdx",
    "invalidSignature",
    "fabricatedDeposit",
    "fabricatedWithdrawal",
    "nativeScriptDecoding",
    "missingSignature",
    "missingNativeScriptTx",
    "withdrawnReferenceInput",
    "canonicalDecodability",
    "committedFieldShape",
    "minFee",
    "withdrawalMistag",
    "doubleWithdraw",
    "crossBlockDuplicateEvent",
    "l2TxMistag",
    "withdrawnInput",
    "valueNotPreserved",
    "inputSetUniqueness",
    "mintAuthorization",
    "networkId",
    "missingNativeScriptUtxo",
    "nativeScriptInvalid",
    "minAda",
    "fieldPreimageLengthMismatch",
    "fieldItemWidthIllegal",
    "witnessScriptDecoding",
    "scriptIntegrityHashMissing",
    "transactionOutputNonCanonical",
    "resolvedOutputNonCanonical",
    "mintDeclaredAssetLimit",
    "spendInputSignerMissing",
    "protectedOutputSignerMissing",
    "observersForbiddenOnUntaggedNetwork",
    "observerOrderInvalid",
    "redeemerCanonicity",
    "outputReferenceScriptDecoding",
    "executionSourceScriptDecoding",
    "receivePurposeLanguage",
    "unusedScriptWitness",
    "missingScriptSource",
    "missingRedeemer",
    "unusedRedeemer",
    "executionNativeScriptInvalid",
    "scriptIntegrityHashMismatch",
    "distinctAssetAccumulationLimit",
  ] as const);

export type DeploymentManifestFraudProofCatalogueCategory =
  (typeof DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number];

/** Canonical first-step deployment contract for each catalogue category. */
export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY =
  Object.freeze({
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
  } as const satisfies Record<
    DeploymentManifestFraudProofCatalogueCategory,
    string
  >);

/** Frozen wire identities; category array position is presentation only. */
export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS =
  Object.freeze({
    doubleSpend: "00000000",
    nonExistentInput: "00000001",
    nonExistentInputNoIndex: "00000002",
    invalidRange: "00000003",
    transitionTrace: "00000004",
    zeroInput: "00000005",
    validationTraceDispute: "00000006",
    daHashPreimage: "00000007",
    noReferenceInput: "00000008",
    referenceInputNoIdx: "00000009",
    invalidSignature: "0000000a",
    fabricatedDeposit: "0000000b",
    fabricatedWithdrawal: "0000000c",
    nativeScriptDecoding: "0000000d",
    missingSignature: "0000000e",
    missingNativeScriptTx: "0000000f",
    withdrawnReferenceInput: "00000010",
    canonicalDecodability: "00000011",
    committedFieldShape: "00000012",
    minFee: "00000013",
    withdrawalMistag: "00000014",
    doubleWithdraw: "00000015",
    crossBlockDuplicateEvent: "00000016",
    l2TxMistag: "00000017",
    withdrawnInput: "00000018",
    valueNotPreserved: "00000019",
    inputSetUniqueness: "0000001a",
    mintAuthorization: "0000001b",
    networkId: "0000001c",
    missingNativeScriptUtxo: "0000001d",
    nativeScriptInvalid: "0000001e",
    minAda: "0000001f",
    fieldPreimageLengthMismatch: "00000020",
    fieldItemWidthIllegal: "00000021",
    witnessScriptDecoding: "00000022",
    scriptIntegrityHashMissing: "00000023",
    transactionOutputNonCanonical: "00000029",
    resolvedOutputNonCanonical: "00000026",
    mintDeclaredAssetLimit: "0000002c",
    spendInputSignerMissing: "00000027",
    protectedOutputSignerMissing: "0000002b",
    observersForbiddenOnUntaggedNetwork: "00000024",
    outputReferenceScriptDecoding: "0000002a",
    executionSourceScriptDecoding: "00000031",
    observerOrderInvalid: "00000025",
    redeemerCanonicity: "00000028",
    receivePurposeLanguage: "00000034",
    unusedScriptWitness: "0000002f",
    missingScriptSource: "0000002d",
    missingRedeemer: "0000002e",
    unusedRedeemer: "00000030",
    executionNativeScriptInvalid: "00000032",
    scriptIntegrityHashMismatch: "00000033",
    distinctAssetAccumulationLimit: "00000035",
  } as const satisfies Readonly<
    Record<DeploymentManifestFraudProofCatalogueCategory, string>
  >);

export type DeploymentManifestFraudProofCatalogueCategoryIdentity = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export type DeploymentManifestFraudProofCatalogueIdentity = {
  readonly root: string;
  readonly categories: Readonly<
    Record<
      DeploymentManifestFraudProofCatalogueCategory,
      DeploymentManifestFraudProofCatalogueCategoryIdentity
    >
  >;
};

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
    // #579: all three "V1 transaction-field" roles — the preimage publication
    // and the two receipt roles — are gone with the contracts they named. A
    // reference-script role whose contract no longer exists cannot be deployed,
    // so leaving one would make this map describe a deployment that can never be
    // performed.
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

export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES = Object.freeze({
  "reference-script-auth minting": "ReferenceScriptAuthMint",
  "hub-oracle minting": "HubOracleMint",
  "da-params-governor spending": "DaParamsGovernorSpend",
  "da-params-governor minting": "DaParamsGovernorMint",
  "da-attestation spending": "DaAttestationSpend",
  "da-attestation minting": "DaAttestationMint",
  "state-queue spending": "StateQueueSpend",
  "state-queue minting": "StateQueueMint",
  "state-queue commit withdrawal": "StateQueueCommitYield",
  "state-queue unattested-timeout withdrawal": "StateQueueUnattestedYield",
  "state-queue unavailable-timeout withdrawal": "StateQueueUnavailableYield",
  "state-queue fraud-removal withdrawal": "StateQueueFraudRemovalYield",
  "state-queue merge withdrawal": "StateQueueMergeYield",
  "scheduler spending": "SchedulerSpend",
  "scheduler minting": "SchedulerMint",
  "registered-operators spending": "RegisteredOperatorsSpend",
  "registered-operators minting": "RegisteredOperatorsMint",
  "active-operators spending": "ActiveOperatorsSpend",
  "active-operators minting": "ActiveOperatorsMint",
  "retired-operators spending": "RetiredOperatorsSpend",
  "retired-operators minting": "RetiredOperatorsMint",
  "fraud-proof-catalogue minting": "FraudProofCatalogueMint",
  "deposit spending": "DepositSpend",
  "deposit minting": "DepositMint",
  "withdrawal spending": "WithdrawalSpend",
  "withdrawal minting": "WithdrawalMint",
  "settlement minting": "SettlementMint",
  "payout spending": "PayoutSpend",
  "payout minting": "PayoutMint",
  "reserve spending": "ReserveSpend",
  "reserve observer": "ReserveObserver",
  "membership proof withdrawal": "MembershipProofWithdraw",
  // #579: `V1TxFieldPreimageSpend`, `V1TxFieldReceiptSpend` and
  // `V1TxFieldReceiptMint` retired with their roles. Token names are minted
  // per deployed reference script; a name with no script mints nothing.
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
  "V1 fraud-proof transition-trace route": "V1FpTransitionTraceRoute",
  "V1 fraud-proof transition-trace final-0": "V1FpTransitionTraceFinal0",
  "V1 fraud-proof transition-trace final-1": "V1FpTransitionTraceFinal1",
  "V1 fraud-proof transition-trace final-2": "V1FpTransitionTraceFinal2",
  "V1 fraud-proof transition-trace final-3": "V1FpTransitionTraceFinal3",
  "V1 fraud-proof transition-trace final-4": "V1FpTransitionTraceFinal4",
  "V1 fraud-proof transition-trace final-5": "V1FpTransitionTraceFinal5",
  "V1 fraud-proof transition-trace final-6": "V1FpTransitionTraceFinal6",
  "V1 fraud-proof transition-trace final-7": "V1FpTransitionTraceFinal7",
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
  "availability-challenge spending": "AvailabilityChallengeSpend",
  "availability-challenge minting": "AvailabilityChallengeMint",
} as const);

export const DEPLOYMENT_MANIFEST_STEP_NAMES = Object.freeze([
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
  "phasRegistration",
  "operatorRegistration",
  "operatorActivation",
] as const);

export const DEPLOYMENT_MANIFEST_L1_FINALITY = Object.freeze({
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const);

export type DeploymentManifestL1Finality =
  typeof DEPLOYMENT_MANIFEST_L1_FINALITY;

export type DeploymentManifestCanonicalRational = Readonly<{
  numerator: string;
  denominator: string;
}>;

/**
 * Exact release-bound subset used to size prover funding, collateral and
 * reference-script fees. All ledger naturals use canonical decimal strings;
 * rationals retain numerator/denominator identity and never pass through a
 * JavaScript float.
 */
export type DeploymentManifestCardanoProtocolParameters = Readonly<{
  minFeeA: string;
  minFeeB: string;
  priceMemory: DeploymentManifestCanonicalRational;
  priceSteps: DeploymentManifestCanonicalRational;
  coinsPerUtxoByte: string;
  collateralPercentage: string;
  maxCollateralInputs: string;
  maxTxSize: string;
  maxValueSize: string;
  maxTxExUnits: Readonly<{ memory: string; steps: string }>;
  referenceScriptFee: Readonly<{
    base: DeploymentManifestCanonicalRational;
    range: string;
    multiplier: DeploymentManifestCanonicalRational;
    maximumSizeBytes: string;
  }>;
}>;

export const DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE = Object.freeze({
  "public-preprod-launch-v1": Object.freeze({
    profile: "public-preprod-launch-v1" as const,
    requiredBondLovelace: 100_000_000_000,
    slashingPenaltyLovelace: 25_000_000_000,
    inactivitySlashingPenaltyLovelace: 10_000_000_000,
    fraudProverRewardLovelace: 75_000_000_000,
    proverCollateralFloorLovelace: 5_000_000,
  }),
  "bounded-acceptance-v1": Object.freeze({
    profile: "bounded-acceptance-v1" as const,
    requiredBondLovelace: 900_000_000,
    slashingPenaltyLovelace: 500_000_000,
    inactivitySlashingPenaltyLovelace: 100_000_000,
    fraudProverRewardLovelace: 400_000_000,
    proverCollateralFloorLovelace: 5_000_000,
  }),
} as const);

export type DeploymentManifestEconomicsProfile =
  keyof typeof DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE;

export type DeploymentManifestEconomics =
  (typeof DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE)[DeploymentManifestEconomicsProfile];

export type DeploymentManifestAvailabilityChallenge = Readonly<{
  responseClasses: Readonly<{
    smallPayloadMaxBytes: 65_536;
    smallResponseWindowMs: 3_600_000;
    fullPayloadMaxBytes: 67_108_864;
    fullResponseWindowMs: 172_800_000;
  }>;
  responseGeometry: Readonly<{
    chunkByteLength: number;
    trancheByteLength: number;
    maxTrancheCount: number;
  }>;
  daBondLovelace: number;
  challengerBondLovelace: number;
  maxOpenFeeLovelace: number;
  maxPublicationFeeLovelace: number;
  maxSettlementFeeLovelace: number;
  maxCloseFeeLovelace: number;
  maxTimeoutFeeLovelace: number;
  /** Exact enterprise vkey credential that owns the retained per-header bond. */
  bondOwnerCredential: string;
}>;

export const DEPLOYMENT_MANIFEST_ROOT_KEYS = Object.freeze([
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
] as const);

export type DeploymentManifestJsonValue =
  | null
  | boolean
  | number
  | string
  | readonly DeploymentManifestJsonValue[]
  | { readonly [key: string]: DeploymentManifestJsonValue };

export const MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION =
  "midgard-deployment-marker-v1" as const;

export type DeploymentMarker = {
  readonly schemaVersion: typeof MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION;
  readonly manifestId: string;
};

const requireRecord = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${field} must be a plain object`);
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new Error(`${field} must contain only string keys`);
  }
  return value as Record<string, unknown>;
};

const requireDeploymentManifestId = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${field} must be lowercase SHA-256 hex`);
  }
  return value;
};

/**
 * Parse the release-bound operator/fraud-proof economics block without a node
 * package dependency. Only the two compiled launch profiles and their exact
 * tuples are admissible; a network label is never consulted.
 */
export const parseDeploymentManifestEconomics = (
  value: unknown,
): DeploymentManifestEconomics => {
  const candidate = requireRecord(value, "Deployment manifest economics");
  const required = [
    "profile",
    "requiredBondLovelace",
    "slashingPenaltyLovelace",
    "inactivitySlashingPenaltyLovelace",
    "fraudProverRewardLovelace",
    "proverCollateralFloorLovelace",
  ] as const;
  if (
    Object.keys(candidate).length !== required.length ||
    required.some(
      (key) => !Object.prototype.hasOwnProperty.call(candidate, key),
    )
  ) {
    throw new Error(
      `Deployment manifest economics must contain exactly ${required.join(", ")}`,
    );
  }
  if (
    candidate.profile !== "public-preprod-launch-v1" &&
    candidate.profile !== "bounded-acceptance-v1"
  ) {
    throw new Error(
      "Deployment manifest economics.profile must be public-preprod-launch-v1 or bounded-acceptance-v1",
    );
  }
  const profile = candidate.profile;
  const expected = DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE[profile];
  for (const key of required.slice(1)) {
    const observed = candidate[key];
    if (!Number.isSafeInteger(observed) || observed !== expected[key]) {
      throw new Error(
        `Deployment manifest economics.${key} must equal ${expected[key].toString()} for ${profile}`,
      );
    }
  }
  if (
    expected.requiredBondLovelace !==
    expected.slashingPenaltyLovelace + expected.fraudProverRewardLovelace
  ) {
    throw new Error(
      "Deployment manifest economics required bond must equal slash plus reward",
    );
  }
  if (
    expected.requiredBondLovelace -
      expected.inactivitySlashingPenaltyLovelace <=
    0
  ) {
    throw new Error(
      "Deployment manifest economics required bond minus inactivity penalty must be positive",
    );
  }
  return expected;
};

const exactAvailabilityInteger = (value: unknown, field: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) <= 0) {
    throw new Error(`${field} must be a positive safe integer`);
  }
  return value as number;
};

/**
 * Absolute Q58 response-publication safety ceiling. This is deliberately
 * separate from the 4,095-byte transaction-field proof chunk bound: the
 * activated DA response chunk length is release-authenticated and must be
 * justified by the signed transaction-size measurement artifact.
 */
export const MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES = 15_148;

/**
 * Parse the release-authenticated Q58 geometry, response classes and fee/bond
 * ceilings. These values are deployment identity: neither a network label nor
 * caller metadata may choose them after the scripts are applied.
 */
export const parseDeploymentManifestAvailabilityChallenge = (
  value: unknown,
): DeploymentManifestAvailabilityChallenge => {
  const candidate = requireRecord(
    value,
    "Deployment manifest availabilityChallenge",
  );
  const required = [
    "responseClasses",
    "responseGeometry",
    "daBondLovelace",
    "challengerBondLovelace",
    "maxOpenFeeLovelace",
    "maxPublicationFeeLovelace",
    "maxSettlementFeeLovelace",
    "maxCloseFeeLovelace",
    "maxTimeoutFeeLovelace",
    "bondOwnerCredential",
  ] as const;
  if (
    Object.keys(candidate).length !== required.length ||
    required.some(
      (key) => !Object.prototype.hasOwnProperty.call(candidate, key),
    )
  ) {
    throw new Error(
      `Deployment manifest availabilityChallenge must contain exactly ${required.join(", ")}`,
    );
  }

  const responseClasses = requireRecord(
    candidate.responseClasses,
    "Deployment manifest availabilityChallenge.responseClasses",
  );
  const expectedClasses = {
    smallPayloadMaxBytes: 65_536,
    smallResponseWindowMs: 3_600_000,
    fullPayloadMaxBytes: 67_108_864,
    fullResponseWindowMs: 172_800_000,
  } as const;
  if (
    Object.keys(responseClasses).length !== Object.keys(expectedClasses).length
  ) {
    throw new Error(
      "Deployment manifest availabilityChallenge.responseClasses must contain exactly the canonical V1 response class fields",
    );
  }
  for (const [key, expected] of Object.entries(expectedClasses)) {
    if (responseClasses[key] !== expected) {
      throw new Error(
        `Deployment manifest availabilityChallenge.responseClasses.${key} must equal ${expected.toString()}`,
      );
    }
  }

  const geometry = requireRecord(
    candidate.responseGeometry,
    "Deployment manifest availabilityChallenge.responseGeometry",
  );
  const geometryKeys = [
    "chunkByteLength",
    "trancheByteLength",
    "maxTrancheCount",
  ] as const;
  if (
    Object.keys(geometry).length !== geometryKeys.length ||
    geometryKeys.some(
      (key) => !Object.prototype.hasOwnProperty.call(geometry, key),
    )
  ) {
    throw new Error(
      `Deployment manifest availabilityChallenge.responseGeometry must contain exactly ${geometryKeys.join(", ")}`,
    );
  }
  const chunkByteLength = exactAvailabilityInteger(
    geometry.chunkByteLength,
    "Deployment manifest availabilityChallenge.responseGeometry.chunkByteLength",
  );
  const trancheByteLength = exactAvailabilityInteger(
    geometry.trancheByteLength,
    "Deployment manifest availabilityChallenge.responseGeometry.trancheByteLength",
  );
  const maxTrancheCount = exactAvailabilityInteger(
    geometry.maxTrancheCount,
    "Deployment manifest availabilityChallenge.responseGeometry.maxTrancheCount",
  );
  if (
    chunkByteLength > MIDGARD_DA_AVAILABILITY_MAX_RESPONSE_CHUNK_SAFETY_BYTES ||
    trancheByteLength < expectedClasses.smallPayloadMaxBytes ||
    trancheByteLength > expectedClasses.fullPayloadMaxBytes ||
    maxTrancheCount > MIDGARD_CONSENSUS_PROFILE.limits.maxOutputCount ||
    Math.ceil(expectedClasses.fullPayloadMaxBytes / trancheByteLength) >
      maxTrancheCount
  ) {
    throw new Error(
      "Deployment manifest availabilityChallenge.responseGeometry violates canonical V1 safety/coverage bounds",
    );
  }

  const daBondLovelace = exactAvailabilityInteger(
    candidate.daBondLovelace,
    "Deployment manifest availabilityChallenge.daBondLovelace",
  );
  const challengerBondLovelace = exactAvailabilityInteger(
    candidate.challengerBondLovelace,
    "Deployment manifest availabilityChallenge.challengerBondLovelace",
  );
  const maxPublicationFeeLovelace = exactAvailabilityInteger(
    candidate.maxPublicationFeeLovelace,
    "Deployment manifest availabilityChallenge.maxPublicationFeeLovelace",
  );
  const maxOpenFeeLovelace = exactAvailabilityInteger(
    candidate.maxOpenFeeLovelace,
    "Deployment manifest availabilityChallenge.maxOpenFeeLovelace",
  );
  const maxSettlementFeeLovelace = exactAvailabilityInteger(
    candidate.maxSettlementFeeLovelace,
    "Deployment manifest availabilityChallenge.maxSettlementFeeLovelace",
  );
  const maxCloseFeeLovelace = exactAvailabilityInteger(
    candidate.maxCloseFeeLovelace,
    "Deployment manifest availabilityChallenge.maxCloseFeeLovelace",
  );
  const maxTimeoutFeeLovelace = exactAvailabilityInteger(
    candidate.maxTimeoutFeeLovelace,
    "Deployment manifest availabilityChallenge.maxTimeoutFeeLovelace",
  );
  if (challengerBondLovelace !== daBondLovelace) {
    throw new Error(
      "Deployment manifest availabilityChallenge DA and challenger bonds must match exactly",
    );
  }
  const bondOwnerCredential = candidate.bondOwnerCredential;
  if (
    typeof bondOwnerCredential !== "string" ||
    !/^[0-9a-f]{56}$/u.test(bondOwnerCredential)
  ) {
    throw new Error(
      "Deployment manifest availabilityChallenge.bondOwnerCredential must be exactly 28 lowercase hex bytes",
    );
  }
  let publicationCount = 0;
  for (
    let offset = 0;
    offset < expectedClasses.fullPayloadMaxBytes;
    offset += trancheByteLength
  ) {
    publicationCount += Math.ceil(
      Math.min(
        trancheByteLength,
        expectedClasses.fullPayloadMaxBytes - offset,
      ) / chunkByteLength,
    );
  }
  if (
    publicationCount * maxPublicationFeeLovelace +
      maxTrancheCount * maxSettlementFeeLovelace +
      Math.max(maxCloseFeeLovelace, maxTimeoutFeeLovelace) >=
    challengerBondLovelace
  ) {
    throw new Error(
      "Deployment manifest availabilityChallenge challenger bond must cover every maximum-size publication, tranche-settlement, and terminal fee ceiling",
    );
  }
  return Object.freeze({
    responseClasses: Object.freeze(expectedClasses),
    responseGeometry: Object.freeze({
      chunkByteLength,
      trancheByteLength,
      maxTrancheCount,
    }),
    daBondLovelace,
    challengerBondLovelace,
    maxOpenFeeLovelace,
    maxPublicationFeeLovelace,
    maxSettlementFeeLovelace,
    maxCloseFeeLovelace,
    maxTimeoutFeeLovelace,
    bondOwnerCredential,
  });
};

export const parseDeploymentMarker = (value: unknown): DeploymentMarker => {
  const candidate = requireRecord(value, "Deployment marker V1");
  const keys = Object.keys(candidate);
  if (
    keys.length !== 2 ||
    !Object.prototype.hasOwnProperty.call(candidate, "schemaVersion") ||
    !Object.prototype.hasOwnProperty.call(candidate, "manifestId")
  ) {
    throw new Error(
      "Deployment marker V1 must contain exactly schemaVersion and manifestId",
    );
  }
  if (candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION) {
    throw new Error(
      `Deployment marker V1 schemaVersion must be ${MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION}`,
    );
  }
  return {
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
    manifestId: requireDeploymentManifestId(
      candidate.manifestId,
      "Deployment marker V1 manifestId",
    ),
  };
};

export const makeDeploymentMarker = (manifestId: string): DeploymentMarker =>
  parseDeploymentMarker({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
    manifestId,
  });

export const assertDeploymentMarkerMatches = (
  expected: DeploymentMarker,
  actual: unknown,
  boundary = "deployment boundary",
): DeploymentMarker => {
  const canonicalExpected = parseDeploymentMarker(expected);
  const canonicalActual = parseDeploymentMarker(actual);
  if (canonicalActual.manifestId !== canonicalExpected.manifestId) {
    throw new Error(
      `${boundary} deployment marker mismatch: expected ${canonicalExpected.manifestId}, found ${canonicalActual.manifestId}`,
    );
  }
  return canonicalActual;
};

const normalizeDeploymentManifestJsonValueInternal = (
  value: unknown,
  field: string,
  stringifyBigInt: boolean,
): DeploymentManifestJsonValue => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return value;
  }
  if (typeof value === "bigint") {
    if (stringifyBigInt) {
      return value.toString(10);
    }
    throw new Error(`${field} must contain only JSON-safe values`);
  }
  if (typeof value === "number") {
    if (!Number.isFinite(value)) {
      throw new Error(`${field} must contain only finite numbers`);
    }
    return value;
  }
  if (Array.isArray(value)) {
    return value.map((entry, index) =>
      normalizeDeploymentManifestJsonValueInternal(
        entry,
        `${field}[${index.toString()}]`,
        stringifyBigInt,
      ),
    );
  }
  if (typeof value !== "object" || value === null) {
    throw new Error(`${field} must contain only JSON-safe values`);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${field} must contain only plain records`);
  }
  if (Reflect.ownKeys(value).length !== Object.keys(value).length) {
    throw new Error(`${field} must contain only string keys`);
  }
  return Object.fromEntries(
    Object.entries(value as Record<string, unknown>).map(([key, entry]) => {
      if (entry === undefined) {
        throw new Error(`${field}.${key} must not be undefined`);
      }
      return [
        key,
        normalizeDeploymentManifestJsonValueInternal(
          entry,
          `${field}.${key}`,
          stringifyBigInt,
        ),
      ];
    }),
  );
};

export const normalizeDeploymentManifestJsonValue = (
  value: unknown,
  field = "value",
): DeploymentManifestJsonValue =>
  normalizeDeploymentManifestJsonValueInternal(
    value,
    `Deployment manifest ${field}`,
    true,
  );

const stableJson = (value: DeploymentManifestJsonValue): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  return `{${Object.entries(value)
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([key, entry]) => `${JSON.stringify(key)}:${stableJson(entry)}`)
    .join(",")}}`;
};

export const computeDeploymentManifestJsonDigest = (value: unknown): string => {
  const normalized = normalizeDeploymentManifestJsonValueInternal(
    value,
    "Deployment manifest JSON digest input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

const exactRoot = (candidate: Record<string, unknown>): void => {
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_ROOT_KEYS);
  for (const key of Object.keys(candidate)) {
    if (!expected.has(key)) {
      throw new Error(`Deployment manifest value.${key} is unexpected`);
    }
  }
  for (const key of DEPLOYMENT_MANIFEST_ROOT_KEYS) {
    if (!Object.prototype.hasOwnProperty.call(candidate, key)) {
      throw new Error(`Deployment manifest value.${key} is required`);
    }
  }
};

export const computeDeploymentManifestId = (
  identityInput: Record<string, unknown>,
): string => {
  if (Object.prototype.hasOwnProperty.call(identityInput, "manifestId")) {
    throw new Error("Deployment manifest identity input must omit manifestId");
  }
  const normalized = normalizeDeploymentManifestJsonValueInternal(
    identityInput,
    "Deployment manifest identity input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

export const verifyDeploymentManifestIdentity = (
  value: unknown,
): Record<string, unknown> => {
  const candidate = requireRecord(value, "Deployment manifest value");
  if (candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION) {
    throw new Error(
      `Deployment manifest schemaVersion must be ${MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION}`,
    );
  }
  exactRoot(candidate);
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
  parseDeploymentManifestEconomics(candidate.economics);
  parseDeploymentManifestAvailabilityChallenge(candidate.availabilityChallenge);
  if (
    typeof candidate.manifestId !== "string" ||
    !/^[0-9a-f]{64}$/u.test(candidate.manifestId)
  ) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const { manifestId, ...identityInput } = candidate;
  const expectedManifestId = computeDeploymentManifestId(identityInput);
  if (manifestId !== expectedManifestId) {
    throw new Error(
      `Deployment manifest id mismatch: expected ${expectedManifestId}, found ${manifestId}`,
    );
  }
  return candidate;
};

const requireExactKeys = (
  value: Record<string, unknown>,
  required: readonly string[],
  optional: readonly string[] = [],
  field: string,
): void => {
  const allowed = new Set([...required, ...optional]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) {
      throw new Error(`Deployment manifest ${field}.${key} is unexpected`);
    }
  }
  for (const key of required) {
    if (!Object.prototype.hasOwnProperty.call(value, key)) {
      throw new Error(`Deployment manifest ${field}.${key} is required`);
    }
  }
};

const requireString = (value: unknown, field: string): string => {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`Deployment manifest ${field} must be a non-empty string`);
  }
  return value;
};

const requireHex = (
  value: unknown,
  bytes: number | undefined,
  field: string,
): string => {
  const text = requireString(value, field);
  const pattern =
    bytes === undefined
      ? /^(?:[0-9a-f]{2})+$/u
      : new RegExp(`^[0-9a-f]{${(bytes * 2).toString()}}$`, "u");
  if (!pattern.test(text)) {
    throw new Error(
      `Deployment manifest ${field} must be lowercase canonical hex`,
    );
  }
  return text;
};

const requireInteger = (value: unknown, field: string, minimum = 0): number => {
  if (
    typeof value !== "number" ||
    !Number.isSafeInteger(value) ||
    value < minimum
  ) {
    throw new Error(
      `Deployment manifest ${field} must be an integer >= ${minimum.toString()}`,
    );
  }
  return value;
};

const requireCanonicalNatural = (value: unknown, field: string): string => {
  if (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical natural decimal string`,
    );
  }
  return value;
};

const greatestCommonDivisor = (left: bigint, right: bigint): bigint => {
  let a = left;
  let b = right;
  while (b !== 0n) {
    const remainder = a % b;
    a = b;
    b = remainder;
  }
  return a;
};

const requireCanonicalRational = (
  value: unknown,
  field: string,
): DeploymentManifestCanonicalRational => {
  const candidate = requireRecord(value, `Deployment manifest ${field}`);
  requireExactKeys(candidate, ["numerator", "denominator"], [], field);
  const numerator = requireCanonicalNatural(
    candidate.numerator,
    `${field}.numerator`,
  );
  const denominator = requireCanonicalNatural(
    candidate.denominator,
    `${field}.denominator`,
  );
  if (denominator === "0") {
    throw new Error(
      `Deployment manifest ${field}.denominator must be positive`,
    );
  }
  if (greatestCommonDivisor(BigInt(numerator), BigInt(denominator)) !== 1n) {
    throw new Error(`Deployment manifest ${field} must be reduced`);
  }
  return Object.freeze({ numerator, denominator });
};

export const parseDeploymentManifestCardanoProtocolParameters = (
  value: unknown,
): DeploymentManifestCardanoProtocolParameters => {
  const candidate = requireRecord(
    value,
    "Deployment manifest cardanoProtocolParameters.snapshot",
  );
  requireExactKeys(
    candidate,
    [
      "minFeeA",
      "minFeeB",
      "priceMemory",
      "priceSteps",
      "coinsPerUtxoByte",
      "collateralPercentage",
      "maxCollateralInputs",
      "maxTxSize",
      "maxValueSize",
      "maxTxExUnits",
      "referenceScriptFee",
    ],
    [],
    "cardanoProtocolParameters.snapshot",
  );
  const maxTxExUnits = requireRecord(
    candidate.maxTxExUnits,
    "Deployment manifest cardanoProtocolParameters.snapshot.maxTxExUnits",
  );
  requireExactKeys(
    maxTxExUnits,
    ["memory", "steps"],
    [],
    "cardanoProtocolParameters.snapshot.maxTxExUnits",
  );
  const referenceScriptFee = requireRecord(
    candidate.referenceScriptFee,
    "Deployment manifest cardanoProtocolParameters.snapshot.referenceScriptFee",
  );
  requireExactKeys(
    referenceScriptFee,
    ["base", "range", "multiplier", "maximumSizeBytes"],
    [],
    "cardanoProtocolParameters.snapshot.referenceScriptFee",
  );
  const parsed = {
    minFeeA: requireCanonicalNatural(
      candidate.minFeeA,
      "cardanoProtocolParameters.snapshot.minFeeA",
    ),
    minFeeB: requireCanonicalNatural(
      candidate.minFeeB,
      "cardanoProtocolParameters.snapshot.minFeeB",
    ),
    priceMemory: requireCanonicalRational(
      candidate.priceMemory,
      "cardanoProtocolParameters.snapshot.priceMemory",
    ),
    priceSteps: requireCanonicalRational(
      candidate.priceSteps,
      "cardanoProtocolParameters.snapshot.priceSteps",
    ),
    coinsPerUtxoByte: requireCanonicalNatural(
      candidate.coinsPerUtxoByte,
      "cardanoProtocolParameters.snapshot.coinsPerUtxoByte",
    ),
    collateralPercentage: requireCanonicalNatural(
      candidate.collateralPercentage,
      "cardanoProtocolParameters.snapshot.collateralPercentage",
    ),
    maxCollateralInputs: requireCanonicalNatural(
      candidate.maxCollateralInputs,
      "cardanoProtocolParameters.snapshot.maxCollateralInputs",
    ),
    maxTxSize: requireCanonicalNatural(
      candidate.maxTxSize,
      "cardanoProtocolParameters.snapshot.maxTxSize",
    ),
    maxValueSize: requireCanonicalNatural(
      candidate.maxValueSize,
      "cardanoProtocolParameters.snapshot.maxValueSize",
    ),
    maxTxExUnits: Object.freeze({
      memory: requireCanonicalNatural(
        maxTxExUnits.memory,
        "cardanoProtocolParameters.snapshot.maxTxExUnits.memory",
      ),
      steps: requireCanonicalNatural(
        maxTxExUnits.steps,
        "cardanoProtocolParameters.snapshot.maxTxExUnits.steps",
      ),
    }),
    referenceScriptFee: Object.freeze({
      base: requireCanonicalRational(
        referenceScriptFee.base,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.base",
      ),
      range: requireCanonicalNatural(
        referenceScriptFee.range,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.range",
      ),
      multiplier: requireCanonicalRational(
        referenceScriptFee.multiplier,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.multiplier",
      ),
      maximumSizeBytes: requireCanonicalNatural(
        referenceScriptFee.maximumSizeBytes,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.maximumSizeBytes",
      ),
    }),
  } satisfies DeploymentManifestCardanoProtocolParameters;
  if (
    BigInt(parsed.maxTxSize) === 0n ||
    BigInt(parsed.maxValueSize) === 0n ||
    BigInt(parsed.maxTxExUnits.memory) === 0n ||
    BigInt(parsed.maxTxExUnits.steps) === 0n ||
    BigInt(parsed.coinsPerUtxoByte) === 0n ||
    BigInt(parsed.maxCollateralInputs) === 0n ||
    BigInt(parsed.referenceScriptFee.range) === 0n ||
    BigInt(parsed.referenceScriptFee.maximumSizeBytes) === 0n ||
    BigInt(parsed.referenceScriptFee.base.numerator) === 0n ||
    BigInt(parsed.referenceScriptFee.multiplier.numerator) === 0n
  ) {
    throw new Error(
      "Deployment manifest funding protocol-parameter bounds must be positive",
    );
  }
  return Object.freeze(parsed);
};

const protocolParameterNatural = (value: unknown, field: string): string => {
  if (typeof value === "bigint" && value >= 0n) return value.toString(10);
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value.toString(10);
  }
  if (typeof value === "string" && /^(?:0|[1-9][0-9]*)$/u.test(value)) {
    return value;
  }
  throw new Error(`Ogmios protocol parameter ${field} must be a natural`);
};

const protocolParameterRational = (
  value: unknown,
  field: string,
): DeploymentManifestCanonicalRational => {
  let numerator: bigint;
  let denominator: bigint;
  if (typeof value === "string" && /^[0-9]+\/[1-9][0-9]*$/u.test(value)) {
    const [left, right] = value.split("/") as [string, string];
    numerator = BigInt(left);
    denominator = BigInt(right);
  } else if (
    (typeof value === "number" && Number.isFinite(value) && value >= 0) ||
    (typeof value === "string" &&
      /^(?:0|[1-9][0-9]*)(?:\.[0-9]+)?$/u.test(value))
  ) {
    const decimal = typeof value === "number" ? value.toString() : value;
    if (/e/iu.test(decimal)) {
      throw new Error(
        `Ogmios protocol parameter ${field} must not use exponent notation`,
      );
    }
    const [whole, fractional = ""] = decimal.split(".") as [string, string?];
    denominator = 10n ** BigInt(fractional.length);
    numerator = BigInt(`${whole}${fractional}`);
  } else {
    throw new Error(
      `Ogmios protocol parameter ${field} must be an exact nonnegative rational`,
    );
  }
  const divisor = greatestCommonDivisor(numerator, denominator);
  return Object.freeze({
    numerator: (numerator / divisor).toString(10),
    denominator: (denominator / divisor).toString(10),
  });
};

/**
 * Derives the release identity directly from Ogmios' raw Conway protocol-
 * parameter response.  Consumers compare this canonical value with the
 * signed deployment snapshot; a provider-normalized projection is never an
 * authority at runtime.
 */
export const deriveDeploymentManifestCardanoProtocolParametersFromOgmios = (
  value: unknown,
): DeploymentManifestCardanoProtocolParameters => {
  const envelope = requireRecord(value, "Ogmios protocol parameters response");
  const raw = requireRecord(
    Object.prototype.hasOwnProperty.call(envelope, "result")
      ? envelope.result
      : envelope,
    "Ogmios protocol parameters result",
  );
  const minFeeConstant = requireRecord(
    raw.minFeeConstant,
    "Ogmios minFeeConstant",
  );
  const minFeeAda = requireRecord(
    minFeeConstant.ada,
    "Ogmios minFeeConstant.ada",
  );
  const maxTransactionSize = requireRecord(
    raw.maxTransactionSize,
    "Ogmios maxTransactionSize",
  );
  const maxValueSize = requireRecord(raw.maxValueSize, "Ogmios maxValueSize");
  const maxExecutionUnits = requireRecord(
    raw.maxExecutionUnitsPerTransaction,
    "Ogmios maxExecutionUnitsPerTransaction",
  );
  const prices = requireRecord(
    raw.scriptExecutionPrices,
    "Ogmios scriptExecutionPrices",
  );
  const referenceFee = requireRecord(
    raw.minFeeReferenceScripts,
    "Ogmios minFeeReferenceScripts",
  );
  const canonicalMaximum = raw.maxReferenceScriptsSizePerTransaction;
  const legacyMaximum = raw.maxReferenceScriptsSize;
  if (canonicalMaximum === undefined && legacyMaximum === undefined) {
    throw new Error(
      "Ogmios protocol parameters omit maxReferenceScriptsSizePerTransaction",
    );
  }
  const maximum = requireRecord(
    canonicalMaximum ?? legacyMaximum,
    "Ogmios maxReferenceScriptsSizePerTransaction",
  );
  if (
    canonicalMaximum !== undefined &&
    legacyMaximum !== undefined &&
    protocolParameterNatural(
      requireRecord(
        canonicalMaximum,
        "Ogmios maxReferenceScriptsSizePerTransaction",
      ).bytes,
      "maxReferenceScriptsSizePerTransaction.bytes",
    ) !==
      protocolParameterNatural(
        requireRecord(legacyMaximum, "Ogmios maxReferenceScriptsSize").bytes,
        "maxReferenceScriptsSize.bytes",
      )
  ) {
    throw new Error("Ogmios reference-script maximum aliases disagree");
  }
  return parseDeploymentManifestCardanoProtocolParameters({
    minFeeA: protocolParameterNatural(
      raw.minFeeCoefficient,
      "minFeeCoefficient",
    ),
    minFeeB: protocolParameterNatural(
      minFeeAda.lovelace,
      "minFeeConstant.ada.lovelace",
    ),
    priceMemory: protocolParameterRational(
      prices.memory,
      "scriptExecutionPrices.memory",
    ),
    priceSteps: protocolParameterRational(
      prices.cpu,
      "scriptExecutionPrices.cpu",
    ),
    coinsPerUtxoByte: protocolParameterNatural(
      raw.minUtxoDepositCoefficient,
      "minUtxoDepositCoefficient",
    ),
    collateralPercentage: protocolParameterNatural(
      raw.collateralPercentage,
      "collateralPercentage",
    ),
    maxCollateralInputs: protocolParameterNatural(
      raw.maxCollateralInputs,
      "maxCollateralInputs",
    ),
    maxTxSize: protocolParameterNatural(
      maxTransactionSize.bytes,
      "maxTransactionSize.bytes",
    ),
    maxValueSize: protocolParameterNatural(
      maxValueSize.bytes,
      "maxValueSize.bytes",
    ),
    maxTxExUnits: {
      memory: protocolParameterNatural(
        maxExecutionUnits.memory,
        "maxExecutionUnitsPerTransaction.memory",
      ),
      steps: protocolParameterNatural(
        maxExecutionUnits.cpu,
        "maxExecutionUnitsPerTransaction.cpu",
      ),
    },
    referenceScriptFee: {
      base: protocolParameterRational(
        referenceFee.base,
        "minFeeReferenceScripts.base",
      ),
      range: protocolParameterNatural(
        referenceFee.range,
        "minFeeReferenceScripts.range",
      ),
      multiplier: protocolParameterRational(
        referenceFee.multiplier,
        "minFeeReferenceScripts.multiplier",
      ),
      maximumSizeBytes: protocolParameterNatural(
        maximum.bytes,
        "maxReferenceScriptsSizePerTransaction.bytes",
      ),
    },
  });
};

const requireFinalOutRef = (
  value: unknown,
  field: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  const outRef = requireRecord(value, `Deployment manifest ${field}`);
  requireExactKeys(outRef, ["txHash", "outputIndex"], [], field);
  return {
    txHash: requireHex(outRef.txHash, 32, `${field}.txHash`),
    outputIndex: requireInteger(outRef.outputIndex, `${field}.outputIndex`),
  };
};

const requireIsoTimestamp = (value: unknown, field: string): string => {
  const text = requireString(value, field);
  const milliseconds = Date.parse(text);
  if (
    !Number.isFinite(milliseconds) ||
    new Date(milliseconds).toISOString() !== text
  ) {
    throw new Error(
      `Deployment manifest ${field} must be a canonical ISO timestamp`,
    );
  }
  return text;
};

type LucidDataSchema = Parameters<typeof Data.to>[1];

const FraudProofCatalogueProofNeighborSchema = Data.Object({
  nibble: Data.Integer(),
  prefix: Data.Bytes(),
  root: Data.Bytes(),
});

const FraudProofCatalogueProofStepSchema = Data.Enum([
  Data.Object({
    Branch: Data.Object({
      skip: Data.Integer(),
      neighbors: Data.Bytes(),
    }),
  }),
  Data.Object({
    Fork: Data.Object({
      skip: Data.Integer(),
      neighbor: FraudProofCatalogueProofNeighborSchema,
    }),
  }),
  Data.Object({
    Leaf: Data.Object({
      skip: Data.Integer(),
      key: Data.Bytes(),
      value: Data.Bytes(),
    }),
  }),
]);

const FraudProofCatalogueProofSchema = Data.Array(
  FraudProofCatalogueProofStepSchema,
);

type FraudProofCatalogueProofData = Data.Static<
  typeof FraudProofCatalogueProofSchema
>;

const MPF_NULL_HASH = Buffer.alloc(32);
const MPF_PATH_NIBBLE_COUNT = 64;

const mpfHash = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

const mpfCombine = (left: Uint8Array, right: Uint8Array): Buffer =>
  mpfHash(Buffer.concat([Buffer.from(left), Buffer.from(right)]));

const mpfNibbleAt = (path: Uint8Array, index: number): number => {
  if (
    !Number.isSafeInteger(index) ||
    index < 0 ||
    index >= MPF_PATH_NIBBLE_COUNT
  ) {
    throw new Error("Fraud-proof catalogue MPF path cursor is invalid");
  }
  const byte = path[Math.floor(index / 2)]!;
  return index % 2 === 0 ? Math.floor(byte / 16) : byte % 16;
};

const mpfPathNibbles = (
  path: Uint8Array,
  start: number,
  end: number,
): Buffer => {
  const result: number[] = [];
  for (let cursor = start; cursor < end; cursor += 1) {
    result.push(mpfNibbleAt(path, cursor));
  }
  return Buffer.from(result);
};

const mpfSuffix = (path: Uint8Array, cursor: number): Buffer => {
  if (
    !Number.isSafeInteger(cursor) ||
    cursor < 0 ||
    cursor > MPF_PATH_NIBBLE_COUNT
  ) {
    throw new Error("Fraud-proof catalogue MPF suffix cursor is invalid");
  }
  if (cursor % 2 === 0) {
    return Buffer.concat([
      Buffer.from([0xff]),
      Buffer.from(path).subarray(cursor / 2),
    ]);
  }
  return Buffer.concat([
    Buffer.from([0, mpfNibbleAt(path, cursor)]),
    Buffer.from(path).subarray((cursor + 1) / 2),
  ]);
};

const mpfSparseChildrenRoot = (
  children: ReadonlyMap<number, Uint8Array>,
): Buffer => {
  let level = Array.from<Uint8Array>({ length: 16 }).fill(MPF_NULL_HASH);
  for (const [nibble, root] of children) {
    if (!Number.isSafeInteger(nibble) || nibble < 0 || nibble > 15) {
      throw new Error("Fraud-proof catalogue MPF child nibble is invalid");
    }
    level[nibble] = root;
  }
  while (level.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(mpfCombine(level[index]!, level[index + 1]!));
    }
    level = next;
  }
  return Buffer.from(level[0]!);
};

type FraudProofCatalogueMpfEntry = {
  readonly path: Buffer;
  readonly valueHash: Buffer;
};

const reconstructFraudProofCatalogueMpfNode = (
  entries: readonly FraudProofCatalogueMpfEntry[],
  cursor: number,
): Buffer => {
  if (entries.length === 0) {
    throw new Error("Fraud-proof catalogue MPF node must not be empty");
  }
  if (entries.length === 1) {
    return mpfCombine(
      mpfSuffix(entries[0]!.path, cursor),
      entries[0]!.valueHash,
    );
  }

  let branchCursor = cursor;
  while (
    branchCursor < MPF_PATH_NIBBLE_COUNT &&
    entries.every(
      ({ path }) =>
        mpfNibbleAt(path, branchCursor) ===
        mpfNibbleAt(entries[0]!.path, branchCursor),
    )
  ) {
    branchCursor += 1;
  }
  if (branchCursor >= MPF_PATH_NIBBLE_COUNT) {
    throw new Error("Fraud-proof catalogue MPF contains duplicate key paths");
  }

  const grouped = new Map<number, FraudProofCatalogueMpfEntry[]>();
  for (const entry of entries) {
    const nibble = mpfNibbleAt(entry.path, branchCursor);
    const group = grouped.get(nibble) ?? [];
    group.push(entry);
    grouped.set(nibble, group);
  }
  const childRoots = new Map<number, Buffer>();
  for (const [nibble, group] of grouped) {
    childRoots.set(
      nibble,
      reconstructFraudProofCatalogueMpfNode(group, branchCursor + 1),
    );
  }
  return mpfCombine(
    mpfPathNibbles(entries[0]!.path, cursor, branchCursor),
    mpfSparseChildrenRoot(childRoots),
  );
};

const encodeFraudProofCatalogueKey = (categoryId: string): Buffer =>
  encodeCbor(Buffer.from(categoryId, "hex"));

const encodeFraudProofCatalogueValue = (scriptHash: string): Buffer =>
  encodeCbor(Buffer.from(scriptHash, "hex"));

const proofDataToMpfSteps = (
  proof: FraudProofCatalogueProofData,
): readonly MidgardMpfProofStep[] =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        kind: "branch",
        skip: Number(step.Branch.skip),
        neighbors: Buffer.from(step.Branch.neighbors, "hex"),
      };
    }
    if ("Fork" in step) {
      return {
        kind: "fork",
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: Buffer.from(step.Fork.neighbor.prefix, "hex"),
          root: Buffer.from(step.Fork.neighbor.root, "hex"),
        },
      };
    }
    return {
      kind: "leaf",
      skip: Number(step.Leaf.skip),
      key: Buffer.from(step.Leaf.key, "hex"),
      value: Buffer.from(step.Leaf.value, "hex"),
    };
  });

export const verifyDeploymentManifestFraudProofCatalogueIdentity = (
  catalogue: DeploymentManifestFraudProofCatalogueIdentity,
): DeploymentManifestFraudProofCatalogueIdentity => {
  requireExactKeys(
    catalogue as unknown as Record<string, unknown>,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireExactKeys(
    catalogue.categories as unknown as Record<string, unknown>,
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const declaredRoot = requireHex(
    catalogue.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const entries: FraudProofCatalogueMpfEntry[] = [];
  const encodedEntries = new Map<
    DeploymentManifestFraudProofCatalogueCategory,
    { readonly key: Buffer; readonly value: Buffer }
  >();
  const parsedCategories = {} as Record<
    DeploymentManifestFraudProofCatalogueCategory,
    DeploymentManifestFraudProofCatalogueCategoryIdentity
  >;

  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.forEach(
    (categoryName) => {
      const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
      const candidate = requireRecord(
        (catalogue.categories as unknown as Record<string, unknown>)[
          categoryName
        ],
        `Deployment manifest ${field}`,
      );
      requireExactKeys(
        candidate,
        ["categoryId", "scriptHash", "membershipProofCbor"],
        [],
        field,
      );
      const category = {
        categoryId: requireHex(candidate.categoryId, 4, `${field}.categoryId`),
        scriptHash: requireHex(candidate.scriptHash, 28, `${field}.scriptHash`),
        membershipProofCbor: requireHex(
          candidate.membershipProofCbor,
          undefined,
          `${field}.membershipProofCbor`,
        ),
      } satisfies DeploymentManifestFraudProofCatalogueCategoryIdentity;
      parsedCategories[categoryName] = category;
      const expectedCategoryId =
        DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName];
      if (category.categoryId !== expectedCategoryId) {
        throw new Error(
          `Deployment manifest ${field}.categoryId must be ${expectedCategoryId}`,
        );
      }
      const key = encodeFraudProofCatalogueKey(category.categoryId);
      const value = encodeFraudProofCatalogueValue(category.scriptHash);
      encodedEntries.set(categoryName, { key, value });
      entries.push({
        path: mpfHash(key),
        valueHash: mpfHash(value),
      });
    },
  );

  const reconstructedRoot = reconstructFraudProofCatalogueMpfNode(
    entries,
    0,
  ).toString("hex");
  if (reconstructedRoot !== declaredRoot) {
    throw new Error(
      `Deployment manifest fraud-proof catalogue root mismatch: declared=${declaredRoot}, reconstructed=${reconstructedRoot}`,
    );
  }

  for (const categoryName of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = parsedCategories[categoryName];
    const encoded = encodedEntries.get(categoryName)!;
    let proof: FraudProofCatalogueProofData;
    try {
      proof = Data.from(
        category.membershipProofCbor,
        FraudProofCatalogueProofSchema,
      ) as unknown as FraudProofCatalogueProofData;
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor is not exact Proof CBOR: ${String(cause)}`,
      );
    }
    const canonicalProofCbor = Data.to(
      proof,
      FraudProofCatalogueProofSchema as unknown as LucidDataSchema,
    );
    if (canonicalProofCbor !== category.membershipProofCbor) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor is not canonical`,
      );
    }
    let proofRoot: string;
    try {
      proofRoot = buildMidgardMpfProofFoldTrace({
        key: encoded.key,
        value: encoded.value,
        steps: proofDataToMpfSteps(proof),
      }).terminal.includingRoot.toString("hex");
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor is invalid: ${String(cause)}`,
      );
    }
    if (proofRoot !== declaredRoot) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor does not prove membership in catalogue root`,
      );
    }
  }
  return catalogue;
};

// Script-hash derivation is a pure function of (type, cborHex), but each
// derivation pays a full CBOR decode of the compiled script. A manifest
// carries every deployment contract, and callers re-verify the manifest on
// every authority check, so uncached derivation is quadratic in practice.
// A cache hit returns exactly what re-derivation would, including for
// tampered manifests: a changed script changes the key.
const SCRIPT_HASH_DERIVATION_CACHE_LIMIT = 4096;
const scriptHashDerivationCache = new Map<string, string>();
const deriveScriptHashCached = (
  type: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3",
  cborHex: string,
): string => {
  const key = `${type}:${cborHex}`;
  const cached = scriptHashDerivationCache.get(key);
  if (cached !== undefined) {
    return cached;
  }
  const derived = validatorToScriptHash({ type, script: cborHex });
  if (scriptHashDerivationCache.size >= SCRIPT_HASH_DERIVATION_CACHE_LIMIT) {
    const oldest = scriptHashDerivationCache.keys().next().value;
    if (oldest !== undefined) {
      scriptHashDerivationCache.delete(oldest);
    }
  }
  scriptHashDerivationCache.set(key, derived);
  return derived;
};

const validateFinalizedContracts = (
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
    [],
    "contracts",
  );
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const scriptHashByName = new Map<string, string>();
  for (const contractName of DEPLOYMENT_MANIFEST_CONTRACT_NAMES) {
    const field = `contracts.${contractName}`;
    const entry = requireRecord(contracts[contractName], field);
    requireExactKeys(
      entry,
      ["refScriptUTxO", "contract", "scriptHash"],
      contractName === "fraudProofCatalogueMint" ? ["fraudProofCatalogue"] : [],
      field,
    );
    if (referenceScriptContractNames.has(contractName)) {
      requireFinalOutRef(entry.refScriptUTxO, `${field}.refScriptUTxO`);
    } else if (entry.refScriptUTxO !== null) {
      throw new Error(
        `Deployment manifest ${field}.refScriptUTxO must be null because the contract has no reference-script role`,
      );
    }
    const contract = requireRecord(entry.contract, `${field}.contract`);
    requireExactKeys(contract, ["type", "cborHex"], [], `${field}.contract`);
    if (
      contract.type !== "Native" &&
      contract.type !== "PlutusV1" &&
      contract.type !== "PlutusV2" &&
      contract.type !== "PlutusV3"
    ) {
      throw new Error(
        `Deployment manifest ${field}.contract.type is unsupported`,
      );
    }
    const cborHex = requireHex(
      contract.cborHex,
      undefined,
      `${field}.contract.cborHex`,
    );
    const scriptHash = requireHex(entry.scriptHash, 28, `${field}.scriptHash`);
    let derivedScriptHash: string;
    try {
      derivedScriptHash = deriveScriptHashCached(contract.type, cborHex);
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
    scriptHashByName.set(contractName, scriptHash);
  }

  const catalogueMint = requireRecord(
    contracts.fraudProofCatalogueMint,
    "contracts.fraudProofCatalogueMint",
  );
  const catalogue = requireRecord(
    catalogueMint.fraudProofCatalogue,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireExactKeys(
    catalogue,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireHex(
    catalogue.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const categories = requireRecord(
    catalogue.categories,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  requireExactKeys(
    categories,
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const parsedCategories = {} as Record<
    DeploymentManifestFraudProofCatalogueCategory,
    DeploymentManifestFraudProofCatalogueCategoryIdentity
  >;
  for (const categoryName of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const contractName =
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[categoryName];
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = requireRecord(categories[categoryName], field);
    requireExactKeys(
      category,
      ["categoryId", "scriptHash", "membershipProofCbor"],
      [],
      field,
    );
    const categoryId = requireHex(
      category.categoryId,
      4,
      `${field}.categoryId`,
    );
    const scriptHash = requireHex(
      category.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    const membershipProofCbor = requireHex(
      category.membershipProofCbor,
      undefined,
      `${field}.membershipProofCbor`,
    );
    if (scriptHash !== scriptHashByName.get(contractName)) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    parsedCategories[categoryName] = {
      categoryId,
      scriptHash,
      membershipProofCbor,
    };
  }
  verifyDeploymentManifestFraudProofCatalogueIdentity({
    root: requireHex(
      catalogue.root,
      32,
      "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
    ),
    categories: parsedCategories,
  });
};

const validateFinalizedReferenceScripts = (
  referenceScripts: Record<string, unknown>,
  referenceScriptAuthPolicy: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  const roles = Object.keys(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  );
  requireExactKeys(referenceScripts, roles, [], "referenceScripts");
  const policyId = requireHex(
    referenceScriptAuthPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  for (const role of roles) {
    const field = `referenceScripts.${role}`;
    const reference = requireRecord(referenceScripts[role], field);
    requireExactKeys(
      reference,
      ["status", "roleUnit", "scriptHash", "outRef"],
      [],
      field,
    );
    if (reference.status !== "confirmed") {
      throw new Error(`Deployment manifest ${field}.status must be confirmed`);
    }
    const tokenName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    const expectedRoleUnit =
      policyId + bytesToHex(new TextEncoder().encode(tokenName));
    if (reference.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
      ];
    const contract = requireRecord(
      contracts[contractName],
      `contracts.${contractName}`,
    );
    const scriptHash = requireHex(
      reference.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    if (scriptHash !== contract.scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractName}.scriptHash`,
      );
    }
    const contractOutRef = requireFinalOutRef(
      contract.refScriptUTxO,
      `contracts.${contractName}.refScriptUTxO`,
    );
    const expectedOutRef = `${contractOutRef.txHash}#${contractOutRef.outputIndex.toString()}`;
    if (reference.outRef !== expectedOutRef) {
      throw new Error(
        `Deployment manifest ${field}.outRef must equal ${expectedOutRef}`,
      );
    }
  }
};

const validateFinalizedDa = (value: unknown): void => {
  const da = requireRecord(value, "Deployment manifest da");
  requireExactKeys(
    da,
    ["committeeVkeys", "committeeSignersHash", "threshold", "transportProfile"],
    [],
    "da",
  );
  if (!Array.isArray(da.committeeVkeys) || da.committeeVkeys.length === 0) {
    throw new Error(
      "Deployment manifest da.committeeVkeys must be a non-empty array",
    );
  }
  const committeeVkeys = da.committeeVkeys.map((entry, index) =>
    requireHex(entry, 32, `da.committeeVkeys[${index.toString()}]`),
  );
  if (new Set(committeeVkeys).size !== committeeVkeys.length) {
    throw new Error("Deployment manifest da.committeeVkeys must be unique");
  }
  const committeeSignersHash = requireHex(
    da.committeeSignersHash,
    32,
    "da.committeeSignersHash",
  );
  const expectedCommitteeSignersHash = bytesToHex(
    blake2b(hexToBytes(committeeVkeys.join("")), { dkLen: 32 }),
  );
  if (committeeSignersHash !== expectedCommitteeSignersHash) {
    throw new Error(
      `Deployment manifest da.committeeSignersHash mismatch: expected ${expectedCommitteeSignersHash}`,
    );
  }
  const threshold = requireInteger(da.threshold, "da.threshold", 1);
  if (threshold > committeeVkeys.length) {
    throw new Error("Deployment manifest da.threshold exceeds committee size");
  }
  const transport = requireRecord(
    da.transportProfile,
    "Deployment manifest da.transportProfile",
  );
  requireExactKeys(
    transport,
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
  if (transport.protocolVersion !== DA_TRANSPORT_PROTOCOL_VERSION) {
    throw new Error(
      "Deployment manifest da.transportProfile.protocolVersion is unsupported",
    );
  }
  if (
    transport.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_SCHEMA_VERSION
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.runtimeManifestSchemaVersion is unsupported",
    );
  }
  if (
    transport.envelopeEncoding !== "identity" &&
    transport.envelopeEncoding !== "zstd"
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.envelopeEncoding is unsupported",
    );
  }
  requireInteger(transport.zstdLevel, "da.transportProfile.zstdLevel", 1);
  if (
    stableJson(
      normalizeDeploymentManifestJsonValueInternal(
        transport.limits,
        "Deployment manifest da.transportProfile.limits",
        false,
      ),
    ) !== stableJson(DA_TRANSPORT_LIMITS)
  ) {
    throw new Error(
      "Deployment manifest da.transportProfile.limits must exactly match canonical V1",
    );
  }
  const retentionDays = requireInteger(
    transport.retentionDays,
    "da.transportProfile.retentionDays",
    1,
  );
  // Existing >= 15-day transport-profile floor: never weakened.
  if (retentionDays < DA_TRANSPORT_LIMITS.minimumRetentionDays) {
    throw new Error(
      "Deployment manifest da.transportProfile.retentionDays is too short",
    );
  }
  // Q54: additionally bind the window to the derived challengeability horizon
  // (block maturity + worst-case proof-time bound), so deployment identity -
  // not a literal - is what the DA and proof stores enforce against.
  if (
    !retentionDaysCoverWindow(
      retentionDays,
      "Deployment manifest da.transportProfile.retentionDays",
    )
  ) {
    throw new Error(
      `Deployment manifest da.transportProfile.retentionDays must cover the canonical V1 retention window (requiredRetentionMs=${String(
        MIDGARD_RETENTION_WINDOW.requiredRetentionMs,
      )})`,
    );
  }
};

// manifestIds whose deep finalized verification already succeeded in this
// process. Reusing one is sound only because verifyDeploymentManifestIdentity
// runs uncached on every call: it re-hashes the manifest's full normalized
// content and requires manifestId to equal that hash, so a mutated manifest
// either fails identity verification outright or arrives under a new
// manifestId and misses this cache. Everything the deep pass checks is a pure
// function of that same content plus module constants.
const VERIFIED_FINALIZED_MANIFEST_ID_CACHE_LIMIT = 64;
const verifiedFinalizedManifestIds = new Set<string>();

export const verifyFinalizedDeploymentManifest = (
  value: unknown,
): Record<string, unknown> => {
  const candidate = verifyDeploymentManifestIdentity(value);
  const verifiedManifestId = candidate.manifestId as string;
  if (verifiedFinalizedManifestIds.has(verifiedManifestId)) {
    return candidate;
  }
  if (
    candidate.network !== "Mainnet" &&
    candidate.network !== "Preprod" &&
    candidate.network !== "Preview" &&
    candidate.network !== "Custom"
  ) {
    throw new Error("Deployment manifest network is unsupported");
  }
  const createdAt = requireIsoTimestamp(candidate.createdAt, "createdAt");
  const updatedAt = requireIsoTimestamp(candidate.updatedAt, "updatedAt");
  if (updatedAt < createdAt) {
    throw new Error("Deployment manifest updatedAt must not precede createdAt");
  }
  requireString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );

  const cardano = requireRecord(
    candidate.cardanoProtocolParameters,
    "Deployment manifest cardanoProtocolParameters",
  );
  requireExactKeys(
    cardano,
    ["snapshot", "digest"],
    [],
    "cardanoProtocolParameters",
  );
  const cardanoDigest = requireHex(
    cardano.digest,
    32,
    "cardanoProtocolParameters.digest",
  );
  parseDeploymentManifestCardanoProtocolParameters(cardano.snapshot);
  const expectedCardanoDigest = computeDeploymentManifestJsonDigest(
    cardano.snapshot,
  );
  if (cardanoDigest !== expectedCardanoDigest) {
    throw new Error(
      `Deployment manifest cardanoProtocolParameters.digest mismatch: expected ${expectedCardanoDigest}`,
    );
  }

  const genesis = requireRecord(
    candidate.genesis,
    "Deployment manifest genesis",
  );
  requireExactKeys(genesis, ["headerHash", "utxoSetDigest"], [], "genesis");
  requireHex(genesis.headerHash, 28, "genesis.headerHash");
  requireHex(genesis.utxoSetDigest, 32, "genesis.utxoSetDigest");

  const oneShot = requireRecord(
    candidate.hubOracleOneShot,
    "Deployment manifest hubOracleOneShot",
  );
  requireExactKeys(
    oneShot,
    ["txHash", "outputIndex", "outRef", "status"],
    [],
    "hubOracleOneShot",
  );
  const oneShotTxHash = requireHex(
    oneShot.txHash,
    32,
    "hubOracleOneShot.txHash",
  );
  const oneShotOutputIndex = requireInteger(
    oneShot.outputIndex,
    "hubOracleOneShot.outputIndex",
  );
  const expectedOneShotOutRef = `${oneShotTxHash}#${oneShotOutputIndex.toString()}`;
  if (oneShot.outRef !== expectedOneShotOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef must equal ${expectedOneShotOutRef}`,
    );
  }
  if (oneShot.status !== "consumed_by_init") {
    throw new Error(
      "Deployment manifest hubOracleOneShot.status must be consumed_by_init",
    );
  }

  const authPolicy = requireRecord(
    candidate.referenceScriptAuthPolicy,
    "Deployment manifest referenceScriptAuthPolicy",
  );
  requireExactKeys(
    authPolicy,
    ["policyId", "nativeScript", "tokenNames", "postTimelockAudit"],
    [],
    "referenceScriptAuthPolicy",
  );
  const policyId = requireHex(
    authPolicy.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  const nativeScript = requireRecord(
    authPolicy.nativeScript,
    "Deployment manifest referenceScriptAuthPolicy.nativeScript",
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
  const nativeScriptCbor = requireHex(
    nativeScript.cborHex,
    undefined,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  requireInteger(
    nativeScript.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  requireInteger(
    nativeScript.expiresAtUnixTime,
    "referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime",
  );
  requireInteger(
    nativeScript.timelockDurationMs,
    "referenceScriptAuthPolicy.nativeScript.timelockDurationMs",
    1,
  );
  const derivedPolicyId = deriveScriptHashCached("Native", nativeScriptCbor);
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }
  const tokenNames = requireRecord(
    authPolicy.tokenNames,
    "Deployment manifest referenceScriptAuthPolicy.tokenNames",
  );
  const roles = Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES);
  requireExactKeys(
    tokenNames,
    roles,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const role of roles) {
    const expected =
      DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    if (tokenNames[role] !== expected) {
      throw new Error(
        `Deployment manifest referenceScriptAuthPolicy.tokenNames.${role} must equal ${expected}`,
      );
    }
  }
  const audit = requireRecord(
    authPolicy.postTimelockAudit,
    "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit",
  );
  requireExactKeys(
    audit,
    ["required", "rule"],
    [],
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  if (audit.required !== true) {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit.required must be true",
    );
  }
  requireString(audit.rule, "referenceScriptAuthPolicy.postTimelockAudit.rule");

  const contracts = requireRecord(
    candidate.contracts,
    "Deployment manifest contracts",
  );
  validateFinalizedContracts(contracts);
  const authContract = requireRecord(
    contracts.referenceScriptAuthMint,
    "contracts.referenceScriptAuthMint",
  );
  if (authContract.scriptHash !== policyId) {
    throw new Error(
      "Deployment manifest contracts.referenceScriptAuthMint.scriptHash must match referenceScriptAuthPolicy.policyId",
    );
  }
  validateFinalizedReferenceScripts(
    requireRecord(
      candidate.referenceScripts,
      "Deployment manifest referenceScripts",
    ),
    authPolicy,
    contracts,
  );
  validateFinalizedDa(candidate.da);

  const proofEvidence = requireRecord(
    candidate.proofEvidence,
    "Deployment manifest proofEvidence",
  );
  requireExactKeys(
    proofEvidence,
    ["digest", "blueprintHash"],
    [],
    "proofEvidence",
  );
  if (proofEvidence.digest !== MIDGARD_RELEASE_EVIDENCE_DIGEST) {
    throw new Error(
      "Deployment manifest proofEvidence.digest must match compiled canonical V1 evidence",
    );
  }
  requireHex(proofEvidence.blueprintHash, 32, "proofEvidence.blueprintHash");

  const steps = requireRecord(candidate.steps, "Deployment manifest steps");
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_STEP_NAMES, [], "steps");
  const supportedStepStatuses = new Set([
    "pending",
    "in_progress",
    "submitted",
    "complete",
    "attached",
    "failed",
    "blocked_requires_fresh_redeploy",
  ]);
  for (const stepName of DEPLOYMENT_MANIFEST_STEP_NAMES) {
    const field = `steps.${stepName}`;
    const step = requireRecord(steps[stepName], field);
    requireExactKeys(step, ["status"], ["txHash"], field);
    if (!supportedStepStatuses.has(String(step.status))) {
      throw new Error(`Deployment manifest ${field}.status is unsupported`);
    }
    if (step.txHash !== undefined) {
      requireHex(step.txHash, 32, `${field}.txHash`);
    }
  }
  for (const requiredStep of [
    "prepareHubOracleNonce",
    "deployNodeRuntimeReferenceScripts",
    "initProtocol",
  ]) {
    const step = requireRecord(steps[requiredStep], `steps.${requiredStep}`);
    if (step.status !== "complete") {
      throw new Error(
        `Deployment manifest steps.${requiredStep}.status must be complete`,
      );
    }
  }

  const dispute = requireRecord(
    candidate.validationDispute,
    "Deployment manifest validationDispute",
  );
  requireExactKeys(
    dispute,
    ["version", "responseWindowMs", "maxBisectionRounds", "maturityMs"],
    [],
    "validationDispute",
  );
  const expectedDispute = {
    version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
    responseWindowMs:
      MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
    maxBisectionRounds:
      MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
    maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
  } as const;
  for (const [key, expected] of Object.entries(expectedDispute)) {
    if (dispute[key] !== expected) {
      throw new Error(
        `Deployment manifest validationDispute.${key} must equal ${expected.toString()}`,
      );
    }
  }

  const l1Finality = requireRecord(
    candidate.l1Finality,
    "Deployment manifest l1Finality",
  );
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
  if (
    verifiedFinalizedManifestIds.size >=
    VERIFIED_FINALIZED_MANIFEST_ID_CACHE_LIMIT
  ) {
    const oldest = verifiedFinalizedManifestIds.values().next().value;
    if (oldest !== undefined) {
      verifiedFinalizedManifestIds.delete(oldest);
    }
  }
  verifiedFinalizedManifestIds.add(verifiedManifestId);
  return candidate;
};
