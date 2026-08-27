import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";

import { encodeCbor } from "./codec/cbor.js";
import {
  isMidgardConsensusProfileV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
} from "./consensus-profile-v1.js";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "./da-transport.js";
import {
  buildMidgardMpfProofFoldTraceV1,
  type MidgardMpfProofStepV1,
} from "./mpf-proof-fold-v1.js";
import {
  MIDGARD_RETENTION_WINDOW_V1,
  retentionDaysCoverWindowV1,
} from "./retention-window-v1.js";

export const DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES = Object.freeze([
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
  "fraudProofNativeScriptDecodingStep03",
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
  "fraudProofTransitionTraceControl",
  "fraudProofTransitionTraceSource",
  "fraudProofTransitionTraceWithdrawal",
  "fraudProofTransitionTraceForced",
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
  "fraudProofTransitionTraceL1Event",
  "fraudProofTransitionTraceDuplicate",
] as const);

export const DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER =
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
  ] as const);

export type DeploymentManifestV1FraudProofCatalogueCategory =
  (typeof DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number];

export type DeploymentManifestV1FraudProofCatalogueCategoryIdentity = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export type DeploymentManifestV1FraudProofCatalogueIdentity = {
  readonly root: string;
  readonly categories: Readonly<
    Record<
      DeploymentManifestV1FraudProofCatalogueCategory,
      DeploymentManifestV1FraudProofCatalogueCategoryIdentity
    >
  >;
};

export const DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze({
    "reference-script-auth minting": "referenceScriptAuthMint",
    "hub-oracle minting": "hubOracleMint",
    "da-params-governor spending": "daParamsGovernorSpend",
    "da-params-governor minting": "daParamsGovernorMint",
    "da-attestation spending": "daAttestationSpend",
    "da-attestation minting": "daAttestationMint",
    "state-queue spending": "stateQueueSpend",
    "state-queue minting": "stateQueueMint",
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
    "V1 fraud-proof native-script-decoding step-03":
      "fraudProofNativeScriptDecodingStep03",
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
  } as const);

export const DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES =
  Object.freeze({
    "reference-script-auth minting": "ReferenceScriptAuthMint",
    "hub-oracle minting": "HubOracleMint",
    "da-params-governor spending": "DaParamsGovernorSpend",
    "da-params-governor minting": "DaParamsGovernorMint",
    "da-attestation spending": "DaAttestationSpend",
    "da-attestation minting": "DaAttestationMint",
    "state-queue spending": "StateQueueSpend",
    "state-queue minting": "StateQueueMint",
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
    "V1 immutable CEK program-material publication":
      "V1CekProgramMaterialSpend",
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
    "V1 fraud-proof fabricated-withdrawal step-01":
      "V1FpFabricatedWithdrawalS01",
    "V1 fraud-proof fabricated-withdrawal step-02":
      "V1FpFabricatedWithdrawalS02",
    "V1 fraud-proof fabricated-withdrawal step-03":
      "V1FpFabricatedWithdrawalS03",
    "V1 fraud-proof fabricated-withdrawal step-04":
      "V1FpFabricatedWithdrawalS04",
    "V1 fraud-proof native-script-decoding step-01":
      "V1FpNativeScriptDecodingS01",
    "V1 fraud-proof native-script-decoding step-02":
      "V1FpNativeScriptDecodingS02",
    "V1 fraud-proof native-script-decoding step-03":
      "V1FpNativeScriptDecodingS03",
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
    "V1 fraud-proof committed-field-shape step-01":
      "V1FpCommittedFieldShapeS01",
    "V1 fraud-proof committed-field-shape step-02":
      "V1FpCommittedFieldShapeS02",
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
    "V1 fraud-proof transition-trace route": "V1FpTransitionTraceRoute",
    "V1 fraud-proof transition-trace final-0": "V1FpTransitionTraceFinal0",
    "V1 fraud-proof transition-trace final-1": "V1FpTransitionTraceFinal1",
    "V1 fraud-proof transition-trace final-2": "V1FpTransitionTraceFinal2",
    "V1 fraud-proof transition-trace final-3": "V1FpTransitionTraceFinal3",
    "V1 fraud-proof transition-trace final-4": "V1FpTransitionTraceFinal4",
    "V1 fraud-proof transition-trace final-5": "V1FpTransitionTraceFinal5",
    "V1 fraud-proof transition-trace final-6": "V1FpTransitionTraceFinal6",
    "V1 fraud-proof transition-trace final-7": "V1FpTransitionTraceFinal7",
  } as const);

export const DEPLOYMENT_MANIFEST_V1_STEP_NAMES = Object.freeze([
  "prepareHubOracleNonce",
  "deployNodeRuntimeReferenceScripts",
  "initProtocol",
  "phasRegistration",
  "operatorRegistration",
  "operatorActivation",
] as const);

export const DEPLOYMENT_MANIFEST_V1_ROOT_KEYS = Object.freeze([
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
] as const);

export type DeploymentManifestV1JsonValue =
  | null
  | boolean
  | number
  | string
  | readonly DeploymentManifestV1JsonValue[]
  | { readonly [key: string]: DeploymentManifestV1JsonValue };

export const MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION =
  "midgard-deployment-marker-v1" as const;

export type DeploymentMarkerV1 = {
  readonly schemaVersion: typeof MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION;
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

const requireDeploymentManifestIdV1 = (
  value: unknown,
  field: string,
): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${field} must be lowercase SHA-256 hex`);
  }
  return value;
};

export const parseDeploymentMarkerV1 = (value: unknown): DeploymentMarkerV1 => {
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
  if (candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION) {
    throw new Error(
      `Deployment marker V1 schemaVersion must be ${MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION}`,
    );
  }
  return {
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
    manifestId: requireDeploymentManifestIdV1(
      candidate.manifestId,
      "Deployment marker V1 manifestId",
    ),
  };
};

export const makeDeploymentMarkerV1 = (
  manifestId: string,
): DeploymentMarkerV1 =>
  parseDeploymentMarkerV1({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
    manifestId,
  });

export const assertDeploymentMarkerV1Matches = (
  expected: DeploymentMarkerV1,
  actual: unknown,
  boundary = "deployment boundary",
): DeploymentMarkerV1 => {
  const canonicalExpected = parseDeploymentMarkerV1(expected);
  const canonicalActual = parseDeploymentMarkerV1(actual);
  if (canonicalActual.manifestId !== canonicalExpected.manifestId) {
    throw new Error(
      `${boundary} deployment marker mismatch: expected ${canonicalExpected.manifestId}, found ${canonicalActual.manifestId}`,
    );
  }
  return canonicalActual;
};

const normalizeDeploymentManifestV1JsonValueInternal = (
  value: unknown,
  field: string,
  stringifyBigInt: boolean,
): DeploymentManifestV1JsonValue => {
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
      normalizeDeploymentManifestV1JsonValueInternal(
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
        normalizeDeploymentManifestV1JsonValueInternal(
          entry,
          `${field}.${key}`,
          stringifyBigInt,
        ),
      ];
    }),
  );
};

export const normalizeDeploymentManifestV1JsonValue = (
  value: unknown,
  field = "value",
): DeploymentManifestV1JsonValue =>
  normalizeDeploymentManifestV1JsonValueInternal(
    value,
    `Deployment manifest ${field}`,
    true,
  );

const stableJson = (value: DeploymentManifestV1JsonValue): string => {
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

export const computeDeploymentManifestV1JsonDigest = (
  value: unknown,
): string => {
  const normalized = normalizeDeploymentManifestV1JsonValueInternal(
    value,
    "Deployment manifest JSON digest input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

const exactRoot = (candidate: Record<string, unknown>): void => {
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_V1_ROOT_KEYS);
  for (const key of Object.keys(candidate)) {
    if (!expected.has(key)) {
      throw new Error(`Deployment manifest value.${key} is unexpected`);
    }
  }
  for (const key of DEPLOYMENT_MANIFEST_V1_ROOT_KEYS) {
    if (!Object.prototype.hasOwnProperty.call(candidate, key)) {
      throw new Error(`Deployment manifest value.${key} is required`);
    }
  }
};

export const computeDeploymentManifestV1Id = (
  identityInput: Record<string, unknown>,
): string => {
  if (Object.prototype.hasOwnProperty.call(identityInput, "manifestId")) {
    throw new Error("Deployment manifest identity input must omit manifestId");
  }
  const normalized = normalizeDeploymentManifestV1JsonValueInternal(
    identityInput,
    "Deployment manifest identity input",
    false,
  );
  return bytesToHex(sha256(new TextEncoder().encode(stableJson(normalized))));
};

export const verifyDeploymentManifestV1Identity = (
  value: unknown,
): Record<string, unknown> => {
  const candidate = requireRecord(value, "Deployment manifest value");
  if (
    candidate.schemaVersion !== MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION
  ) {
    throw new Error(
      `Deployment manifest schemaVersion must be ${MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION}`,
    );
  }
  exactRoot(candidate);
  if (!isMidgardConsensusProfileV1(candidate.consensusProfile)) {
    throw new Error(
      "Deployment manifest consensusProfile must exactly match canonical V1",
    );
  }
  if (
    candidate.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_V1_DIGEST
  ) {
    throw new Error(
      "Deployment manifest consensusProfileDigest must exactly match canonical V1",
    );
  }
  if (
    typeof candidate.manifestId !== "string" ||
    !/^[0-9a-f]{64}$/u.test(candidate.manifestId)
  ) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const { manifestId, ...identityInput } = candidate;
  const expectedManifestId = computeDeploymentManifestV1Id(identityInput);
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

const expectedFraudProofCatalogueCategoryId = (index: number): string => {
  const bytes = Buffer.alloc(4);
  bytes.writeUInt32BE(index);
  return bytes.toString("hex");
};

const proofDataToMpfSteps = (
  proof: FraudProofCatalogueProofData,
): readonly MidgardMpfProofStepV1[] =>
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

export const verifyDeploymentManifestV1FraudProofCatalogueIdentity = (
  catalogue: DeploymentManifestV1FraudProofCatalogueIdentity,
): DeploymentManifestV1FraudProofCatalogueIdentity => {
  requireExactKeys(
    catalogue as unknown as Record<string, unknown>,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireExactKeys(
    catalogue.categories as unknown as Record<string, unknown>,
    DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
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
    DeploymentManifestV1FraudProofCatalogueCategory,
    { readonly key: Buffer; readonly value: Buffer }
  >();
  const parsedCategories = {} as Record<
    DeploymentManifestV1FraudProofCatalogueCategory,
    DeploymentManifestV1FraudProofCatalogueCategoryIdentity
  >;

  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.forEach(
    (categoryName, index) => {
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
      } satisfies DeploymentManifestV1FraudProofCatalogueCategoryIdentity;
      parsedCategories[categoryName] = category;
      const expectedCategoryId = expectedFraudProofCatalogueCategoryId(index);
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

  for (const categoryName of DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
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
      proofRoot = buildMidgardMpfProofFoldTraceV1({
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

const validateFinalizedContracts = (
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    contracts,
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
    [],
    "contracts",
  );
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const scriptHashByName = new Map<string, string>();
  for (const contractName of DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES) {
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
      derivedScriptHash = validatorToScriptHash({
        type: contract.type,
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
  const contractByCategory = {
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
  } as const satisfies Record<
    DeploymentManifestV1FraudProofCatalogueCategory,
    string
  >;
  requireExactKeys(
    categories,
    DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const parsedCategories = {} as Record<
    DeploymentManifestV1FraudProofCatalogueCategory,
    DeploymentManifestV1FraudProofCatalogueCategoryIdentity
  >;
  for (const categoryName of DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const contractName = contractByCategory[categoryName];
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
  verifyDeploymentManifestV1FraudProofCatalogueIdentity({
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
    DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
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
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
      ];
    const expectedRoleUnit =
      policyId + bytesToHex(new TextEncoder().encode(tokenName));
    if (reference.roleUnit !== expectedRoleUnit) {
      throw new Error(
        `Deployment manifest ${field}.roleUnit mismatch: expected ${expectedRoleUnit}`,
      );
    }
    const contractName =
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
        role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
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
  if (transport.protocolVersion !== DA_TRANSPORT_V1_PROTOCOL_VERSION) {
    throw new Error(
      "Deployment manifest da.transportProfile.protocolVersion is unsupported",
    );
  }
  if (
    transport.runtimeManifestSchemaVersion !==
    DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION
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
      normalizeDeploymentManifestV1JsonValueInternal(
        transport.limits,
        "Deployment manifest da.transportProfile.limits",
        false,
      ),
    ) !== stableJson(DA_TRANSPORT_LIMITS_V1)
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
  if (retentionDays < DA_TRANSPORT_LIMITS_V1.minimumRetentionDays) {
    throw new Error(
      "Deployment manifest da.transportProfile.retentionDays is too short",
    );
  }
  // Q54: additionally bind the window to the derived challengeability horizon
  // (block maturity + worst-case proof-time bound), so deployment identity -
  // not a literal - is what the DA and proof stores enforce against.
  if (
    !retentionDaysCoverWindowV1(
      retentionDays,
      "Deployment manifest da.transportProfile.retentionDays",
    )
  ) {
    throw new Error(
      `Deployment manifest da.transportProfile.retentionDays must cover the canonical V1 retention window (requiredRetentionMs=${String(
        MIDGARD_RETENTION_WINDOW_V1.requiredRetentionMs,
      )})`,
    );
  }
};

export const verifyFinalizedDeploymentManifestV1 = (
  value: unknown,
): Record<string, unknown> => {
  const candidate = verifyDeploymentManifestV1Identity(value);
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
  const expectedCardanoDigest = computeDeploymentManifestV1JsonDigest(
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
  const derivedPolicyId = validatorToScriptHash({
    type: "Native",
    script: nativeScriptCbor,
  });
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }
  const tokenNames = requireRecord(
    authPolicy.tokenNames,
    "Deployment manifest referenceScriptAuthPolicy.tokenNames",
  );
  const roles = Object.keys(
    DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  );
  requireExactKeys(
    tokenNames,
    roles,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const role of roles) {
    const expected =
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
        role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
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
  if (proofEvidence.digest !== MIDGARD_V1_RELEASE_EVIDENCE_DIGEST) {
    throw new Error(
      "Deployment manifest proofEvidence.digest must match compiled canonical V1 evidence",
    );
  }
  requireHex(proofEvidence.blueprintHash, 32, "proofEvidence.blueprintHash");

  const steps = requireRecord(candidate.steps, "Deployment manifest steps");
  requireExactKeys(steps, DEPLOYMENT_MANIFEST_V1_STEP_NAMES, [], "steps");
  const supportedStepStatuses = new Set([
    "pending",
    "in_progress",
    "submitted",
    "complete",
    "attached",
    "failed",
    "blocked_requires_fresh_redeploy",
  ]);
  for (const stepName of DEPLOYMENT_MANIFEST_V1_STEP_NAMES) {
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
    version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
    responseWindowMs:
      MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs,
    maxBisectionRounds:
      MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
    maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
  } as const;
  for (const [key, expected] of Object.entries(expectedDispute)) {
    if (dispute[key] !== expected) {
      throw new Error(
        `Deployment manifest validationDispute.${key} must equal ${expected.toString()}`,
      );
    }
  }
  return candidate;
};
