import {
  CANONICAL_DECODABILITY_VIOLATION_ID,
  COMMITTED_FIELD_SHAPE_VIOLATION_ID,
  DA_HASH_PREIMAGE_VIOLATION_ID,
  DOUBLE_WITHDRAW_VIOLATION_ID,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  type FraudProofCatalogueCategoryName,
  INPUT_NO_IDX_VIOLATION_ID,
  INVALID_SIGNATURE_VIOLATION_ID,
  MIN_ADA_VIOLATION_ID,
  MIN_FEE_VIOLATION_ID,
  MINT_AUTHORIZATION_VIOLATION_ID,
  MISSING_SIGNATURE_VIOLATION_ID,
  NATIVE_SCRIPT_DECODING_VIOLATION_ID,
  NATIVE_SCRIPT_INVALID_VIOLATION_ID,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID,
  WITHDRAWAL_MISTAG_VIOLATION_ID,
  WITHDRAWN_INPUT_VIOLATION_ID,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
} from "@al-ft/midgard-sdk";

import { INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID } from "../invalid-signature/wrongful-rejection.js";

export const FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION =
  "midgard-fraud-proof-classification-v1" as const;

export const DOUBLE_SPEND_VIOLATION_ID = "double-spend" as const;

export const NETWORK_ID_VIOLATION_ID = "network-id" as const;

export type FraudProofClassificationRule = {
  readonly category: FraudProofCatalogueCategoryName;
  /** Stable, ordered violation identifiers routed to this family. */
  readonly violationIds: readonly [string, ...string[]];
};

/**
 * Q55/W-O6's versioned violation-to-family authority.
 *
 * The outer order is deliberately the append-only catalogue order. The inner
 * order is the stable specificity order within one family. A detector never
 * supplies a family name: it supplies only a violation identifier, and this
 * table selects the catalogue family. This is a taxonomy, not an executable
 * availability claim: production availability is determined independently by
 * the exact sealed replay/adapter registry. Unknown identifiers classify as
 * `unprovable_gap`.
 */
export const FRAUD_PROOF_CLASSIFICATION_RULES = Object.freeze([
  { category: "doubleSpend", violationIds: [DOUBLE_SPEND_VIOLATION_ID] },
  {
    category: "nonExistentInput",
    violationIds: [
      "no-input",
      "non-existent-input",
      "non-existent-input-wrongful-rejection",
    ],
  },
  {
    category: "nonExistentInputNoIndex",
    violationIds: [INPUT_NO_IDX_VIOLATION_ID],
  },
  { category: "invalidRange", violationIds: ["invalid-range"] },
  {
    category: "transitionTrace",
    violationIds: [
      "transition-trace",
      "trace-boundary",
      "trace-link",
      "event-to-step-mismatch",
      "source-membership-mismatch",
      "invalid-one-step-transition",
      "omitted-due-l1-event",
      "duplicate-trace-event",
      "out-of-window-source-event",
      "count-fault",
      "accepted-transaction-transition-mismatch",
    ],
  },
  { category: "zeroInput", violationIds: ["zero-input"] },
  {
    category: "validationTraceDispute",
    violationIds: ["validation-trace"],
  },
  {
    category: "daHashPreimage",
    violationIds: [DA_HASH_PREIMAGE_VIOLATION_ID],
  },
  {
    category: "noReferenceInput",
    violationIds: [
      "no-reference-input",
      "no-reference-input-wrongful-rejection",
    ],
  },
  {
    category: "referenceInputNoIdx",
    violationIds: [REFERENCE_INPUT_NO_IDX_VIOLATION_ID],
  },
  {
    category: "invalidSignature",
    violationIds: [
      INVALID_SIGNATURE_VIOLATION_ID,
      INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID,
    ],
  },
  {
    category: "fabricatedDeposit",
    violationIds: [FABRICATED_DEPOSIT_VIOLATION_ID],
  },
  {
    category: "fabricatedWithdrawal",
    violationIds: [FABRICATED_WITHDRAWAL_VIOLATION_ID],
  },
  {
    category: "nativeScriptDecoding",
    violationIds: [NATIVE_SCRIPT_DECODING_VIOLATION_ID],
  },
  {
    category: "missingSignature",
    violationIds: [
      MISSING_SIGNATURE_VIOLATION_ID,
      "missing-signature-wrongful-rejection",
    ],
  },
  {
    category: "withdrawnReferenceInput",
    violationIds: [WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID],
  },
  {
    category: "canonicalDecodability",
    violationIds: [CANONICAL_DECODABILITY_VIOLATION_ID],
  },
  {
    category: "committedFieldShape",
    violationIds: [COMMITTED_FIELD_SHAPE_VIOLATION_ID],
  },
  { category: "minFee", violationIds: [MIN_FEE_VIOLATION_ID] },
  {
    category: "withdrawalMistag",
    violationIds: [
      WITHDRAWAL_MISTAG_VIOLATION_ID,
      "withdrawal-valid-marked-invalid",
      "withdrawal-invalid-marked-valid",
    ],
  },
  {
    category: "doubleWithdraw",
    violationIds: [DOUBLE_WITHDRAW_VIOLATION_ID],
  },
  {
    category: "l2TxMistag",
    violationIds: ["l2-tx-mistag", "valid-l2-tx-marked-invalid"],
  },
  {
    category: "withdrawnInput",
    violationIds: [WITHDRAWN_INPUT_VIOLATION_ID],
  },
  {
    category: "valueNotPreserved",
    violationIds: [
      "value-not-preserved",
      "value-not-preserved-wrongful-rejection",
    ],
  },
  {
    category: "inputSetUniqueness",
    violationIds: [
      "input-set-uniqueness",
      "input-set-uniqueness-wrongful-rejection",
      "duplicate-spend-input",
      "duplicate-reference-input",
      "spend-reference-overlap",
    ],
  },
  {
    category: "mintAuthorization",
    violationIds: [MINT_AUTHORIZATION_VIOLATION_ID],
  },
  {
    category: "networkId",
    violationIds: [NETWORK_ID_VIOLATION_ID, "network-id-wrongful-rejection"],
  },
  {
    category: "nativeScriptInvalid",
    violationIds: [NATIVE_SCRIPT_INVALID_VIOLATION_ID],
  },
  { category: "minAda", violationIds: [MIN_ADA_VIOLATION_ID] },
  {
    category: "fieldPreimageLengthMismatch",
    violationIds: ["field-preimage-length-mismatch"],
  },
  {
    category: "fieldItemWidthIllegal",
    violationIds: ["field-item-width-illegal"],
  },
  {
    category: "witnessScriptDecoding",
    violationIds: [
      "witness-script-header-malformed",
      "witness-native-script-malformed",
      "witness-native-script-node-limit",
      "witness-native-script-depth-limit",
    ],
  },
  {
    category: "scriptIntegrityHashMissing",
    violationIds: ["script-integrity-hash-missing"],
  },
  {
    category: "transactionOutputNonCanonical",
    violationIds: ["transaction-output-non-canonical"],
  },
  {
    category: "resolvedOutputNonCanonical",
    violationIds: ["resolved-output-non-canonical"],
  },
  {
    category: "mintDeclaredAssetLimit",
    violationIds: ["mint-declared-asset-limit"],
  },
  {
    category: "spendInputSignerMissing",
    violationIds: ["spend-input-signer-missing"],
  },
  {
    category: "protectedOutputSignerMissing",
    violationIds: ["protected-output-signer-missing"],
  },
  {
    category: "observersForbiddenOnUntaggedNetwork",
    violationIds: ["observers-forbidden-on-untagged-network"],
  },
  {
    category: "observerOrderInvalid",
    violationIds: ["observer-order-invalid"],
  },
  {
    category: "redeemerCanonicity",
    violationIds: ["redeemer-malformed"],
  },
  {
    category: "outputReferenceScriptDecoding",
    violationIds: [
      "output-reference-script-malformed",
      "output-reference-script-node-limit",
      "output-reference-script-depth-limit",
    ],
  },
  {
    category: "executionSourceScriptDecoding",
    violationIds: [
      "execution-native-script-malformed",
      "execution-native-script-node-limit",
      "execution-native-script-depth-limit",
    ],
  },
  {
    category: "receivePurposeLanguage",
    violationIds: ["receive-purpose-plutus-v3-forbidden"],
  },
  {
    category: "unusedScriptWitness",
    violationIds: ["unused-script-witness"],
  },
  {
    category: "missingScriptSource",
    violationIds: ["script-source-missing"],
  },
  {
    category: "missingRedeemer",
    violationIds: ["redeemer-missing"],
  },
  {
    category: "unusedRedeemer",
    violationIds: ["unused-redeemer"],
  },
  {
    category: "executionNativeScriptInvalid",
    violationIds: ["execution-native-script-invalid"],
  },
  {
    category: "scriptIntegrityHashMismatch",
    violationIds: ["script-integrity-hash-mismatch"],
  },
  {
    category: "distinctAssetAccumulationLimit",
    violationIds: [
      "input-asset-accumulation-limit",
      "output-asset-accumulation-limit",
      "mint-asset-accumulation-limit",
    ],
  },
  {
    category: "mintItemNonCanonical",
    violationIds: ["mint-item-non-canonical"],
  },
] as const satisfies readonly FraudProofClassificationRule[]);

type RegisteredClassificationRule =
  (typeof FRAUD_PROOF_CLASSIFICATION_RULES)[number];

export type RegisteredFraudProofViolationId =
  RegisteredClassificationRule["violationIds"][number];

export type ResolvedClassificationRule = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly familyPriority: number;
  readonly violationPriority: number;
};
