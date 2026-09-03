import {
  admitAuthenticatedStateQueueHeaderObservation,
  CANONICAL_DECODABILITY_VIOLATION_ID,
  COMMITTED_FIELD_SHAPE_VIOLATION_ID,
  CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID,
  DA_HASH_PREIMAGE_VIOLATION_ID,
  DOUBLE_WITHDRAW_VIOLATION_ID,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  INPUT_NO_IDX_VIOLATION_ID,
  INVALID_SIGNATURE_VIOLATION_ID,
  MIN_ADA_VIOLATION_ID,
  MIN_FEE_VIOLATION_ID,
  MINT_AUTHORIZATION_VIOLATION_ID,
  MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID,
  MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID,
  MISSING_SIGNATURE_VIOLATION_ID,
  NATIVE_SCRIPT_DECODING_VIOLATION_ID,
  NATIVE_SCRIPT_INVALID_VIOLATION_ID,
  REFERENCE_INPUT_NO_IDX_VIOLATION_ID,
  WITHDRAWAL_MISTAG_VIOLATION_ID,
  WITHDRAWN_INPUT_VIOLATION_ID,
  WITHDRAWN_REFERENCE_INPUT_VIOLATION_ID,
} from "@al-ft/midgard-sdk";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence-v1.js";

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
    violationIds: ["no-input", "non-existent-input"],
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
    violationIds: ["no-reference-input"],
  },
  {
    category: "referenceInputNoIdx",
    violationIds: [REFERENCE_INPUT_NO_IDX_VIOLATION_ID],
  },
  {
    category: "invalidSignature",
    violationIds: [INVALID_SIGNATURE_VIOLATION_ID],
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
    violationIds: [MISSING_SIGNATURE_VIOLATION_ID],
  },
  {
    category: "missingNativeScriptTx",
    violationIds: [MISSING_NATIVE_SCRIPT_TX_VIOLATION_ID],
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
    category: "crossBlockDuplicateEvent",
    violationIds: [CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID],
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
    violationIds: ["value-not-preserved"],
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
  { category: "networkId", violationIds: [NETWORK_ID_VIOLATION_ID] },
  {
    category: "missingNativeScriptUtxo",
    violationIds: [MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID],
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
] as const satisfies readonly FraudProofClassificationRule[]);

type RegisteredClassificationRule =
  (typeof FRAUD_PROOF_CLASSIFICATION_RULES)[number];

export type RegisteredFraudProofViolationId =
  RegisteredClassificationRule["violationIds"][number];

type ResolvedClassificationRule = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly familyPriority: number;
  readonly violationPriority: number;
};

const classificationByViolationId = new Map<
  string,
  ResolvedClassificationRule
>();

for (const [
  familyPriority,
  rule,
] of FRAUD_PROOF_CLASSIFICATION_RULES.entries()) {
  if (rule.category !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER[familyPriority]) {
    throw new Error(
      `classification rule ${familyPriority.toString()} must be ${String(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER[familyPriority])}, got ${rule.category}`,
    );
  }
  for (const [violationPriority, violationId] of rule.violationIds.entries()) {
    if (classificationByViolationId.has(violationId)) {
      throw new Error(`duplicate fraud-proof violation id: ${violationId}`);
    }
    classificationByViolationId.set(violationId, {
      category: rule.category,
      familyPriority,
      violationPriority,
    });
  }
}

export type CanonicalViolationDetection = {
  /** Stable detector-owned identity, used only as a deterministic tie-break. */
  readonly detectionId: string;
  readonly headerHash: string;
  readonly violationId: string;
  /** Earliest invalid transition/event ordinal in the committed block. */
  readonly position: bigint;
  /** Public diagnostic text only; never used to select a proof family. */
  readonly diagnostic?: string;
};

export type UnprovableGap = CanonicalViolationDetection & {
  readonly reason: "unregistered_violation";
};

export type CanonicalBlockClassification =
  | {
      readonly schemaVersion: typeof FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION;
      /** Empty detector output is not a complete canonical replay verdict. */
      readonly decision: "no_fault_detected";
      readonly headerHash: string;
      readonly detections: readonly [];
      readonly unprovableGaps: readonly [];
    }
  | {
      readonly schemaVersion: typeof FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION;
      readonly decision: "unprovable_gap";
      readonly headerHash: string;
      readonly selected: UnprovableGap;
      readonly detections: readonly CanonicalViolationDetection[];
      readonly unprovableGaps: readonly UnprovableGap[];
    }
  | {
      readonly schemaVersion: typeof FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION;
      readonly decision: "fault_detected";
      readonly headerHash: string;
      readonly category: FraudProofCatalogueCategoryName;
      readonly selected: CanonicalViolationDetection;
      readonly detections: readonly CanonicalViolationDetection[];
      readonly unprovableGaps: readonly UnprovableGap[];
    };

const validateDetection = (
  detection: CanonicalViolationDetection,
  headerHash: string,
): CanonicalViolationDetection => {
  if (detection.detectionId.length === 0) {
    throw new Error("canonical violation detectionId must not be empty");
  }
  if (detection.headerHash !== headerHash) {
    throw new Error(
      `violation ${detection.detectionId} targets header ${detection.headerHash}, expected ${headerHash}`,
    );
  }
  if (detection.violationId.length === 0) {
    throw new Error("canonical violation id must not be empty");
  }
  if (detection.position < 0n) {
    throw new Error("canonical violation position must not be negative");
  }
  return detection;
};

const compareDetections = (
  left: CanonicalViolationDetection,
  right: CanonicalViolationDetection,
): number => {
  if (left.position !== right.position) {
    return left.position < right.position ? -1 : 1;
  }
  const leftRule = classificationByViolationId.get(left.violationId);
  const rightRule = classificationByViolationId.get(right.violationId);
  const leftFamilyPriority =
    leftRule?.familyPriority ?? Number.MAX_SAFE_INTEGER;
  const rightFamilyPriority =
    rightRule?.familyPriority ?? Number.MAX_SAFE_INTEGER;
  if (leftFamilyPriority !== rightFamilyPriority) {
    return leftFamilyPriority - rightFamilyPriority;
  }
  const leftViolationPriority =
    leftRule?.violationPriority ?? Number.MAX_SAFE_INTEGER;
  const rightViolationPriority =
    rightRule?.violationPriority ?? Number.MAX_SAFE_INTEGER;
  if (leftViolationPriority !== rightViolationPriority) {
    return leftViolationPriority - rightViolationPriority;
  }
  if (left.violationId !== right.violationId) {
    return left.violationId < right.violationId ? -1 : 1;
  }
  return left.detectionId < right.detectionId
    ? -1
    : left.detectionId > right.detectionId
      ? 1
      : 0;
};

/**
 * Classifies detections from one authenticated committed block.
 *
 * An empty detection set yields only `no_fault_detected`; it is not sufficient
 * to claim a complete replay or a verified block. Any unknown mapping is
 * surfaced as `unprovable_gap`; it is never coerced to a generic family and
 * never dropped as if the block were healthy.
 */
export const classifyCanonicalBlockViolations = async ({
  evidence,
  detections,
  minimumConfirmationDepth,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly detections: readonly CanonicalViolationDetection[];
  readonly minimumConfirmationDepth?: number;
}): Promise<CanonicalBlockClassification> => {
  blockTransactionsFromCanonicalEvidence(evidence);
  const observation = await admitAuthenticatedStateQueueHeaderObservation({
    observation: evidence.observation,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  if (observation.headerHash !== evidence.headerHash) {
    throw new Error(
      `canonical evidence header mismatch: observation=${observation.headerHash}, evidence=${evidence.headerHash}`,
    );
  }
  const seenDetectionIds = new Set<string>();
  const ordered = detections
    .map((detection) => validateDetection(detection, evidence.headerHash))
    .map((detection) => {
      if (seenDetectionIds.has(detection.detectionId)) {
        throw new Error(
          `duplicate canonical violation detectionId: ${detection.detectionId}`,
        );
      }
      seenDetectionIds.add(detection.detectionId);
      return detection;
    })
    .sort(compareDetections);
  if (ordered.length === 0) {
    return {
      schemaVersion: FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION,
      decision: "no_fault_detected",
      headerHash: evidence.headerHash,
      detections: [],
      unprovableGaps: [],
    };
  }

  const unprovableGaps = ordered
    .filter(
      (detection) => !classificationByViolationId.has(detection.violationId),
    )
    .map(
      (detection): UnprovableGap => ({
        ...detection,
        reason: "unregistered_violation",
      }),
    );
  const earliestPosition = ordered[0]!.position;
  const earliest = ordered.filter(
    (detection) => detection.position === earliestPosition,
  );
  const selectedProvable = earliest.find((detection) =>
    classificationByViolationId.has(detection.violationId),
  );
  if (selectedProvable === undefined) {
    const selected = unprovableGaps.find(
      (gap) => gap.position === earliestPosition,
    );
    if (selected === undefined) {
      throw new Error("classification invariant: earliest gap disappeared");
    }
    return {
      schemaVersion: FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION,
      decision: "unprovable_gap",
      headerHash: evidence.headerHash,
      selected,
      detections: ordered,
      unprovableGaps,
    };
  }
  const rule = classificationByViolationId.get(selectedProvable.violationId);
  if (rule === undefined) {
    throw new Error("classification invariant: selected rule disappeared");
  }
  return {
    schemaVersion: FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION,
    decision: "fault_detected",
    headerHash: evidence.headerHash,
    category: rule.category,
    selected: selectedProvable,
    detections: ordered,
    unprovableGaps,
  };
};
