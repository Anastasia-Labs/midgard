import {
  admitAuthenticatedStateQueueHeaderObservation,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import {
  FRAUD_PROOF_CLASSIFICATION_RULES,
  FRAUD_PROOF_CLASSIFICATION_SCHEMA_VERSION,
  type ResolvedClassificationRule,
} from "./classification.fraud-proof-classification-rules.js";
import {
  compareEventOrder,
  detectionEventOrder,
  type DetectionSubject,
  type EventOrder,
} from "./detection-subject.js";

const classificationByViolationId = new Map<
  string,
  ResolvedClassificationRule
>();

// Same-event family precedence is the catalogue order: each rule's index is
// its family priority.
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

export type CanonicalViolationDetection = DetectionSubject & {
  /** Stable detector-owned identity, used only as a deterministic tie-break. */
  readonly detectionId: string;
  readonly headerHash: string;
  readonly violationId: string;
  /**
   * Reported ordinal within the detection's own source frontier. It never
   * orders detections: the declared subject's step in the authenticated
   * transition trace does, independent of the committed `event_to_step`.
   */
  readonly position: bigint;
  /** Public diagnostic text only; never used to select a proof family. */
  readonly diagnostic?: string;
  /** Exact event authenticated by a semantic transition proof, when available. */
  readonly provenTransitionEventKeyCbor?: string;
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
  orderOf: (detection: CanonicalViolationDetection) => EventOrder,
  left: CanonicalViolationDetection,
  right: CanonicalViolationDetection,
): number => {
  const byEvent = compareEventOrder(orderOf(left), orderOf(right));
  if (byEvent !== 0) {
    return byEvent;
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
  const orders = new Map<CanonicalViolationDetection, EventOrder>();
  const orderOf = (detection: CanonicalViolationDetection): EventOrder =>
    orders.get(detection)!;
  const ordered = detections
    .map((detection) => validateDetection(detection, evidence.headerHash))
    .map((detection) => {
      if (seenDetectionIds.has(detection.detectionId)) {
        throw new Error(
          `duplicate canonical violation detectionId: ${detection.detectionId}`,
        );
      }
      seenDetectionIds.add(detection.detectionId);
      orders.set(
        detection,
        detectionEventOrder(evidence.reconstruction, detection),
      );
      return detection;
    })
    .sort((left, right) => compareDetections(orderOf, left, right));
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
  const earliestOrder = orderOf(ordered[0]!);
  const atEarliest = (detection: CanonicalViolationDetection) =>
    compareEventOrder(orderOf(detection), earliestOrder) === 0;
  const earliest = ordered.filter(atEarliest);
  const selectedProvable = earliest.find((detection) =>
    classificationByViolationId.has(detection.violationId),
  );
  if (selectedProvable === undefined) {
    const selected = unprovableGaps.find((gap) =>
      earliest.some((detection) => detection.detectionId === gap.detectionId),
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
