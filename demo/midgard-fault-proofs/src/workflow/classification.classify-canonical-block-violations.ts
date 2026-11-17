import {
  admitAuthenticatedStateQueueHeaderObservation,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  blockTransactionsFromCanonicalEvidence,
  type CanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import { isCommittedInvalidNormalLeaf } from "../evidence/committed-normal-leaf-validity.js";
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

/**
 * Same-event family precedence: the catalogue order with exactly the
 * owner-ruled corners moved. The catalogue order itself stays append-only
 * because deployment identity is derived from it.
 *
 * Decision 0009 (owner ruling 2026-11-10): `doubleWithdraw` ranks ahead of
 * `withdrawalMistag`.
 * Two payable leaves naming one L2 output make the later one also a "valid
 * marked, output absent" `withdrawalMistag` finding at the same event; the
 * watcher proves the pair (two steps) rather than the mistag (five). Both
 * slash the same block. A mistagged leaf that is not the later half of such a
 * pair has no `doubleWithdraw` finding at its event, so it stays
 * `withdrawalMistag`.
 *
 * Decision 0010 (owner ruling 2026-11-15): `nonExistentInputNoIndex`
 * ranks ahead of `nonExistentInput`, and `referenceInputNoIdx` ahead of
 * `noReferenceInput`.
 * An input or reference input whose output index is out of range for a
 * producer committed by the same block is also absent from the event's
 * pre-state ledger, so the ledger-absence family finds it at the same event.
 * The watcher proves the out-of-range index, which needs no ledger proof. Both
 * slash the same block. An absent input with no same-block producer has no
 * NoIdx finding, so it stays with the ledger-absence family.
 *
 * Decision of 2026-11-17, by the precedent of 0009 and 0010: `withdrawnInput`
 * ranks ahead of `nonExistentInput`, and `withdrawnReferenceInput` ahead of
 * `noReferenceInput`. The owner may overrule it.
 * A valid withdrawal of the same block removes the output before the
 * transaction's event, so the ledger-absence family also finds the input
 * absent at the same event. The watcher proves the withdrawal. Both slash the
 * same block. An input absent for any other reason has no withdrawn finding,
 * so it stays with the ledger-absence family.
 */
const OWNER_RULED_PROMOTIONS: readonly Readonly<{
  promoted: FraudProofCatalogueCategoryName;
  before: FraudProofCatalogueCategoryName;
}>[] = [
  { promoted: "doubleWithdraw", before: "withdrawalMistag" },
  { promoted: "nonExistentInputNoIndex", before: "nonExistentInput" },
  { promoted: "referenceInputNoIdx", before: "noReferenceInput" },
  { promoted: "withdrawnInput", before: "nonExistentInput" },
  { promoted: "withdrawnReferenceInput", before: "noReferenceInput" },
];

const FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE: readonly FraudProofCatalogueCategoryName[] =
  (() => {
    const order = [...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER];
    for (const { promoted, before } of OWNER_RULED_PROMOTIONS) {
      const from = order.indexOf(promoted);
      const to = order.indexOf(before);
      if (from < 0 || to < 0 || from < to) {
        throw new Error(
          `${promoted} precedence does not match the catalogue order`,
        );
      }
      order.splice(from, 1);
      order.splice(to, 0, promoted);
    }
    return Object.freeze(order);
  })();

for (const [ruleIndex, rule] of FRAUD_PROOF_CLASSIFICATION_RULES.entries()) {
  if (rule.category !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER[ruleIndex]) {
    throw new Error(
      `classification rule ${ruleIndex.toString()} must be ${String(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER[ruleIndex])}, got ${rule.category}`,
    );
  }
  const familyPriority = FRAUD_PROOF_CLASSIFICATION_FAMILY_PRECEDENCE.indexOf(
    rule.category,
  );
  if (familyPriority < 0) {
    throw new Error(`classification precedence omits ${rule.category}`);
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
 * The families whose proofs open a normal leaf whatever its committed
 * validity code. A normal leaf is the operator's acceptance verdict, so a leaf
 * committed with a non-zero code is itself the fault, and `l2TxMistag` proves
 * it (`l2-tx-mistag/step-01.ak` accepts any code but 0). `daHashPreimage`
 * binds the leaf at either code (`da-hash-preimage/rule.ak`). Every other
 * family's step opens an accepted subject only at code 0, `transitionTrace`
 * included (`accepted-transaction-yields.ak` expects a normal leaf's code to
 * be 0), so its finding at such a leaf can never be proven.
 */
const OPENS_INVALID_TAGGED_NORMAL_LEAF: ReadonlySet<FraudProofCatalogueCategoryName> =
  new Set(["l2TxMistag", "daHashPreimage"]);

/**
 * Whether the classifier may select `detection`: a finding about normal
 * leaves is admitted only when no leaf it names carries a non-zero committed
 * validity code (`isCommittedInvalidNormalLeaf`), unless its family proves
 * such leaves. The installed catalogue always holds `l2TxMistag`, which finds
 * every such leaf at the same event, so dropping the others never loses the
 * leaf's fault. An unregistered finding is kept, so it still surfaces as a
 * gap.
 */
const admitsDetection = (
  reconstruction: CanonicalBlockEvidence["reconstruction"],
  detection: CanonicalViolationDetection,
): boolean => {
  if (detection.frontier !== "accepted") return true;
  const rule = classificationByViolationId.get(detection.violationId);
  if (rule === undefined || OPENS_INVALID_TAGGED_NORMAL_LEAF.has(rule.category))
    return true;
  return detection.subjectEventKeyCbors.every((eventKeyCbor) => {
    const source = reconstruction.sourceEventsByFingerprint.get(eventKeyCbor);
    // An unknown event is left for the event order to refuse.
    return (
      source?.phase !== "L2Transaction" ||
      !isCommittedInvalidNormalLeaf(source.entry.value)
    );
  });
};

/**
 * The detections from one block the classifier may select, in their given
 * order (see `admitsDetection`). A hold or prerequisite check that weighs
 * findings must weigh only these, or a finding the classifier drops would
 * decide a block it cannot prove.
 */
export const admittedCanonicalViolationDetections = <
  Detection extends CanonicalViolationDetection,
>(
  evidence: CanonicalBlockEvidence,
  detections: readonly Detection[],
): Detection[] =>
  detections.filter((detection) =>
    admitsDetection(evidence.reconstruction, detection),
  );

/**
 * The detection `classifyCanonicalBlockViolations` orders first among the
 * admitted `detections` from one block, or undefined when none is admitted.
 */
export const firstCanonicalViolationDetection = <
  Detection extends CanonicalViolationDetection,
>(
  evidence: CanonicalBlockEvidence,
  detections: readonly Detection[],
): Detection | undefined => {
  const orderOf = (detection: CanonicalViolationDetection): EventOrder =>
    detectionEventOrder(evidence.reconstruction, detection);
  return admittedCanonicalViolationDetections(evidence, detections).reduce<
    Detection | undefined
  >(
    (first, detection) =>
      first === undefined || compareDetections(orderOf, detection, first) < 0
        ? detection
        : first,
    undefined,
  );
};

/**
 * Whether `classifyCanonicalBlockViolations` decides `fault_detected` on
 * `detections` from one block, rather than `unprovable_gap` or
 * `no_fault_detected`: the first admitted detection is registered exactly
 * when a registered finding exists at the earliest admitted event.
 */
export const selectsProvableFault = (
  evidence: CanonicalBlockEvidence,
  detections: readonly CanonicalViolationDetection[],
): boolean => {
  const first = firstCanonicalViolationDetection(evidence, detections);
  return (
    first !== undefined && classificationByViolationId.has(first.violationId)
  );
};

/**
 * Classifies detections from one authenticated committed block.
 *
 * Only admitted detections are classified (see `admitsDetection`).
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
    .filter((detection) => admitsDetection(evidence.reconstruction, detection))
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
