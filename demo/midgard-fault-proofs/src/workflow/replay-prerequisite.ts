import {
  CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID,
  DOUBLE_WITHDRAW_VIOLATION_ID,
  EventKey,
  type EventKey as EventKeyValue,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  WITHDRAWAL_MISTAG_VIOLATION_ID,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  eventKeyFingerprint,
  type SourceEventRecord,
} from "../transition-trace/reconstruct.js";
import {
  type CanonicalViolationDetection,
  FRAUD_PROOF_CLASSIFICATION_RULES,
} from "./classification.js";
import {
  authenticatedEventOrder,
  compareEventOrder,
  detectionEventOrder,
} from "./detection-subject.js";

export type ReplayPrerequisite =
  | "accepted_terminal"
  | "matching_source_origin"
  | "present_source_origin"
  | "present_spend_input"
  | "prior_transition_effect"
  | "representable_field_shape"
  | "representable_validity_flag";

export type ReplayPrerequisiteFailure = Readonly<{
  headerHash: string;
  eventKeyCbor: string;
  prerequisite: ReplayPrerequisite;
}>;

/** Completed findings survive when another event leaves a proof's domain. */
export class CanonicalReplayPrerequisiteError extends Error {
  constructor(
    readonly failures: readonly ReplayPrerequisiteFailure[],
    readonly detections: readonly CanonicalViolationDetection[] = [],
  ) {
    super(
      `Canonical replay prerequisites are unresolved: ${JSON.stringify(failures)}`,
    );
    this.name = "CanonicalReplayPrerequisiteError";
    this.failures = Object.freeze(
      failures.map((failure) => Object.freeze({ ...failure })),
    );
    this.detections = Object.freeze(
      detections.map((detection) => Object.freeze({ ...detection })),
    );
    Object.freeze(this);
  }
}

export const replayPrerequisiteFailure = (
  headerHash: string,
  eventKey: EventKeyValue,
  prerequisite: ReplayPrerequisite,
): CanonicalReplayPrerequisiteError =>
  new CanonicalReplayPrerequisiteError([
    Object.freeze({
      headerHash,
      eventKeyCbor: Data.to(eventKey, EventKey),
      prerequisite,
    }),
  ]);

export const completeReplayFindings = <T extends CanonicalViolationDetection>(
  detections: readonly T[],
  failures: readonly ReplayPrerequisiteFailure[],
): readonly T[] => {
  if (failures.length > 0)
    throw new CanonicalReplayPrerequisiteError(failures, detections);
  return detections;
};

/** Collect every event, while keeping ordinary corruption/acquisition errors fatal. */
export const collectReplayFindingBatches = async <
  T extends CanonicalViolationDetection,
>(
  tasks: readonly Promise<readonly T[]>[],
): Promise<readonly T[]> => {
  const detections: T[] = [];
  const partialDetections: CanonicalViolationDetection[] = [];
  const failures: ReplayPrerequisiteFailure[] = [];
  for (const result of await Promise.allSettled(tasks)) {
    if (result.status === "fulfilled") {
      detections.push(...result.value);
    } else if (result.reason instanceof CanonicalReplayPrerequisiteError) {
      partialDetections.push(...result.reason.detections);
      failures.push(...result.reason.failures);
    } else throw result.reason;
  }
  if (failures.length > 0)
    throw new CanonicalReplayPrerequisiteError(failures, [
      ...detections,
      ...partialDetections,
    ]);
  return detections;
};

export const collectReplayFindings = <T extends CanonicalViolationDetection>(
  tasks: readonly Promise<T | null>[],
): Promise<readonly T[]> =>
  collectReplayFindingBatches(
    tasks.map(async (task) => {
      const finding = await task;
      return finding === null ? [] : [finding];
    }),
  );

/** A payable withdrawal of an output the replayed ledger lacks is exactly what
 * the same-block withdrawal families prove: a mistagged leaf, or the second
 * payable leaf of a double withdrawal. Both address the committed leaf index. */
const withdrawalPrerequisiteCovered = (
  evidence: CanonicalBlockEvidence,
  entry: CanonicalBlockEvidence["reconstruction"]["withdrawals"][number],
  detections: readonly CanonicalViolationDetection[],
): boolean => {
  const leaf = evidence.reconstruction.withdrawals.indexOf(entry);
  if (leaf < 0) return false;
  return detections.some((detection) => {
    if (detection.headerHash !== evidence.headerHash) return false;
    if (detection.violationId === DOUBLE_WITHDRAW_VIOLATION_ID) {
      const [, first, second] = detection.detectionId.split(":");
      return first === leaf.toString() || second === leaf.toString();
    }
    return (
      detection.violationId === WITHDRAWAL_MISTAG_VIOLATION_ID &&
      detection.position === BigInt(leaf)
    );
  });
};

/**
 * Decision 0007 (`docs/fault-proofs/decisions/0007-operator-owned-event-validity.md`):
 * a committed deposit or withdrawal leaf whose L1 origin is absent, or whose
 * authentic origin differs in content, *is* the fabricated-family fraud. The
 * replay cannot project such a leaf, so it records a prerequisite, and only the
 * finding owed to that family — at this leaf's own committed position — may
 * discharge it.
 *
 * The replay itself cannot tell a never-authenticated identity from an origin
 * that was authenticated and has since been consumed or settled; a live-UTxO
 * capture shows the same emptiness for both, and the second is a
 * data-availability problem rather than fraud. The distinction is made where the
 * evidence lives: `classifyFabricatedDepositFault` establishes absence only by
 * exhibiting the committed identity in the authenticated *live* output-reference
 * set and refuses a consumed outref
 * (`consumed_live_utxo_fallback_refused`), and the withdrawal family follows the
 * same rule. A consumed origin therefore yields no finding, this prerequisite
 * stays undischarged, and the block fails closed instead of being convicted.
 *
 * Forced-transaction origins have no fabricated family, so 0007 does not reach
 * them and they never take this route.
 */
const fabricatedSourceOriginCovered = (
  evidence: CanonicalBlockEvidence,
  source: SourceEventRecord | undefined,
  detections: readonly CanonicalViolationDetection[],
): boolean => {
  const family =
    source?.phase === "Deposit"
      ? {
          leaves: evidence.reconstruction.deposits as readonly unknown[],
          violationId: FABRICATED_DEPOSIT_VIOLATION_ID as string,
        }
      : source?.phase === "Withdrawal"
        ? {
            leaves: evidence.reconstruction.withdrawals as readonly unknown[],
            violationId: FABRICATED_WITHDRAWAL_VIOLATION_ID as string,
          }
        : undefined;
  if (family === undefined) return false;
  const leaf = family.leaves.indexOf(source!.entry);
  if (leaf < 0) return false;
  return detections.some(
    (detection) =>
      detection.headerHash === evidence.headerHash &&
      detection.violationId === family.violationId &&
      detection.position === BigInt(leaf),
  );
};

/**
 * Whether a registered finding convicts a transition at or before the event
 * the replay could not open, on the block's one event order (phase rank, then
 * the authenticated transition step). Any such finding makes the block
 * removable, and selection orders on the same axis, so the chosen proof is
 * never later than the unopened event and no earlier fault the replay did
 * not reach can hide behind it (GOAL_SPEC §6). The order comes only from each
 * finding's declared subject events, never from its position or violation id,
 * and an advisory finding without a classification rule never discharges.
 */
const coveredAtOrBefore = (
  evidence: CanonicalBlockEvidence,
  failure: ReplayPrerequisiteFailure,
  detections: readonly CanonicalViolationDetection[],
): boolean => {
  const limit = (() => {
    try {
      return authenticatedEventOrder(
        evidence.reconstruction,
        failure.eventKeyCbor,
      );
    } catch {
      return undefined;
    }
  })();
  if (limit === undefined) return false;
  return detections.some(
    (detection) =>
      detection.headerHash === evidence.headerHash &&
      FRAUD_PROOF_CLASSIFICATION_RULES.some((rule) =>
        rule.violationIds.some((id) => id === detection.violationId),
      ) &&
      compareEventOrder(
        detectionEventOrder(evidence.reconstruction, detection),
        limit,
      ) <= 0,
  );
};

/** Discharges a replay prerequisite, or fails closed. Deposit and withdrawal
 * events keep their decision-0007 coverage; normal and forced transactions are
 * covered by any registered finding at or before them. */
export const assertReplayPrerequisiteCovered = (
  evidence: CanonicalBlockEvidence,
  failure: ReplayPrerequisiteFailure,
  detections: readonly CanonicalViolationDetection[],
): void => {
  if (failure.headerHash !== evidence.headerHash)
    throw new CanonicalReplayPrerequisiteError([failure]);
  const eventKey = Data.from(failure.eventKeyCbor, EventKey);
  const source = evidence.reconstruction.sourceEventsByFingerprint.get(
    eventKeyFingerprint(eventKey),
  );
  if (
    failure.prerequisite === "present_source_origin" ||
    failure.prerequisite === "matching_source_origin"
  ) {
    if (fabricatedSourceOriginCovered(evidence, source, detections)) return;
    throw new CanonicalReplayPrerequisiteError([failure]);
  }
  if (source?.phase === "Deposit" || source?.phase === "Withdrawal") {
    if (failure.prerequisite === "prior_transition_effect") {
      if (
        detections.some(
          (detection) =>
            detection.headerHash === evidence.headerHash &&
            detection.violationId === "transition-trace" &&
            detection.provenTransitionEventKeyCbor === failure.eventKeyCbor,
        )
      )
        return;
      // A deposit whose output the ledger already holds repeats a settled
      // event; the cross-block finding at that source position names it.
      if (
        source.phase === "Deposit" &&
        detections.some(
          (detection) =>
            detection.headerHash === evidence.headerHash &&
            detection.violationId ===
              CROSS_BLOCK_DUPLICATE_EVENT_VIOLATION_ID &&
            detection.position ===
              BigInt(evidence.reconstruction.sourceEvents.indexOf(source)),
        )
      )
        return;
      throw new CanonicalReplayPrerequisiteError([failure]);
    }
    if (
      source.phase === "Withdrawal" &&
      failure.prerequisite === "present_spend_input" &&
      withdrawalPrerequisiteCovered(evidence, source.entry, detections)
    )
      return;
    throw new CanonicalReplayPrerequisiteError([failure]);
  }
  if (source === undefined || !coveredAtOrBefore(evidence, failure, detections))
    throw new CanonicalReplayPrerequisiteError([failure]);
};
