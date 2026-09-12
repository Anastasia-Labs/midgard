import {
  DOUBLE_WITHDRAW_VIOLATION_ID,
  EventKey,
  type EventKey as EventKeyValue,
  WITHDRAWAL_MISTAG_VIOLATION_ID,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { eventKeyFingerprint } from "../transition-trace/reconstruct.js";
import {
  type CanonicalViolationDetection,
  FRAUD_PROOF_CLASSIFICATION_RULES,
} from "./classification.js";
import { TYPED_REASON_DISPOSITIONS } from "./reason-disposition.js";

export type ReplayPrerequisite =
  | "accepted_terminal"
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

const directTransactionCategories = new Set<string>([
  ...Object.values(TYPED_REASON_DISPOSITIONS)
    .filter(({ proving }) => proving === "non_interactive")
    .flatMap(({ categories }) => categories),
  "doubleSpend",
  "nonExistentInputNoIndex",
  "referenceInputNoIdx",
  "missingNativeScriptTx",
  "missingNativeScriptUtxo",
  "withdrawnReferenceInput",
  "withdrawnInput",
  "committedFieldShape",
  "l2TxMistag",
  "mintItemNonCanonical",
]);

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

/** The union may discharge a proof-domain failure only with a direct finding
 * for its exact transaction. Positions in separate source frontiers must never
 * be mistaken for an event identity. */
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
  if (failure.prerequisite === "prior_transition_effect") {
    if (
      source !== undefined &&
      detections.some(
        (detection) =>
          detection.headerHash === evidence.headerHash &&
          detection.violationId === "transition-trace" &&
          detection.provenTransitionEventKeyCbor === failure.eventKeyCbor,
      )
    )
      return;
    throw new CanonicalReplayPrerequisiteError([failure]);
  }
  if (source?.phase === "Withdrawal") {
    if (
      failure.prerequisite === "present_spend_input" &&
      withdrawalPrerequisiteCovered(evidence, source.entry, detections)
    )
      return;
    throw new CanonicalReplayPrerequisiteError([failure]);
  }
  if (source?.phase !== "L2Transaction")
    throw new CanonicalReplayPrerequisiteError([failure]);
  const position = evidence.transactions.findIndex(
    (transaction) => transaction.nodeTxId === source.entry.txId,
  );
  if (position < 0) throw new CanonicalReplayPrerequisiteError([failure]);
  const allowed =
    failure.prerequisite === "present_spend_input"
      ? new Set([
          "doubleSpend",
          "nonExistentInput",
          "nonExistentInputNoIndex",
          "inputSetUniqueness",
          "withdrawnInput",
        ])
      : failure.prerequisite === "representable_field_shape"
        ? new Set(["committedFieldShape", "mintItemNonCanonical"])
        : failure.prerequisite === "representable_validity_flag"
          ? new Set(["l2TxMistag"])
          : directTransactionCategories;
  const covered = detections.some((detection) => {
    if (detection.headerHash !== evidence.headerHash) return false;
    const rule = FRAUD_PROOF_CLASSIFICATION_RULES.find((candidate) =>
      candidate.violationIds.some((id) => id === detection.violationId),
    );
    if (rule === undefined || !allowed.has(rule.category)) return false;
    if (rule.category === "doubleSpend") {
      // This proof covers both spenders. Its ordering coordinate identifies
      // only the second transaction in the canonical transaction vector, which
      // need not be the second spender in the operator's transition trace.
      const [family, first, second] = detection.detectionId.split(":");
      return (
        family === "double-spend" &&
        second === detection.position.toString() &&
        (first === position.toString() || second === position.toString())
      );
    }
    if (detection.position !== BigInt(position)) return false;
    // Wrongful rejections belong to the forced frontier, even when its ordinal
    // happens to equal this normal transaction's ordinal.
    if (detection.violationId.includes("wrongful-rejection")) return false;
    if (evidence.reconstruction.forcedTransactions[position] !== undefined)
      return false;
    return true;
  });
  if (!covered) throw new CanonicalReplayPrerequisiteError([failure]);
};
