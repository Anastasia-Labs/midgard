import {
  admittedDecisions,
  type HeaderDecision,
  type HeaderFaultDecision,
} from "./header-classifier.authenticated-state-queue-observation-digest.js";

export const requireRunnableHeaderFault = (
  decision: HeaderDecision,
): HeaderFaultDecision => {
  if (!admittedDecisions.has(decision)) {
    throw new Error("production header decision was not module-admitted");
  }
  if (decision.decision !== "fault_detected") {
    throw new Error(
      `only fault_detected may authorize a runnable job; received=${decision.decision}`,
    );
  }
  return decision;
};

/** Persistable exact envelope; does not recreate runnable admission. */
export const headerDecisionEnvelope = (
  decision: HeaderDecision,
): HeaderDecision => {
  if (!admittedDecisions.has(decision)) {
    throw new Error("production header decision was not module-admitted");
  }
  return decision;
};
