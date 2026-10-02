import { type StepSummary, type TxObservation } from "./runner.js";
import {
  type CleanRunGate,
  type E2ERunSummary,
  type FinalFunctionalGate,
  type RunVerdict,
  type StepRetrySummary,
  stepTxObservations,
  type TransactionEvidence,
} from "./summary.parse-step-retry-summary.js";

export const buildStepRetrySummary = (
  steps: readonly StepSummary[],
): readonly StepRetrySummary[] => {
  const byStep = new Map<string, StepSummary[]>();
  for (const step of steps) {
    byStep.set(step.id, [...(byStep.get(step.id) ?? []), step]);
  }
  return Array.from(byStep.entries()).map(([stepId, attempts]) => {
    const ordered = [...attempts].sort(
      (a, b) => Date.parse(a.startedAt) - Date.parse(b.startedAt),
    );
    const latest = ordered[ordered.length - 1]!;
    const firstSuccess = ordered.find((step) => step.status === "success");
    return {
      stepId,
      attempts: ordered.length,
      failedAttempts: ordered.filter((step) => step.status !== "success")
        .length,
      latestStatus: latest.status,
      latestError: latest.error,
      firstSuccessAt: firstSuccess?.finishedAt ?? null,
      lastFinishedAt: latest.finishedAt,
    };
  });
};

const txLabelFromObservation = (observation: TxObservation): string => {
  const field = observation.field ?? "";
  if (field.endsWith(".registerTxHash")) {
    return "operator-registration";
  }
  if (field.endsWith(".activateTxHash")) {
    return "operator-activation";
  }
  if (field.endsWith(".deregisterTxHash")) {
    return "operator-deregistration";
  }
  if (field.endsWith(".mergeTxHash")) {
    return "merge";
  }
  if (field.endsWith(".initTxHash")) {
    return "init";
  }
  return observation.stepId.replace(/^submit-/, "").replace(/^project-/, "");
};

export const evidenceStatuses = new Set([
  "submitted",
  "confirmed",
  "committed",
]);

export const transactionEvidenceFromStepSummaries = (
  steps: readonly StepSummary[],
): readonly TransactionEvidence[] =>
  steps.flatMap((step) =>
    stepTxObservations(step).flatMap((observation) => {
      if (!evidenceStatuses.has(observation.role)) {
        return [];
      }
      return [
        {
          label: txLabelFromObservation(observation),
          txHash: observation.txHash,
          status: observation.role as TransactionEvidence["status"],
          source:
            observation.field === undefined
              ? `${observation.source}:${observation.stepId}`
              : `${observation.source}:${observation.stepId}:${observation.field}`,
        } satisfies TransactionEvidence,
      ];
    }),
  );

const transactionStatusRank = (
  status: TransactionEvidence["status"],
): number => {
  switch (status) {
    case "rejected":
      return 6;
    case "committed":
      return 5;
    case "confirmed":
      return 4;
    case "submitted":
      return 3;
    case "accepted":
    case "queued":
      return 2;
    case "unknown":
      return 1;
  }
};

export const mergeTransactionEvidence = (
  evidence: readonly TransactionEvidence[],
): readonly TransactionEvidence[] => {
  const byKey = new Map<string, TransactionEvidence>();
  for (const entry of evidence) {
    const key = `${entry.label}:${entry.txHash}`;
    const previous = byKey.get(key);
    if (
      previous === undefined ||
      transactionStatusRank(entry.status) >
        transactionStatusRank(previous.status)
    ) {
      byKey.set(key, entry);
    }
  }
  return Array.from(byKey.values());
};

const transactionFunctionalGateStatus = (
  status: TransactionEvidence["status"],
): FinalFunctionalGate["status"] => {
  switch (status) {
    case "confirmed":
    case "committed":
      return "satisfied";
    case "rejected":
      return "failed";
    case "accepted":
    case "queued":
    case "submitted":
    case "unknown":
      return "pending";
  }
};

const selectFunctionalTransactionEvidence = (
  attempts: readonly TransactionEvidence[],
): TransactionEvidence => {
  const committed = attempts.find((tx) => tx.status === "committed");
  if (committed !== undefined) {
    return committed;
  }
  const confirmed = attempts.find((tx) => tx.status === "confirmed");
  if (confirmed !== undefined) {
    return confirmed;
  }
  const rejected = attempts.find((tx) => tx.status === "rejected");
  if (rejected !== undefined) {
    return rejected;
  }
  return attempts[0]!;
};

const transactionFunctionalGates = (
  transactions: readonly TransactionEvidence[],
): readonly FinalFunctionalGate[] => {
  const byLabel = new Map<string, TransactionEvidence[]>();
  for (const tx of transactions) {
    byLabel.set(tx.label, [...(byLabel.get(tx.label) ?? []), tx]);
  }
  return Array.from(byLabel.entries()).map(([label, attempts]) => {
    const selected = selectFunctionalTransactionEvidence(attempts);
    return {
      label: `transaction:${label}`,
      status: transactionFunctionalGateStatus(selected.status),
      source: selected.source,
      details: {
        txHash: selected.txHash,
        status: selected.status,
        attempts: attempts.length.toString(),
      },
    };
  });
};

export const buildFinalFunctionalGates = ({
  http,
  db,
  transactions,
}: Pick<
  E2ERunSummary,
  "http" | "db" | "transactions"
>): readonly FinalFunctionalGate[] => [
  ...http.map(
    (entry): FinalFunctionalGate => ({
      label: entry.label,
      status: entry.semanticStatus,
      source: entry.source,
      details: {
        method: entry.method,
        statusCode: entry.statusCode.toString(),
      },
    }),
  ),
  ...db.map(
    (entry): FinalFunctionalGate => ({
      label: entry.label,
      status: entry.status,
      source: entry.source,
      details: entry.details,
    }),
  ),
  ...transactionFunctionalGates(transactions),
];

const recomputeStepCleanRunVerdict = (
  steps: readonly StepSummary[],
): RunVerdict => {
  if (
    steps.some(
      (step) => step.status === "failed" || step.status === "runner_error",
    )
  ) {
    return "failed";
  }
  if (steps.some((step) => step.status === "timeout")) {
    return "blocked";
  }
  if (steps.some((step) => step.status === "signaled")) {
    return "interrupted";
  }
  if (steps.length > 0 && steps.every((step) => step.status === "success")) {
    return "success";
  }
  return "unknown";
};

const cleanRunGateVerdict = (
  cleanRunGates: readonly CleanRunGate[],
): RunVerdict => {
  if (cleanRunGates.length === 0) {
    return "success";
  }
  if (cleanRunGates.some((gate) => gate.status === "failed")) {
    return "failed";
  }
  if (cleanRunGates.some((gate) => gate.status === "blocked")) {
    return "blocked";
  }
  if (cleanRunGates.some((gate) => gate.status === "interrupted")) {
    return "interrupted";
  }
  if (cleanRunGates.some((gate) => gate.status === "unknown")) {
    return "unknown";
  }
  if (cleanRunGates.every((gate) => gate.status === "satisfied")) {
    return "success";
  }
  return "unknown";
};

const verdictSeverity = (verdict: RunVerdict): number => {
  switch (verdict) {
    case "failed":
      return 5;
    case "blocked":
      return 4;
    case "interrupted":
      return 3;
    case "unknown":
      return 2;
    case "success":
      return 1;
  }
};

const mostSevereVerdict = (left: RunVerdict, right: RunVerdict): RunVerdict =>
  verdictSeverity(left) >= verdictSeverity(right) ? left : right;

export const recomputeCleanRunVerdict = ({
  steps,
  transactions,
  cleanRunGates,
}: Pick<
  E2ERunSummary,
  "steps" | "transactions" | "cleanRunGates"
>): RunVerdict =>
  mostSevereVerdict(
    mostSevereVerdict(
      recomputeStepCleanRunVerdict(steps),
      cleanRunGateVerdict(cleanRunGates),
    ),
    transactions.some((tx) => tx.status === "rejected") ? "failed" : "success",
  );

export const recomputeFunctionalVerdict = (
  gates: readonly FinalFunctionalGate[],
): RunVerdict => {
  if (gates.some((gate) => gate.status === "failed")) {
    return "failed";
  }
  if (gates.some((gate) => gate.status === "blocked")) {
    return "blocked";
  }
  if (gates.length > 0 && gates.every((gate) => gate.status === "satisfied")) {
    return "success";
  }
  return "unknown";
};
