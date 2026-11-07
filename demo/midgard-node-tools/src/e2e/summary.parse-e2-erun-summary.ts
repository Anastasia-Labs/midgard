import { isDeepStrictEqual } from "node:util";

import {
  arrayOf,
  exactRecord,
  isoTimestamp,
  nonEmptyString,
  oneOf,
  stringArray,
} from "midgard-node/artifact-schema";

import {
  parseE2EStep,
  parseTxObservation,
  type StepSummary,
} from "./runner.js";
import {
  buildFinalFunctionalGates,
  buildStepRetrySummary,
  mergeTransactionEvidence,
  recomputeCleanRunVerdict,
  recomputeFunctionalVerdict,
  transactionEvidenceFromStepSummaries,
} from "./summary.build-final-functional-gates.js";
import {
  E2E_SUMMARY_SCHEMA_VERSION,
  type E2ERunSummary,
  type NextSafeAction,
  parseCleanRunGate,
  parseDbEvidence,
  parseFinalFunctionalGate,
  parseHttpEvidence,
  parseRawEvidenceRef,
  parseStepRetrySummary,
  parseTransactionEvidence,
  type RunVerdict,
  stepTxObservations,
  type TransactionEvidence,
} from "./summary.parse-step-retry-summary.js";

export const parseE2ERunSummary = (
  value: unknown,
  label = "E2E run summary",
): E2ERunSummary => {
  const input = exactRecord(value, label, [
    "schemaVersion",
    "runId",
    "mode",
    "verdict",
    "cleanRunVerdict",
    "functionalVerdict",
    "nextSafeAction",
    "startedAt",
    "updatedAt",
    "steps",
    "stepRetrySummary",
    "txObservations",
    "finalFunctionalGates",
    "cleanRunGates",
    "transactions",
    "http",
    "db",
    "rawEvidence",
    "notes",
  ]);
  if (input.schemaVersion !== E2E_SUMMARY_SCHEMA_VERSION) {
    throw new Error(
      `${label}.schemaVersion must be ${E2E_SUMMARY_SCHEMA_VERSION}`,
    );
  }
  const verdicts = [
    "success",
    "failed",
    "blocked",
    "interrupted",
    "unknown",
  ] as const;
  const parsed: E2ERunSummary = {
    schemaVersion: E2E_SUMMARY_SCHEMA_VERSION,
    runId: nonEmptyString(input.runId, `${label}.runId`),
    mode: oneOf(input.mode, `${label}.mode`, [
      "attach",
      "resume",
      "fresh",
      "unknown",
    ]),
    verdict: oneOf(input.verdict, `${label}.verdict`, verdicts),
    cleanRunVerdict: oneOf(
      input.cleanRunVerdict,
      `${label}.cleanRunVerdict`,
      verdicts,
    ),
    functionalVerdict: oneOf(
      input.functionalVerdict,
      `${label}.functionalVerdict`,
      verdicts,
    ),
    nextSafeAction: oneOf(input.nextSafeAction, `${label}.nextSafeAction`, [
      "none_run_complete",
      "fix_pre_submit_and_rerun_step",
      "reconcile_submitted_tx_before_rerun",
      "wait_until_deposit_projection_due",
      "inspect_state_queue_lease",
      "investigate_unknown",
    ]),
    startedAt: isoTimestamp(input.startedAt, `${label}.startedAt`),
    updatedAt: isoTimestamp(input.updatedAt, `${label}.updatedAt`),
    steps: arrayOf(input.steps, `${label}.steps`, parseE2EStep),
    stepRetrySummary: arrayOf(
      input.stepRetrySummary,
      `${label}.stepRetrySummary`,
      parseStepRetrySummary,
    ),
    txObservations: arrayOf(
      input.txObservations,
      `${label}.txObservations`,
      parseTxObservation,
    ),
    finalFunctionalGates: arrayOf(
      input.finalFunctionalGates,
      `${label}.finalFunctionalGates`,
      parseFinalFunctionalGate,
    ),
    cleanRunGates: arrayOf(
      input.cleanRunGates,
      `${label}.cleanRunGates`,
      parseCleanRunGate,
    ),
    transactions: arrayOf(
      input.transactions,
      `${label}.transactions`,
      parseTransactionEvidence,
    ),
    http: arrayOf(input.http, `${label}.http`, parseHttpEvidence),
    db: arrayOf(input.db, `${label}.db`, parseDbEvidence),
    rawEvidence: arrayOf(
      input.rawEvidence,
      `${label}.rawEvidence`,
      parseRawEvidenceRef,
    ),
    notes: stringArray(input.notes, `${label}.notes`),
  };
  const expectedStepRetrySummary = buildStepRetrySummary(parsed.steps);
  const expectedTxObservations = parsed.steps.flatMap(stepTxObservations);
  const expectedTransactions = mergeTransactionEvidence([
    ...parsed.transactions,
    ...transactionEvidenceFromStepSummaries(parsed.steps),
  ]);
  const expectedFinalFunctionalGates = buildFinalFunctionalGates({
    ...parsed,
    transactions: expectedTransactions,
  });
  const expectedCleanRunVerdict = recomputeCleanRunVerdict({
    ...parsed,
    transactions: expectedTransactions,
  });
  const expectedFunctionalVerdict = recomputeFunctionalVerdict(
    expectedFinalFunctionalGates,
  );
  const expectedVerdict = operatorVerdict({
    cleanRunVerdict: expectedCleanRunVerdict,
    functionalVerdict: expectedFunctionalVerdict,
  });
  const expectedNextSafeAction = classifyNextSafeAction({
    ...parsed,
    transactions: expectedTransactions,
    verdict: expectedVerdict,
    functionalVerdict: expectedFunctionalVerdict,
  });
  if (
    Date.parse(parsed.updatedAt) < Date.parse(parsed.startedAt) ||
    !isDeepStrictEqual(parsed.stepRetrySummary, expectedStepRetrySummary) ||
    !isDeepStrictEqual(parsed.txObservations, expectedTxObservations) ||
    !isDeepStrictEqual(parsed.transactions, expectedTransactions) ||
    !isDeepStrictEqual(
      parsed.finalFunctionalGates,
      expectedFinalFunctionalGates,
    ) ||
    parsed.cleanRunVerdict !== expectedCleanRunVerdict ||
    parsed.functionalVerdict !== expectedFunctionalVerdict ||
    parsed.verdict !== expectedVerdict ||
    parsed.nextSafeAction !== expectedNextSafeAction
  ) {
    throw new Error(`${label} derived evidence or verdict is inconsistent`);
  }
  return parsed;
};

export const createE2ERunSummary = ({
  runId,
  mode = "unknown",
  now = new Date(),
}: {
  readonly runId: string;
  readonly mode?: E2ERunSummary["mode"];
  readonly now?: Date;
}): E2ERunSummary => {
  const timestamp = now.toISOString();
  return parseE2ERunSummary({
    schemaVersion: E2E_SUMMARY_SCHEMA_VERSION,
    runId,
    mode,
    verdict: "unknown",
    cleanRunVerdict: "unknown",
    functionalVerdict: "unknown",
    nextSafeAction: "investigate_unknown",
    startedAt: timestamp,
    updatedAt: timestamp,
    steps: [],
    stepRetrySummary: [],
    txObservations: [],
    finalFunctionalGates: [],
    cleanRunGates: [],
    transactions: [],
    http: [],
    db: [],
    rawEvidence: [],
    notes: [],
  });
};

const unresolvedTransactionStatuses = new Set(["submitted", "unknown"]);

const reconciledTransactionStatuses = new Set([
  "confirmed",
  "committed",
  "rejected",
]);

const hasReconciledTransactionHash = (
  transactions: readonly TransactionEvidence[],
  txHash: string,
): boolean =>
  transactions.some(
    (tx) =>
      tx.txHash.toLowerCase() === txHash.toLowerCase() &&
      reconciledTransactionStatuses.has(tx.status),
  );

const hasUnresolvedTransaction = (
  transactions: readonly TransactionEvidence[],
): boolean =>
  transactions.some(
    (tx) =>
      unresolvedTransactionStatuses.has(tx.status) &&
      !hasReconciledTransactionHash(transactions, tx.txHash),
  );

const stepHasUnresolvedTxObservation = (
  step: StepSummary,
  transactions: readonly TransactionEvidence[],
): boolean =>
  stepTxObservations(step).some(
    (observation) =>
      unresolvedTransactionStatuses.has(observation.role) &&
      !hasReconciledTransactionHash(transactions, observation.txHash),
  );

export const operatorVerdict = ({
  cleanRunVerdict,
  functionalVerdict,
}: {
  readonly cleanRunVerdict: RunVerdict;
  readonly functionalVerdict: RunVerdict;
}): RunVerdict =>
  functionalVerdict === "unknown" ? cleanRunVerdict : functionalVerdict;

export const hasUnresolvedTransactionRisk = ({
  steps,
  transactions,
  cleanRunGates,
}: Pick<E2ERunSummary, "steps" | "transactions" | "cleanRunGates">): boolean =>
  cleanRunGates.some((gate) => gate.status === "blocked") ||
  hasUnresolvedTransaction(transactions) ||
  steps.some(
    (step) =>
      (step.status === "timeout" || step.status === "signaled") &&
      stepHasUnresolvedTxObservation(step, transactions),
  );

export const classifyNextSafeAction = (
  summary: Pick<
    E2ERunSummary,
    | "verdict"
    | "functionalVerdict"
    | "steps"
    | "transactions"
    | "http"
    | "db"
    | "cleanRunGates"
    | "notes"
  >,
): NextSafeAction => {
  if (hasUnresolvedTransactionRisk(summary)) {
    return "reconcile_submitted_tx_before_rerun";
  }
  if (
    summary.verdict === "success" ||
    summary.functionalVerdict === "success"
  ) {
    return "none_run_complete";
  }
  if (
    summary.http.some(
      (entry) =>
        entry.label === "merge" && entry.semanticStatus !== "satisfied",
    ) ||
    summary.notes.some((note) => note.includes("state_queue_lease"))
  ) {
    return "inspect_state_queue_lease";
  }
  if (
    summary.db.some(
      (entry) =>
        entry.label === "deposit_projection" && entry.status === "pending",
    )
  ) {
    return "wait_until_deposit_projection_due";
  }
  if (summary.steps.some((step) => step.status === "failed")) {
    return "fix_pre_submit_and_rerun_step";
  }
  return "investigate_unknown";
};
