import { isDeepStrictEqual } from "node:util";

import {
  arrayOf,
  exactRecord,
  integer,
  isoTimestamp,
  nonEmptyString,
  nonNegativeInteger,
  oneOf,
  openRecord,
  positiveInteger,
  stringArray,
  stringValue,
} from "midgard-node/artifact-schema";
import {
  writeJsonFileAtomic,
  writeTextFileAtomic,
} from "midgard-node/files/atomic-write";

import {
  parseE2EStep,
  parseTxObservation,
  type StepStatus,
  type StepSummary,
  type TxObservation,
} from "./runner.js";

export const E2E_SUMMARY_SCHEMA_VERSION = "midgard-e2e-summary-v1";

export type RunVerdict =
  | "success"
  | "failed"
  | "blocked"
  | "interrupted"
  | "unknown";

export type NextSafeAction =
  | "none_run_complete"
  | "fix_pre_submit_and_rerun_step"
  | "reconcile_submitted_tx_before_rerun"
  | "wait_until_deposit_projection_due"
  | "inspect_state_queue_lease"
  | "investigate_unknown";

export type TransactionEvidence = {
  readonly label: string;
  readonly txHash: string;
  readonly status:
    | "submitted"
    | "confirmed"
    | "queued"
    | "accepted"
    | "committed"
    | "rejected"
    | "unknown";
  readonly source: string;
};

export type StepRetrySummary = {
  readonly stepId: string;
  readonly attempts: number;
  readonly failedAttempts: number;
  readonly latestStatus: StepStatus;
  readonly latestError: string | null;
  readonly firstSuccessAt: string | null;
  readonly lastFinishedAt: string;
};

export type FinalFunctionalGate = {
  readonly label: string;
  readonly status: "satisfied" | "pending" | "blocked" | "failed";
  readonly source: string;
  readonly details: Readonly<Record<string, string>>;
};

export type CleanRunGate = {
  readonly label: string;
  readonly status:
    | "satisfied"
    | "failed"
    | "blocked"
    | "interrupted"
    | "unknown";
  readonly source: string;
  readonly details: Readonly<Record<string, string>>;
};

export type HttpEvidence = {
  readonly label: string;
  readonly method: string;
  readonly url: string;
  readonly statusCode: number;
  readonly semanticStatus: "satisfied" | "pending" | "blocked" | "failed";
  readonly source: string;
};

export type DbEvidence = {
  readonly label: string;
  readonly status: "satisfied" | "pending" | "blocked" | "failed";
  readonly source: string;
  readonly details: Readonly<Record<string, string>>;
};

export type RawEvidenceRef = {
  readonly label: string;
  readonly path: string;
};

export type E2ERunSummary = {
  readonly schemaVersion: typeof E2E_SUMMARY_SCHEMA_VERSION;
  readonly runId: string;
  readonly mode: "attach" | "resume" | "fresh" | "unknown";
  readonly verdict: RunVerdict;
  readonly cleanRunVerdict: RunVerdict;
  readonly functionalVerdict: RunVerdict;
  readonly nextSafeAction: NextSafeAction;
  readonly startedAt: string;
  readonly updatedAt: string;
  readonly steps: readonly StepSummary[];
  readonly stepRetrySummary: readonly StepRetrySummary[];
  readonly txObservations: readonly TxObservation[];
  readonly finalFunctionalGates: readonly FinalFunctionalGate[];
  readonly cleanRunGates: readonly CleanRunGate[];
  readonly transactions: readonly TransactionEvidence[];
  readonly http: readonly HttpEvidence[];
  readonly db: readonly DbEvidence[];
  readonly rawEvidence: readonly RawEvidenceRef[];
  readonly notes: readonly string[];
};

const parseLowerHex64 = (value: unknown, label: string): string => {
  const parsed = nonEmptyString(value, label);
  if (!/^[0-9a-f]{64}$/u.test(parsed)) {
    throw new Error(`${label} must be 64 lowercase hexadecimal characters`);
  }
  return parsed;
};

const parseStringDetails = (
  value: unknown,
  label: string,
): Readonly<Record<string, string>> => {
  const input = openRecord(value, label);
  // Detail key names are intentionally open; only their value domain is fixed.
  return Object.fromEntries(
    Object.entries(input).map(([key, entry]) => [
      key,
      stringValue(entry, `${label}.${key}`),
    ]),
  );
};

const parseStepRetrySummary = (
  value: unknown,
  label: string,
): StepRetrySummary => {
  const input = exactRecord(value, label, [
    "stepId",
    "attempts",
    "failedAttempts",
    "latestStatus",
    "latestError",
    "firstSuccessAt",
    "lastFinishedAt",
  ]);
  const parsed: StepRetrySummary = {
    stepId: nonEmptyString(input.stepId, `${label}.stepId`),
    attempts: positiveInteger(input.attempts, `${label}.attempts`),
    failedAttempts: nonNegativeInteger(
      input.failedAttempts,
      `${label}.failedAttempts`,
    ),
    latestStatus: oneOf(input.latestStatus, `${label}.latestStatus`, [
      "success",
      "failed",
      "signaled",
      "timeout",
      "runner_error",
    ]),
    latestError:
      input.latestError === null
        ? null
        : nonEmptyString(input.latestError, `${label}.latestError`),
    firstSuccessAt:
      input.firstSuccessAt === null
        ? null
        : isoTimestamp(input.firstSuccessAt, `${label}.firstSuccessAt`),
    lastFinishedAt: isoTimestamp(
      input.lastFinishedAt,
      `${label}.lastFinishedAt`,
    ),
  };
  return parsed;
};

const parseFinalFunctionalGate = (
  value: unknown,
  label: string,
): FinalFunctionalGate => {
  const input = exactRecord(value, label, [
    "label",
    "status",
    "source",
    "details",
  ]);
  return {
    label: nonEmptyString(input.label, `${label}.label`),
    status: oneOf(input.status, `${label}.status`, [
      "satisfied",
      "pending",
      "blocked",
      "failed",
    ]),
    source: nonEmptyString(input.source, `${label}.source`),
    details: parseStringDetails(input.details, `${label}.details`),
  };
};

const parseCleanRunGate = (value: unknown, label: string): CleanRunGate => {
  const input = exactRecord(value, label, [
    "label",
    "status",
    "source",
    "details",
  ]);
  return {
    label: nonEmptyString(input.label, `${label}.label`),
    status: oneOf(input.status, `${label}.status`, [
      "satisfied",
      "failed",
      "blocked",
      "interrupted",
      "unknown",
    ]),
    source: nonEmptyString(input.source, `${label}.source`),
    details: parseStringDetails(input.details, `${label}.details`),
  };
};

const parseTransactionEvidence = (
  value: unknown,
  label: string,
): TransactionEvidence => {
  const input = exactRecord(value, label, [
    "label",
    "txHash",
    "status",
    "source",
  ]);
  return {
    label: nonEmptyString(input.label, `${label}.label`),
    txHash: parseLowerHex64(input.txHash, `${label}.txHash`),
    status: oneOf(input.status, `${label}.status`, [
      "submitted",
      "confirmed",
      "queued",
      "accepted",
      "committed",
      "rejected",
      "unknown",
    ]),
    source: nonEmptyString(input.source, `${label}.source`),
  };
};

const parseHttpEvidence = (value: unknown, label: string): HttpEvidence => {
  const input = exactRecord(value, label, [
    "label",
    "method",
    "url",
    "statusCode",
    "semanticStatus",
    "source",
  ]);
  return {
    label: nonEmptyString(input.label, `${label}.label`),
    method: nonEmptyString(input.method, `${label}.method`),
    url: nonEmptyString(input.url, `${label}.url`),
    statusCode: integer(input.statusCode, `${label}.statusCode`),
    semanticStatus: oneOf(input.semanticStatus, `${label}.semanticStatus`, [
      "satisfied",
      "pending",
      "blocked",
      "failed",
    ]),
    source: nonEmptyString(input.source, `${label}.source`),
  };
};

const parseDbEvidence = (value: unknown, label: string): DbEvidence => {
  const input = exactRecord(value, label, [
    "label",
    "status",
    "source",
    "details",
  ]);
  return {
    label: nonEmptyString(input.label, `${label}.label`),
    status: oneOf(input.status, `${label}.status`, [
      "satisfied",
      "pending",
      "blocked",
      "failed",
    ]),
    source: nonEmptyString(input.source, `${label}.source`),
    details: parseStringDetails(input.details, `${label}.details`),
  };
};

const parseRawEvidenceRef = (value: unknown, label: string): RawEvidenceRef => {
  const input = exactRecord(value, label, ["label", "path"]);
  return {
    label: nonEmptyString(input.label, `${label}.label`),
    path: nonEmptyString(input.path, `${label}.path`),
  };
};

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

const stepTxObservations = (step: StepSummary): readonly TxObservation[] =>
  (step as StepSummary & { readonly txObservations?: readonly TxObservation[] })
    .txObservations ?? [];

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
  return observation.stepId
    .replace(/^submit-/, "")
    .replace(/^project-/, "")
    .replace(/^init-protocol$/, "init");
};

const evidenceStatuses = new Set(["submitted", "confirmed", "committed"]);
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
  if (steps.some((step) => step.status === "timeout")) {
    return "blocked";
  }
  if (steps.some((step) => step.status === "signaled")) {
    return "interrupted";
  }
  if (
    steps.some(
      (step) => step.status === "failed" || step.status === "runner_error",
    )
  ) {
    return "failed";
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
  if (cleanRunGates.some((gate) => gate.status === "blocked")) {
    return "blocked";
  }
  if (cleanRunGates.some((gate) => gate.status === "interrupted")) {
    return "interrupted";
  }
  if (cleanRunGates.some((gate) => gate.status === "failed")) {
    return "failed";
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
    case "blocked":
      return 5;
    case "interrupted":
      return 4;
    case "failed":
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
  if (gates.some((gate) => gate.status === "blocked")) {
    return "blocked";
  }
  if (gates.some((gate) => gate.status === "failed")) {
    return "failed";
  }
  if (gates.length > 0 && gates.every((gate) => gate.status === "satisfied")) {
    return "success";
  }
  return "unknown";
};

const operatorVerdict = ({
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

export const updateE2ERunSummary = (
  summary: E2ERunSummary,
  patch: Partial<
    Pick<
      E2ERunSummary,
      | "steps"
      | "transactions"
      | "http"
      | "db"
      | "cleanRunGates"
      | "rawEvidence"
      | "notes"
    >
  >,
  now = new Date(),
): E2ERunSummary => {
  const patched = {
    ...summary,
    ...patch,
    updatedAt: now.toISOString(),
  };
  const stepRetrySummary = buildStepRetrySummary(patched.steps);
  const txObservations = patched.steps.flatMap(stepTxObservations);
  const transactions = mergeTransactionEvidence([
    ...patched.transactions,
    ...transactionEvidenceFromStepSummaries(patched.steps),
  ]);
  const finalFunctionalGates = buildFinalFunctionalGates({
    ...patched,
    transactions,
  });
  const cleanRunVerdict = recomputeCleanRunVerdict({
    ...patched,
    transactions,
  });
  const functionalVerdict = recomputeFunctionalVerdict(finalFunctionalGates);
  const verdict = operatorVerdict({ cleanRunVerdict, functionalVerdict });
  const next = {
    ...patched,
    stepRetrySummary,
    txObservations,
    transactions,
    finalFunctionalGates,
    cleanRunGates: patched.cleanRunGates,
    cleanRunVerdict,
    functionalVerdict,
    verdict,
  };
  return parseE2ERunSummary({
    ...next,
    nextSafeAction: classifyNextSafeAction({ ...next, verdict }),
  });
};

export const writeSummaryJsonAtomic = async (
  path: string,
  summary: E2ERunSummary,
): Promise<void> => {
  await writeJsonFileAtomic(path, parseE2ERunSummary(summary));
};

export const renderSummaryMarkdown = (summary: E2ERunSummary): string => {
  const renderDetails = (details: Readonly<Record<string, string>>): string =>
    Object.entries(details)
      .map(([key, value]) => `${key}=${value.replaceAll("|", "\\|")}`)
      .join(",") || "-";
  const lines = [
    "# Midgard E2E Run Summary",
    "",
    `- runId: ${summary.runId}`,
    `- mode: ${summary.mode}`,
    `- verdict: ${summary.verdict}`,
    `- cleanRunVerdict: ${summary.cleanRunVerdict}`,
    `- functionalVerdict: ${summary.functionalVerdict}`,
    `- nextSafeAction: ${summary.nextSafeAction}`,
    "",
    "## Final Functional Gates",
    "",
    "| gate | status | source | details |",
    "| --- | --- | --- | --- |",
    ...summary.finalFunctionalGates.map(
      (gate) =>
        `| ${gate.label} | ${gate.status} | ${gate.source} | ${renderDetails(gate.details)} |`,
    ),
    "",
    "## Clean Run Quality Gates",
    "",
    "| gate | status | source | details |",
    "| --- | --- | --- | --- |",
    ...summary.cleanRunGates.map(
      (gate) =>
        `| ${gate.label} | ${gate.status} | ${gate.source} | ${renderDetails(gate.details)} |`,
    ),
    ...(summary.cleanRunGates.length === 0
      ? ["| - | unknown | - | no clean-run quality gates recorded |"]
      : []),
    "",
    "## Step Retry Summary",
    "",
    "| step | attempts | failedAttempts | latestStatus | firstSuccessAt | latestError |",
    "| --- | ---: | ---: | --- | --- | --- |",
    ...summary.stepRetrySummary.map(
      (step) =>
        `| ${step.stepId} | ${step.attempts.toString()} | ${step.failedAttempts.toString()} | ${step.latestStatus} | ${step.firstSuccessAt ?? "-"} | ${step.latestError ?? "-"} |`,
    ),
    "",
    "## Step Status",
    "",
    "| step | status | durationMs | rawHashes | log |",
    "| --- | --- | ---: | --- | --- |",
    ...summary.steps.map(
      (step) =>
        `| ${step.id} | ${step.status} | ${step.durationMs.toString()} | ${step.observedTxHashes.join(",") || "-"} | ${step.rawLogPath} |`,
    ),
    "",
    "## Transactions",
    "",
    "| label | status | txHash | source |",
    "| --- | --- | --- | --- |",
    ...summary.transactions.map(
      (tx) => `| ${tx.label} | ${tx.status} | ${tx.txHash} | ${tx.source} |`,
    ),
    "",
    "## Transaction Observations",
    "",
    "| step | role | txHash | source | field |",
    "| --- | --- | --- | --- | --- |",
    ...summary.txObservations.map(
      (tx) =>
        `| ${tx.stepId} | ${tx.role} | ${tx.txHash} | ${tx.source} | ${tx.field ?? "-"} |`,
    ),
    "",
    "## Endpoint Evidence",
    "",
    "| label | method | statusCode | semanticStatus | source |",
    "| --- | --- | ---: | --- | --- |",
    ...summary.http.map(
      (entry) =>
        `| ${entry.label} | ${entry.method} | ${entry.statusCode.toString()} | ${entry.semanticStatus} | ${entry.source} |`,
    ),
    "",
    "## Database Evidence",
    "",
    "| label | status | source | details |",
    "| --- | --- | --- | --- |",
    ...summary.db.map(
      (entry) =>
        `| ${entry.label} | ${entry.status} | ${entry.source} | ${renderDetails(entry.details)} |`,
    ),
    "",
    "## Raw Evidence",
    "",
    ...summary.rawEvidence.map((entry) => `- ${entry.label}: ${entry.path}`),
    "",
    "## Notes",
    "",
    ...summary.notes.map((note) => `- ${note}`),
    "",
  ];
  return `${lines.join("\n")}`;
};

export const writeSummaryMarkdownAtomic = async (
  path: string,
  summary: E2ERunSummary,
): Promise<void> => {
  await writeTextFileAtomic(path, renderSummaryMarkdown(summary));
};
