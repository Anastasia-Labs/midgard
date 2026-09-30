import {
  exactRecord,
  integer,
  isoTimestamp,
  nonEmptyString,
  nonNegativeInteger,
  oneOf,
  openRecord,
  positiveInteger,
  stringValue,
} from "midgard-node/artifact-schema";

import {
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

export const parseStepRetrySummary = (
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

export const parseFinalFunctionalGate = (
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

export const parseCleanRunGate = (
  value: unknown,
  label: string,
): CleanRunGate => {
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

export const parseTransactionEvidence = (
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

export const parseHttpEvidence = (
  value: unknown,
  label: string,
): HttpEvidence => {
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

export const parseDbEvidence = (value: unknown, label: string): DbEvidence => {
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

export const parseRawEvidenceRef = (
  value: unknown,
  label: string,
): RawEvidenceRef => {
  const input = exactRecord(value, label, ["label", "path"]);
  return {
    label: nonEmptyString(input.label, `${label}.label`),
    path: nonEmptyString(input.path, `${label}.path`),
  };
};

export const stepTxObservations = (
  step: StepSummary,
): readonly TxObservation[] =>
  (step as StepSummary & { readonly txObservations?: readonly TxObservation[] })
    .txObservations ?? [];
