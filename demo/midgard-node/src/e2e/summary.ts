import { mkdir, rename, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import type { StepSummary } from "@/e2e/runner.js";

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
  readonly nextSafeAction: NextSafeAction;
  readonly startedAt: string;
  readonly updatedAt: string;
  readonly steps: readonly StepSummary[];
  readonly transactions: readonly TransactionEvidence[];
  readonly http: readonly HttpEvidence[];
  readonly db: readonly DbEvidence[];
  readonly rawEvidence: readonly RawEvidenceRef[];
  readonly notes: readonly string[];
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
  return {
    schemaVersion: E2E_SUMMARY_SCHEMA_VERSION,
    runId,
    mode,
    verdict: "unknown",
    nextSafeAction: "investigate_unknown",
    startedAt: timestamp,
    updatedAt: timestamp,
    steps: [],
    transactions: [],
    http: [],
    db: [],
    rawEvidence: [],
    notes: [],
  };
};

export const classifyNextSafeAction = (
  summary: Pick<
    E2ERunSummary,
    "verdict" | "steps" | "transactions" | "http" | "db" | "notes"
  >,
): NextSafeAction => {
  if (summary.verdict === "success") {
    return "none_run_complete";
  }
  if (
    summary.steps.some(
      (step) => step.status === "timeout" && step.observedTxHashes.length > 0,
    ) ||
    summary.transactions.some(
      (tx) => tx.status === "submitted" || tx.status === "unknown",
    )
  ) {
    return "reconcile_submitted_tx_before_rerun";
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

export const recomputeVerdict = (
  summary: Pick<E2ERunSummary, "steps" | "http" | "db" | "transactions">,
): RunVerdict => {
  if (summary.steps.some((step) => step.status === "timeout")) {
    return "blocked";
  }
  if (summary.steps.some((step) => step.status === "signaled")) {
    return "interrupted";
  }
  if (summary.steps.some((step) => step.status === "failed")) {
    return "failed";
  }
  if (
    summary.http.some((entry) => entry.semanticStatus === "blocked") ||
    summary.db.some((entry) => entry.status === "blocked") ||
    summary.transactions.some((tx) => tx.status === "rejected")
  ) {
    return "blocked";
  }
  if (
    summary.http.some((entry) => entry.semanticStatus === "failed") ||
    summary.db.some((entry) => entry.status === "failed")
  ) {
    return "failed";
  }
  if (
    summary.steps.length > 0 &&
    summary.steps.every((step) => step.status === "success") &&
    summary.http.every((entry) => entry.semanticStatus === "satisfied") &&
    summary.db.every((entry) => entry.status === "satisfied") &&
    summary.transactions.every(
      (tx) => tx.status === "confirmed" || tx.status === "committed",
    )
  ) {
    return "success";
  }
  return "unknown";
};

export const updateE2ERunSummary = (
  summary: E2ERunSummary,
  patch: Partial<
    Pick<
      E2ERunSummary,
      "steps" | "transactions" | "http" | "db" | "rawEvidence" | "notes"
    >
  >,
  now = new Date(),
): E2ERunSummary => {
  const next = {
    ...summary,
    ...patch,
    updatedAt: now.toISOString(),
  };
  const verdict = recomputeVerdict(next);
  return {
    ...next,
    verdict,
    nextSafeAction: classifyNextSafeAction({ ...next, verdict }),
  };
};

export const writeSummaryJsonAtomic = async (
  path: string,
  summary: E2ERunSummary,
): Promise<void> => {
  await mkdir(dirname(path), { recursive: true });
  const tmpPath = `${path}.tmp-${process.pid.toString()}-${Date.now().toString()}`;
  await writeFile(tmpPath, `${JSON.stringify(summary, null, 2)}\n`, "utf8");
  await rename(tmpPath, path);
};

export const renderSummaryMarkdown = (summary: E2ERunSummary): string => {
  const lines = [
    "# Midgard E2E Run Summary",
    "",
    `- runId: ${summary.runId}`,
    `- mode: ${summary.mode}`,
    `- verdict: ${summary.verdict}`,
    `- nextSafeAction: ${summary.nextSafeAction}`,
    "",
    "## Step Status",
    "",
    "| step | status | durationMs | txHashes | log |",
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
    "| label | status | source |",
    "| --- | --- | --- |",
    ...summary.db.map(
      (entry) => `| ${entry.label} | ${entry.status} | ${entry.source} |`,
    ),
    "",
    "## Raw Evidence",
    "",
    ...summary.rawEvidence.map((entry) => `- ${entry.label}: ${entry.path}`),
    "",
  ];
  return `${lines.join("\n")}`;
};

export const writeSummaryMarkdownAtomic = async (
  path: string,
  summary: E2ERunSummary,
): Promise<void> => {
  await mkdir(dirname(path), { recursive: true });
  const tmpPath = `${path}.tmp-${process.pid.toString()}-${Date.now().toString()}`;
  await writeFile(tmpPath, renderSummaryMarkdown(summary), "utf8");
  await rename(tmpPath, path);
};
