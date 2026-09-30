import {
  writeJsonFileAtomic,
  writeTextFileAtomic,
} from "midgard-node/files/atomic-write";

import {
  buildFinalFunctionalGates,
  buildStepRetrySummary,
  mergeTransactionEvidence,
  recomputeCleanRunVerdict,
  recomputeFunctionalVerdict,
  transactionEvidenceFromStepSummaries,
} from "./summary.build-final-functional-gates.js";
import {
  classifyNextSafeAction,
  operatorVerdict,
  parseE2ERunSummary,
} from "./summary.parse-e2-erun-summary.js";
import {
  type E2ERunSummary,
  stepTxObservations,
} from "./summary.parse-step-retry-summary.js";

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
