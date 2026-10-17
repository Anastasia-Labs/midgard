import { readFile } from "node:fs/promises";

import { E2E_STEP_SCHEMA_VERSION, type StepSummary } from "../e2e/runner.js";
import { type RunVerdict, type TransactionEvidence } from "../e2e/summary.js";
import type { StackRunInputs } from "./e2e-finalize-summary.stack-inputs.js";
import {
  type E2EStateCorrectionAcceptance,
  parseE2EStateCorrectionAcceptance,
} from "./e2e-state-correction-acceptance.js";
import {
  type StateCorrectionIndependentAuthority,
  type StateCorrectionIndependentSourcePaths,
} from "./e2e-state-correction-reconciliation.js";
import {
  type E2EL2StressSummary,
  type E2EL2StressTransaction,
  parseE2EL2StressSummary,
} from "./e2e-stress-l2-throughput/index.js";
import type { StressMetricWindow } from "./stress-stage-metrics.js";

export type FinalizeSummaryOptions = {
  /** The `e2e-stack` run whose records and database the summary reads. */
  readonly stackRun: Pick<StackRunInputs, "expectation" | "endpoint">;
  readonly outDir?: string;
  readonly mode?: "attach" | "resume" | "fresh" | "unknown";
  readonly adminApiKey?: string;
  readonly nodeLogPath?: string;
  readonly stressSummaryPath?: string;
  readonly stateCorrectionEvidencePath?: string;
  readonly stateCorrectionIndependentSourcePaths?: StateCorrectionIndependentSourcePaths;
  readonly stateCorrectionIndependentAuthority?: StateCorrectionIndependentAuthority;
  readonly stateCorrectionLocalAuthorityConfig?: {
    readonly providerFailover: string | undefined;
    readonly kupoUrl: string;
    readonly ogmiosUrl: string;
  };
};

export type FinalizeSummaryResult = {
  readonly summaryJsonPath: string;
  readonly summaryMarkdownPath: string;
  readonly verdict: string;
  readonly functionalVerdict: RunVerdict;
  readonly cleanRunVerdict: RunVerdict;
  readonly nextSafeAction: string;
};

type HttpProbe = {
  readonly statusCode: number;
  readonly body: unknown;
};

export type CountRow = {
  readonly label: string;
  readonly count: number | bigint | string;
};

export const timestampForPath = (date = new Date()): string =>
  date
    .toISOString()
    .replaceAll(/[-:]/g, "")
    .replace(/\.\d{3}Z$/, "Z");

export const countValue = (
  value: number | bigint | string | undefined,
): bigint => {
  if (value === undefined) {
    return 0n;
  }
  return typeof value === "bigint" ? value : BigInt(value);
};

export const fetchJson = async (
  url: string,
  headers: Readonly<Record<string, string>> = {},
): Promise<HttpProbe> => {
  const response = await fetch(url, { headers });
  let body: unknown = null;
  try {
    body = await response.json();
  } catch {
    body = await response.text();
  }
  return {
    statusCode: response.status,
    body,
  };
};

export const hasEmptyHeaders = (body: unknown): boolean =>
  typeof body === "object" &&
  body !== null &&
  Array.isArray((body as { readonly headers?: unknown }).headers) &&
  (body as { readonly headers: readonly unknown[] }).headers.length === 0;

export const isReady = (body: unknown): boolean =>
  typeof body === "object" &&
  body !== null &&
  (body as { readonly ready?: unknown }).ready === true;

export const collectorStep = ({
  startedAt,
  finishedAt,
  rawLogPath,
}: {
  readonly startedAt: string;
  readonly finishedAt: string;
  readonly rawLogPath: string;
}): StepSummary => ({
  schemaVersion: E2E_STEP_SCHEMA_VERSION,
  id: "collect-final-evidence",
  status: "success",
  command: {
    command: "midgard-node e2e-finalize-summary",
    args: [],
    cwd: process.cwd(),
    envKeys: [],
    envFiles: [],
    envInheritance: "process",
  },
  pid: process.pid,
  startedAt,
  finishedAt,
  durationMs: Math.max(0, Date.parse(finishedAt) - Date.parse(startedAt)),
  exitCode: 0,
  signal: null,
  timedOut: false,
  rawLogPath,
  observedTxHashes: [],
  hashObservations: [],
  txObservations: [],
  parsedJson: null,
  error: null,
});

export const loadStressSummary = async (
  path: string,
): Promise<E2EL2StressSummary> =>
  parseE2EL2StressSummary(JSON.parse(await readFile(path, "utf8")) as unknown);

export const loadStateCorrectionAcceptance = async (
  path: string,
): Promise<E2EStateCorrectionAcceptance> =>
  parseE2EStateCorrectionAcceptance(
    JSON.parse(await readFile(path, "utf8")) as unknown,
  );

export const stressTxPhaseToEvidenceStatus = (
  tx: E2EL2StressTransaction,
): TransactionEvidence["status"] => {
  if (
    tx.acceptance.status === "rejected" ||
    tx.finality.status === "rejected"
  ) {
    return "rejected";
  }
  if (tx.finality.status === "committed") {
    return "committed";
  }
  if (tx.acceptance.status === "accepted") {
    return "accepted";
  }
  if (tx.txHash !== null && tx.submission.status === "submitted") {
    return "submitted";
  }
  return "unknown";
};

export const stressMetricDetails = (
  metric: StressMetricWindow,
  prefix: string,
): Readonly<Record<string, string>> => ({
  [`${prefix}Status`]: metric.status,
  [`${prefix}Count`]: metric.count.toString(),
  [`${prefix}MissingCount`]: metric.missingCount.toString(),
  [`${prefix}StartedAt`]: metric.startedAt ?? "",
  [`${prefix}FinishedAt`]: metric.finishedAt ?? "",
  [`${prefix}DurationMs`]: metric.durationMs?.toString() ?? "",
  [`${prefix}PerSecond`]: metric.perSecond?.toString() ?? "",
  [`${prefix}Source`]: metric.source,
  [`${prefix}Precision`]: metric.precision,
  [`${prefix}Notes`]: metric.notes.join(","),
});

export const fullFinalityDrainRequested = (
  metric: StressMetricWindow,
): boolean => !metric.notes.includes("full_finality_drain_not_requested");
