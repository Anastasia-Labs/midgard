import { mkdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { defaultMidgardNodeEndpoint } from "@/commands/command-utils.js";
import { E2E_STEP_SCHEMA_VERSION, type StepSummary } from "@/e2e/runner.js";
import {
  createE2ERunSummary,
  type DbEvidence,
  type TransactionEvidence,
  updateE2ERunSummary,
  writeSummaryJsonAtomic,
  writeSummaryMarkdownAtomic,
} from "@/e2e/summary.js";
import type { Database } from "@/services/database.js";

export type FinalizeSummaryOptions = {
  readonly outDir?: string;
  readonly runId?: string;
  readonly mode?: "attach" | "resume" | "fresh" | "unknown";
  readonly nodeUrl?: string;
  readonly adminApiKey?: string;
  readonly nodeLogPath?: string;
  readonly stepSummaryPaths?: readonly string[];
  readonly transactions?: readonly TransactionEvidence[];
};

export type FinalizeSummaryResult = {
  readonly summaryJsonPath: string;
  readonly summaryMarkdownPath: string;
  readonly verdict: string;
  readonly nextSafeAction: string;
};

type HttpProbe = {
  readonly statusCode: number;
  readonly body: unknown;
};

type CountRow = {
  readonly label: string;
  readonly count: number | bigint | string;
};

const timestampForPath = (date = new Date()): string =>
  date
    .toISOString()
    .replaceAll(/[-:]/g, "")
    .replace(/\.\d{3}Z$/, "Z");

const countValue = (value: number | bigint | string | undefined): bigint => {
  if (value === undefined) {
    return 0n;
  }
  return typeof value === "bigint" ? value : BigInt(value);
};

const fetchJson = async (
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

const hasEmptyHeaders = (body: unknown): boolean =>
  typeof body === "object" &&
  body !== null &&
  Array.isArray((body as { readonly headers?: unknown }).headers) &&
  (body as { readonly headers: readonly unknown[] }).headers.length === 0;

const isReady = (body: unknown): boolean =>
  typeof body === "object" &&
  body !== null &&
  (body as { readonly ready?: unknown }).ready === true;

const collectorStep = ({
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
  parsedJson: null,
  error: null,
});

const loadStepSummaries = async (
  paths: readonly string[],
): Promise<readonly StepSummary[]> =>
  Promise.all(
    paths.map(
      async (path) => JSON.parse(await readFile(path, "utf8")) as StepSummary,
    ),
  );

export const REQUIRED_FRESH_E2E_STEP_IDS = [
  "hub-oracle-nonce",
  "reference-scripts",
  "init-protocol",
  "operator-lifecycle",
  "midgard-node-ready",
  "submit-deposit",
  "project-deposits",
  "submit-l2-transfer-a",
  "submit-l2-transfer-b",
  "merge-tail",
] as const;

export const REQUIRED_FRESH_TRANSACTION_LABELS = [
  "hub-oracle-nonce",
  "init",
  "operator-registration",
  "operator-activation",
  "deposit",
  "l2-transfer-a",
  "l2-transfer-b",
  "header-commit-a",
  "header-commit-b",
] as const;

export const requiredFreshEvidence = ({
  mode,
  steps,
  transactions,
}: {
  readonly mode: FinalizeSummaryOptions["mode"];
  readonly steps: readonly StepSummary[];
  readonly transactions: readonly TransactionEvidence[];
}): readonly DbEvidence[] => {
  if (mode !== "fresh") {
    return [];
  }
  const stepIds = new Set(steps.map((step) => step.id));
  const txLabels = new Set(transactions.map((tx) => tx.label));
  const missingStepIds = REQUIRED_FRESH_E2E_STEP_IDS.filter(
    (stepId) => !stepIds.has(stepId),
  );
  const missingTransactionLabels = REQUIRED_FRESH_TRANSACTION_LABELS.filter(
    (label) => !txLabels.has(label),
  );
  return [
    {
      label: "required_fresh_steps",
      status: missingStepIds.length === 0 ? "satisfied" : "failed",
      source: "e2e-run-step",
      details: {
        required: REQUIRED_FRESH_E2E_STEP_IDS.join(","),
        missing: missingStepIds.join(","),
      },
    },
    {
      label: "required_transaction_evidence",
      status: missingTransactionLabels.length === 0 ? "satisfied" : "failed",
      source: "e2e-finalize-summary",
      details: {
        required: REQUIRED_FRESH_TRANSACTION_LABELS.join(","),
        missing: missingTransactionLabels.join(","),
      },
    },
  ];
};

const collectDbCounts = (): Effect.Effect<
  ReadonlyMap<string, bigint>,
  never,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<CountRow>`
      SELECT 'deposits_consumed' AS label, COUNT(*) AS count
        FROM deposits_utxos WHERE status = 'consumed'
      UNION ALL
      SELECT 'tx_admissions_accepted' AS label, COUNT(*) AS count
        FROM tx_admissions WHERE status = 'accepted'
      UNION ALL
      SELECT 'pending_finalizations_finalized' AS label, COUNT(*) AS count
        FROM pending_block_finalizations WHERE status = 'finalized'
      UNION ALL
      SELECT 'pending_finalizations_unfinished' AS label, COUNT(*) AS count
        FROM pending_block_finalizations WHERE status <> 'finalized'
      UNION ALL
      SELECT 'mempool' AS label, COUNT(*) AS count FROM mempool
      UNION ALL
      SELECT 'processed_mempool' AS label, COUNT(*) AS count FROM processed_mempool
      UNION ALL
      SELECT 'blocks' AS label, COUNT(*) AS count FROM blocks
      UNION ALL
      SELECT 'latest_ledger' AS label, COUNT(*) AS count FROM latest_ledger
      UNION ALL
      SELECT 'immutable' AS label, COUNT(*) AS count FROM immutable
      UNION ALL
      SELECT 'confirmed_ledger' AS label, COUNT(*) AS count FROM confirmed_ledger
      UNION ALL
      SELECT 'local_mutation_jobs_unfinished' AS label, COUNT(*) AS count
        FROM local_mutation_jobs WHERE status <> 'completed'
      UNION ALL
      SELECT 'da_payloads' AS label, COUNT(*) AS count FROM da_payloads
    `;
    return new Map(
      rows.map((row) => [row.label, countValue(row.count)] as const),
    );
  }).pipe(Effect.orDie);

export const finalizeE2ESummaryProgram = (
  options: FinalizeSummaryOptions,
): Effect.Effect<FinalizeSummaryResult, never, Database> =>
  Effect.gen(function* () {
    const startedAt = new Date().toISOString();
    const runId = options.runId ?? `e2e-run-${timestampForPath()}`;
    const outDir = options.outDir ?? join("logs", runId);
    const rawLogPath = join(outDir, "collector.log");
    yield* Effect.promise(() => mkdir(outDir, { recursive: true }));

    const nodeUrl = (options.nodeUrl ?? defaultMidgardNodeEndpoint()).replace(
      /\/+$/,
      "",
    );
    const adminHeaders: Readonly<Record<string, string>> =
      options.adminApiKey === undefined || options.adminApiKey.length === 0
        ? {}
        : { "x-midgard-admin-key": options.adminApiKey };

    const [readyz, stateQueue, counts, stepSummaries] = yield* Effect.all(
      [
        Effect.promise(() => fetchJson(`${nodeUrl}/readyz`)),
        Effect.promise(() => fetchJson(`${nodeUrl}/stateQueue`, adminHeaders)),
        collectDbCounts(),
        Effect.promise(() => loadStepSummaries(options.stepSummaryPaths ?? [])),
      ],
      { concurrency: "unbounded" },
    );
    const finishedAt = new Date().toISOString();

    const pendingUnfinished =
      counts.get("pending_finalizations_unfinished") ?? 0n;
    const volatileResidue =
      (counts.get("mempool") ?? 0n) +
      (counts.get("processed_mempool") ?? 0n) +
      (counts.get("blocks") ?? 0n) +
      (counts.get("latest_ledger") ?? 0n) +
      (counts.get("local_mutation_jobs_unfinished") ?? 0n);
    const finalized = counts.get("pending_finalizations_finalized") ?? 0n;
    const consumedDeposits = counts.get("deposits_consumed") ?? 0n;
    const acceptedL2Txs = counts.get("tx_admissions_accepted") ?? 0n;
    const immutableRows = counts.get("immutable") ?? 0n;
    const confirmedLedgerRows = counts.get("confirmed_ledger") ?? 0n;
    const daPayloads = counts.get("da_payloads") ?? 0n;
    const transactions = options.transactions ?? [];
    const allSteps = [
      ...stepSummaries,
      collectorStep({
        startedAt,
        finishedAt,
        rawLogPath,
      }),
    ];

    const base = createE2ERunSummary({
      runId,
      mode: options.mode ?? "unknown",
      now: new Date(startedAt),
    });
    const summary = updateE2ERunSummary(
      base,
      {
        steps: allSteps,
        transactions,
        http: [
          {
            label: "readyz",
            method: "GET",
            url: `${nodeUrl}/readyz`,
            statusCode: readyz.statusCode,
            semanticStatus:
              readyz.statusCode === 200 && isReady(readyz.body)
                ? "satisfied"
                : "failed",
            source: "e2e-finalize-summary",
          },
          {
            label: "stateQueue",
            method: "GET",
            url: `${nodeUrl}/stateQueue`,
            statusCode: stateQueue.statusCode,
            semanticStatus:
              stateQueue.statusCode === 200 && hasEmptyHeaders(stateQueue.body)
                ? "satisfied"
                : "failed",
            source: "e2e-finalize-summary",
          },
        ],
        db: [
          ...requiredFreshEvidence({
            mode: options.mode ?? "unknown",
            steps: allSteps,
            transactions,
          }),
          {
            label: "finalization_residue",
            status:
              pendingUnfinished === 0n && volatileResidue === 0n
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: Object.fromEntries(
              [
                "pending_finalizations_unfinished",
                "mempool",
                "processed_mempool",
                "blocks",
                "latest_ledger",
                "local_mutation_jobs_unfinished",
              ].map((label) => [label, (counts.get(label) ?? 0n).toString()]),
            ),
          },
          {
            label: "deposits_consumed",
            status: consumedDeposits === 1n ? "satisfied" : "failed",
            source: "postgres",
            details: {
              consumed: consumedDeposits.toString(),
              expected: "1",
            },
          },
          {
            label: "finalized_headers",
            status: finalized >= 2n ? "satisfied" : "failed",
            source: "postgres",
            details: {
              finalized: finalized.toString(),
              expectedMinimum: "2",
            },
          },
          {
            label: "accepted_l2_txs",
            status:
              acceptedL2Txs === 2n && immutableRows >= 2n
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: {
              accepted: acceptedL2Txs.toString(),
              expectedAccepted: "2",
              immutable: immutableRows.toString(),
              expectedImmutableMinimum: "2",
            },
          },
          {
            label: "confirmed_ledger",
            status:
              confirmedLedgerRows > 0n &&
              daPayloads >= 2n &&
              consumedDeposits === 1n
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: {
              confirmedLedger: confirmedLedgerRows.toString(),
              daPayloads: daPayloads.toString(),
              expectedDaPayloadsMinimum: "2",
              consumedDeposits: consumedDeposits.toString(),
            },
          },
        ],
        rawEvidence:
          options.nodeLogPath === undefined
            ? []
            : [{ label: "node-log", path: options.nodeLogPath }],
        notes: [
          "Generated by e2e-finalize-summary from live endpoints and database counts.",
        ],
      },
      new Date(finishedAt),
    );

    const summaryJsonPath = join(outDir, "summary.json");
    const summaryMarkdownPath = join(outDir, "summary.md");
    yield* Effect.promise(() =>
      writeSummaryJsonAtomic(summaryJsonPath, summary),
    );
    yield* Effect.promise(() =>
      writeSummaryMarkdownAtomic(summaryMarkdownPath, summary),
    );
    return {
      summaryJsonPath,
      summaryMarkdownPath,
      verdict: summary.verdict,
      nextSafeAction: summary.nextSafeAction,
    };
  });
