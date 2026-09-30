import { mkdir } from "node:fs/promises";
import { join } from "node:path";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { defaultMidgardNodeEndpoint } from "midgard-node/commands/command-utils";
import type { Database } from "midgard-node/services/database";

import {
  createE2ERunSummary,
  updateE2ERunSummary,
  writeSummaryJsonAtomic,
  writeSummaryMarkdownAtomic,
} from "../e2e/summary.js";
import {
  collectorStep,
  countValue,
  fetchJson,
  type FinalizeSummaryOptions,
  type FinalizeSummaryResult,
  hasEmptyHeaders,
  isReady,
  loadStateCorrectionAcceptance,
  loadStepSummaries,
  loadStressSummary,
  timestampForPath,
} from "./e2e-finalize-summary.collector-step.js";
import {
  collectDbCounts,
  requiredFreshEvidence,
  requiredFreshStepAttemptQualityCounts,
} from "./e2e-finalize-summary.required-fresh-evidence.js";
import { stressEvidenceFromSummary } from "./e2e-finalize-summary.stress-evidence-from-summary.js";
import { stateCorrectionAcceptanceEvidence } from "./e2e-state-correction-acceptance.js";
import {
  createLocalKupmiosStateCorrectionAuthority,
  loadLocalAuthorityDeployment,
} from "./e2e-state-correction-local-authority.js";
import { reconcileStateCorrectionIndependentEvidence } from "./e2e-state-correction-reconciliation.js";

export const finalizeE2ESummaryProgram = (
  options: FinalizeSummaryOptions,
): Effect.Effect<FinalizeSummaryResult, never, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
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
    const stressSummaryPath = options.stressSummaryPath;
    const stateCorrectionEvidencePath = options.stateCorrectionEvidencePath;

    const [
      readyz,
      stateQueue,
      counts,
      stepSummaries,
      stressSummary,
      stateCorrectionAcceptance,
    ] = yield* Effect.all(
      [
        Effect.promise(() => fetchJson(`${nodeUrl}/readyz`)),
        Effect.promise(() => fetchJson(`${nodeUrl}/stateQueue`, adminHeaders)),
        collectDbCounts(),
        Effect.promise(() => loadStepSummaries(options.stepSummaryPaths ?? [])),
        stressSummaryPath === undefined
          ? Effect.succeed(undefined)
          : Effect.promise(() => loadStressSummary(stressSummaryPath)),
        stateCorrectionEvidencePath === undefined
          ? Effect.succeed(undefined)
          : Effect.promise(() =>
              loadStateCorrectionAcceptance(stateCorrectionEvidencePath),
            ),
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
      (counts.get("local_mutation_jobs_unfinished") ?? 0n);
    const finalized = counts.get("pending_finalizations_finalized") ?? 0n;
    const txBearingHeaders =
      counts.get("pending_finalization_tx_headers") ?? 0n;
    const finalizedTxBearingHeaders =
      counts.get("pending_finalizations_finalized_tx_headers") ?? 0n;
    const consumedDeposits = counts.get("deposits_consumed") ?? 0n;
    const acceptedL2Txs = counts.get("tx_admissions_accepted") ?? 0n;
    const immutableRows = counts.get("immutable") ?? 0n;
    const confirmedLedgerRows = counts.get("confirmed_ledger") ?? 0n;
    const daPayloads = counts.get("da_payloads") ?? 0n;
    const stressEvidence = stressEvidenceFromSummary({
      ...(stressSummary === undefined ? {} : { stressSummary }),
      ...(stressSummaryPath === undefined ? {} : { stressSummaryPath }),
    });
    const stateCorrectionIndependentSourcePaths =
      options.stateCorrectionIndependentSourcePaths;
    const stateCorrectionIndependentAuthority =
      options.stateCorrectionIndependentAuthority;
    const stateCorrectionLocalAuthorityConfig =
      options.stateCorrectionLocalAuthorityConfig;
    const stateCorrectionDeployment =
      stateCorrectionIndependentSourcePaths === undefined
        ? undefined
        : yield* Effect.promise(() =>
            loadLocalAuthorityDeployment(
              stateCorrectionIndependentSourcePaths.deploymentManifestPath,
            ),
          );
    const effectiveStateCorrectionAuthority =
      stateCorrectionIndependentAuthority ??
      (stateCorrectionIndependentSourcePaths === undefined ||
      stateCorrectionLocalAuthorityConfig === undefined ||
      stateCorrectionDeployment === undefined
        ? undefined
        : yield* Effect.promise(async () => {
            return createLocalKupmiosStateCorrectionAuthority({
              ...stateCorrectionLocalAuthorityConfig,
              ...stateCorrectionDeployment,
              observeDatabase: async () => {
                const [row] = await Effect.runPromise(sql<{
                  readonly unfinished_mutation_jobs: number | bigint | string;
                  readonly pending_finalizations: number | bigint | string;
                }>`
                  SELECT
                    (SELECT COUNT(*) FROM local_mutation_jobs
                      WHERE status <> 'completed') AS unfinished_mutation_jobs,
                    (SELECT COUNT(*) FROM pending_block_finalizations
                      WHERE status <> 'finalized') AS pending_finalizations
                `);
                if (row === undefined) {
                  throw new Error(
                    "live Q57 database observation returned no row",
                  );
                }
                return {
                  unfinishedMutationJobs: Number(
                    countValue(row.unfinished_mutation_jobs),
                  ),
                  pendingFinalizations: Number(
                    countValue(row.pending_finalizations),
                  ),
                };
              },
            });
          }));
    const stateCorrectionIndependentEvidence =
      stateCorrectionAcceptance === undefined ||
      stateCorrectionIndependentSourcePaths === undefined ||
      effectiveStateCorrectionAuthority === undefined
        ? undefined
        : yield* Effect.promise(() =>
            reconcileStateCorrectionIndependentEvidence({
              expectedRunId: runId,
              claim: stateCorrectionAcceptance,
              paths: stateCorrectionIndependentSourcePaths,
              authority: effectiveStateCorrectionAuthority,
            }),
          );
    const stateCorrectionEvidence =
      stateCorrectionIndependentEvidence ??
      stateCorrectionAcceptanceEvidence({
        expectedRunId: runId,
        ...(stateCorrectionAcceptance === undefined
          ? {}
          : { evidence: stateCorrectionAcceptance }),
        ...(stateCorrectionEvidencePath === undefined
          ? {}
          : { evidencePath: stateCorrectionEvidencePath }),
      });
    const transactions = [
      ...(options.transactions ?? []),
      ...stressEvidence.transactions,
      ...stateCorrectionEvidence.transactions,
    ];
    const expectedL2Count =
      stressSummary === undefined
        ? 2n
        : 2n + BigInt(stressEvidence.acceptedStressCount);
    const allSteps = [
      ...stepSummaries,
      collectorStep({
        startedAt,
        finishedAt,
        rawLogPath,
      }),
    ];
    const requiredFresh = requiredFreshEvidence({
      mode: options.mode ?? "unknown",
      steps: allSteps,
      transactions: options.transactions ?? [],
    });

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
          ...requiredFresh.db,
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
            status:
              finalizedTxBearingHeaders >= txBearingHeaders
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: {
              finalized: finalized.toString(),
              finalizedTxBearing: finalizedTxBearingHeaders.toString(),
              expectedTxBearingMinimum: txBearingHeaders.toString(),
            },
          },
          {
            label: "accepted_l2_txs",
            status:
              (stressSummary === undefined
                ? acceptedL2Txs === expectedL2Count
                : acceptedL2Txs >= expectedL2Count) &&
              immutableRows >= expectedL2Count
                ? "satisfied"
                : "failed",
            source: "postgres",
            details:
              stressSummary === undefined
                ? {
                    accepted: acceptedL2Txs.toString(),
                    expectedAccepted: expectedL2Count.toString(),
                    immutable: immutableRows.toString(),
                    expectedImmutableMinimum: expectedL2Count.toString(),
                  }
                : {
                    accepted: acceptedL2Txs.toString(),
                    expectedAcceptedMinimum: expectedL2Count.toString(),
                    immutable: immutableRows.toString(),
                    expectedImmutableMinimum: expectedL2Count.toString(),
                  },
          },
          {
            label: "confirmed_ledger",
            status:
              confirmedLedgerRows > 0n &&
              daPayloads >= finalized &&
              consumedDeposits === 1n
                ? "satisfied"
                : "failed",
            source: "postgres",
            details: {
              confirmedLedger: confirmedLedgerRows.toString(),
              daPayloads: daPayloads.toString(),
              expectedDaPayloadsMinimum: finalized.toString(),
              consumedDeposits: consumedDeposits.toString(),
            },
          },
          ...stressEvidence.db,
          ...stateCorrectionEvidence.db,
        ],
        cleanRunGates: [
          ...requiredFresh.cleanRunGates,
          ...stressEvidence.cleanRunGates,
        ],
        rawEvidence: [
          ...(options.nodeLogPath === undefined
            ? []
            : [{ label: "node-log", path: options.nodeLogPath }]),
          ...stressEvidence.rawEvidence,
          ...stateCorrectionEvidence.rawEvidence,
        ],
        notes: [
          "Generated by e2e-finalize-summary from live endpoints and database counts.",
          ...stressEvidence.notes,
          ...stateCorrectionEvidence.notes,
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
      functionalVerdict: summary.functionalVerdict,
      cleanRunVerdict: summary.cleanRunVerdict,
      nextSafeAction: summary.nextSafeAction,
      requiredFreshStepAttemptQuality: requiredFreshStepAttemptQualityCounts(
        summary.cleanRunGates,
      ),
    };
  });
