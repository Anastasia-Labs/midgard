import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  fetchNodeUtxosByAddress,
  formatJson,
} from "midgard-node/commands/command-utils";
import {
  parseSubmitL2TransferConfig,
  type SubmitL2TransferResult,
} from "midgard-node/commands/submit-l2-transfer";
import { percentileOfUnsorted as percentile } from "midgard-node/percentile";
import { sleep } from "midgard-node/sleep";

import {
  buildStressMetrics,
  type StressStageMetricDbSources,
} from "../stress-stage-metrics.js";
import { appendEvent } from "./artifact-files.js";
import { artifactConfig } from "./config.js";
import { E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION } from "./constants.js";
import { observeFinalityBounded } from "./finality.js";
import { runOpenLoopUpperBoundStress } from "./open-loop.js";
import { measurementPolicyForConfig } from "./policy.js";
import {
  type FinalityObserverResult,
  type PendingFinalityTransaction,
  pollUntilAccepted,
} from "./polling.js";
import { renderStressSummaryMarkdown } from "./report.js";
import {
  abortReason,
  errorMessage,
  isAbortLikeError,
  signalWasAborted,
  throwIfAborted,
} from "./runtime.js";
import { parseE2EL2StressSummary } from "./summary-artifact.js";
import {
  type E2EL2StressConfig,
  type E2EL2StressRunResult,
  type E2EL2StressRuntime,
  type E2EL2StressTransaction,
} from "./types.js";
import { spendableUtxosForLovelace, walletForWorker } from "./wallets.js";

export const runE2EL2StressThroughput = async (
  config: E2EL2StressConfig,
  runtime: E2EL2StressRuntime,
): Promise<E2EL2StressRunResult> => {
  if (config.loadModel === "open-loop-upper-bound") {
    return await runOpenLoopUpperBoundStress(config, runtime);
  }
  const fetchUtxos = runtime.fetchUtxos ?? fetchNodeUtxosByAddress;
  const fetchImpl = runtime.fetch ?? fetch;
  const sleepImpl = runtime.sleep ?? sleep;
  const now = runtime.now ?? (() => new Date());
  const signal = runtime.abortSignal;

  await mkdir(config.outDir, { recursive: true });
  const configJsonPath = join(config.outDir, "config.json");
  const eventsNdjsonPath = join(config.outDir, "events.ndjson");
  const summaryJsonPath = join(config.outDir, "summary.json");
  const summaryMarkdownPath = join(config.outDir, "summary.md");
  await writeFile(configJsonPath, `${formatJson(artifactConfig(config))}\n`, {
    encoding: "utf8",
    flag: "w",
  });

  const startedAtDate = now();
  const startedAt = startedAtDate.toISOString();
  await appendEvent(eventsNdjsonPath, {
    event: "stress_started",
    at: startedAt,
    runId: config.runId,
    configJsonPath,
  });

  if (config.mode === "parallel-fanout") {
    const requiredLovelace = config.lovelace + config.feeHeadroomLovelace;
    await Promise.all(
      config.stressWallets.slice(0, config.concurrency).map(async (wallet) => {
        const utxos = await fetchUtxos(config.nodeEndpoint, wallet.address);
        await appendEvent(eventsNdjsonPath, {
          event: "stress_wallet_preflight",
          at: now().toISOString(),
          address: wallet.address,
          seedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
          utxoCount: utxos.length,
          spendableUtxoCount: spendableUtxosForLovelace(utxos, requiredLovelace)
            .length,
          requiredLovelace: requiredLovelace.toString(10),
          transferLovelace: config.lovelace.toString(10),
          feeHeadroomLovelace: config.feeHeadroomLovelace.toString(10),
        });
        if (spendableUtxosForLovelace(utxos, requiredLovelace).length === 0) {
          throw new Error(
            `Stress wallet ${wallet.resolvedWalletSeedPhrase.resolvedFrom} has no spendable L2 UTxO with at least ${requiredLovelace.toString(10)} lovelace (transfer ${config.lovelace.toString(10)} + fee headroom ${config.feeHeadroomLovelace.toString(10)}) at ${wallet.address}. Fund independent stress wallets before rerunning parallel stress.`,
          );
        }
      }),
    );
  }

  const terminalTransactions: E2EL2StressTransaction[] = [];
  const pendingFinalityTransactions: PendingFinalityTransaction[] = [];
  const submitLatencies: number[] = [];
  const acceptanceLatencies: number[] = [];
  const commitLatencies: number[] = [];
  let interruptedReason: string | undefined;
  let submissionFailureCount = 0;

  const executeTransfer = async (
    index: number,
    workerIndex: number,
  ): Promise<void> => {
    if (signalWasAborted(signal)) {
      interruptedReason = interruptedReason ?? abortReason(signal);
      return;
    }
    const wallet = walletForWorker(config, workerIndex);
    const destinationAddress = config.destinationAddress ?? wallet.address;
    const submitStartedAt = now();
    await appendEvent(eventsNdjsonPath, {
      event: "transfer_submit_started",
      at: submitStartedAt.toISOString(),
      index,
      workerIndex,
      senderAddress: wallet.address,
      destinationAddress,
      walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
    });

    let submitResult: SubmitL2TransferResult | undefined;
    let submittedAtDate: Date | undefined;
    let submittedAt: string | undefined;
    let submitDurationMs: number | undefined;
    try {
      const transferConfig = parseSubmitL2TransferConfig({
        l2Address: destinationAddress,
        lovelace: config.lovelace.toString(10),
        assetSpecs: [],
        nodeEndpoint: config.nodeEndpoint,
        submitRequestTimeoutMs: config.submitRequestTimeoutMs,
      });
      submitResult = await runtime.submitTransfer({
        index,
        phase: "stress",
        config: transferConfig,
        resolvedWalletSeedPhrase: wallet.resolvedWalletSeedPhrase,
        walletAddress: wallet.address,
        destinationAddress,
        walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
      });
      throwIfAborted(signal);
      submittedAtDate = now();
      submittedAt = submittedAtDate.toISOString();
      submitDurationMs = Math.max(
        0,
        submittedAtDate.getTime() - submitStartedAt.getTime(),
      );
      submitLatencies.push(submitDurationMs);
      await appendEvent(eventsNdjsonPath, {
        event: "transfer_submitted",
        at: submittedAt,
        index,
        workerIndex,
        txHash: submitResult.txId,
        submitStatus: submitResult.status,
        selectedInputs: submitResult.selectedInputs,
      });

      const accepted = await pollUntilAccepted({
        config,
        eventsNdjsonPath,
        fetchImpl,
        sleepImpl,
        signal,
        now,
        txHash: submitResult.txId,
        submittedAtMs: submittedAtDate.getTime(),
      });
      if (accepted.acceptance.durationMs !== undefined) {
        acceptanceLatencies.push(accepted.acceptance.durationMs);
      }
      const acceptedTx: E2EL2StressTransaction = {
        index,
        phase: "stress",
        txHash: submitResult.txId,
        senderAddress: submitResult.senderAddress,
        destinationAddress: submitResult.destinationAddress,
        selectedInputs: submitResult.selectedInputs,
        submission: {
          status: "submitted",
          submittedAt,
          durationMs: submitDurationMs,
        },
        acceptance: accepted.acceptance,
        finality: accepted.finality,
        workerIndex,
        walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
      };

      if (accepted.acceptance.status === "accepted") {
        await appendEvent(eventsNdjsonPath, {
          event: "transfer_accepted",
          at: accepted.acceptance.acceptedAt ?? now().toISOString(),
          index,
          workerIndex,
          txHash: submitResult.txId,
          acceptanceStatus: accepted.acceptance.status,
          finalityStatus: accepted.finality.status,
        });
      }

      if (accepted.finality.durationMs !== undefined) {
        commitLatencies.push(accepted.finality.durationMs);
      }

      if (
        accepted.acceptance.status === "accepted" &&
        accepted.finality.status === "not_observed"
      ) {
        pendingFinalityTransactions.push({
          tx: {
            ...acceptedTx,
            txHash: submitResult.txId,
            submission: {
              ...acceptedTx.submission,
              submittedAt,
            },
          },
          submittedAtMs: submittedAtDate.getTime(),
        });
        return;
      }

      terminalTransactions.push(acceptedTx);
      await appendEvent(eventsNdjsonPath, {
        event: "transfer_finished",
        at: now().toISOString(),
        index,
        workerIndex,
        txHash: submitResult.txId,
        acceptanceStatus: acceptedTx.acceptance.status,
        finalityStatus: acceptedTx.finality.status,
      });
    } catch (error) {
      if (isAbortLikeError(error) || signalWasAborted(signal)) {
        interruptedReason =
          interruptedReason ??
          (signalWasAborted(signal)
            ? abortReason(signal)
            : errorMessage(error));
        if (
          submitResult !== undefined &&
          submittedAtDate !== undefined &&
          submittedAt !== undefined &&
          submitDurationMs !== undefined
        ) {
          terminalTransactions.push({
            index,
            phase: "stress",
            txHash: submitResult.txId,
            senderAddress: submitResult.senderAddress,
            destinationAddress: submitResult.destinationAddress,
            selectedInputs: submitResult.selectedInputs,
            submission: {
              status: "submitted",
              submittedAt,
              durationMs: submitDurationMs,
            },
            acceptance: {
              status: "not_observed",
              error: interruptedReason,
            },
            finality: {
              status: "not_observed",
              error: interruptedReason,
            },
            workerIndex,
            walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
          });
        }
        await appendEvent(eventsNdjsonPath, {
          event: "stress.interrupt.received",
          at: now().toISOString(),
          index,
          workerIndex,
          reason: interruptedReason,
        });
        return;
      }
      const failedAt = now().toISOString();
      const message = errorMessage(error);
      const tx: E2EL2StressTransaction = {
        index,
        phase: "stress",
        txHash: null,
        senderAddress: wallet.address,
        destinationAddress,
        selectedInputs: [],
        submission: {
          status: "failed",
          submittedAt: null,
          error: message,
        },
        acceptance: {
          status: "not_submitted",
          error: message,
        },
        finality: {
          status: "not_observed",
        },
        workerIndex,
        walletSeedSource: wallet.resolvedWalletSeedPhrase.resolvedFrom,
      };
      terminalTransactions.push(tx);
      await appendEvent(eventsNdjsonPath, {
        event: "transfer_failed",
        at: failedAt,
        index,
        workerIndex,
        error: message,
      });
      submissionFailureCount += 1;
      if (
        interruptedReason === undefined &&
        submissionFailureCount > config.maxSubmissionFailures
      ) {
        interruptedReason =
          `submission/build failure threshold exceeded: ${submissionFailureCount.toString()} failure(s) > ` +
          `allowed ${config.maxSubmissionFailures.toString()} (--max-submission-failures); aborting to avoid ` +
          `silently shrinking the sample. Last error: ${message}`;
        await appendEvent(eventsNdjsonPath, {
          event: "stress.abort.max_submission_failures_exceeded",
          at: now().toISOString(),
          submissionFailureCount,
          maxSubmissionFailures: config.maxSubmissionFailures,
          reason: interruptedReason,
        });
      }
    }
  };

  let nextIndex = 0;
  const workers = Array.from(
    { length: config.concurrency },
    async (_unused, workerIndex) => {
      while (true) {
        if (signalWasAborted(signal) || interruptedReason !== undefined) {
          interruptedReason =
            interruptedReason ??
            (signalWasAborted(signal) ? abortReason(signal) : "interrupted");
          return;
        }
        const index = nextIndex;
        nextIndex += 1;
        if (index >= config.count) {
          return;
        }
        await executeTransfer(index, workerIndex);
      }
    },
  );
  await Promise.all(workers);

  const submissionFinishedAtDate = now();
  const submissionFinishedAt = submissionFinishedAtDate.toISOString();
  await appendEvent(eventsNdjsonPath, {
    event: "stress_submission_finished",
    at: submissionFinishedAt,
    submittedCount:
      terminalTransactions.filter((tx) => tx.txHash !== null).length +
      pendingFinalityTransactions.length,
    acceptedCount:
      terminalTransactions.filter((tx) => tx.acceptance.status === "accepted")
        .length + pendingFinalityTransactions.length,
  });

  const finalityObserverResult: FinalityObserverResult =
    pendingFinalityTransactions.length === 0 || interruptedReason !== undefined
      ? {
          transactions: pendingFinalityTransactions.map((entry) => entry.tx),
          summary: {
            mode: "post-submit-bounded" as const,
            maxConcurrentRequests: Math.max(
              1,
              config.finalityObserverMaxConcurrentRequests,
            ),
            maxObservedConcurrentRequests: 0,
            observedTransactionCount: 0,
            pollRequestCount: 0,
            batchCount: 0,
            errorCount: 0,
          },
        }
      : await observeFinalityBounded({
          config,
          eventsNdjsonPath,
          fetchImpl,
          sleepImpl,
          signal,
          now,
          transactions: pendingFinalityTransactions,
        });
  interruptedReason =
    interruptedReason ?? finalityObserverResult.interruptedReason;
  for (const tx of finalityObserverResult.transactions) {
    if (tx.finality.durationMs !== undefined) {
      commitLatencies.push(tx.finality.durationMs);
    }
    await appendEvent(eventsNdjsonPath, {
      event: "transfer_finished",
      at: now().toISOString(),
      index: tx.index,
      workerIndex: tx.workerIndex,
      txHash: tx.txHash,
      acceptanceStatus: tx.acceptance.status,
      finalityStatus: tx.finality.status,
    });
  }
  const finishedAtDate = now();
  const finishedAt = finishedAtDate.toISOString();
  const sortedTransactions = [
    ...terminalTransactions,
    ...finalityObserverResult.transactions,
  ].sort((left, right) => left.index - right.index);
  const notStartedCount = Math.max(0, config.count - sortedTransactions.length);
  const submittedCount = sortedTransactions.filter(
    (tx) => tx.txHash !== null,
  ).length;
  const acceptedCount = sortedTransactions.filter(
    (tx) => tx.acceptance.status === "accepted",
  ).length;
  const submissionFailedCount = sortedTransactions.filter(
    (tx) => tx.submission.status === "failed",
  ).length;
  const acceptanceNotObservedCount = sortedTransactions.filter(
    (tx) => tx.acceptance.status === "not_observed",
  ).length;
  const acceptanceTimedOutCount = sortedTransactions.filter(
    (tx) => tx.acceptance.status === "timeout",
  ).length;
  const finalityTimedOutCount = sortedTransactions.filter(
    (tx) => tx.finality.status === "timeout",
  ).length;
  const observedCommittedCount = sortedTransactions.filter(
    (tx) => tx.finality.status === "committed",
  ).length;
  const unknownFinalityCount = sortedTransactions.filter(
    (tx) =>
      tx.acceptance.status === "accepted" &&
      tx.finality.status === "not_observed",
  ).length;
  const rejectedCount = sortedTransactions.filter(
    (tx) =>
      tx.acceptance.status === "rejected" || tx.finality.status === "rejected",
  ).length;
  const durationMs = Math.max(
    0,
    finishedAtDate.getTime() - startedAtDate.getTime(),
  );
  const submissionDurationMs = Math.max(
    0,
    submissionFinishedAtDate.getTime() - startedAtDate.getTime(),
  );
  const stressTxHashes = sortedTransactions.flatMap((tx) =>
    tx.txHash === null ? [] : [tx.txHash],
  );
  let dbMetricSources: StressStageMetricDbSources | undefined;
  if (runtime.collectStageMetricSources !== undefined) {
    try {
      dbMetricSources = await runtime.collectStageMetricSources({
        txHashes: stressTxHashes,
      });
      await appendEvent(eventsNdjsonPath, {
        event: "stress.stage_metrics.db_sources_collected",
        at: now().toISOString(),
        txHashCount: stressTxHashes.length,
        l2AdmissionRows: dbMetricSources.l2Admissions.length,
        l1CommitRows: dbMetricSources.l1Commits.length,
        immutableRows: dbMetricSources.immutableObservations.length,
        residueRows: dbMetricSources.residue.length,
      });
    } catch (error) {
      await appendEvent(eventsNdjsonPath, {
        event: "stress.stage_metrics.db_sources_failed",
        at: now().toISOString(),
        error: errorMessage(error),
      });
    }
  }
  const metrics = buildStressMetrics({
    requestedCount: config.count,
    submittedCount,
    acceptedCount,
    observedCommittedCount,
    startedAt,
    submissionFinishedAt,
    finishedAt,
    transactions: sortedTransactions,
    ...(dbMetricSources === undefined ? {} : { dbSources: dbMetricSources }),
    ...(runtime.fullFinalityDrainProof === undefined
      ? {}
      : { fullFinalityDrainProof: runtime.fullFinalityDrainProof }),
  });
  const groundTruth =
    runtime.collectGroundTruthMetrics === undefined
      ? undefined
      : await runtime.collectGroundTruthMetrics({
          windowStart: startedAt,
          windowEnd: finishedAt,
          txHashSample: stressTxHashes.slice(0, 1_000),
          offeredCount: config.count,
          calibrationProofRef: null,
        });
  const fingerprint =
    groundTruth?.fingerprint ??
    (runtime.collectEnvironmentFingerprint === undefined
      ? undefined
      : await runtime.collectEnvironmentFingerprint({
          calibrationProofRef: null,
        }));
  const summary = parseE2EL2StressSummary({
    schemaVersion: E2E_L2_STRESS_SUMMARY_SCHEMA_VERSION,
    runId: config.runId,
    status: interruptedReason === undefined ? "completed" : "interrupted",
    ...(interruptedReason === undefined ? {} : { interruptedReason }),
    loadModel: config.loadModel,
    workloadProfile: config.workloadProfile,
    classification: "closed_loop_smoke",
    rateSemantics: "burst_cycle_rate",
    burstCycleRatePerSecond: metrics.l2Admission.perSecond,
    mode: config.mode,
    measurementPolicy: measurementPolicyForConfig(config),
    requestedCount: config.count,
    notStartedCount,
    submittedCount,
    submissionFailedCount,
    acceptedCount,
    acceptanceNotObservedCount,
    acceptanceTimedOutCount,
    finalityTimedOutCount,
    observedCommittedCount,
    unknownFinalityCount,
    rejectedCount,
    concurrency: config.concurrency,
    finalityObserver: finalityObserverResult.summary,
    startedAt,
    submissionFinishedAt,
    finishedAt,
    submissionDurationMs,
    durationMs,
    metrics,
    ...(groundTruth === undefined ? {} : { groundTruth }),
    ...(fingerprint === undefined ? {} : { fingerprint }),
    latencyMs: {
      submitP50: percentile(submitLatencies, 0.5),
      submitP95: percentile(submitLatencies, 0.95),
      acceptanceP50: percentile(acceptanceLatencies, 0.5),
      acceptanceP95: percentile(acceptanceLatencies, 0.95),
      commitP50: percentile(commitLatencies, 0.5),
      commitP95: percentile(commitLatencies, 0.95),
    },
    artifactPaths: {
      configJson: configJsonPath,
      eventsNdjson: eventsNdjsonPath,
      summaryJson: summaryJsonPath,
      summaryMarkdown: summaryMarkdownPath,
    },
    transactions: sortedTransactions,
  });

  await writeFile(summaryJsonPath, `${formatJson(summary)}\n`, "utf8");
  await writeFile(summaryMarkdownPath, renderStressSummaryMarkdown(summary), {
    encoding: "utf8",
  });
  await appendEvent(eventsNdjsonPath, {
    event: "stress_finished",
    at: finishedAt,
    summaryJsonPath,
    summaryMarkdownPath,
    submittedCount,
    acceptedCount,
    observedCommittedCount,
    rejectedCount,
    submissionFailedCount,
    acceptanceTimedOutCount,
    finalityTimedOutCount,
    unknownFinalityCount,
    notStartedCount,
    status: summary.status,
  });

  return {
    summary,
    configJsonPath,
    eventsNdjsonPath,
    summaryJsonPath,
    summaryMarkdownPath,
  };
};
