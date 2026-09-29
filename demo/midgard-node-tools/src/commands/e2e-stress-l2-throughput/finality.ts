import { formatJson } from "midgard-node/commands/command-utils";

import { appendEvent } from "./artifact-files.js";
import {
  type FinalityObserverResult,
  nextPollDelayMs,
  type PendingFinalityTransaction,
  readTxStatus,
  txStatusFromBody,
} from "./polling.js";
import {
  abortReason,
  isAbortLikeError,
  signalWasAborted,
  sleepWithAbort,
  StressInterruptedError,
} from "./runtime.js";
import {
  type E2EL2StressConfig,
  type E2EL2StressTransaction,
} from "./types.js";

export const observeFinalityBounded = async ({
  config,
  eventsNdjsonPath,
  fetchImpl,
  sleepImpl,
  signal,
  now,
  transactions,
}: {
  readonly config: E2EL2StressConfig;
  readonly eventsNdjsonPath: string;
  readonly fetchImpl: typeof fetch;
  readonly sleepImpl: (ms: number) => Promise<void>;
  readonly signal?: AbortSignal;
  readonly now: () => Date;
  readonly transactions: readonly PendingFinalityTransaction[];
}): Promise<FinalityObserverResult> => {
  const maxConcurrentRequests = Math.max(
    1,
    config.finalityObserverMaxConcurrentRequests,
  );
  const observerStartedAt = now();
  const observerStartedAtMs = observerStartedAt.getTime();
  const pending = transactions.map((entry) => ({
    ...entry,
    deadlineMs: observerStartedAtMs + config.commitObservationTimeoutMs,
    nextPollAtMs: observerStartedAtMs,
    pollAttempt: 0,
  }));
  const completed: E2EL2StressTransaction[] = [];
  let maxObservedConcurrentRequests = 0;
  let activeRequests = 0;
  let pollRequestCount = 0;
  let batchCount = 0;
  let errorCount = 0;
  let interruptedReason: string | undefined;

  await appendEvent(eventsNdjsonPath, {
    event: "stress.observer.started",
    at: observerStartedAt.toISOString(),
    mode: "post-submit-bounded",
    transactionCount: pending.length,
    maxConcurrentRequests,
  });

  while (pending.length > 0) {
    if (signalWasAborted(signal)) {
      interruptedReason = abortReason(signal);
      break;
    }
    const cycleAt = now();
    const cycleAtMs = cycleAt.getTime();
    for (let index = pending.length - 1; index >= 0; index -= 1) {
      const entry = pending[index]!;
      if (cycleAtMs > entry.deadlineMs) {
        completed.push({
          ...entry.tx,
          finality: {
            status: "timeout",
            error: `Timed out waiting for /tx-status committed after ${config.commitObservationTimeoutMs.toString()}ms.`,
          },
        });
        pending.splice(index, 1);
      }
    }
    if (pending.length === 0) {
      break;
    }
    const due = pending
      .filter((entry) => entry.nextPollAtMs <= cycleAtMs)
      .slice(0, maxConcurrentRequests);
    if (due.length === 0) {
      const nextAtMs = Math.min(
        ...pending.map((entry) =>
          Math.min(entry.nextPollAtMs, entry.deadlineMs),
        ),
      );
      try {
        await sleepWithAbort(
          sleepImpl,
          Math.max(1, nextAtMs - cycleAtMs),
          signal,
        );
      } catch (error) {
        if (isAbortLikeError(error) && signalWasAborted(signal)) {
          interruptedReason = abortReason(signal);
          break;
        }
        throw error;
      }
      continue;
    }

    batchCount += 1;
    const outcomes = await Promise.all(
      due.map(async (entry) => {
        activeRequests += 1;
        maxObservedConcurrentRequests = Math.max(
          maxObservedConcurrentRequests,
          activeRequests,
        );
        try {
          pollRequestCount += 1;
          const probe = await readTxStatus({
            fetchImpl,
            nodeEndpoint: config.nodeEndpoint,
            signal,
            txHash: entry.tx.txHash,
          });
          return {
            entry,
            polledAt: now(),
            statusCode: probe.statusCode,
            status: txStatusFromBody(probe.body),
            body: probe.body,
          };
        } catch (error) {
          if (isAbortLikeError(error) && signalWasAborted(signal)) {
            return {
              entry,
              polledAt: now(),
              error: new StressInterruptedError(abortReason(signal)),
            };
          }
          return { entry, polledAt: now(), error };
        } finally {
          activeRequests -= 1;
        }
      }),
    );

    let committedCount = 0;
    let rejectedCount = 0;
    let pendingCount = 0;
    for (const outcome of outcomes) {
      const pendingIndex = pending.findIndex(
        (entry) => entry.tx.index === outcome.entry.tx.index,
      );
      if (pendingIndex < 0) {
        continue;
      }
      if ("error" in outcome) {
        if (
          outcome.error instanceof StressInterruptedError &&
          signalWasAborted(signal)
        ) {
          interruptedReason = outcome.error.message;
          break;
        }
        errorCount += 1;
        pending[pendingIndex] = {
          ...outcome.entry,
          nextPollAtMs:
            outcome.polledAt.getTime() +
            nextPollDelayMs(outcome.entry.pollAttempt, config),
          pollAttempt: outcome.entry.pollAttempt + 1,
        };
        continue;
      }
      if (outcome.status === "committed") {
        committedCount += 1;
        completed.push({
          ...outcome.entry.tx,
          finality: {
            status: "committed",
            committedAt: outcome.polledAt.toISOString(),
            durationMs: Math.max(
              0,
              outcome.polledAt.getTime() - outcome.entry.submittedAtMs,
            ),
          },
        });
        pending.splice(pendingIndex, 1);
        continue;
      }
      if (outcome.status === "rejected") {
        rejectedCount += 1;
        completed.push({
          ...outcome.entry.tx,
          finality: {
            status: "rejected",
            error: formatJson(outcome.body),
          },
        });
        pending.splice(pendingIndex, 1);
        continue;
      }
      pendingCount += 1;
      pending[pendingIndex] = {
        ...outcome.entry,
        nextPollAtMs:
          outcome.polledAt.getTime() +
          nextPollDelayMs(outcome.entry.pollAttempt, config),
        pollAttempt: outcome.entry.pollAttempt + 1,
      };
    }
    await appendEvent(eventsNdjsonPath, {
      event: "stress.observer.batch_polled",
      at: now().toISOString(),
      batchCount,
      polledCount: outcomes.length,
      committedCount,
      rejectedCount,
      pendingCount,
      remainingCount: pending.length,
      maxConcurrentRequests,
      maxObservedConcurrentRequests,
    });
    if (interruptedReason !== undefined) {
      break;
    }
  }

  const finalTransactions = [...completed, ...pending.map((entry) => entry.tx)];
  await appendEvent(eventsNdjsonPath, {
    event: "stress.observer.finished",
    at: now().toISOString(),
    observedTransactionCount: transactions.length,
    completedCount: completed.length,
    remainingCount: pending.length,
    pollRequestCount,
    batchCount,
    errorCount,
    maxConcurrentRequests,
    maxObservedConcurrentRequests,
    ...(interruptedReason === undefined ? {} : { interruptedReason }),
  });

  return {
    transactions: finalTransactions,
    summary: {
      mode: "post-submit-bounded",
      maxConcurrentRequests,
      maxObservedConcurrentRequests,
      observedTransactionCount: transactions.length,
      pollRequestCount,
      batchCount,
      errorCount,
    },
    ...(interruptedReason === undefined ? {} : { interruptedReason }),
  };
};
