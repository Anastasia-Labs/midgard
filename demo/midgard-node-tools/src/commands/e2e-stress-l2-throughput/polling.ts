import { formatJson } from "midgard-node/commands/command-utils";

import { appendEvent } from "./artifact-files.js";
import { DEFAULT_POLL_BACKOFF_MULTIPLIER } from "./constants.js";
import { errorMessage, sleepWithAbort, throwIfAborted } from "./runtime.js";
import {
  type E2EL2StressAcceptanceState,
  type E2EL2StressConfig,
  type E2EL2StressFinalityObserverSummary,
  type E2EL2StressFinalityState,
  type E2EL2StressSubmissionState,
  type E2EL2StressTransaction,
} from "./types.js";

export const nextPollDelayMs = (
  attempt: number,
  config: Pick<
    E2EL2StressConfig,
    "pollIntervalMs" | "pollInitialIntervalMs" | "pollMaxIntervalMs"
  >,
): number => {
  if (config.pollIntervalMs !== undefined) {
    return config.pollIntervalMs;
  }
  const scaled =
    config.pollInitialIntervalMs * DEFAULT_POLL_BACKOFF_MULTIPLIER ** attempt;
  return Math.min(config.pollMaxIntervalMs, scaled);
};

export const readTxStatus = async ({
  fetchImpl,
  nodeEndpoint,
  signal,
  txHash,
}: {
  readonly fetchImpl: typeof fetch;
  readonly nodeEndpoint: string;
  readonly signal?: AbortSignal;
  readonly txHash: string;
}): Promise<{
  readonly statusCode: number;
  readonly body: unknown;
}> => {
  throwIfAborted(signal);
  const response = await fetchImpl(
    `${nodeEndpoint}/tx-status?tx_hash=${encodeURIComponent(txHash)}`,
    signal === undefined ? undefined : { signal },
  );
  const responseText = await response.text();
  let body: unknown = responseText;
  try {
    body = JSON.parse(responseText) as unknown;
  } catch {
    // Keep the raw response text for event evidence.
  }
  return {
    statusCode: response.status,
    body,
  };
};

export const txStatusFromBody = (body: unknown): string | null => {
  if (typeof body !== "object" || body === null) {
    return null;
  }
  const status = (body as { readonly status?: unknown }).status;
  return typeof status === "string" ? status : null;
};

const isAcceptedOrLaterTxStatus = (status: string | null): boolean =>
  status === "accepted" ||
  status === "pending_commit" ||
  status === "awaiting_local_recovery" ||
  status === "committed";

export const pollUntilAccepted = async ({
  config,
  eventsNdjsonPath,
  fetchImpl,
  sleepImpl,
  signal,
  now,
  txHash,
  submittedAtMs,
}: {
  readonly config: E2EL2StressConfig;
  readonly eventsNdjsonPath: string;
  readonly fetchImpl: typeof fetch;
  readonly sleepImpl: (ms: number) => Promise<void>;
  readonly signal?: AbortSignal;
  readonly now: () => Date;
  readonly txHash: string;
  readonly submittedAtMs: number;
}): Promise<{
  readonly acceptance: E2EL2StressAcceptanceState;
  readonly finality: E2EL2StressFinalityState;
}> => {
  const deadlineMs = submittedAtMs + config.acceptanceTimeoutMs;
  let attempt = 0;
  while (true) {
    throwIfAborted(signal);
    const polledAt = now();
    try {
      const probe = await readTxStatus({
        fetchImpl,
        nodeEndpoint: config.nodeEndpoint,
        signal,
        txHash,
      });
      const observedStatus = txStatusFromBody(probe.body);
      await appendEvent(eventsNdjsonPath, {
        event: "tx_status",
        at: polledAt.toISOString(),
        txHash,
        statusCode: probe.statusCode,
        status: observedStatus,
      });
      if (isAcceptedOrLaterTxStatus(observedStatus)) {
        const elapsedMs = Math.max(0, polledAt.getTime() - submittedAtMs);
        return {
          acceptance: {
            status: "accepted",
            acceptedAt: polledAt.toISOString(),
            durationMs: elapsedMs,
          },
          ...(observedStatus === "committed"
            ? {
                finality: {
                  status: "committed",
                  committedAt: polledAt.toISOString(),
                  durationMs: elapsedMs,
                },
              }
            : { finality: { status: "not_observed" } }),
        };
      }
      if (observedStatus === "rejected") {
        const error = formatJson(probe.body);
        return {
          acceptance: {
            status: "rejected",
            error,
          },
          finality: {
            status: "rejected",
            error,
          },
        };
      }
    } catch (error) {
      await appendEvent(eventsNdjsonPath, {
        event: "tx_status_error",
        at: polledAt.toISOString(),
        txHash,
        error: errorMessage(error),
      });
    }

    if (now().getTime() >= deadlineMs) {
      return {
        acceptance: {
          status: "timeout",
          error: `Timed out waiting for /tx-status accepted after ${config.acceptanceTimeoutMs.toString()}ms.`,
        },
        finality: {
          status: "not_observed",
        },
      };
    }
    await sleepWithAbort(sleepImpl, nextPollDelayMs(attempt, config), signal);
    attempt += 1;
  }
};

export type PendingFinalityTransaction = {
  readonly tx: E2EL2StressTransaction & {
    readonly txHash: string;
    readonly submission: E2EL2StressSubmissionState & {
      readonly submittedAt: string;
    };
  };
  readonly submittedAtMs: number;
};

export type FinalityObserverResult = {
  readonly transactions: readonly E2EL2StressTransaction[];
  readonly summary: E2EL2StressFinalityObserverSummary;
  readonly interruptedReason?: string;
};
