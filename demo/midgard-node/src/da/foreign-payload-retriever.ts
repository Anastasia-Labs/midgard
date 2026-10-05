import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  WatcherPublicDaClient,
  type WatcherPublicDaPayload,
} from "midgard-watcher/public-da-client";

import { verifyForeignPayload } from "../workers/t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";
import { FOREIGN_DA_RETRY_POLICY } from "./foreign-da-retry-policy.js";

/** Canonical count relationships are also enforced by the retained-payload table. */
export const verifyDownloadedForeignPayload = (
  headerHash: string,
  header: SDK.Header,
  payload: SDK.DaPayload,
) =>
  verifyForeignPayload({ foreignHeaderHash: headerHash, header, payload }).pipe(
    Effect.map(
      (matches) =>
        matches &&
        header.totalEventCount ===
          header.depositCount +
            header.forcedTransactionCount +
            header.withdrawalCount +
            header.l2TransactionCount &&
        header.transitionStepCount === header.totalEventCount &&
        header.validationTraceCount ===
          header.forcedTransactionCount + header.l2TransactionCount,
    ),
  );

type RetryState = { failures: number; nextAttemptAt: number };

/** Retry state controls network work only. Durable safety evidence is never cleared. */
export class ForeignPayloadRetriever {
  private readonly retries = new Map<string, RetryState>();
  constructor(
    private readonly client: WatcherPublicDaClient,
    private readonly now = () => performance.now(),
  ) {}

  async fetch(
    headerHash: string,
    header: SDK.Header,
  ): Promise<WatcherPublicDaPayload | undefined> {
    const now = this.now();
    for (const [hash, state] of this.retries) {
      if (
        (state.failures >= FOREIGN_DA_RETRY_POLICY.attemptsPerEpisode &&
          now >= state.nextAttemptAt) ||
        now >= state.nextAttemptAt + FOREIGN_DA_RETRY_POLICY.cooldownMs
      )
        this.retries.delete(hash);
    }
    const state = this.retries.get(headerHash);
    if (state !== undefined && now < state.nextAttemptAt) return undefined;
    if (
      state === undefined &&
      this.retries.size >= FOREIGN_DA_RETRY_POLICY.maxHeaders
    )
      return undefined;
    try {
      const result = await this.client.fetchPayloadByHeader({
        headerHash,
        validateInnerPayload: async (bytes, signal) => {
          const payload = SDK.decodeDaPayload(bytes);
          const matches = await Effect.runPromise(
            verifyDownloadedForeignPayload(headerHash, header, payload),
            { signal },
          );
          if (!matches)
            throw new Error(
              "Foreign payload does not reconstruct the retained header roots and counts",
            );
        },
      });
      this.retries.delete(headerHash);
      return result;
    } catch (error) {
      const failures = (state?.failures ?? 0) + 1;
      this.retries.set(headerHash, {
        failures,
        nextAttemptAt:
          this.now() +
          (failures >= FOREIGN_DA_RETRY_POLICY.attemptsPerEpisode
            ? FOREIGN_DA_RETRY_POLICY.cooldownMs
            : Math.min(
                FOREIGN_DA_RETRY_POLICY.backoffMs * 2 ** (failures - 1),
                FOREIGN_DA_RETRY_POLICY.backoffMaxMs,
              )),
      });
      throw error;
    }
  }
}
