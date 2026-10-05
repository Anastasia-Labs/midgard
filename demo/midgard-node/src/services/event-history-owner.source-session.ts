import { setTimeout as delay } from "node:timers/promises";

import {
  type EventHistorySourceBinding,
  readBoundEventHistoryNetworkTip,
} from "../l1-event-history-source.js";
import type { HistoryTransportOptions } from "../l1-event-history-transport.js";
import { isRecoverableHistorySourceFailure } from "./event-history-owner.source-failure.js";
import type { makeHistorySourceOutage } from "./event-history-owner.source-outage.js";

type HistorySourceOutage = ReturnType<typeof makeHistorySourceOutage>;
type Warn = (message: string, annotations: Record<string, unknown>) => void;

/** The first-start scan and locating an old activation can outlive one
 * lease. Renew only after a fresh, source-authenticated response on this
 * bound source (genesis check plus network tip), never from Kupo navigation
 * or a cached receipt. Sequential: one read at a time, never a ledger scan.
 * The follower's heartbeats replace this startup loop: its first tip aborts
 * the signal, which ends this loop silently. */
export const monitorHistoryStartupHealth = async (input: {
  readonly binding: EventHistorySourceBinding;
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly heartbeatIntervalMs: number;
  readonly signal: AbortSignal;
  readonly renew: () => Promise<void>;
  readonly onFailure: (cause: unknown) => void;
}) => {
  try {
    while (true) {
      await delay(input.heartbeatIntervalMs, undefined, {
        signal: input.signal,
      });
      await readBoundEventHistoryNetworkTip({
        binding: input.binding,
        ogmiosUrl: input.transport.ogmiosUrl,
        timeoutMs: input.transport.timeoutMs,
        webSocketFactory: input.transport.webSocketFactory,
        signal: input.signal,
      });
      input.signal.throwIfAborted();
      await input.renew();
    }
  } catch (cause) {
    if (!input.signal.aborted) input.onFailure(cause);
  }
};

/** While the source is silent nothing source-authenticated renews the lease.
 * Renewing with the gate closed keeps this owner the only one that may append
 * and admits nothing: every new session re-validates the lease first. A
 * transport-class renewal failure is retried on the next interval; any other
 * stops the owner. */
export const makeHistoryLeaseKeeper = (input: {
  readonly intervalMs: number;
  readonly signal: AbortSignal;
  readonly outage: HistorySourceOutage;
  readonly stopped: () => boolean;
  readonly renew: () => Promise<void>;
  readonly fail: (cause: unknown) => void;
}) => {
  let keeper: Promise<void> | undefined;
  return {
    keep: () => {
      if (keeper !== undefined) return;
      const running = (async () => {
        while (input.outage.reconnecting && !input.stopped()) {
          await input
            .renew()
            .catch((cause: unknown) =>
              isRecoverableHistorySourceFailure(cause)
                ? input.outage.lost(cause)
                : input.fail(cause),
            );
          await delay(input.intervalMs, undefined, {
            signal: input.signal,
          }).catch(() => undefined);
        }
      })();
      keeper = running;
      void running.finally(() => {
        if (keeper === running) keeper = undefined;
      });
    },
    joined: () => keeper ?? Promise.resolve(),
  };
};

/** Wait out the reconnect schedule until the lease re-validates. True means
 * a new session may start. False means the owner stops: it closed, the
 * re-validation was refused (fail was called). Elapsed outage time only
 * escalates diagnostics; it never admits production or stops recovery. */
export const awaitHistorySourceReconnect = async (input: {
  readonly outage: HistorySourceOutage;
  readonly signal: AbortSignal;
  readonly stopped: () => boolean;
  readonly revalidate: () => Promise<unknown>;
  readonly fail: (cause: unknown) => void;
  readonly warn: Warn;
}): Promise<boolean> => {
  while (!input.stopped()) {
    const elapsed = input.outage.exceeded();
    const retryInMs = input.outage.nextDelayMs();
    input.warn(
      elapsed === undefined
        ? "History source unavailable; reconnecting"
        : "History source outage exceeded escalation threshold; continuing bounded reconnects",
      {
        event:
          elapsed === undefined
            ? "history_source_reconnect"
            : "history_source_outage_escalated",
        ...input.outage.status(),
        retryInMs,
        ...(elapsed === undefined ? {} : { outageMs: Math.round(elapsed) }),
      },
    );
    try {
      await delay(retryInMs, undefined, { signal: input.signal });
    } catch {
      return false;
    }
    try {
      await input.revalidate();
      return !input.stopped();
    } catch (cause) {
      if (input.stopped()) return false;
      if (!isRecoverableHistorySourceFailure(cause)) {
        input.fail(cause);
        return false;
      }
      input.outage.lost(cause);
    }
  }
  return false;
};
