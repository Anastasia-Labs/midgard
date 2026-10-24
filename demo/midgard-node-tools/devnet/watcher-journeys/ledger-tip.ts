import { setTimeout as pause } from "node:timers/promises";

/** How long one Ogmios state query may take before it counts as a blip. */
export const OGMIOS_QUERY_TIMEOUT_MS = 10_000;

/** How long Ogmios may stay unreachable before a read gives up. */
export const OGMIOS_MAX_TRANSPORT_OUTAGE_MS = 120_000;

/**
 * An HTTP read that failed without the server's answer: the request timed out
 * or was aborted (`AbortSignal.timeout`), or undici's fetch lost its
 * connection (`TypeError("fetch failed")` wraps a refused or reset
 * connection; `"terminated"` is a body reset mid-read). An answer that says
 * something is wrong (an Ogmios JSON-RPC error, a missing or malformed
 * result, a non-200 status) is never one.
 */
export const isTransientTransportError = (error: unknown): boolean => {
  if (typeof error !== "object" || error === null) return false;
  const { name, message } = error as { name?: unknown; message?: unknown };
  if (name === "TimeoutError" || name === "AbortError") return true;
  return (
    error instanceof TypeError &&
    (message === "fetch failed" || message === "terminated")
  );
};

/**
 * Runs the read-only Ogmios query `read` again after a transport blip
 * (`isTransientTransportError`), pausing `pollMs` between tries, until
 * it answers or Ogmios has been unreachable for `maxOutageMs`. Any other
 * error is rethrown at once; the outage error carries the last blip as its
 * cause.
 */
export const retryOgmiosTransport = async <T>(
  read: () => Promise<T>,
  {
    maxOutageMs = OGMIOS_MAX_TRANSPORT_OUTAGE_MS,
    pollMs = 1_000,
    now = Date.now,
    sleep = pause,
  }: {
    readonly maxOutageMs?: number;
    readonly pollMs?: number;
    readonly now?: () => number;
    readonly sleep?: (milliseconds: number) => Promise<unknown>;
  } = {},
): Promise<T> => {
  const outageStart = now();
  for (;;) {
    try {
      return await read();
    } catch (error) {
      if (!isTransientTransportError(error)) throw error;
      if (now() - outageStart >= maxOutageMs)
        throw new Error(
          `Ogmios stayed unreachable for ${maxOutageMs.toString()}ms`,
          { cause: error },
        );
    }
    await sleep(pollMs);
  }
};

/**
 * The node validates a transaction's lower validity bound against the slot of
 * its ledger tip, not against the wall clock: after a block gap a transaction
 * whose `invalidBefore` has already passed by the clock is still rejected as
 * "before its validity interval" until the next block lands.  Waits until the
 * tip has reached the target slot, polling the tip reader. A transport blip
 * is read through (`retryOgmiosTransport`); any other read error fails the
 * wait.
 */
export const awaitLedgerTipSlot = async ({
  targetSlot,
  readTipSlot,
  timeoutMs,
  pollMs = 1_000,
  maxTransportOutageMs = OGMIOS_MAX_TRANSPORT_OUTAGE_MS,
  now = Date.now,
  sleep = pause,
}: {
  readonly targetSlot: number;
  readonly readTipSlot: () => Promise<number>;
  readonly timeoutMs: number;
  readonly pollMs?: number;
  readonly maxTransportOutageMs?: number;
  readonly now?: () => number;
  readonly sleep?: (milliseconds: number) => Promise<unknown>;
}): Promise<number> => {
  const deadline = now() + timeoutMs;
  for (;;) {
    const tipSlot = await retryOgmiosTransport(readTipSlot, {
      maxOutageMs: maxTransportOutageMs,
      pollMs,
      now,
      sleep,
    });
    if (tipSlot >= targetSlot) return tipSlot;
    if (now() >= deadline) {
      throw new Error(
        `Ledger tip stalled at slot ${tipSlot.toString()} before reaching slot ${targetSlot.toString()} within ${timeoutMs.toString()}ms`,
      );
    }
    await sleep(pollMs);
  }
};

/**
 * The `result` of one Ogmios JSON-RPC query over HTTP, aborted after
 * `timeoutMs` so a hung connection surfaces as a transport error.
 */
export const postOgmiosQuery = async (
  ogmiosUrl: string,
  method: string,
  timeoutMs: number = OGMIOS_QUERY_TIMEOUT_MS,
): Promise<unknown> => {
  const response = await fetch(ogmiosUrl, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ jsonrpc: "2.0", method, id: null }),
    signal: AbortSignal.timeout(timeoutMs),
  });
  const { result } = (await response.json()) as { result?: unknown };
  return result;
};

/** Slot of the node's ledger tip as Ogmios reports it. */
export const readOgmiosTipSlot = async (
  ogmiosUrl: string,
  { timeoutMs = OGMIOS_QUERY_TIMEOUT_MS }: { readonly timeoutMs?: number } = {},
): Promise<number> => {
  const result = (await postOgmiosQuery(
    ogmiosUrl,
    "queryNetwork/tip",
    timeoutMs,
  )) as { slot?: unknown } | undefined;
  const slot = result?.slot;
  if (typeof slot !== "number" || !Number.isSafeInteger(slot))
    throw new Error("Ogmios did not report a tip slot");
  return slot;
};
