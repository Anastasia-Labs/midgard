import { setTimeout as pause } from "node:timers/promises";

/**
 * The node validates a transaction's lower validity bound against the slot of
 * its ledger tip, not against the wall clock: after a block gap a transaction
 * whose `invalidBefore` has already passed by the clock is still rejected as
 * "before its validity interval" until the next block lands.  Waits until the
 * tip has reached the target slot, polling the tip reader.
 */
export const awaitLedgerTipSlot = async ({
  targetSlot,
  readTipSlot,
  timeoutMs,
  pollMs = 1_000,
  now = Date.now,
  sleep = pause,
}: {
  readonly targetSlot: number;
  readonly readTipSlot: () => Promise<number>;
  readonly timeoutMs: number;
  readonly pollMs?: number;
  readonly now?: () => number;
  readonly sleep?: (milliseconds: number) => Promise<unknown>;
}): Promise<number> => {
  const deadline = now() + timeoutMs;
  for (;;) {
    const tipSlot = await readTipSlot();
    if (tipSlot >= targetSlot) return tipSlot;
    if (now() >= deadline) {
      throw new Error(
        `Ledger tip stalled at slot ${tipSlot.toString()} before reaching slot ${targetSlot.toString()} within ${timeoutMs.toString()}ms`,
      );
    }
    await sleep(pollMs);
  }
};

/** Slot of the node's ledger tip as Ogmios reports it. */
export const readOgmiosTipSlot = async (ogmiosUrl: string): Promise<number> => {
  const response = await fetch(ogmiosUrl, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryNetwork/tip",
      id: null,
    }),
  });
  const { result } = (await response.json()) as {
    result?: { slot?: unknown };
  };
  const slot = result?.slot;
  if (typeof slot !== "number" || !Number.isSafeInteger(slot))
    throw new Error("Ogmios did not report a tip slot");
  return slot;
};
