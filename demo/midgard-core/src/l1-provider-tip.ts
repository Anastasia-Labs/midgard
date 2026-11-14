/**
 * Parsers for the tip answers of the external L1 providers a tool reads:
 * the Ogmios `queryNetwork/tip` result, Kupo's JSON `/health` checkpoint
 * and Blockfrost's `/blocks/latest` block. No network access: callers fetch
 * the answer and hand it here, so the node's provider adapters and the user
 * CLI read the same shapes the same way.
 */

/** A point an adapter's reads are answered at. */
export type L1ViewPoint = Readonly<{ slot: number; id: string }>;

/** The external provider has not reached the point a read needs yet: retry. */
export class L1ExternalBehindError extends Error {
  override readonly name = "L1ExternalBehindError";
  readonly retryable = true;
}

const exactSlot = (value: unknown, what: string): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
    throw new Error(`${what} is not a slot: ${JSON.stringify(value)}`);
  return value;
};

/** The Ogmios tip from a `queryNetwork/tip` answer. */
export const ogmiosTipPointOf = (payload: unknown): L1ViewPoint => {
  const answer = payload as { result?: unknown; error?: unknown };
  if (answer?.error !== undefined)
    throw new Error(
      `Ogmios queryNetwork/tip failed: ${JSON.stringify(answer.error)}`,
    );
  const result = answer?.result as { slot?: unknown; id?: unknown } | string;
  if (result === "origin" || typeof result !== "object" || result === null)
    throw new L1ExternalBehindError("Ogmios reports the chain at its origin");
  if (typeof result.id !== "string" || !/^[0-9a-f]{64}$/u.test(result.id))
    throw new Error("Ogmios queryNetwork/tip has no 32-byte block id");
  return { slot: exactSlot(result.slot, "the Ogmios tip"), id: result.id };
};

/** Kupo's most recent checkpoint slot, from its JSON `/health`. */
export const kupoCheckpointSlotOf = (payload: unknown): number | null => {
  const checkpoint = (payload as { most_recent_checkpoint?: unknown })
    ?.most_recent_checkpoint;
  return checkpoint === null || checkpoint === undefined
    ? null
    : exactSlot(checkpoint, "Kupo's most recent checkpoint");
};

/** The latest block's point from a `/blocks/latest` answer. */
export const blockfrostTipPointOf = (payload: unknown): L1ViewPoint => {
  const block = payload as { slot?: unknown; hash?: unknown };
  if (
    typeof block?.slot !== "number" ||
    !Number.isSafeInteger(block.slot) ||
    block.slot < 0 ||
    typeof block.hash !== "string" ||
    !/^[0-9a-f]{64}$/u.test(block.hash)
  )
    throw new Error(
      `Blockfrost /blocks/latest is not a block with a slot and hash: ${JSON.stringify(payload)}`,
    );
  return { slot: block.slot, id: block.hash };
};
