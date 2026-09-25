import { setTimeout as pause } from "node:timers/promises";

import { Schema } from "effect";

const canonicalSlotSchema = Schema.Number.pipe(
  Schema.filter((slot) => Number.isSafeInteger(slot) && slot >= 0),
);
const blockHashSchema = Schema.String.pipe(Schema.pattern(/^[0-9a-f]{64}$/u));
const canonicalTipSchema = Schema.Struct({
  error: Schema.optional(Schema.Null),
  result: Schema.Struct({ slot: canonicalSlotSchema, id: blockHashSchema }),
});
const indexerCheckpointsSchema = Schema.Array(
  Schema.Struct({
    slot_no: canonicalSlotSchema,
    header_hash: blockHashSchema,
  }),
);

/** Local provider stalls and dropped connections say nothing about the chain. */
const isTransientReadFailure = (error: unknown): boolean =>
  (error instanceof DOMException && error.name === "TimeoutError") ||
  (error instanceof TypeError && error.message === "fetch failed");

/** The barrier prevents Kupo lag from being mistaken for an expired transaction. */
export const synchronizePublicationIndexer = async (
  ogmiosUrl: string,
  kupoUrl: string,
): Promise<number> => {
  const deadline = Date.now() + 60_000;
  const retryTransient = async <A>(read: () => Promise<A>): Promise<A> => {
    while (true) {
      try {
        return await read();
      } catch (error) {
        if (!isTransientReadFailure(error) || Date.now() >= deadline)
          throw error;
        await pause(500);
      }
    }
  };
  const readTip = () =>
    retryTransient(async () => {
      const response = await fetch(ogmiosUrl, {
        method: "POST",
        signal: AbortSignal.timeout(10_000),
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          method: "queryLedgerState/tip",
          params: {},
          id: "publication-tip",
        }),
      });
      const body: unknown = await response.json();
      if (!response.ok || !Schema.is(canonicalTipSchema)(body))
        throw new Error("Cannot establish canonical publication tip");
      return body.result;
    });
  const readCheckpoints = () =>
    retryTransient(async () => {
      const response = await fetch(`${kupoUrl}/checkpoints`, {
        signal: AbortSignal.timeout(10_000),
      });
      const checkpoints: unknown = await response.json();
      if (!response.ok || !Schema.is(indexerCheckpointsSchema)(checkpoints))
        throw new Error("Cannot read publication indexer checkpoints");
      return checkpoints;
    });
  let tip = await readTip();
  while (true) {
    if (Date.now() >= deadline)
      throw new Error("Publication indexer has not reached canonical node tip");
    const checkpoints = await readCheckpoints();
    if (
      checkpoints.some(
        (checkpoint) =>
          checkpoint.slot_no === tip.slot && checkpoint.header_hash === tip.id,
      )
    ) {
      const current = await readTip();
      if (current.slot === tip.slot && current.id === tip.id) return tip.slot;
      tip = current;
      continue;
    }
    if (Date.now() >= deadline)
      throw new Error("Publication indexer has not reached canonical node tip");
    await pause(500);
    tip = await readTip();
  }
};
