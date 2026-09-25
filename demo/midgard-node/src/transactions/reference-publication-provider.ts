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

/** The barrier prevents Kupo lag from being mistaken for an expired transaction. */
export const synchronizePublicationIndexer = async (
  ogmiosUrl: string,
  kupoUrl: string,
): Promise<number> => {
  const readTip = async () => {
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
  };
  let tip = await readTip();
  const deadline = Date.now() + 60_000;
  while (true) {
    if (Date.now() >= deadline)
      throw new Error("Publication indexer has not reached canonical node tip");
    const checkpointResponse = await fetch(`${kupoUrl}/checkpoints`, {
      signal: AbortSignal.timeout(10_000),
    });
    const checkpoints: unknown = await checkpointResponse.json();
    if (
      !checkpointResponse.ok ||
      !Schema.is(indexerCheckpointsSchema)(checkpoints)
    )
      throw new Error("Cannot read publication indexer checkpoints");
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
