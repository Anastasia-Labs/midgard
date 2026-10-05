import type { OutRefLike } from "@al-ft/midgard-core/out-ref";

import type { BoundHistoryChainBlock } from "../l1-event-history-source.js";
import {
  type HistoryTransportOptions,
  readEventHistoryCreatingBody,
} from "../l1-event-history-transport.js";
import { MissingBody } from "./event-history-owner.history-owner-change.js";

/** A per-block, lazy body cache: tracked outputs resolve first, and only
 * references actually needed by transition decoding trigger archive reads.
 * The transport's signal is the source session the block came from. */
export const withCreatingBodies = async <A>(
  transport: HistoryTransportOptions,
  block: BoundHistoryChainBlock,
  work: (body: (txHash: string) => string) => A | Promise<A>,
): Promise<A> => {
  const { signal } = transport;
  const bodies = new Map<string, string>();
  const getBody = (txHash: string) => {
    const value = bodies.get(txHash);
    if (value === undefined) throw new MissingBody(txHash);
    return value;
  };
  while (true) {
    signal.throwIfAborted();
    try {
      return await work(getBody);
    } catch (cause) {
      if (!(cause instanceof MissingBody)) throw cause;
      const refs = new Map<string, OutRefLike>();
      for (const transaction of block.transactions)
        for (const ref of transaction.references)
          if (ref.txHash === cause.txHash)
            refs.set(`${ref.txHash}#${ref.outputIndex}`, ref);
      if (refs.size === 0) throw cause;
      let lastFailure: unknown = cause;
      for (const ref of refs.values()) {
        try {
          bodies.set(
            cause.txHash,
            await readEventHistoryCreatingBody(transport, ref),
          );
          break;
        } catch (error) {
          signal.throwIfAborted();
          lastFailure = error;
        }
      }
      if (!bodies.has(cause.txHash)) throw lastFailure;
    }
  }
};
