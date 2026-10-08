import type { ChainSyncEvent, ChainSyncStream } from "@al-ft/l1-node-transport";

const pause = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

/**
 * A chain-sync transport serving `events` in order, as a node does at its
 * tip: the test appends events (roll-forwards and roll-backwards) and every
 * open stream serves them as they arrive. A new stream resumes after the
 * last acknowledged event.
 */
export const growingChainSync = (events: ChainSyncEvent[]) => {
  let acked = 0;
  return {
    openChainSync: (): ChainSyncStream => {
      let position = acked;
      let closed = false;
      return {
        opened: Promise.resolve(),
        next: async () => {
          for (;;) {
            if (closed) return undefined;
            if (position < events.length) {
              position += 1;
              return events[position - 1];
            }
            await pause(2);
          }
        },
        ack: (seq: bigint) => {
          const index = events.findIndex((event) => event.seq === seq);
          if (index >= 0) acked = Math.max(acked, index + 1);
        },
        close: () => {
          closed = true;
          return Promise.resolve();
        },
      } as unknown as ChainSyncStream;
    },
  };
};
