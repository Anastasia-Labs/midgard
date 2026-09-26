import type { ObservedStateQueueSnapshot } from "../../src/domain.js";
import type { StateQueueProvider } from "../../src/l1/state-queue-scanner.js";

/**
 * Serves `provider`'s nodes as a full state-queue snapshot whose root is
 * final, read at one tip. With final nodes, the committee's first scan of it
 * records a durable replay anchor, which every decision requires. The queue
 * it names must not change between scans: there is no replay source to
 * authenticate a change.
 */
export const withFinalSnapshot = <Provider extends StateQueueProvider>(
  provider: Provider,
): Provider & Required<Pick<StateQueueProvider, "fetchStateQueueSnapshot">> =>
  Object.assign(Object.create(provider) as Provider, {
    fetchStateQueueSnapshot: async (): Promise<ObservedStateQueueSnapshot> => ({
      nodes: await provider.fetchStateQueueNodes(),
      confirmedHeaderHash: "00".repeat(28),
      confirmedStateOutRef: `${"00".repeat(32)}#0`,
      tipBlockNo: 1_000,
      observedChainPoint: {
        slot: 0,
        blockHash: "00".repeat(32),
        blockHeight: 0,
        depth: 1_000,
        finalized: true,
        providerSource: "fixture",
      },
    }),
  });
