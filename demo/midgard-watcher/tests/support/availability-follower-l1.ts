import type { FraudProofRawL1Point } from "@al-ft/midgard-fault-proofs";
import type { Provider } from "@lucid-evolution/lucid";
import { vi } from "vitest";

import type { WatcherAvailabilityL1 } from "../../src/availability/follower-reads.js";
import { ok, refused } from "../../src/l1-follower/raw-reads.types.js";

/** Every follower read the availability actor makes, as one mock each. */
export const availabilityFollowerIo = () => ({
  /** The canonical check after a capture's reads; undefined is canonical. */
  status: vi.fn<(...args: any[]) => any>(),
  address: vi.fn<(...args: any[]) => any>(),
  inclusion: vi.fn<(...args: any[]) => any>(),
  outrefs: vi.fn<(...args: any[]) => any>(),
  history: vi.fn<(...args: any[]) => any>(),
  transaction: vi.fn<(...args: any[]) => any>(),
  predecessor: vi.fn<(...args: any[]) => any>(),
});

export type AvailabilityFollowerIo = ReturnType<typeof availabilityFollowerIo>;

/**
 * The watcher follower's raw reads and store, answered by `io`. Each mock
 * receives one object naming its arguments and returns the read's value; a
 * transaction the mock does not return is `not_stored`.
 */
export const availabilityFollowerL1 = (
  io: AvailabilityFollowerIo,
): WatcherAvailabilityL1 & Readonly<{ provider: Provider }> => ({
  store: {
    pointStatus: async (point) =>
      ((await io.status({
        point: {
          slot: point.slot.toString(),
          blockHash: point.hash.toString("hex"),
        },
      })) as Awaited<
        ReturnType<WatcherAvailabilityL1["store"]["pointStatus"]>
      >) ?? { kind: "canonical", height: 0, depth: 0 },
  },
  reads: {
    addressUtxosAtPoint: async (address, point) =>
      ok(await io.address({ address, point })),
    utxosByOutRefAtPoint: async (outRefs, point) =>
      ok({
        unknown: [],
        beyondRetention: [],
        ...(await io.outrefs({ outRefs, point })),
      }),
    unitHistoryAtPoint: async (unit, point) =>
      ok({ checkpoint: point, ...(await io.history({ unit, point })) }),
    transactionInclusion: async (txHash) =>
      ok((await io.inclusion({ txHash })) as FraudProofRawL1Point | null),
    rawTransaction: async (txHash, expectedInclusionPoint) => {
      const transaction = await io.transaction({
        txHash,
        expectedInclusionPoint,
      });
      return transaction === undefined
        ? refused("not_stored", `${txHash} is not stored`)
        : ok({
            transaction,
            unresolvedInputs: [],
            unresolvedReferenceInputs: [],
          });
    },
    predecessorPoint: async (point) => ok(await io.predecessor({ point })),
  },
  provider: {} as Provider,
});
