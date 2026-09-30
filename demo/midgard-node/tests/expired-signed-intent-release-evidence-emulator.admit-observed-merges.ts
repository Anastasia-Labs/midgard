import { expect } from "vitest";

import {
  observeMerges,
  type ObserverContext,
  observerTick,
} from "./expired-signed-intent-release-evidence-emulator.merge-checkpoint.js";
import { readObserver } from "./helpers/correction-rewind-scenario.js";

/** The merges `observeMerges` recorded reach the release depth: the observer
 * admits them (final), its cursor unchanged. */
export const admitObservedMerges = async (
  scenario: ObserverContext,
  observed: Awaited<ReturnType<typeof observeMerges>>,
) => {
  const result = await observerTick(scenario, {
    readQueue: async () => observed.queue,
    observeTransitions: async () => {
      throw new Error("The cursor is current; nothing new is observed");
    },
    canonicalDepth: async () => scenario.requiredFinalityDepth,
  });
  expect(result.admittedTransactionHashes).toEqual(observed.transactionHashes);
  expect(
    (await readObserver()).admitted.map(
      ({ transactionHash }) => transactionHash,
    ),
  ).toEqual(observed.transactionHashes);
};
