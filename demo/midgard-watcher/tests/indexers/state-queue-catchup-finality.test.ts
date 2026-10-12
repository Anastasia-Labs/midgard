import { describe, expect, it } from "vitest";

import { assertWatcherStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  followerUserEventsDeployment,
  openFollowerUserEvents,
  syntheticChain,
  transactionHash,
} from "../support/follower-user-events-fixture.js";
import {
  commitTransaction,
  createSyntheticStateQueueHeader,
  initializationTransaction,
} from "../support/state-queue-observation-fixture.commit-transaction.js";

// A follower store fed a synthetic Init and Commit. No transaction submission,
// Plutus evaluation, or public-chain inclusion.
describe("state-queue catch-up finality", () => {
  it("observes a commit exactly when the follower's tip makes it release-deep", async () => {
    const deployment = followerUserEventsDeployment();
    const { authority } = deployment;
    const initialization = initializationTransaction(authority);
    const chain = syntheticChain();
    chain.next([initialization]);
    const commitBlock = chain.next([
      commitTransaction(
        authority,
        transactionHash(initialization),
        createSyntheticStateQueueHeader(),
      ),
    ]);
    // The commit is one block short of the release depth at this tip.
    chain.empties(RELEASE_FINALITY_DEPTH - 2);
    const follower = await openFollowerUserEvents({
      deployment,
      origin: chain.anchor,
    });
    try {
      await follower.apply(chain.blocks);
      const early = await follower.observe();
      assertWatcherStateQueueObservation(early);
      expect(early.finalizedHeaders).toEqual([]);

      await follower.apply([chain.next([])]);
      const caughtUp = await follower.observe();
      assertWatcherStateQueueObservation(caughtUp);
      expect(caughtUp.finalizedHeaders).toHaveLength(1);
      expect(caughtUp.finalizedHeaders[0]).toMatchObject({
        observedBlockHash: commitBlock.point.blockHash,
        observedSlot: commitBlock.point.slot,
        finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
      });
    } finally {
      await follower.close();
    }
  }, 60_000);
});
