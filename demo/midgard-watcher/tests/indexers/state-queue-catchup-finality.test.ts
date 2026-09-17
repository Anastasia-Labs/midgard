import { localKupmiosHttpOgmiosRawSourceDetails } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import { assertWatcherStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";

// Real observation and concrete Kupo/Ogmios source, synthetic local transport.
// No transaction submission, Plutus evaluation, or public-chain inclusion.
describe("state-queue catch-up source finality", () => {
  it("observes a later finalized commit after the source's initial boundary", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture({
      composeCommitBlock: async ({
        transport,
        initializationBlock,
        commitTransactionCbor,
      }) => {
        let parent = initializationBlock;
        for (let index = 0; index < 16; index++) {
          parent = await transport.makeBlock({ transactions: [], parent });
        }
        return { transactions: [commitTransactionCbor], parent };
      },
    });
    let capture: Awaited<ReturnType<typeof fixture.observeFresh>> | undefined;
    try {
      // Init pins a provider boundary; Commit has a newer native finality tip
      // while reusing the same raw source.
      let observationFailure: unknown;
      try {
        capture = await fixture.observeFresh();
      } catch (error) {
        observationFailure = error;
      }
      const queries = await fixture.transport.readNativeQueries();
      const initializationQuery = queries.find(
        ({ target }) =>
          target.blockHash === fixture.initializationBlock.point.blockHash,
      );
      const commitQuery = queries.find(
        ({ target }) =>
          target.blockHash === fixture.commitBlock.point.blockHash,
      );
      expect(initializationQuery).toBeDefined();
      expect(commitQuery).toBeDefined();
      const commitBlockNo = BigInt(fixture.commitBlock.point.blockNo);
      // The same Commit is too recent at the old pinned tip, and finalized at
      // the actual later native tip. Admission must refresh its provider tip.
      expect(
        BigInt(initializationQuery!.tip.blockNo) - commitBlockNo + 1n,
      ).toBeLessThan(30n);
      expect(
        BigInt(commitQuery!.tip.blockNo) - commitBlockNo + 1n,
      ).toBeGreaterThanOrEqual(30n);
      if (observationFailure !== undefined) throw observationFailure;
      if (capture === undefined)
        throw new Error("observation capture is absent");
      assertWatcherStateQueueObservation(capture.initialObservation);
      assertWatcherStateQueueObservation(capture.observation);
      expect(capture.header.headerHash).toBe(fixture.headerHash);
      expect(capture.initialObservation.nativePoint.finalityDepth).toBe("30");
      expect(capture.header.finalityDepth).toBe("30");
      expect(
        localKupmiosHttpOgmiosRawSourceDetails(capture.localRuntime.rawSource)
          ?.confirmationDepth,
      ).toBe(30);
      expect(
        BigInt(capture.localObservation.block.chainPoint.depth),
      ).toBeGreaterThanOrEqual(30n);
    } finally {
      await capture?.close();
      await fixture.close();
    }
  }, 60_000);
});
