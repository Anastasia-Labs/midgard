import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { localKupmiosHttpOgmiosRawSourceDetails } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import { assertWatcherStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";

// Real observation and concrete Kupo/Ogmios source, synthetic local transport.
// No transaction submission, Plutus evaluation, or public-chain inclusion.

/** The compiled deployment profile's release depth (3 testing, 30 public). */
const RELEASE_DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;
describe("state-queue catch-up source finality", () => {
  it("observes a later finalized commit after the source's initial boundary", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture({
      composeCommitBlock: async ({
        transport,
        initializationBlock,
        commitTransactionCbor,
      }) => {
        // The fixture's first exact-point tip is Init + 41 (base depth 40 plus
        // query 1). This gap leaves Commit exactly one block short of the
        // release depth there, and 43 deep at its own later query tip.
        let parent = initializationBlock;
        for (let index = 0; index < 42 - RELEASE_DEPTH; index++) {
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
      ).toBeLessThan(BigInt(RELEASE_DEPTH));
      expect(
        BigInt(commitQuery!.tip.blockNo) - commitBlockNo + 1n,
      ).toBeGreaterThanOrEqual(BigInt(RELEASE_DEPTH));
      if (observationFailure !== undefined) throw observationFailure;
      if (capture === undefined)
        throw new Error("observation capture is absent");
      assertWatcherStateQueueObservation(capture.initialObservation);
      assertWatcherStateQueueObservation(capture.observation);
      expect(capture.header.headerHash).toBe(fixture.headerHash);
      expect(capture.initialObservation.nativePoint.finalityDepth).toBe(
        RELEASE_DEPTH.toString(),
      );
      expect(capture.header.finalityDepth).toBe(RELEASE_DEPTH.toString());
      expect(
        localKupmiosHttpOgmiosRawSourceDetails(capture.localRuntime.rawSource)
          ?.confirmationDepth,
      ).toBe(RELEASE_DEPTH);
      expect(
        BigInt(capture.localObservation.block.chainPoint.depth),
      ).toBeGreaterThanOrEqual(BigInt(RELEASE_DEPTH));
    } finally {
      await capture?.close();
      await fixture.close();
    }
  }, 60_000);
});
