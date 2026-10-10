import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  type FollowerUserEvents,
  type FollowerUserEventsDeployment,
  followerUserEventsDeployment,
  openFollowerUserEvents,
  syntheticChain,
  transactionHash,
} from "./follower-user-events-fixture.js";
import {
  closeAll,
  commitTransaction,
  createSyntheticStateQueueHeader,
  initializationTransaction,
} from "./state-queue-observation-fixture.commit-transaction.js";
import type { SyntheticUserEventBlock } from "./user-event-origin-fixture.js";

export type SyntheticStateQueueObservationCapture = Readonly<{
  /** The follower store the observation was read from, with its user events. */
  follower: FollowerUserEvents;
  observation: WatcherAuthenticatedStateQueueObservation;
  header: WatcherStateQueueHeaderObservation;
  close(): Promise<void>;
}>;

export type SyntheticStateQueueObservationFixture = Readonly<{
  deployment: FollowerUserEventsDeployment;
  initializationTransactionCbor: string;
  commitTransactionCbor: string;
  initializationBlock: SyntheticUserEventBlock;
  commitBlock: SyntheticUserEventBlock;
  header: SDK.Header;
  headerHash: string;
  observeFresh(): Promise<SyntheticStateQueueObservationCapture>;
  close(): Promise<void>;
}>;

/**
 * A synthetic Init and Commit of `header` on the synthetic origin
 * deployment. Each `observeFresh` opens a new follower store (watcher and
 * event projections), feeds it the chain from Init through the commit and
 * empty blocks until the commit is `depthAtTip` deep (the release depth by
 * default), and reads the observation at the release depth as the decision
 * driver reads it.
 */
export const createSyntheticStateQueueObservationFixture = async (
  input: Readonly<{
    header?: SDK.Header;
    ruleBundleCommitment?: string;
    /** How deep the commit block is at the store's tip; at least the release depth. */
    depthAtTip?: number;
    /** The commit block's transactions; the commit alone by default. */
    composeCommitBlock?: (
      input: Readonly<{
        deployment: FollowerUserEventsDeployment;
        commitTransactionCbor: string;
      }>,
    ) => Readonly<{ transactions: readonly string[] }>;
  }> = {},
): Promise<SyntheticStateQueueObservationFixture> => {
  const depthAtTip = input.depthAtTip ?? RELEASE_FINALITY_DEPTH;
  if (!Number.isSafeInteger(depthAtTip) || depthAtTip < RELEASE_FINALITY_DEPTH)
    throw new Error("Synthetic SQ commit must be at least release-deep");
  const headerCborHex = Data.to(
    input.header ?? createSyntheticStateQueueHeader(),
    SDK.Header,
  );
  const header = Object.freeze(Data.from(headerCborHex, SDK.Header));
  const headerHash = computeHash28(Buffer.from(headerCborHex, "hex")).toString(
    "hex",
  );
  const deployment = followerUserEventsDeployment(input.ruleBundleCommitment);
  const { authority } = deployment;
  const initializationTransactionCbor = initializationTransaction(authority);
  const commitTransactionCbor = commitTransaction(
    authority,
    transactionHash(initializationTransactionCbor),
    header,
  );
  const chain = syntheticChain();
  const initializationBlock = chain.next([initializationTransactionCbor]);
  const commitBlock = chain.next(
    input.composeCommitBlock?.({ deployment, commitTransactionCbor })
      .transactions ?? [commitTransactionCbor],
  );
  chain.empties(depthAtTip - 1);
  const blocks = Object.freeze([...chain.blocks]);
  const captures = new Set<SyntheticStateQueueObservationCapture>();
  let closed = false;
  const observeFresh =
    async (): Promise<SyntheticStateQueueObservationCapture> => {
      if (closed) throw new Error("Synthetic SQ fixture is closed");
      const follower = await openFollowerUserEvents({
        deployment,
        origin: chain.anchor,
      });
      try {
        await follower.apply(blocks);
        const observation = await follower.observe();
        assertWatcherStateQueueObservation(observation);
        const observedHeader = observation.finalizedHeaders.find(
          (item) => item.headerHash === headerHash,
        );
        if (observedHeader === undefined)
          throw new Error("Synthetic SQ commit omitted its requested header");
        assertWatcherStateQueueHeaderObservation(observedHeader);
        const capture: SyntheticStateQueueObservationCapture = Object.freeze({
          follower,
          observation,
          header: observedHeader,
          close: async () => {
            captures.delete(capture);
            await follower.close();
          },
        });
        captures.add(capture);
        return capture;
      } catch (error) {
        await follower.close();
        throw error;
      }
    };
  return Object.freeze({
    deployment,
    initializationTransactionCbor,
    commitTransactionCbor,
    initializationBlock,
    commitBlock,
    header,
    headerHash,
    observeFresh,
    close: async () => {
      if (closed) return;
      closed = true;
      await closeAll([...captures].map((capture) => () => capture.close()));
    },
  });
};
