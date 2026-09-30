import { createWatcherLocalUserEventPublisher } from "../../src/indexers/user-event-history.js";
import { readWatcherLocalBackfillFinality } from "../../src/l1/finality-engine.js";
import { makeLocalDepositReplayFixture } from "../support/local-event-replay-fixture.js";
import {
  durableFixture,
  historyLifecycle,
  openOrigin,
} from "../support/local-user-event-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

// Prepare deterministic ordinary block contents before the SQ transport builds
// its commit. This retired template capability never authorizes the later replay.
export const ordinaryDepositReplayTemplate = async () => {
  const fixture = await createSyntheticUserEventOriginFixture();
  let publisher:
    | Awaited<ReturnType<typeof createWatcherLocalUserEventPublisher>>
    | undefined;
  try {
    const { pair, input, origin, facts } = await openOrigin(fixture);
    const durable = await durableFixture(
      readWatcherLocalBackfillFinality(pair.finality).policy,
    );
    publisher = await createWatcherLocalUserEventPublisher({
      ...input,
      origin,
      runtime: durable.runtime,
      archive: durable.archive,
    });
    await publisher.publish(pair);
    await publisher.publish(
      await fixture.openFinalizedBlock(fixture.emptySuccessorBlock),
    );
    const deposit = historyLifecycle(facts);
    const block = await fixture.makeBlock({
      transactions: [deposit.create],
      creatingBodies: [fixture.initializationBodyCbor],
    });
    await publisher.publish(await fixture.openFinalizedBlock(block));
    const authority = await publisher.eventAuthority({
      ...(await fixture.openFinalizedBlock(block)),
      kind: "deposit",
      eventId: deposit.expectedEventId,
    });
    const replay = await makeLocalDepositReplayFixture(
      authority,
      fixture.deploymentIdentity.programCommitments,
    );
    return {
      header: replay.observation.header,
      ruleBundleCommitment: replay.ruleBundleCommitment,
    };
  } finally {
    publisher?.close();
    await fixture.close();
  }
};
