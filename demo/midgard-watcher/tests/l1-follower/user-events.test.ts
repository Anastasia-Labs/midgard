/**
 * The watcher's user events read from follower facts
 * (`createWatcherFollowerUserEvents`): a deposit, a withdrawal and a forced
 * order read at a state-queue header's cutoff (the commit's block, through
 * the commit's transaction index), refused when admitted after it, and
 * fenced by the store's rewinds and resets as capabilities. The header is
 * the store's own observation of a synthetic Init/Commit pair.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import { WatcherUserEventUnavailable } from "../../src/l1-follower/user-events.js";
import {
  assertWatcherUserEventAuthorityCurrent,
  readWatcherUserEventAuthority,
  type WatcherUserEventAuthority,
} from "../../src/verification/user-event.js";
import {
  FIXTURE_LIST_INCLUSION_TIME,
  type FollowerUserEvents,
  followerUserEventsDeployment,
  FORCED_ORDER_INCLUSION_TIME,
  forcedOrderBurnTransaction,
  forcedOrderTransaction,
  listOrderTransaction,
  openFollowerUserEvents,
  PLACEHOLDER_FORCED_PAYLOAD,
  syntheticChain,
  transactionHash,
  type UserEventId,
  userEventId,
} from "../support/follower-user-events-fixture.js";
import {
  commitTransaction,
  createSyntheticStateQueueHeader,
  initializationTransaction,
} from "../support/state-queue-observation-fixture.commit-transaction.js";

const deployment = followerUserEventsDeployment();
const K = RELEASE_FINALITY_DEPTH + 2;

const opened: FollowerUserEvents[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((follower) => follower.close()));
});

const ID = Object.freeze({
  deposit: userEventId("d1"),
  withdrawal: userEventId("e1"),
  forced: userEventId("f1"),
  depositAfterCommit: userEventId("d2"),
  forcedAfterCommit: userEventId("f2"),
  depositLaterBlock: userEventId("d3"),
  neverAdmitted: userEventId("d4"),
});

/**
 * Init, then the commit's block: a deposit, a withdrawal and a forced
 * order before the commit (indexes 0-2), the commit (3), a deposit and a
 * forced order after it (4, 5); then a block with another deposit, and
 * empty blocks until the commit is release-deep.
 */
const commitChain = async () => {
  const follower = await openFollowerUserEvents({ deployment, k: K });
  opened.push(follower);
  const chain = syntheticChain();
  const initializationTx = initializationTransaction(deployment.authority);
  const commitTx = commitTransaction(
    deployment.authority,
    transactionHash(initializationTx),
    createSyntheticStateQueueHeader(),
  );
  const forcedTx = forcedOrderTransaction(deployment, ID.forced);
  const initializationBlock = chain.next([initializationTx]);
  const commitBlock = chain.next([
    listOrderTransaction(deployment, "deposit", ID.deposit),
    listOrderTransaction(deployment, "withdrawal", ID.withdrawal),
    forcedTx,
    commitTx,
    listOrderTransaction(deployment, "deposit", ID.depositAfterCommit),
    forcedOrderTransaction(deployment, ID.forcedAfterCommit),
  ]);
  const laterBlock = chain.next([
    listOrderTransaction(deployment, "deposit", ID.depositLaterBlock),
  ]);
  chain.empties(RELEASE_FINALITY_DEPTH - 2);
  await follower.apply(chain.blocks);
  const observation = await follower.observe();
  expect(observation.finalizedHeaders).toHaveLength(1);
  const header = observation.finalizedHeaders[0]!;
  expect(header.observedTransactionHash).toBe(transactionHash(commitTx));
  expect(header.observedBlockHash).toBe(commitBlock.point.blockHash);
  return {
    follower,
    chain,
    header,
    forcedTx,
    initializationBlock,
    commitBlock,
    laterBlock,
  };
};

type Setup = Awaited<ReturnType<typeof commitChain>>;

const authorityFor = (
  setup: Setup,
  kind: "deposit" | "withdrawal" | "forced_order",
  event: UserEventId,
): Promise<WatcherUserEventAuthority> =>
  setup.follower.userEvents.eventAuthority({
    kind,
    eventId: event.cborHex,
    throughHeader: setup.header,
  });

const unavailable = async (
  read: Promise<unknown>,
  message: RegExp,
): Promise<void> => {
  const error = await read.then(
    () => undefined,
    (cause: unknown) => cause,
  );
  expect(error).toBeInstanceOf(WatcherUserEventUnavailable);
  expect((error as Error).message).toMatch(message);
};

describe("watcher user events from follower facts", () => {
  it("reads a deposit, a withdrawal and a forced order admitted before the commit in its block", async () => {
    const setup = await commitChain();
    const cases = [
      ["deposit", ID.deposit, 0, deployment.scripts.deposit.policyId],
      ["withdrawal", ID.withdrawal, 1, deployment.scripts.withdrawal.policyId],
      ["forced_order", ID.forced, 2, deployment.scripts.forcedOrder.policyId],
    ] as const;
    for (const [kind, event, transactionIndex, policyId] of cases) {
      const authority = await authorityFor(setup, kind, event);
      const read = await readWatcherUserEventAuthority(authority);
      expect(read).toMatchObject({
        deploymentManifestId: deployment.deploymentIdentity.manifestId,
        blueprintHash: deployment.deploymentIdentity.blueprintHash,
        network: deployment.deploymentIdentity.network,
        event: {
          kind,
          eventId: event.cborHex,
          nonceOutRef: event.nonce,
          policyId,
          assetNameHex: event.key,
          admission: {
            blockHash: setup.commitBlock.point.blockHash,
            slot: setup.commitBlock.point.slot,
            blockNo: setup.commitBlock.point.blockNo,
            transactionIndex: transactionIndex.toString(),
            outputIndex: "0",
          },
        },
        throughHeader: {
          headerHash: setup.header.headerHash,
          observedTransactionHash: setup.header.observedTransactionHash,
          observedBlockHash: setup.commitBlock.point.blockHash,
          observedSlot: setup.commitBlock.point.slot,
          transactionIndex: "3",
        },
      });
      expect(() =>
        assertWatcherUserEventAuthorityCurrent(authority),
      ).not.toThrow();
    }
    const deposit = await readWatcherUserEventAuthority(
      await authorityFor(setup, "deposit", ID.deposit),
    );
    expect(deposit.event.inclusionTime).toBe(
      FIXTURE_LIST_INCLUSION_TIME.toString(),
    );
    expect(deposit.event.originalAssetsCborHex).not.toBeNull();
    const forced = await readWatcherUserEventAuthority(
      await authorityFor(setup, "forced_order", ID.forced),
    );
    expect(forced.event).toMatchObject({
      inclusionTime: FORCED_ORDER_INCLUSION_TIME.toString(),
      originalAssetsCborHex: null,
      eventCborHex: Data.to(
        {
          id: ID.forced.id,
          tx: {
            tx_id: PLACEHOLDER_FORCED_PAYLOAD.tx_id,
            transaction_commitment:
              PLACEHOLDER_FORCED_PAYLOAD.transaction_commitment,
            submitted_source: PLACEHOLDER_FORCED_PAYLOAD.submitted_source,
          },
        },
        SDK.TxOrderEvent,
      ),
      admission: { transactionHash: transactionHash(setup.forcedTx) },
    });
  });

  it("refuses an event admitted after the cutoff, in the commit's block or later, or never", async () => {
    const setup = await commitChain();
    await unavailable(
      authorityFor(setup, "deposit", ID.depositAfterCommit),
      /is not admitted by the cutoff/u,
    );
    await unavailable(
      authorityFor(setup, "forced_order", ID.forcedAfterCommit),
      /is not admitted by the cutoff/u,
    );
    await unavailable(
      authorityFor(setup, "deposit", ID.depositLaterBlock),
      /is not admitted by the cutoff/u,
    );
    await unavailable(
      authorityFor(setup, "deposit", ID.neverAdmitted),
      /is not admitted by the cutoff/u,
    );
    // The withdrawal list holds no deposit's id.
    await unavailable(
      authorityFor(setup, "withdrawal", ID.deposit),
      /is not admitted by the cutoff/u,
    );
  });

  it("keeps a capability current across a rewind above its cutoff, and retires it once a rewind removes the cutoff block", async () => {
    const setup = await commitChain();
    const authorities = [
      await authorityFor(setup, "deposit", ID.deposit),
      await authorityFor(setup, "forced_order", ID.forced),
    ];
    await setup.follower.rewindTo(setup.commitBlock.point);
    for (const authority of authorities) {
      expect(() =>
        assertWatcherUserEventAuthorityCurrent(authority),
      ).not.toThrow();
      await expect(readWatcherUserEventAuthority(authority)).resolves.toEqual(
        expect.objectContaining({
          throughHeader: expect.objectContaining({ transactionIndex: "3" }),
        }),
      );
    }
    await setup.follower.rewindTo(setup.initializationBlock.point);
    for (const authority of authorities) {
      expect(() => assertWatcherUserEventAuthorityCurrent(authority)).toThrow(
        "user-event authority was retired by an L1 rewind",
      );
      await expect(readWatcherUserEventAuthority(authority)).rejects.toThrow(
        "user-event authority was retired by an L1 rewind",
      );
    }
    // A new read at the removed header finds no commit to cut off at.
    await unavailable(
      authorityFor(setup, "deposit", ID.deposit),
      /commit transaction is not stored/u,
    );
  });

  it("retires a capability when the store resets", async () => {
    const setup = await commitChain();
    const authority = await authorityFor(setup, "withdrawal", ID.withdrawal);
    await setup.follower.reset();
    expect(() => assertWatcherUserEventAuthorityCurrent(authority)).toThrow(
      "user-event authority was retired by an L1 rewind",
    );
    await expect(readWatcherUserEventAuthority(authority)).rejects.toThrow(
      "user-event authority was retired by an L1 rewind",
    );
  });

  it("retires a capability once its user events close: no later rewind can be heard", async () => {
    const setup = await commitChain();
    const authority = await authorityFor(setup, "deposit", ID.deposit);
    setup.follower.userEvents.close();
    expect(() => assertWatcherUserEventAuthorityCurrent(authority)).toThrow();
    await expect(readWatcherUserEventAuthority(authority)).rejects.toThrow();
    await expect(authorityFor(setup, "deposit", ID.deposit)).rejects.toThrow();
  });

  it("fences a header cutoff with no event read: kept across a rewind above it, retired by a rewind removing it, a reset and close", async () => {
    const setup = await commitChain();
    const rewound = await setup.follower.userEvents.headerFence(setup.header);
    await setup.follower.rewindTo(setup.commitBlock.point);
    expect(rewound.current()).toBe(true);
    await expect(rewound.refresh()).resolves.toBeUndefined();
    await setup.follower.rewindTo(setup.initializationBlock.point);
    expect(rewound.current()).toBe(false);
    await expect(rewound.refresh()).rejects.toThrow(
      "the header cutoff was retired by an L1 rewind",
    );
    // A new fence at the removed header finds no commit to cut off at.
    await unavailable(
      setup.follower.userEvents.headerFence(setup.header),
      /commit transaction is not stored/u,
    );

    const again = await commitChain();
    const afterReset = await again.follower.userEvents.headerFence(
      again.header,
    );
    await again.follower.reset();
    expect(afterReset.current()).toBe(false);
    await expect(afterReset.refresh()).rejects.toThrow(
      "the header cutoff was retired by an L1 rewind",
    );

    const third = await commitChain();
    const afterClose = await third.follower.userEvents.headerFence(
      third.header,
    );
    third.follower.userEvents.close();
    expect(afterClose.current()).toBe(false);
    await expect(afterClose.refresh()).rejects.toThrow(
      "the header cutoff was retired by an L1 rewind",
    );
  });

  it("refuses copies of a capability", async () => {
    const setup = await commitChain();
    const authority = await authorityFor(setup, "deposit", ID.deposit);
    const copy = { ...authority } as WatcherUserEventAuthority;
    expect(() => assertWatcherUserEventAuthorityCurrent(copy)).toThrow(
      "user-event authority is not admitted",
    );
    await expect(readWatcherUserEventAuthority(copy)).rejects.toThrow(
      "user-event authority is not admitted",
    );
    await expect(readWatcherUserEventAuthority(authority)).resolves.toEqual(
      expect.objectContaining({
        event: expect.objectContaining({ eventId: ID.deposit.cborHex }),
      }),
    );
  });

  it("answers unavailable for a forced order whose history pruning removed", async () => {
    const follower = await openFollowerUserEvents({ deployment, k: K });
    opened.push(follower);
    const chain = syntheticChain();
    const initializationTx = initializationTransaction(deployment.authority);
    const commitTx = commitTransaction(
      deployment.authority,
      transactionHash(initializationTx),
      createSyntheticStateQueueHeader(),
    );
    const forcedTx = forcedOrderTransaction(deployment, ID.forced);
    chain.next([initializationTx]);
    chain.next([forcedTx, commitTx]);
    // The order is consumed and its token burned: its unit leaves L1.
    chain.next([forcedOrderBurnTransaction(deployment, ID.forced, forcedTx)]);
    chain.empties(RELEASE_FINALITY_DEPTH - 2);
    await follower.apply(chain.blocks);
    const header = (await follower.observe()).finalizedHeaders[0]!;
    // An open objective pins the header; the forced order's unit is not
    // held until a capture names it.
    await expect(
      follower.proofRetention.pin({
        category: "validationTraceDispute",
        headerHash: header.headerHash,
      }),
    ).resolves.toEqual({ kind: "pinned" });
    chain.empties(K + 2);
    await follower.apply(chain.blocks.slice(-(K + 2)));
    await follower.pruneAll();
    await unavailable(
      follower.userEvents.eventAuthority({
        kind: "forced_order",
        eventId: ID.forced.cborHex,
        throughHeader: header,
      }),
      /history was pruned/u,
    );
  });
});
