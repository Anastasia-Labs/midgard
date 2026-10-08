/**
 * The follower's event refusals reach the watcher as one status-only
 * degradation: a user's malformed order is counted and named, never a
 * readiness reason.
 */
import { afterEach, describe, expect, it } from "vitest";

import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  eventRefusalDegradationsIn,
  L1_USER_EVENT_REFUSED,
} from "../../src/l1-follower/event-refusals.js";
import {
  type FollowerUserEvents,
  followerUserEventsDeployment,
  listOrderTransaction,
  openFollowerUserEvents,
  syntheticChain,
  syntheticTransaction,
  transactionHash,
  type UserEventId,
  userEventId,
} from "../support/follower-user-events-fixture.js";

const deployment = followerUserEventsDeployment();
const K = RELEASE_FINALITY_DEPTH + 2;

const opened: FollowerUserEvents[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((follower) => follower.close()));
});

/** A deposit list output whose datum is no list node. */
const malformedOrder = (event: UserEventId): string => {
  const list = deployment.scripts.eventProjection.lists.find(
    (entry) => entry.kind === "deposit",
  )!;
  return syntheticTransaction({
    inputs: [event.nonce],
    outputs: [
      {
        addressHex: list.listAddress,
        lovelace: 7_000_000n,
        units: [[list.policyId, event.key, 1n]],
        datumCbor: "d87980",
      },
    ],
    mint: [[list.policyId, event.key, 1n]],
  });
};

const degradations = (follower: FollowerUserEvents) =>
  follower.store.transaction("read", eventRefusalDegradationsIn);

describe("watcher event refusal degradations", () => {
  it("counts each refused order by reason and names the newest", async () => {
    const follower = await openFollowerUserEvents({ deployment, k: K });
    opened.push(follower);
    const chain = syntheticChain();
    chain.next([
      listOrderTransaction(deployment, "deposit", userEventId("a1")),
    ]);
    await follower.apply(chain.blocks);
    expect(await degradations(follower)).toEqual([]);

    const first = malformedOrder(userEventId("a2"));
    const second = malformedOrder(userEventId("a3"));
    chain.next([first]);
    chain.next([second]);
    await follower.apply(chain.blocks.slice(-2));
    const [refused, ...rest] = await degradations(follower);
    expect(rest).toEqual([]);
    expect(refused).toMatchObject({ reason: L1_USER_EVENT_REFUSED, count: 2 });
    expect(refused!.detail).toContain("malformed=2");
    expect(refused!.detail).toContain(
      `deposit order ${transactionHash(second)}#0`,
    );
    expect(refused!.detail).not.toContain(transactionHash(first));
  });

  it("drops a refusal once it is k deep and pruned", async () => {
    const follower = await openFollowerUserEvents({ deployment, k: K });
    opened.push(follower);
    const chain = syntheticChain();
    chain.next([malformedOrder(userEventId("b1"))]);
    await follower.apply(chain.blocks);
    expect((await degradations(follower))[0]?.count).toBe(1);
    chain.empties(K + 2);
    await follower.apply(chain.blocks.slice(-(K + 2)));
    await follower.pruneAll();
    expect(await degradations(follower)).toEqual([]);
  });
});
