/**
 * The driver's state-queue hook (plan §4.1, §5.5 P1, N2) over a simulated
 * chain on a SQLite and a Postgres follower store: it runs first on every
 * driver run, publishes the landed queue it read at the applied view, and
 * holds `/readyz` on `state_queue_unhealthy` exactly while the queue is
 * unhealthy. A third party's output at the queue address is ignored; an
 * orphan with a valid datum is unhealthy while the walk is still served; a
 * rollback that removes the orphan, then one that removes the tail,
 * recomputes a healthy queue.
 */
import type { FactStore, OutRef, TrackedSet } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  createFollowerDriver,
  DRIVER_HOOK_ORDER,
  type FollowerChange,
  type SinkResult,
} from "../src/l1-events/driver.js";
import {
  landedElements,
  landedStateQueueHook,
  type LandedStateQueueRead,
  readLandedStateQueueFrom,
  STATE_QUEUE_UNHEALTHY,
  stateQueueProjection,
  stateQueueTrackedSet,
} from "../src/l1-state-queue/index.js";
import { l1FollowerReadiness } from "../src/services/l1-follower.readiness.js";
import { EVENTS_CONFIG } from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
  DROP_ALL_TIMEOUT_MS,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  nodeDatum,
  OTHER_POLICY,
  QUEUE_ADDRESS,
  queueOutput,
  rootDatum,
  SIM_QUEUE_CONFIG,
  simHeader,
} from "./helpers/state-queue-sim.fixtures.js";
import {
  followingAtTip,
  runningFollower,
} from "./readiness-l1-follower.fixture.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

const GENESIS = "00".repeat(28);
const ROOT = SDK.STATE_QUEUE_ROOT_ASSET_NAME;
const PREFIX = SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX;

const union = (a: TrackedSet, b: TrackedSet): TrackedSet => ({
  addresses: new Set([...a.addresses, ...b.addresses]),
  paymentCredentials: new Set([
    ...a.paymentCredentials,
    ...b.paymentCredentials,
  ]),
  policies: new Set([...a.policies, ...b.policies]),
});

const APPLIED: SinkResult = {
  kind: "applied",
  inserted: 0,
  orphans: 0,
  refused: [],
};

type Published = {
  change: FollowerChange["kind"];
  read: LandedStateQueueRead["kind"];
  healthy?: boolean;
  nodes?: number;
  policyOutputs?: number;
};

describe.each(["sqlite", "postgres"] as const)(
  "the landed state-queue hook over a %s follower store",
  (dialect) => {
    const open = storeOpener(dialect, databases);

    it("runs first, publishes P1 at each applied view and holds readiness exactly while the queue is unhealthy", async () => {
      expect(DRIVER_HOOK_ORDER[0]).toBe("landedStateQueue");
      const store = await open(
        [
          eventProjection(EVENTS_CONFIG),
          stateQueueProjection(SIM_QUEUE_CONFIG),
        ],
        6,
      );
      opened.push(store);
      const chain = new ChainDriver(
        store,
        union(
          eventTrackedSet(EVENTS_CONFIG),
          stateQueueTrackedSet(SIM_QUEUE_CONFIG),
        ),
      );
      await chain.init();
      const outside = () => chain.chain.outsideInput();
      const nonce = () => chain.chain.nonce();

      // The root, then one appended block.
      const [rootTx] = await chain.forward([
        {
          inputs: [outside()],
          outputs: [queueOutput(ROOT, rootDatum(GENESIS, null))],
          nonce: nonce(),
        },
      ]);
      const first = simHeader(1, GENESIS);
      const firstHash = SDK.stateQueueHeaderHash(first);
      const rootRef: OutRef = { txHash: rootTx!, index: 0 };
      await chain.forward([
        {
          inputs: [rootRef],
          outputs: [
            queueOutput(ROOT, rootDatum(GENESIS, firstHash)),
            queueOutput(
              PREFIX + firstHash,
              nodeDatum(first, "Unattested", null),
            ),
          ],
          nonce: nonce(),
        },
      ]);

      const published: Published[] = [];
      const driver = createFollowerDriver({
        store,
        config: EVENTS_CONFIG,
        sink: { apply: () => Promise.resolve(APPLIED) },
        hooks: {
          landedStateQueue: landedStateQueueHook({
            store,
            config: SIM_QUEUE_CONFIG,
            publish: (change, read) => {
              published.push({
                change: change.kind,
                read: read.kind,
                ...(read.kind === "ok"
                  ? {
                      healthy: read.queue.healthy,
                      nodes: read.queue.nodes.length,
                      policyOutputs: read.queue.policyOutputCount,
                    }
                  : {}),
              });
              return Promise.resolve();
            },
          }),
        },
      });
      const readiness = () =>
        l1FollowerReadiness(runningFollower(followingAtTip(), driver.holds()))
          .reasons;

      expect(await driver.run()).toMatchObject({ kind: "ran", holds: [] });
      expect(published.at(-1)).toEqual({
        change: "initial",
        read: "ok",
        healthy: true,
        nodes: 1,
        policyOutputs: 2,
      });

      // A third party pays to the queue address: nothing under the policy,
      // or another policy's token on a datum that reads as a root.
      await chain.forward([
        {
          inputs: [outside()],
          outputs: [
            { address: QUEUE_ADDRESS, lovelace: 2_000_000n },
            {
              address: QUEUE_ADDRESS,
              lovelace: 2_000_000n,
              assets: new Map([[OTHER_POLICY, new Map([[ROOT, 1n]])]]),
              datum: rootDatum(GENESIS, null),
            },
          ],
          nonce: nonce(),
        },
      ]);
      expect(await driver.run()).toMatchObject({ holds: [] });
      expect(published.at(-1)).toEqual({
        change: "advance",
        read: "ok",
        healthy: true,
        nodes: 1,
        policyOutputs: 2,
      });
      expect(readiness()).toEqual([]);

      // An orphan with a valid datum: unhealthy, and the walk still served.
      const orphan = simHeader(2, GENESIS);
      await chain.forward([
        {
          inputs: [outside()],
          outputs: [
            queueOutput(
              PREFIX + SDK.stateQueueHeaderHash(orphan),
              nodeDatum(orphan, "Unattested", null),
            ),
          ],
          nonce: nonce(),
        },
      ]);
      const held = await driver.run();
      expect(held).toMatchObject({
        kind: "ran",
        holds: [{ reason: STATE_QUEUE_UNHEALTHY }],
      });
      expect(driver.holds()[0]!.detail).toContain("reason=orphan_node");
      expect(readiness()).toEqual([STATE_QUEUE_UNHEALTHY]);
      const served = await readLandedStateQueueFrom(store, SIM_QUEUE_CONFIG);
      expect(served.kind).toBe("ok");
      if (served.kind !== "ok") return;
      expect(served.queue).toMatchObject({
        healthy: false,
        reason: "orphan_node",
      });
      expect(landedElements(served.queue).map((e) => e.headerHash)).toEqual([
        GENESIS,
        firstHash,
      ]);
      expect(served.queue.strays).toHaveLength(1);

      // Held, the hook runs on every driver run, with nothing moved.
      await driver.run();
      expect(published.at(-1)).toEqual({
        change: "unchanged",
        read: "ok",
        healthy: false,
        nodes: 1,
        policyOutputs: 3,
      });
      expect(readiness()).toEqual([STATE_QUEUE_UNHEALTHY]);

      // The rollback removes the orphan's block: healthy again.
      await chain.backward(1);
      expect(await driver.run()).toMatchObject({
        change: { kind: "rewind" },
        holds: [],
      });
      expect(published.at(-1)).toMatchObject({ healthy: true, nodes: 1 });
      expect(readiness()).toEqual([]);

      // A rollback that removes the tail (and the third-party block): the
      // queue is recomputed at the root alone, healthy.
      await chain.backward(2);
      expect(await driver.run()).toMatchObject({
        change: { kind: "rewind" },
        holds: [],
      });
      expect(published.at(-1)).toEqual({
        change: "rewind",
        read: "ok",
        healthy: true,
        nodes: 0,
        policyOutputs: 1,
      });
    });
  },
);
