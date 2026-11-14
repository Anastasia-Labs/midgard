import { depth, type FactStore, isFinal } from "@al-ft/midgard-l1-follower";
import { afterAll, describe, expect, it } from "vitest";

import { reconcileStandingOf } from "../../src/committee-service.l1-tick.js";
import type { StateQueueHeaderRecord } from "../../src/domain.js";
import { payloadRecord } from "../store-retention.header-record.js";
import {
  committeeOnQueueChain,
  countingCoordinator,
  factStoreDialects,
  reconcilerFor,
} from "./committee-harness.js";
import { SIM_DEPTHS } from "./queue-sim.js";

/**
 * Plan §8.2: whether a live header is owed an L1 reconciliation is read
 * from the tick's landed-queue facts, never from the stored `l1_reconcile`
 * outbox record. The member posts while its follower reads the header's
 * output unattested at a safe depth, whatever that record says; it posts
 * nothing while the output reads attested, and nothing once the attested
 * output is final (depth > k).
 *
 * Posts are counted at the coordinator the real submitter reconciler calls.
 */
const { databases, dialects } = factStoreDialects(
  "midgard_test_c3fix_reconcile",
);
afterAll(async () => {
  await databases.dropAll();
}, 120_000);

/**
 * One queued header with a verified payload, at depth cd (safe): unattested,
 * so owed.
 */
const owedHeader = async (factStore: FactStore) => {
  const harness = await committeeOnQueueChain(factStore);
  const { queue, apply, config, store } = harness;
  await apply(queue.init());
  await apply(queue.append());
  const node = queue.nodes[0]!;
  await store.saveDaPayload(
    payloadRecord(node.hash, config.deploymentFingerprint),
  );
  const appendedAt = queue.chain.tip.height;
  await apply(queue.empty());
  expect(depth(queue.chain.tip.height, appendedAt)).toBe(
    SIM_DEPTHS.confirmationDepth,
  );
  const outRef = `${node.outRef.txHash.toString("hex")}#${node.outRef.index.toString()}`;
  const coordinator = countingCoordinator();
  const service = await harness.service({
    submitterReconciler: reconcilerFor(config, store, coordinator),
  });
  const stored = async (): Promise<StateQueueHeaderRecord> =>
    (await store.getStateQueueHeader(node.hash))!;
  return {
    ...harness,
    build: harness.service,
    node,
    outRef,
    coordinator,
    service,
    stored,
  };
};

describe.each(dialects)(
  "l1_reconcile from the tick's facts (%s)",
  (_, open) => {
    it(
      "posts again when the header's output reads unattested after a rollback at depth <= k",
      { timeout: 120_000 },
      async () => {
        const factStore = await open();
        try {
          const { queue, apply, coordinator, service, outRef, stored } =
            await owedHeader(factStore);
          await service.tick();
          expect(coordinator.posts).toEqual([outRef]);

          // The attestation lands; while its output is shallow, and once it
          // is safe, nothing is posted.
          await apply(queue.attest());
          const attestedAt = queue.chain.tip.height;
          await service.tick();
          await apply(queue.empty());
          await expect(service.tick()).resolves.toMatchObject({
            reconciledHeaders: 1,
          });
          expect(coordinator.posts).toEqual([outRef]);

          // The rollback takes the attestation back: the output the header
          // stands on is the unattested one the first post was for.
          const rolledBack = depth(queue.chain.tip.height, attestedAt);
          expect(isFinal(rolledBack, SIM_DEPTHS)).toBe(false);
          await apply(queue.rollBack(rolledBack));
          await service.tick();
          expect((await stored()).stateQueueOutRef).toBe(outRef);
          expect(coordinator.posts).toEqual([outRef, outRef]);
        } finally {
          await factStore.close();
        }
      },
    );

    it(
      "posts again while the header's output still reads unattested after a reported submit",
      { timeout: 120_000 },
      async () => {
        const factStore = await open();
        try {
          const { queue, apply, coordinator, service, outRef } =
            await owedHeader(factStore);
          await service.tick();
          expect(coordinator.posts).toEqual([outRef]);
          // The submit reported posted, but no attestation lands.
          await apply(queue.empty());
          await service.tick();
          expect(coordinator.posts).toEqual([outRef, outRef]);
        } finally {
          await factStore.close();
        }
      },
    );

    it(
      "posts nothing once the attested output is final",
      { timeout: 120_000 },
      async () => {
        const factStore = await open();
        try {
          const { queue, apply, coordinator, service, outRef } =
            await owedHeader(factStore);
          await service.tick();
          await apply(queue.attest());
          const attestedAt = queue.chain.tip.height;
          while (
            !isFinal(depth(queue.chain.tip.height, attestedAt), SIM_DEPTHS)
          ) {
            await service.tick();
            await apply(queue.empty());
          }
          for (let i = 0; i < 3; i += 1) {
            await expect(service.tick()).resolves.toMatchObject({
              reconciledHeaders: 1,
            });
            await apply(queue.empty());
          }
          expect(coordinator.posts).toEqual([outRef]);
        } finally {
          await factStore.close();
        }
      },
    );

    it(
      "posts once while the first l1_reconcile attempt is in flight at a safe depth",
      { timeout: 120_000 },
      async () => {
        const factStore = await open();
        try {
          const harness = await owedHeader(factStore);
          const { queue, apply, config, store, coordinator, service, outRef } =
            harness;
          coordinator.hold();
          const first = service.tick();
          while (coordinator.posts.length === 0)
            await new Promise((resolve) => setImmediate(resolve));
          // The same member's next tick joins the running one; a second
          // service on the same store defers the effect the first holds.
          const joined = service.tick();
          const other = await harness.build({
            submitterReconciler: reconcilerFor(config, store, coordinator),
          });
          await expect(other.tick()).resolves.toMatchObject({ errors: [] });
          expect(coordinator.posts).toEqual([outRef]);

          // The first attempt's attestation lands, then the attempt returns.
          await apply(queue.attest());
          coordinator.release();
          await first;
          await joined;
          await other.tick();
          await apply(queue.empty());
          await other.tick();
          await service.tick();
          expect(coordinator.posts).toEqual([outRef]);
        } finally {
          await factStore.close();
        }
      },
    );
  },
);

describe("reconcileStandingOf", () => {
  const record = (
    status: StateQueueHeaderRecord["status"],
    finalized: boolean,
    atDepth: number,
  ) =>
    ({
      status,
      finalized,
      observedChainPoint: { depth: atDepth, finalized },
    }) as StateQueueHeaderRecord;
  const K = SIM_DEPTHS.securityParameter;

  it("reads an unattested or attesting output at a safe depth as owed", () => {
    expect(reconcileStandingOf(record("unattested", true, 2), SIM_DEPTHS)).toBe(
      "owed",
    );
    expect(
      reconcileStandingOf(record("attesting", true, K + 5), SIM_DEPTHS),
    ).toBe("owed");
  });

  it("reads an attested output as final only at depth > k", () => {
    expect(reconcileStandingOf(record("attested", true, K), SIM_DEPTHS)).toBe(
      "attested",
    );
    expect(
      reconcileStandingOf(record("attested", true, K + 1), SIM_DEPTHS),
    ).toBe("final");
  });

  it("reads an output below a safe depth, or out of scope, as waiting", () => {
    expect(
      reconcileStandingOf(record("unattested", false, 1), SIM_DEPTHS),
    ).toBe("waiting");
    expect(reconcileStandingOf(record("conflicted", true, 3), SIM_DEPTHS)).toBe(
      "waiting",
    );
  });
});
