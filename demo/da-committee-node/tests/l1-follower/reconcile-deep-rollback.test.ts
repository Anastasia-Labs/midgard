import { depth, isFinal, isSafe } from "@al-ft/midgard-l1-follower";
import { afterAll, describe, expect, it } from "vitest";

import { payloadRecord } from "../store-retention.header-record.js";
import {
  committeeOnQueueChain,
  countingCoordinator,
  factStoreDialects,
  reconcilerFor,
} from "./committee-harness.js";
import { SIM_DEPTHS } from "./queue-sim.js";

/**
 * Plan §8.2 at rollback depths between cd and k: a rollback of k blocks
 * (never final) takes back a header's attestation at depth k or k-1. The
 * member reads the output unattested again and posts once more: at once
 * when the header's node is still safe, or one block later when the
 * rollback also took the block that made it safe (no post below cd). Once
 * the attestation lands again, nothing more is posted, through the tick
 * that reads it final.
 *
 * Posts are counted at the coordinator the real submitter reconciler calls.
 */
const K = SIM_DEPTHS.securityParameter;
const { databases, dialects } = factStoreDialects(
  "midgard_test_c3fix_reconcile_deep",
);
afterAll(async () => {
  await databases.dropAll();
}, 120_000);

describe.each(dialects)(
  "l1_reconcile after a rollback of k blocks (%s)",
  (_, open) => {
    for (const attestationDepth of [K, K - 1]) {
      it(
        `posts again once after the attestation at depth ${attestationDepth.toString()} is rolled back, then stays quiet`,
        { timeout: 120_000 },
        async () => {
          const factStore = await open();
          try {
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
            const outRef = `${node.outRef.txHash.toString("hex")}#${node.outRef.index.toString()}`;
            const coordinator = countingCoordinator();
            const service = await harness.service({
              submitterReconciler: reconcilerFor(config, store, coordinator),
            });
            await service.tick();
            expect(coordinator.posts).toEqual([outRef]);

            await apply(queue.attest());
            const attestedAt = queue.chain.tip.height;
            while (
              depth(queue.chain.tip.height, attestedAt) < attestationDepth
            ) {
              await service.tick();
              await apply(queue.empty());
            }
            await service.tick();
            expect(coordinator.posts).toEqual([outRef]);
            expect(depth(queue.chain.tip.height, attestedAt)).toBe(
              attestationDepth,
            );

            // A rollback of k blocks is never final. At depth k-1 it also
            // takes the block that made the header's node safe.
            expect(isFinal(K, SIM_DEPTHS)).toBe(false);
            await apply(queue.rollBack(K));
            await service.tick();
            const safe = isSafe(
              depth(queue.chain.tip.height, appendedAt),
              SIM_DEPTHS,
            );
            expect(safe).toBe(attestationDepth === K);
            if (!safe) {
              expect(coordinator.posts).toEqual([outRef]);
              await apply(queue.empty());
              await service.tick();
            }
            expect(coordinator.posts).toEqual([outRef, outRef]);

            // The attestation lands again: no further post, through the tick
            // that reads it final and one after.
            await apply(queue.attest());
            const reattestedAt = queue.chain.tip.height;
            while (
              !isFinal(depth(queue.chain.tip.height, reattestedAt), SIM_DEPTHS)
            ) {
              await service.tick();
              await apply(queue.empty());
            }
            await service.tick();
            await apply(queue.empty());
            await service.tick();
            expect(coordinator.posts).toEqual([outRef, outRef]);
          } finally {
            await factStore.close();
          }
        },
      );
    }
  },
);
