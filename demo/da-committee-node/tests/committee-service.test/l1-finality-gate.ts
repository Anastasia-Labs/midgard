import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import { L1_STATE_QUEUE_UNHEALTHY } from "../../src/committee-service.l1-tick.js";
import { nodeDatum, queueOutput } from ".././l1-follower/queue-sim.js";
import { CD, commit, type Harness, harness, readyTick } from "./l1-harness.js";

/**
 * The signing gate on the follower's view: a header record is final, and
 * signable, only once its node output is cd deep in a healthy landed queue.
 */
export const registerL1FinalityGateTests = () => {
  describe("the signing finality gate on the follower", () => {
    const open: Harness[] = [];
    afterEach(async () => {
      for (const h of open.splice(0)) await h.close();
    });
    const start = async (): Promise<Harness> => {
      const h = await harness();
      open.push(h);
      return h;
    };
    const finalized = async (h: Harness, headerHash: string) =>
      (await h.committeeStore.getStateQueueHeader(headerHash))?.finalized;

    it("signs a header only once its commit is cd deep: nothing at cd - 1, one signature a block later", async () => {
      const h = await start();
      const first = await h.header(3);
      h.forward();
      commit(h, first, CD - 1);
      await h.synced();
      await expect(readyTick(h)).resolves.toMatchObject({
        scannedHeaders: 1,
        signedHeaders: 0,
      });
      expect(await finalized(h, first.headerHash)).toBe(false);
      expect(await h.committeeStore.listDaSignatures(first.headerHash)).toEqual(
        [],
      );

      h.forward();
      await h.synced();
      await expect(readyTick(h)).resolves.toMatchObject({ signedHeaders: 1 });
      expect(await finalized(h, first.headerHash)).toBe(true);
      expect(
        await h.committeeStore.listDaSignatures(first.headerHash),
      ).toHaveLength(1);
    });

    it("holds on an unhealthy landed queue (an orphan node) and signs nothing, the header not final at any depth", async () => {
      const h = await start();
      const first = await h.header(3);
      const orphan = await h.header(4);
      h.forward();
      // A node output the root's links never reach.
      h.forward([
        {
          inputs: [h.chain.outsideInput()],
          outputs: [
            queueOutput(
              `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${orphan.headerHash}`,
              nodeDatum(orphan.header, "Unattested", null),
            ),
          ],
          nonce: h.chain.nonce(),
        },
      ]);
      commit(h, first, CD + 1);
      await h.synced();
      for (let i = 0; i < 2; i += 1) {
        const result = await h.service.tick();
        expect(result.signedHeaders).toBe(0);
        expect(result.held).toEqual([
          expect.stringMatching(
            new RegExp(`^${L1_STATE_QUEUE_UNHEALTHY}: orphan_node `, "u"),
          ),
        ]);
      }
      expect(await finalized(h, first.headerHash)).toBe(false);
      expect(await h.committeeStore.listDaSignatures(first.headerHash)).toEqual(
        [],
      );
    });
  });
};
