import { afterEach, describe, expect, it } from "vitest";

import { AvailabilityResponderAwaitingScanError as AwaitingScan } from "../../src/availability/awaiting-scan-error.js";
import { deferredAvailabilityResponder } from "../../src/availability/deferred-responder.js";
import {
  CD,
  commit,
  type Harness,
  harness,
  K,
  readyTick,
} from "./l1-harness.js";

export const registerL1AbsorptionTests = () => {
  describe("L1 absorption on the follower (rollbacks below k)", () => {
    const open: Harness[] = [];
    afterEach(async () => {
      for (const h of open.splice(0)) await h.close();
    });
    const start = async (): Promise<Harness> => {
      const h = await harness();
      open.push(h);
      return h;
    };

    // The commit is signed at depth cd, so a rollback of cd or more blocks
    // takes its block off the chain.
    it.each([
      [1, false],
      [CD, true],
      [CD + 1, true],
    ] as const)(
      "absorbs a rollback of depth %i across a signed decision: it keeps ticking, is ready within one tick, and keeps the decision without re-signing it",
      async (rollback, removesCommit) => {
        const h = await start();
        const first = await h.header(3);
        h.forward();
        h.forward();
        commit(h, first);
        await h.synced();
        await expect(readyTick(h)).resolves.toMatchObject({
          signedHeaders: 1,
        });
        const decided = await h.committeeStore.listDaSignatures(
          first.headerHash,
        );
        expect(decided).toHaveLength(1);

        h.backward(rollback);
        await h.synced();
        // The first tick after the rollback decides again, and is ready.
        await expect(readyTick(h)).resolves.toMatchObject({
          signedHeaders: 0,
        });
        expect(
          await h.committeeStore.listDaSignatures(first.headerHash),
        ).toEqual(decided);
        expect(
          (await h.committeeStore.listSignedDecisions()).map(
            ({ headerHash }) => headerHash,
          ),
        ).toEqual([first.headerHash]);

        // The commit lands again (the same transaction) when it was rolled
        // back, and the chain grows past cd: still the one decision.
        if (removesCommit) commit(h, first);
        else for (let i = 0; i < rollback; i += 1) h.forward();
        await h.synced();
        await expect(readyTick(h)).resolves.toMatchObject({
          scannedHeaders: 1,
          signedHeaders: 0,
        });
        expect(
          await h.committeeStore.listDaSignatures(first.headerHash),
        ).toEqual(decided);
      },
    );

    it("signs the sibling header committed on the fork that replaced a signed one's commit", async () => {
      const h = await start();
      const first = await h.header(3);
      const sibling = await h.header(4);
      h.forward();
      h.forward();
      commit(h, first);
      await h.synced();
      await expect(readyTick(h)).resolves.toMatchObject({ signedHeaders: 1 });

      h.backward(CD);
      commit(h, sibling);
      await h.synced();
      await expect(readyTick(h)).resolves.toMatchObject({
        scannedHeaders: 1,
        signedHeaders: 1,
      });
      // The replaced header's commit is on no chain and the latest final
      // block is past its end time: `cannot_land`, its decision deleted (§11).
      const endTimeMs = Number(first.header.endTime);
      const view = h.service.latestL1View();
      expect(view?.finalBlockTimeMs).toBeGreaterThan(endTimeMs);
      const decided = await h.committeeStore.listSignedDecisions();
      expect(decided).toMatchObject([{ headerHash: sibling.headerHash }]);
      const firstRows = await h.committeeStore.listDaSignatures(
        first.headerHash,
      );
      expect(firstRows).toEqual([]);
    });

    it("stays ready once every other output of the protocol-init tx is spent k deep: the hub oracle keeps it", async () => {
      const h = await start();
      const first = await h.header(3);
      h.correct();
      commit(h, first, K + 2);
      const status = await h.synced();
      expect(status.prune.prunedThroughSlot).toBeGreaterThan(0);
      expect(status.protocolInit).toBe("seen");
      await expect(readyTick(h)).resolves.toMatchObject({ signedHeaders: 1 });
    });

    it("holds, unready with rollback_beyond_k, on a rollback deeper than k, the process up and the decision kept", async () => {
      const h = await start();
      const first = await h.header(3);
      h.forward();
      h.forward();
      commit(h, first, K + 1);
      await h.synced();
      await expect(readyTick(h)).resolves.toMatchObject({ signedHeaders: 1 });
      const decided = await h.committeeStore.listDaSignatures(first.headerHash);
      const tip = await h.availabilityReads.readBoundary();
      const idle = { challenges: 0, status: "idle" as const };
      const responder = deferredAvailabilityResponder(h.l1, async () => ({
        responder: { drain: async () => idle },
        close: () => undefined,
      }));

      h.backward(K + 1);
      await h.reached((current) => current.state === "intervention");
      for (let i = 0; i < 2; i += 1) {
        const result = await h.service.tick();
        expect(result.signedHeaders).toBe(0);
        expect(result.held).toEqual([
          expect.stringMatching(/^rollback_beyond_k: /u),
        ]);
        const snapshot = await h.service.readinessSnapshot();
        expect(snapshot.ready).toBe(false);
        expect(snapshot.reasons).toEqual([
          expect.stringMatching(/^rollback_beyond_k: /u),
        ]);
        expect(snapshot.l1Source).toMatchObject({
          status: "intervention",
          intervention: expect.stringMatching(/^rollback_beyond_k: /u),
        });
        // Availability, promise and retirement reads hold on it too.
        const refused = h.availabilityReads.readBoundary();
        await expect(refused).rejects.toBeInstanceOf(AwaitingScan);
        await expect(refused).rejects.toThrow(/rollback_beyond_k: /u);
        await expect(responder.responder.drain()).resolves.toMatchObject({
          status: "awaiting_scan",
          detail: expect.stringMatching(/rollback_beyond_k: /u),
        });
      }
      // The store kept its view: only the readiness gate holds the reads.
      await expect(h.availabilityReads.viewValid(tip.view)).resolves.toBe(true);
      // The loop stopped on the intervention without failing; the
      // committee keeps ticking and serving its readiness.
      expect(h.loopOutcome()).not.toBe("rejected");
      expect(h.status()).toMatchObject({ state: "intervention" });
      expect(await h.committeeStore.listDaSignatures(first.headerHash)).toEqual(
        decided,
      );
    });
  });
};
