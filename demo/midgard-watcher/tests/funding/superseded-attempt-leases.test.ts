import { rm } from "node:fs/promises";

import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it } from "vitest";

import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import {
  openStore,
  plan,
  prepareTransition,
  retirementFixture,
  signedTransition,
  temporaryDirectories,
} from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

it.each([false, true])(
  "holds an unretired superseded attempt's leases across restart (completed: %s) only until final completion or a deep retirement",
  async (completed) => {
    const opened = await openStore();
    const owner = plan("aa", "66");
    await opened.runtime.store.reserve(owner);
    const signed = signedTransition({ validityUpperBound: 100n });
    const pending = await prepareTransition(opened.runtime.store, {
      plan: owner,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...signed,
      consumedOutRefs: [`${"11".repeat(32)}#0`],
    });
    const abandoned = await opened.runtime.store.abandonPendingTransition({
      journal: [],
      plan: owner,
      expectedRevision: pending.revision,
      transitionDigest: pending.pendingTransition!.transitionDigest,
    });
    const idle = await opened.runtime.store.releaseIdle!({
      plan: owner,
      expectedRevision: abandoned.revision,
    });
    if (completed)
      await opened.runtime.store.release({
        journal: [],
        plan: owner,
        expectedRevision: idle.revision,
      });
    opened.runtime.close();
    const reopened =
      await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
        { path: opened.path },
        () => undefined,
      );
    try {
      const before = (await reopened.store.readAll())[0] as {
        revision: string;
      };
      if (completed) {
        // Final completion ends the superseded attempt's leases; retirement past
        // k is bookkeeping only and never holds funding.
        expect(await reopened.store.readReservedOutRefs({})).not.toContain(
          `${"11".repeat(32)}#0`,
        );
        await expect(reopened.store.reserve(plan("bb", "77"))).resolves.toBe(
          "reserved",
        );
        return;
      }
      expect(await reopened.store.readReservedOutRefs({})).toContain(
        `${"11".repeat(32)}#0`,
      );
      await expect(reopened.store.reserve(plan("bb", "77"))).rejects.toThrow(
        "reserved",
      );
      expect(
        await reopened.store.readSupersededAttempts!({
          reservationId: owner.reservationId,
        }),
      ).toMatchObject([
        {
          transition: {
            transactionHash: signed.transactionHash,
            signedTransactionCborHex: signed.signedTransactionCborHex,
          },
        },
      ]);
      const certificate = retirementFixture(signed.transactionHash);
      const shallowPoint = { ...certificate.canonicalPoint, blockNo: "2210" };
      const shallow = {
        ...certificate,
        canonicalPoint: {
          ...shallowPoint,
          pointId: computeFraudProofRawL1PointId(shallowPoint),
        },
      };
      await expect(
        reopened.store.retireSupersededAttempt!({
          journal: [],
          plan: owner,
          expectedRevision: before.revision,
          transactionHash: signed.transactionHash,
          retirement: shallow,
        }),
      ).rejects.toThrow();
      expect(await reopened.store.readReservedOutRefs({})).toContain(
        `${"11".repeat(32)}#0`,
      );
      const after = await reopened.store.retireSupersededAttempt!({
        journal: [],
        plan: owner,
        expectedRevision: before.revision,
        transactionHash: signed.transactionHash,
        retirement: certificate,
      });
      expect(after.pendingTransition).toBeNull();
      expect(await reopened.store.readReservedOutRefs({})).not.toContain(
        `${"11".repeat(32)}#0`,
      );
      await expect(reopened.store.reserve(plan("bb", "77"))).resolves.toBe(
        "reserved",
      );
    } finally {
      reopened.close();
    }
  },
);
