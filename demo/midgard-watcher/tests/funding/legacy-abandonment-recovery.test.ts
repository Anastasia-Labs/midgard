import { rm } from "node:fs/promises";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it } from "vitest";

import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  abandonmentHandoff,
  completionHandoff,
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
  "reconstructs legacy abandonment leases (completed: %s) and holds them only until final completion or fresh deep canonical proof",
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
    const handoff = abandonmentHandoff(
      owner,
      signed.transactionHash,
      "proof.init",
    );
    const abandoned = await opened.runtime.store.abandonPendingTransition({
      plan: owner,
      expectedRevision: pending.revision,
      transitionDigest: pending.pendingTransition!.transitionDigest,
      handoff,
    });
    const acknowledged = await opened.runtime.store.acknowledgeAbandonment({
      plan: owner,
      expectedRevision: abandoned.revision,
      handoff,
    });
    const idle = await opened.runtime.store.releaseIdle!({
      plan: owner,
      expectedRevision: acknowledged.revision,
    });
    if (completed)
      await opened.runtime.store.release({
        plan: owner,
        expectedRevision: idle.revision,
        handoff: completionHandoff(owner),
      });
    opened.runtime.close();
    // Simulate the exact pre-fix persisted format; this fixture is not a deployment reset.
    const database = new DatabaseSync(opened.path);
    const row = database
      .prepare(
        "SELECT canonical_json FROM watcher_prover_funding_abandonment_v1",
      )
      .get() as { canonical_json: string };
    const legacy = JSON.parse(row.canonical_json);
    delete legacy.handoff.reconciliation.retirement;
    database
      .prepare(
        "UPDATE watcher_prover_funding_abandonment_v1 SET canonical_json=?, record_digest=?",
      )
      .run(
        watcherCanonicalJson(legacy),
        computeDeploymentManifestJsonDigest(legacy),
      );
    database.close();
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
        // Final completion ends the legacy attempt's leases; retirement past
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
        await reopened.store.readLegacyAbandonedTransactions!({
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
        reopened.store.retireLegacyAbandonment!({
          plan: owner,
          expectedRevision: before.revision,
          transactionHash: signed.transactionHash,
          retirement: shallow,
        }),
      ).rejects.toThrow();
      expect(await reopened.store.readReservedOutRefs({})).toContain(
        `${"11".repeat(32)}#0`,
      );
      const after = await reopened.store.retireLegacyAbandonment!({
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
