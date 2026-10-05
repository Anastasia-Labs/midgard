import { rm } from "node:fs/promises";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it } from "vitest";

import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest as reopen } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
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

it.each([
  { completed: false, missingSubmission: false },
  { completed: true, missingSubmission: false },
  { completed: false, missingSubmission: true },
  { completed: true, missingSubmission: true },
])(
  "reconciles overlapping legacy ownership after restart ($completed, missing submission $missingSubmission) while preserving the later pending attempt",
  async ({ completed, missingSubmission }) => {
    const opened = await openStore();
    const old = plan("aa", "66");
    const later = plan("bb", "77");
    const original = signedTransition({ validityUpperBound: 100n });
    const current = signedTransition({ validityUpperBound: 200n });
    const outRef = old.inputs[0]!.outRef;
    await opened.runtime.store.reserve(old);
    const pending = await prepareTransition(opened.runtime.store, {
      plan: old,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...original,
      consumedOutRefs: [outRef],
    });
    const handoff = abandonmentHandoff(
      old,
      original.transactionHash,
      "proof.init",
    );
    const abandoned = await opened.runtime.store.abandonPendingTransition({
      plan: old,
      expectedRevision: pending.revision,
      transitionDigest: pending.pendingTransition!.transitionDigest,
      handoff,
    });
    const acknowledged = await opened.runtime.store.acknowledgeAbandonment({
      plan: old,
      expectedRevision: abandoned.revision,
      handoff,
    });
    const idle = await opened.runtime.store.releaseIdle!({
      plan: old,
      expectedRevision: acknowledged.revision,
    });
    if (completed)
      await opened.runtime.store.release({
        plan: old,
        expectedRevision: idle.revision,
        handoff: completionHandoff(old),
      });
    await opened.runtime.store.reserve(later);
    const laterPending = await prepareTransition(opened.runtime.store, {
      plan: later,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...current,
      consumedOutRefs: [outRef],
    });
    const laterHandoff = await opened.runtime.store.readPendingHandoff({
      reservationId: later.reservationId,
    });
    opened.runtime.close();
    // Fabricate a genuine old-format abandonment, with its exact signed bytes;
    // this is an isolated database fixture, never a deployment reset.
    const db = new DatabaseSync(opened.path);
    const row = db
      .prepare(
        "SELECT canonical_json FROM watcher_prover_funding_abandonment_v1",
      )
      .get() as { canonical_json: string };
    const legacy = JSON.parse(row.canonical_json);
    delete legacy.handoff.reconciliation.retirement;
    db.prepare(
      "UPDATE watcher_prover_funding_abandonment_v1 SET canonical_json=?, record_digest=?",
    ).run(
      watcherCanonicalJson(legacy),
      computeDeploymentManifestJsonDigest(legacy),
    );
    if (missingSubmission)
      db.prepare(
        "DELETE FROM watcher_prover_funding_handoff_v1 WHERE reservation_id=? AND kind='submission'",
      ).run(old.reservationId);
    db.close();
    let runtime = await reopen({ path: opened.path }, () => undefined);
    try {
      expect(await runtime.store.isReconciliationOnly!()).toBe(true);
      await expect(runtime.store.assertSubmissionAuthority!()).rejects.toThrow(
        "reconciliation only",
      );
      await expect(runtime.store.reserve(later)).rejects.toThrow(
        "reconciliation only",
      );
      for (const excludingReservationId of [
        old.reservationId,
        later.reservationId,
      ])
        expect(
          await runtime.store.readReservedOutRefs({ excludingReservationId }),
        ).toContain(outRef);
      const attempts = await runtime.store.readLegacyAbandonedTransactions!({
        reservationId: old.reservationId,
      });
      expect(attempts).toMatchObject([{ transition: original }]);
      const before = (
        (await runtime.store.readAll()) as {
          reservationId: string;
          revision: string;
        }[]
      ).find((record) => record.reservationId === old.reservationId)!;
      const certificate = retirementFixture(original.transactionHash);
      const point = { ...certificate.canonicalPoint, blockNo: "2210" };
      await expect(
        runtime.store.retireLegacyAbandonment!({
          plan: old,
          expectedRevision: before.revision,
          transactionHash: original.transactionHash,
          retirement: {
            ...certificate,
            canonicalPoint: {
              ...point,
              pointId: computeFraudProofRawL1PointId(point),
            },
          },
        }),
      ).rejects.toThrow();
      expect(await runtime.store.isReconciliationOnly!()).toBe(true);
      expect(
        await runtime.store.readLegacyAbandonedTransactions!({
          reservationId: old.reservationId,
        }),
      ).toEqual(attempts);
      await runtime.store.retireLegacyAbandonment!({
        plan: old,
        expectedRevision: before.revision,
        transactionHash: original.transactionHash,
        retirement: certificate,
      });
      expect(await runtime.store.isReconciliationOnly!()).toBe(false);
      await expect(
        runtime.store.assertSubmissionAuthority!(),
      ).resolves.toBeUndefined();
      // BB still owns the original funding output; retirement cannot free it.
      expect(await runtime.store.readReservedOutRefs({})).toContain(outRef);
      expect(
        (await runtime.store.readAll()).find(
          (record) =>
            (record as { reservationId: string }).reservationId ===
            later.reservationId,
        ),
      ).toEqual(laterPending);
      expect(
        await runtime.store.readPendingHandoff({
          reservationId: later.reservationId,
        }),
      ).toEqual(laterHandoff);
      expect(await runtime.store.reserve(later)).toBe("unchanged");
      runtime.close();
      runtime = await reopen({ path: opened.path }, () => undefined);
      expect(await runtime.store.isReconciliationOnly!()).toBe(false);
      expect(
        await runtime.store.readPendingHandoff({
          reservationId: later.reservationId,
        }),
      ).toEqual(laterHandoff);
    } finally {
      runtime.close();
    }
  },
);
