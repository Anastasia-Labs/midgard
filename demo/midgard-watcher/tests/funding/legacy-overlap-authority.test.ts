import "./prover-funding-recovery.registration.js";

import { writeFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs";
import { expect, it, vi } from "vitest";

import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import { setupFundingRecoveryFixture as setup } from "../support/fault-proof-funding-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

it("opens the existing production funding permit read-only under a legacy overlap and restores authority only after retirement", async () => {
  const test = await setup();
  test.useUnspentPendingInputs();
  vi.mocked(test.adapter.reconcile).mockResolvedValue(
    expiredNotFound(test.transactionHash),
  );
  await test.run(await test.recover());
  const later = {
    ...test.plan,
    reservationId: "bb".repeat(32),
    decisionDigest: "77".repeat(32),
  };
  // Only fixture creation uses the unsafe seam; admission and recovery below
  // exercise the actual deployed store and production authority factory.
  const fixtureStore =
    await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
      { path: join(test.journalRoot, "watcher.sqlite") },
      () => undefined,
    );
  await fixtureStore.store.reserve(later);
  fixtureStore.close();
  const laterBefore = (await test.records()).find(
    ({ reservationId }) => reservationId === later.reservationId,
  );
  const db = new DatabaseSync(join(test.journalRoot, "watcher.sqlite"));
  const row = db
    .prepare(
      "SELECT canonical_json FROM watcher_prover_funding_abandonment_v1 WHERE reservation_id=?",
    )
    .get(test.plan.reservationId) as { canonical_json: string };
  const legacy = JSON.parse(row.canonical_json);
  delete legacy.handoff.reconciliation.retirement;
  db.prepare(
    "UPDATE watcher_prover_funding_abandonment_v1 SET canonical_json=?,record_digest=? WHERE reservation_id=?",
  ).run(
    watcherCanonicalJson(legacy),
    computeDeploymentManifestJsonDigest(legacy),
    test.plan.reservationId,
  );
  db.close();
  // This local fixture represents the previous journal format as well as SQL.
  for (const entry of await test.journal.load(test.initial.workflowId)) {
    if (
      entry.event.kind !== "reconciled" ||
      entry.event.outcome !== "not_found"
    )
      continue;
    const { retirement: _retirement, ...event } = entry.event;
    await writeFile(
      join(
        test.journalDirectory,
        test.initial.workflowId,
        `${entry.sequence.toString().padStart(8, "0")}.json`,
      ),
      JSON.stringify({ ...entry, event }) + "\n",
    );
  }
  await test.restartStore();
  const held = async () =>
    await Promise.all(
      [test.plan, later].map(
        async ({ reservationId }) =>
          await test.store.isReconciliationOnly!({ reservationId }),
      ),
    );
  expect(await held()).toEqual([true, true]);
  const admitted = await test.createPermit(test.fresh, "2");
  const journal = test.bind(test.fresh, admitted);
  await expect(
    beginWorkflowFundingReservationAction({
      journal,
      action: { actionId: "next", input: { actionKind: "proof.init" } },
    }),
  ).rejects.toThrow("reconciliation only");
  admitted.controller.restrictToReconciliation(
    "recover old funding promise first",
  );
  vi.mocked(test.adapter.reconcile).mockResolvedValue({
    kind: "unknown",
    reason: "canonical evidence unavailable",
  });
  await test.run(journal);
  expect(await held()).toEqual([true, true]);
  expect(
    (await test.records()).find(
      ({ reservationId }) => reservationId === later.reservationId,
    ),
  ).toEqual(laterBefore);
  vi.mocked(test.adapter.reconcile).mockResolvedValue(
    expiredNotFound(test.transactionHash),
  );
  await test.run(journal);
  expect(await held()).toEqual([false, false]);
  expect(
    (await test.records()).find(
      ({ reservationId }) => reservationId === later.reservationId,
    ),
  ).toEqual(laterBefore);
  expect(await test.store.readReservedOutRefs({})).toContain(
    later.inputs[0]!.outRef,
  );
  expect(test.adapter.preflight).not.toHaveBeenCalled();
  expect(test.adapter.submit).not.toHaveBeenCalled();
});
