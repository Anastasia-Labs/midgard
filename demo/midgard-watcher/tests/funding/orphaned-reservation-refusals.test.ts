import { rm } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  createWorkflowReconciliationPermitController,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WatcherProverFundingOrphanedReservationError } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import {
  admitFresh,
  fundWallet,
  loseJournals,
  orphanReadingFactory,
} from "./orphaned-reservation-fixture.js";
import { retirementFixture } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import type { Fixture } from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const orphanOf = async (fixture: Fixture) =>
  (await fixture.records()).find(
    ({ reservationId }) => reservationId === fixture.plan.reservationId,
  )!;

/** Reconciliation of the original decision after its workflow journal was
 * lost, as a process holding only its recorded entries asks for it. */
const reconcileWithoutJournal = async (fixture: Fixture) => {
  const controller = createWorkflowReconciliationPermitController({
    decision: fixture.old,
    deploymentFingerprint: deploymentIdentity.manifestId,
    rollbackGeneration: "1",
    entries: fixture.originalEntries,
  });
  await rm(fixture.journalDirectory, { recursive: true, force: true });
  const { factory } = orphanReadingFactory(fixture);
  return async () =>
    await factory.create({
      category: "doubleSpend",
      runner: fixture.runner,
      actuationPermit: controller.permit,
      rollbackGeneration: "1",
      decisionDigest: fixture.old.decisionDigest,
      walletAddress,
      walletUtxos: fixture.walletUtxos,
      readWalletUtxos: fixture.readWalletUtxos,
      resolveInputs: fixture.resolveInputs,
      resolveProtocolInputAuthority: async () => {
        throw new Error("unexpected protocol input lookup");
      },
    });
};

describe("orphaned signed funding a fresh execution cannot take over", () => {
  it("reconciliation never hands it over: it holds while an attempt is unretired", async () => {
    const fixture = await setupFundingRecoveryFixture(
      false,
      false,
      false,
      true,
    );
    supersededAttemptReadSchedule.forget(fixture.transactionHash);
    fixture.useUnspentPendingInputs();
    // Absent at the tip: a fresh execution would take it over.
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const reconcile = await reconcileWithoutJournal(fixture);
    const before = await fixture.records();
    await expect(reconcile()).rejects.toBeInstanceOf(
      WatcherProverFundingOrphanedReservationError,
    );
    expect(await fixture.records()).toEqual(before);
  });

  it("reconciliation settles it once every attempt is retired, and still has no workflow to drive", async () => {
    const fixture = await setupFundingRecoveryFixture(
      false,
      false,
      false,
      true,
    );
    supersededAttemptReadSchedule.forget(fixture.transactionHash);
    fixture.chain.mockImplementation(async (txHash) => ({
      kind: "not_found",
      retirement: retirementFixture(txHash),
    }));
    const reconcile = await reconcileWithoutJournal(fixture);
    await expect(reconcile()).rejects.toThrow(
      "reconciliation funding requires its existing durable workflow",
    );
    expect(await orphanOf(fixture)).toMatchObject({
      state: "released",
      activeInputs: [],
      pendingTransition: null,
    });
  });

  it("refuses an attempt recorded under another workflow identity schema", async () => {
    const fixture = await setupFundingRecoveryFixture(
      false,
      false,
      false,
      true,
    );
    await loseJournals(fixture, [fixture.fresh]);
    supersededAttemptReadSchedule.forget(fixture.transactionHash);
    fixture.useUnspentPendingInputs();
    fundWallet(fixture);
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const before = await fixture.records();
    // The orphan's recorded submission names another identity schema.
    const database = new DatabaseSync(
      join(fixture.journalRoot, "watcher.sqlite"),
    );
    const rows = database
      .prepare(
        "SELECT identity_digest, record_digest, canonical_json FROM watcher_prover_funding_handoff_v1 WHERE reservation_id = ? AND kind = 'submission'",
      )
      .all(fixture.plan.reservationId) as {
      identity_digest: string;
      record_digest: string;
      canonical_json: string;
    }[];
    expect(rows).not.toHaveLength(0);
    const write = database.prepare(
      "UPDATE watcher_prover_funding_handoff_v1 SET canonical_json = ?, record_digest = ? WHERE reservation_id = ? AND kind = 'submission' AND identity_digest = ?",
    );
    for (const row of rows) {
      const saved = JSON.parse(row.canonical_json);
      saved.handoff.identity.schemaVersion = 2;
      write.run(
        watcherCanonicalJson(saved),
        computeDeploymentManifestJsonDigest(saved),
        fixture.plan.reservationId,
        row.identity_digest,
      );
    }
    const { factory, plans } = orphanReadingFactory(fixture);
    await expect(admitFresh(fixture, factory)).rejects.toThrow(
      "workflow identity schemaVersion",
    );
    expect(plans).toEqual([]);

    // Restored, the store shows nothing was written meanwhile.
    for (const row of rows)
      write.run(
        row.canonical_json,
        row.record_digest,
        fixture.plan.reservationId,
        row.identity_digest,
      );
    database.close();
    expect(await fixture.records()).toEqual(before);
  });
});
