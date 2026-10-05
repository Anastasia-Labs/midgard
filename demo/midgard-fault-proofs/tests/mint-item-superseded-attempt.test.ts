import { afterEach, describe, expect, it, vi } from "vitest";

import { createMintItemNonCanonicalCentralJournalAdapter } from "../src/mint-item-non-canonical/central-journal.js";
import * as fundingAuthority from "../src/workflow/funding-reservation-permit.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import * as signedReconciliation from "../src/workflow/signed-transaction-reconciliation.js";
import { retiredNotFound } from "./workflow.make-adapter.js";
import { signedRecoveryFixture } from "./workflow-kupmios-source.signed-recovery-fixture.js";

afterEach(() => {
  vi.restoreAllMocks();
});

/** A mint item whose step01 intent is recorded and absent at the tip. */
const absentStep = async () => {
  const fixture = await signedRecoveryFixture({ ttl: null });
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const store: FraudProofWorkflowJournalStore = {
    load: async () => entries,
    append: async (entry, expected) => {
      if (expected !== entries.length) throw new Error("sequence conflict");
      entries.push(entry);
    },
  };
  const restart = () =>
    createMintItemNonCanonicalCentralJournalAdapter({
      store,
      deploymentFingerprint: "1".repeat(64),
      headerHash: "2".repeat(56),
      decisionDigest: "3".repeat(64),
      transactionConfirmed: async () => false,
    });
  const txHash = fixture.input.transactionHash;
  const first = restart();
  await first.begin("submitStep01", "evidence", "none", "step01");
  await first.boundary(
    "submitStep01",
    "evidence",
    "none",
    "step01",
  )({ txHash, signed: fixture.signed, referenceScripts: [] });
  const reconcile = vi.spyOn(
    signedReconciliation,
    "reconcileSignedWorkflowTransaction",
  );
  reconcile.mockResolvedValue({ kind: "not_found" });
  const absences = () =>
    entries.filter(
      ({ event }) =>
        event.kind === "reconciled" &&
        event.outcome === "not_found" &&
        event.txHash === txHash,
    );
  return { txHash, restart, reconcile, absences };
};

describe("mintItemNonCanonical absent attempt", () => {
  it("replaces an attempt absent without retirement at once when a funding reservation is bound", async () => {
    const { restart, absences } = await absentStep();
    vi.spyOn(
      fundingAuthority,
      "workflowJournalHasFundingReservation",
    ).mockReturnValue(true);
    await restart().reconcile("none");
    expect(absences()).toHaveLength(1);
    expect(absences()[0]!.event).not.toHaveProperty("retirement");
    // The superseded attempt does not hold the family: no wait for k. The
    // bound reservation makes the replacement spend one of its inputs.
    await restart().reconcile("none");
    expect(absences()).toHaveLength(1);
    await expect(
      restart().begin("submitStep01", "evidence", "none", "step01"),
    ).resolves.toBeUndefined();
  });

  it("holds an attempt absent without retirement until retirement when no funding reservation is bound", async () => {
    const { txHash, restart, reconcile, absences } = await absentStep();
    for (let pass = 0; pass < 2; pass += 1)
      await expect(restart().reconcile("none")).rejects.toThrow(
        "absent without retirement, and no funding reservation keeps a replacement exclusive",
      );
    expect(absences()).toHaveLength(0);
    await expect(
      restart().begin("submitStep01", "evidence", "none", "step01"),
    ).rejects.toThrow("must reconcile before another build");
    // Retirement past k resolves it, and only then may a replacement build.
    reconcile.mockResolvedValue(retiredNotFound(txHash));
    await restart().reconcile("none");
    expect(absences()).toHaveLength(1);
    expect(absences()[0]!.event).toHaveProperty("retirement");
    await expect(
      restart().begin("submitStep01", "evidence", "none", "step01"),
    ).resolves.toBeUndefined();
  });
});
