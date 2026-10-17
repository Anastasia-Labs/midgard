import { afterEach, expect, it, vi } from "vitest";

import { createMintItemNonCanonicalCentralJournalAdapter } from "../src/mint-item-non-canonical/central-journal.js";
import * as fundingAuthority from "../src/workflow/funding-reservation-permit.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import * as signedReconciliation from "../src/workflow/signed-transaction-reconciliation.js";

afterEach(() => {
  vi.restoreAllMocks();
});

const step01 = "a".repeat(64);
const step02 = "b".repeat(64);

it("supersedes a pending step02 before reobserving the step01 a rollback dropped", async () => {
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const store: FraudProofWorkflowJournalStore = {
    load: async () => entries,
    append: async (entry, expected) => {
      if (expected !== entries.length) throw new Error("sequence conflict");
      entries.push(entry);
    },
  };
  let onChain = new Set<string>();
  const adapter = () =>
    createMintItemNonCanonicalCentralJournalAdapter({
      store,
      deploymentFingerprint: "1".repeat(64),
      headerHash: "2".repeat(56),
      decisionDigest: "3".repeat(64),
      transactionConfirmed: async (txHash) => onChain.has(txHash),
    });
  // step01 confirms; step02, which spends step01's output, is in flight.
  await adapter().begin("submitStep01", "evidence", "none", "step01");
  await adapter().boundary(
    "submitStep01",
    "evidence",
    "none",
    "step01",
  )({ txHash: step01, referenceScripts: [] } as never);
  onChain = new Set([step01]);
  await adapter().reconcile("step01");
  await adapter().begin("submitStep02", "evidence", "step01", "step02");
  await adapter().boundary(
    "submitStep02",
    "evidence",
    "step01",
    "step02",
  )({ txHash: step02, referenceScripts: [] } as never);

  // The funding authority is spied: it holds step02 as the pending attempt
  // and refuses to abandon any other, exactly as the bound permit does.
  vi.spyOn(
    fundingAuthority,
    "workflowJournalHasFundingReservation",
  ).mockReturnValue(true);
  let pending = step02;
  const calls: string[] = [];
  vi.spyOn(fundingAuthority, "readWorkflowFundingRecovery").mockImplementation(
    async () => ({
      transition: {
        actionKind: "proof.init",
        transactionHash: pending,
        signedTransactionCborHex: "00",
        transactionBodySha256: "0".repeat(64),
        consumedOutRefs: [],
        producedInputs: [],
      },
      submissionHandoff: null,
      abandonmentHandoff: null,
      completionHandoff: null,
    }),
  );
  vi.spyOn(
    fundingAuthority,
    "abandonWorkflowFundingReservationTransaction",
  ).mockImplementation(async ({ transactionHash, handoff }) => {
    if (transactionHash !== pending)
      throw new Error(
        "funding abandonment changed its exact transaction identity",
      );
    expect(handoff.reconciliation).not.toHaveProperty("retirement");
    calls.push(`abandon ${transactionHash}`);
  });
  vi.spyOn(
    fundingAuthority,
    "acknowledgeWorkflowFundingAbandonment",
  ).mockImplementation(async ({ handoff }) => {
    calls.push(`acknowledge ${handoff.submissionIntent.txHash}`);
  });
  vi.spyOn(
    fundingAuthority,
    "reobserveWorkflowFundingReservationTransaction",
  ).mockImplementation(async ({ transactionHash }) => {
    calls.push(`reobserve ${transactionHash}`);
    pending = transactionHash;
    return true;
  });
  // Both are absent at the tip; step01 may still be in the mempool.
  vi.spyOn(
    signedReconciliation,
    "reconcileSignedWorkflowTransaction",
  ).mockImplementation(async ({ transactionHash }) =>
    transactionHash === step01 ? { kind: "pending" } : { kind: "not_found" },
  );

  // A rollback drops step01, and the chain is back at its source stage.
  onChain = new Set();
  await expect(adapter().reconcile("none")).rejects.toThrow(
    "Exact mint transaction remains unresolved: pending",
  );
  // step02 is superseded (its inputs become an exclusion set in the bound
  // store) before step01 is reobserved; no identity wedge follows.
  expect(calls).toEqual([
    `abandon ${step02}`,
    `acknowledge ${step02}`,
    `reobserve ${step01}`,
  ]);
  const events = entries.map(({ event }) => event);
  const closed = events.findIndex(
    (event) =>
      event.kind === "reconciled" &&
      event.outcome === "not_found" &&
      event.txHash === step02,
  );
  const reobserved = events.findIndex(
    (event) => event.kind === "reobserved" && event.txHash === step01,
  );
  expect(closed).toBeGreaterThanOrEqual(0);
  expect(reobserved).toBeGreaterThan(closed);
  // A second pass reconciles only step01 and supersedes nothing again.
  await expect(adapter().reconcile("none")).rejects.toThrow(
    "Exact mint transaction remains unresolved: pending",
  );
  expect(calls).toHaveLength(3);
});
