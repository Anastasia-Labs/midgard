import { expect, it, vi } from "vitest";

import {
  executeManifestBoundMintItemNonCanonicalWorkflow,
  MINT_ITEM_NON_CANONICAL_CURSOR_SPEC,
} from "../src/mint-item-non-canonical/workflow.js";
import {
  bindWorkflowActuationJournal,
  bindWorkflowActuationRecoveryIdentity,
  createWorkflowReconciliationPermitController,
} from "../src/workflow/actuation-permit.js";
import * as funding from "../src/workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import { runBoundFamilyRecovery } from "./support/bound-family-recovery-fixture.js";
import {
  customWorkflowRecoveryFixture,
  verifyTerminalRecoveryFinality,
} from "./support/custom-workflow-recovery.js";

it("continues the retained terminal journal under refreshed authority without changing execution identity", async () => {
  const f = await customWorkflowRecoveryFixture(
    MINT_ITEM_NON_CANONICAL_CURSOR_SPEC,
    true,
  );
  const original = f.decision;
  const entries = await f.store.load(f.workflowId);
  const { decisionDigest: _, ...originalFields } = original;
  const refreshedFields = {
    ...originalFields,
    authenticatedObservationDigest: "fe".repeat(32),
  };
  const refreshed = {
    ...refreshedFields,
    decisionDigest: journalJsonDigest(normalizeJournalJson(refreshedFields)),
  };
  // Real read-only authority over a distinct current decision, bound to the
  // original sealed execution through the production recovery check.
  const freshEntries = entries.map((entry) => {
    const identity = {
      ...entry.identity,
      decisionDigest: refreshed.decisionDigest,
    };
    return {
      ...entry,
      identity,
      workflowId: computeFraudProofWorkflowId(identity),
    };
  });
  const controller = createWorkflowReconciliationPermitController({
    decision: refreshed,
    entries: freshEntries,
    deploymentFingerprint: original.deploymentFingerprint,
    rollbackGeneration: "1",
  });
  bindWorkflowActuationRecoveryIdentity({
    permit: controller.permit,
    category: original.category,
    rollbackGeneration: "1",
    originalDecision: original,
  });
  // The retained journal, as a restarted process finds it.
  const store = new MemoryFraudProofWorkflowJournalStore();
  for (const entry of entries) await store.append(entry, entry.sequence);
  const load = vi.spyOn(store, "load");
  const prepare = vi.spyOn(
    funding,
    "prepareWorkflowFundingReservationTransaction",
  );
  try {
    const recovery = {
      ...f,
      journal: bindWorkflowActuationJournal({
        journal: store,
        permit: controller.permit,
        decisionDigest: refreshed.decisionDigest,
        deploymentFingerprint: original.deploymentFingerprint,
        category: original.category,
        headerHash: original.headerHash,
      }),
      workflow: { ...f.workflow, decisionDigest: refreshed.decisionDigest },
    };
    await verifyTerminalRecoveryFinality(
      recovery,
      async (input) =>
        await runBoundFamilyRecovery(
          executeManifestBoundMintItemNonCanonicalWorkflow,
          input,
        ),
    );
    const completed = await store.load(f.workflowId);
    expect(completed.slice(0, entries.length)).toEqual(entries);
    expect(prepare).not.toHaveBeenCalled();
    expect(load).toHaveBeenCalled();
    expect(load.mock.calls.every(([id]) => id === f.workflowId)).toBe(true);
    expect(await store.load(freshEntries[0]!.workflowId)).toEqual([]);
  } finally {
    vi.restoreAllMocks();
  }
});
