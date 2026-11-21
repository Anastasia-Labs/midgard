import { expect, it, vi } from "vitest";

import { MINT_ITEM_NON_CANONICAL_CURSOR_SPEC } from "../src/mint-item-non-canonical/workflow-spec.js";
import {
  bindWorkflowActuationJournal,
  bindWorkflowActuationRecoveryIdentity,
} from "../src/workflow/actuation-permit.assert-workflow-actuation-permit-identity.js";
import { createWorkflowReconciliationPermitController } from "../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import * as funding from "../src/workflow/funding-reservation-permit.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.directory-fraud-proof-workflow-journal-store.js";
import {
  computeFraudProofWorkflowId,
  journalJsonDigest,
  normalizeJournalJson,
} from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import { runRegisteredFamilyReconciliation } from "./support/bound-family-recovery-fixture.js";
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
      controller,
      decision: refreshed,
      store,
      journal: bindWorkflowActuationJournal({
        journal: store,
        permit: controller.permit,
        decisionDigest: refreshed.decisionDigest,
        deploymentFingerprint: original.deploymentFingerprint,
        category: original.category,
        headerHash: original.headerHash,
      }),
    };
    await verifyTerminalRecoveryFinality(
      recovery,
      async (input) =>
        await runRegisteredFamilyReconciliation(input, {
          rollbackGeneration: "1",
        }),
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
