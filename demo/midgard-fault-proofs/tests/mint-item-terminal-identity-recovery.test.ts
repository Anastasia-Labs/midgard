import { mkdtemp, readdir, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { expect, it, vi } from "vitest";

import {
  executeManifestBoundMintItemNonCanonicalWorkflow,
  type ManifestBoundMintItemNonCanonicalWorkflow,
} from "../src/mint-item-non-canonical/workflow.js";
import {
  bindWorkflowActuationJournal,
  bindWorkflowActuationRecoveryIdentity,
  createWorkflowReconciliationPermitController,
} from "../src/workflow/actuation-permit.js";
import * as funding from "../src/workflow/funding-reservation-permit.js";
import type { HeaderFaultDecision } from "../src/workflow/header-classifier.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";

it("continues the retained terminal journal under refreshed authority without changing execution identity", async () => {
  const entries: FraudProofWorkflowJournalEntry[] = JSON.parse(
    await readFile(
      new URL(
        "./fixtures/mint-terminal-included-retained.json",
        import.meta.url,
      ),
      "utf8",
    ),
  );
  const { decision: original }: { decision: HeaderFaultDecision } = JSON.parse(
    await readFile(
      new URL("./fixtures/mint-reconciliation-retained.json", import.meta.url),
      "utf8",
    ),
  );
  const { decisionDigest: _, ...originalFields } = original;
  const refreshedFields = {
    ...originalFields,
    authenticatedObservationDigest: "fe".repeat(32),
  };
  const refreshed = {
    ...refreshedFields,
    decisionDigest: journalJsonDigest(normalizeJournalJson(refreshedFields)),
  };
  // Create real read-only authority with a distinct current decision, then bind
  // the original sealed execution through the production recovery check.
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
  const root = await mkdtemp(join(tmpdir(), "mint-terminal-identity-"));
  try {
    const store = new DirectoryFraudProofWorkflowJournalStore(root);
    for (const entry of entries) await store.append(entry, entry.sequence);
    const load = vi.spyOn(store, "load");
    const journal = bindWorkflowActuationJournal({
      journal: store,
      permit: controller.permit,
      decisionDigest: refreshed.decisionDigest,
      deploymentFingerprint: original.deploymentFingerprint,
      category: original.category,
      headerHash: original.headerHash,
    });
    const retainedTerminal = entries.at(-1)!.event;
    if (retainedTerminal.kind !== "terminal_included")
      throw new Error("fixture must retain terminal inclusion");
    const terminal = structuredClone(retainedTerminal.terminal);
    const observeHeader = vi.fn(async () => {
      throw new Error("must not rebuild removed proof");
    });
    const transactionConfirmed = vi.fn(async () => true);
    const policy = {
      confirmationDepth: 30,
      automaticRecoveryMaxDepth: 2160,
      deepRollbackPolicy: "automated_rewind_replay_incident-v1",
    } as const;
    const workflow = {
      binding: {
        deploymentFingerprint: original.deploymentFingerprint,
        definition: {
          category: original.category,
          headerHash: original.headerHash,
        },
        releaseFinality: {
          schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
          deploymentIdentityDigest: original.deploymentFingerprint,
          blueprintHash: "cc".repeat(32),
          policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
          policy,
        },
      },
      decisionDigest: refreshed.decisionDigest,
      l1: {
        category: original.category,
        observe: async () => ({ stage: { kind: "removed", terminal } }),
        observeHeader,
        transactionConfirmed,
      },
    } as unknown as ManifestBoundMintItemNonCanonicalWorkflow;
    const release = vi.spyOn(funding, "releaseWorkflowFundingReservation");
    const prepare = vi.spyOn(
      funding,
      "prepareWorkflowFundingReservationTransaction",
    );
    const execute = () =>
      executeManifestBoundMintItemNonCanonicalWorkflow({
        workflow,
        journal,
        sources: [],
      });
    await expect(execute()).resolves.toMatchObject({ kind: "pending" });
    expect(await store.load(entries[0]!.workflowId)).toEqual(entries);
    expect(release).not.toHaveBeenCalled();
    Object.assign(terminal.observedAt, { confirmationDepth: 30 });
    await expect(execute()).resolves.toMatchObject({ kind: "completed" });
    const completed = await store.load(entries[0]!.workflowId);
    expect(completed.slice(0, entries.length)).toEqual(entries);
    expect(completed.at(-1)?.event.kind).toBe("completed");
    expect(release).toHaveBeenCalledOnce();
    expect(prepare).not.toHaveBeenCalled();
    expect(observeHeader).not.toHaveBeenCalled();
    expect(load.mock.calls.every(([id]) => id === entries[0]!.workflowId)).toBe(
      true,
    );
    expect(await readdir(root)).toEqual([entries[0]!.workflowId]);
  } finally {
    vi.restoreAllMocks();
    await rm(root, { recursive: true, force: true });
  }
});
