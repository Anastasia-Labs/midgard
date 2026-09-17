import { afterEach, expect, it, vi } from "vitest";

import {
  executeManifestBoundMintItemNonCanonicalWorkflow,
  type ManifestBoundMintItemNonCanonicalWorkflow,
} from "../src/mint-item-non-canonical/workflow.js";
import * as actuation from "../src/workflow/actuation-permit.js";
import * as funding from "../src/workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  validateFraudProofWorkflowJournal,
} from "../src/workflow/journal.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../src/workflow/release-finality-policy.js";

afterEach(() => vi.restoreAllMocks());

const fixture = (depth = 1, removalConfirmed = true) => {
  const headerHash = "22".repeat(28);
  const fingerprint = "11".repeat(32);
  const proofHash = "44".repeat(32);
  const removalHash = "55".repeat(32);
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: fingerprint,
    category: "mintItemNonCanonical" as const,
    target: { kind: "state_queue_header" as const, headerHash },
    decisionDigest: "33".repeat(32),
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const push = (event: FraudProofWorkflowJournalEntry["event"]) =>
    entries.push({
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: entries.length,
      recordedAt: "2026-09-16T12:00:00.000Z",
      event,
    });
  push({ kind: "started" });
  const artifact = {
    category: "mintItemNonCanonical",
    familyIdentity: "retained-evidence",
  };
  push({
    kind: "prepared",
    artifact,
    artifactDigest: journalJsonDigest(artifact),
  });
  for (const [actionId, txHash, sourceStage, targetStage] of [
    ["mintItemNonCanonical:submitStep04", proofHash, "step04", "proven"],
    [
      "mintItemNonCanonical:removeDescendants",
      removalHash,
      "proven",
      "removed",
    ],
  ] as const) {
    push({
      kind: "preflight_passed",
      actionId,
      txHash,
      localEvaluator: "lucid",
      referenceScripts: [],
    });
    push({
      kind: "submission_intent",
      actionId,
      txHash,
      attempt: 1,
      actionInput: {},
      durableRecovery: {
        familyIdentity: "retained-evidence",
        sourceStage,
        targetStage,
      },
    });
    push({ kind: "submitted", actionId, txHash, attempt: 1 });
    if (txHash === proofHash || removalConfirmed) {
      push({ kind: "reconciled", actionId, txHash, outcome: "confirmed" });
      push({ kind: "confirmed", actionId, txHash });
    }
  }
  const terminal: FraudProofWorkflowTerminal = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
    category: identity.category,
    headerHash,
    proofToken: {
      unit: "66".repeat(28),
      outRef: `${proofHash}#0`,
      createdByTxHash: proofHash,
      retainedAtFinalState: true,
    },
    correction: {
      removalTxHash: removalHash,
      removedStateQueueOutRef: `${"77".repeat(32)}#0`,
      fraudulentHeaderAbsent: true,
      referencedProofTokenOutRef: `${proofHash}#0`,
    },
    economics: {
      operatorCredential: "88".repeat(28),
      proverCredential: "99".repeat(28),
      operatorBondInputOutRef: `${"aa".repeat(32)}#0`,
      operatorBondInputLovelace: "900000000",
      slashedLovelace: "500000000",
      proverRewardOutputOutRef: `${removalHash}#0`,
      proverRewardLovelace: "400000000",
      removalFeeLovelace: "500000000",
      duplicateRewardAbsent: true,
    },
    observedAt: {
      slot: "1234",
      blockHash: "bb".repeat(32),
      confirmationDepth: depth,
    },
  };
  const policy = {
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  } as const;
  const observe = vi.fn(async () => ({
    stage: { kind: "removed" as const, terminal },
  }));
  const observeHeader = vi.fn(async () => {
    throw new Error("must not refetch removed header or DA");
  });
  const transactionConfirmed = vi.fn(async () => true);
  const workflow = {
    binding: {
      deploymentFingerprint: fingerprint,
      definition: { category: identity.category, headerHash },
      releaseFinality: {
        schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: fingerprint,
        blueprintHash: "cc".repeat(32),
        policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
        policy,
      },
    },
    decisionDigest: identity.decisionDigest,
    l1: {
      category: identity.category,
      observe,
      observeHeader,
      transactionConfirmed,
    },
  } as unknown as ManifestBoundMintItemNonCanonicalWorkflow;
  const writes = { failTerminalOnce: false };
  const journal: FraudProofWorkflowJournalStore = {
    load: async (id) => {
      expect(id).toBe(workflowId);
      return [...entries];
    },
    append: async (entry, sequence) => {
      expect(sequence).toBe(entries.length);
      if (
        writes.failTerminalOnce &&
        (entry.event.kind === "terminal_included" ||
          entry.event.kind === "completed")
      ) {
        writes.failTerminalOnce = false;
        throw new Error("journal write interrupted after funding release");
      }
      validateFraudProofWorkflowJournal({
        workflowId,
        entries: [...entries, entry],
        expectedIdentity: identity,
      });
      entries.push(structuredClone(entry));
    },
  };
  validateFraudProofWorkflowJournal({
    workflowId,
    entries,
    expectedIdentity: identity,
  });
  const release = vi.spyOn(funding, "releaseWorkflowFundingReservation");
  const execute = () =>
    executeManifestBoundMintItemNonCanonicalWorkflow({
      workflow,
      sources: [],
      journal,
    });
  return {
    entries,
    terminal,
    observe,
    observeHeader,
    transactionConfirmed,
    release,
    execute,
    removalHash,
    writes,
  };
};

it("records terminal inclusion after removal and completes at release depth across restart without fresh DA", async () => {
  const f = fixture();
  const retained = structuredClone(f.entries);
  await expect(f.execute()).resolves.toMatchObject({ kind: "pending" });
  expect(f.entries.at(-1)?.event.kind).toBe("terminal_included");
  expect(f.release).toHaveBeenCalledOnce();
  expect(f.observeHeader).not.toHaveBeenCalled();
  await expect(f.execute()).resolves.toMatchObject({ kind: "pending" });
  expect(
    f.entries.filter(({ event }) => event.kind === "terminal_included"),
  ).toHaveLength(1);
  expect(f.release).toHaveBeenCalledOnce();
  Object.assign(f.terminal.observedAt, { confirmationDepth: 30 });
  await expect(f.execute()).resolves.toMatchObject({ kind: "completed" });
  expect(f.entries.at(-1)?.event.kind).toBe("completed");
  expect(f.release).toHaveBeenCalledTimes(2);
  expect(f.entries.slice(0, retained.length)).toEqual(retained);
  const completeCount = f.entries.length;
  await expect(f.execute()).resolves.toMatchObject({ kind: "completed" });
  expect(f.entries).toHaveLength(completeCount);
  expect(f.release).toHaveBeenCalledTimes(2);
});

it("authenticates the saved removal transaction before recording its recovered terminal", async () => {
  const f = fixture(30, false);
  await expect(f.execute()).resolves.toMatchObject({ kind: "completed" });
  expect(f.transactionConfirmed).toHaveBeenCalledWith({
    headerHash: f.terminal.headerHash,
    txHash: f.removalHash,
  });
  expect(
    f.entries.some(
      ({ event }) =>
        event.kind === "confirmed" && event.txHash === f.removalHash,
    ),
  ).toBe(true);
});

it("reconciliation-only execution remains pending without preparing or submitting when removal is unfinished", async () => {
  const f = fixture(1, false);
  vi.spyOn(actuation, "workflowJournalIsReconciliationOnly").mockReturnValue(
    true,
  );
  f.observe.mockResolvedValue({
    stage: {
      kind: "proof_token",
      fraudProofOutRef: f.terminal.proofToken.outRef,
      stateQueueBlockOutRef: f.terminal.correction.removedStateQueueOutRef,
      nextRemovalOutRef: f.terminal.correction.removedStateQueueOutRef,
    },
  } as never);
  await expect(f.execute()).resolves.toMatchObject({ kind: "pending" });
  expect(f.observeHeader).not.toHaveBeenCalled();
  expect(f.release).not.toHaveBeenCalled();
});

it.each(["economics", "proofToken", "headerHash", "anchor"])(
  "rejects a terminal whose %s changes on independent reobservation",
  async (field) => {
    const f = fixture(30);
    const changed = structuredClone(f.terminal);
    if (field === "economics")
      Object.assign(changed.economics, { slashedLovelace: "1" });
    else if (field === "proofToken")
      Object.assign(changed.proofToken, { outRef: `${"dd".repeat(32)}#0` });
    else if (field === "headerHash")
      Object.assign(changed, { headerHash: "ee".repeat(28) });
    else Object.assign(changed.observedAt, { blockHash: "ff".repeat(32) });
    f.observe
      .mockResolvedValueOnce({
        stage: { kind: "removed", terminal: f.terminal },
      })
      .mockResolvedValue({ stage: { kind: "removed", terminal: changed } });
    await expect(f.execute()).rejects.toThrow("terminal candidate differs");
    expect(f.release).not.toHaveBeenCalled();
  },
);

it.each([1, 30])(
  "restores a depth-%s terminal from its durable funding handoff after a journal-write crash",
  async (depth) => {
    const f = fixture(depth);
    let saved: funding.WorkflowFundingCompletionHandoff | null = null;
    vi.spyOn(funding, "readWorkflowFundingRecovery").mockImplementation(
      async () => ({
        transition: null,
        submissionHandoff: null,
        abandonmentHandoff: null,
        completionHandoff: saved,
      }),
    );
    f.release.mockImplementation(async ({ handoff }) => {
      saved = structuredClone(handoff);
    });
    f.writes.failTerminalOnce = true;
    await expect(f.execute()).rejects.toThrow(
      "journal write interrupted after funding release",
    );
    const afterCrash = structuredClone(f.entries);
    await expect(f.execute()).resolves.toMatchObject({
      kind: depth === 30 ? "completed" : "pending",
    });
    expect(f.release).toHaveBeenCalledOnce();
    expect(f.entries.slice(0, afterCrash.length)).toEqual(afterCrash);
    expect(f.entries.at(-1)?.event.kind).toBe(
      depth === 30 ? "completed" : "terminal_included",
    );
    expect(f.observeHeader).not.toHaveBeenCalled();
  },
);

it("rejects a terminal when its exact proof or removal transaction is no longer canonical", async () => {
  const f = fixture(30);
  f.transactionConfirmed.mockResolvedValue(false);
  await expect(f.execute()).rejects.toThrow(
    "terminal transaction is not authenticated on L1",
  );
  expect(f.release).not.toHaveBeenCalled();
});
