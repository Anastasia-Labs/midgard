import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { createSpendInputSignerMissingCentralJournalAdapter } from "../src/spend-input-signer-missing/central-journal.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import {
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
} from "../src/workflow/journal.js";

const DEPLOYMENT_FINGERPRINT = "1".repeat(64);
const HEADER_HASH = "2".repeat(56);
const DECISION_DIGEST = "3".repeat(64);

const store = () => {
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const value: FraudProofWorkflowJournalStore = {
    load: async () => entries,
    append: async (entry, expectedSequence) => {
      if (expectedSequence !== entries.length)
        throw new Error("sequence conflict");
      entries.push(entry);
    },
  };
  return { entries, value };
};

/**
 * The adapter is built exactly as production builds it — no
 * `testOnlyJournalCategoryAlias`, so the durable workflow identity really is
 * the `spendInputSignerMissing` category the family submits under.
 */
const adapter = (
  value: FraudProofWorkflowJournalStore,
  {
    transactionConfirmed = async () => true,
    headerHash = HEADER_HASH,
    decisionDigest = DECISION_DIGEST,
  }: {
    readonly transactionConfirmed?: (txHash: string) => Promise<boolean>;
    readonly headerHash?: string;
    readonly decisionDigest?: string;
  } = {},
) =>
  createSpendInputSignerMissingCentralJournalAdapter({
    store: value,
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    headerHash,
    decisionDigest,
    transactionConfirmed,
  });

/** Position of the durable pre-submit intent that names `txHash`. */
const intentIndex = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  txHash: string,
): number =>
  entries.findIndex(
    ({ event }) =>
      event.kind === "submission_intent" && event.txHash === txHash,
  );

/** Position of the durable record that the transaction was submitted. */
const submittedIndex = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  txHash: string,
): number =>
  entries.findIndex(
    ({ event }) => event.kind === "submitted" && event.txHash === txHash,
  );

const confirmedIndex = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  txHash: string,
): number =>
  entries.findIndex(
    ({ event }) => event.kind === "confirmed" && event.txHash === txHash,
  );

describe("spendInputSignerMissing central journal adapter", () => {
  it("records the family's own durable workflow identity", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    await bridge.begin("submitInit", "family-evidence", "none", "step01");
    expect(memory.entries.length).toBeGreaterThan(0);
    for (const entry of memory.entries) {
      expect(entry.identity).toEqual({
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        category: "spendInputSignerMissing",
        target: { kind: "state_queue_header", headerHash: HEADER_HASH },
        decisionDigest: DECISION_DIGEST,
      });
      expect(entry.workflowId).toBe(bridge.workflowId);
    }

    // The workflow id must separate journals per challenged header and per
    // decision, or two disputes would share one durable actuation record.
    const otherHeader = store();
    await adapter(otherHeader.value, { headerHash: "5".repeat(56) }).begin(
      "submitInit",
      "family-evidence",
      "none",
      "step01",
    );
    const otherDecision = store();
    await adapter(otherDecision.value, {
      decisionDigest: "6".repeat(64),
    }).begin("submitInit", "family-evidence", "none", "step01");
    expect(
      new Set([
        memory.entries[0]!.workflowId,
        otherHeader.entries[0]!.workflowId,
        otherDecision.entries[0]!.workflowId,
      ]).size,
    ).toBe(3);
  });

  it("persists the exact submission intent before the transaction may be submitted", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    const txHash = "4".repeat(64);
    await bridge.begin("submitInit", "family-evidence", "none", "step01");
    await bridge.boundary(
      "submitInit",
      "family-evidence",
      "none",
      "step01",
    )({
      txHash,
      referenceScripts: [],
    } as never);
    // Durable intent exists when the pre-submit boundary returns, and nothing
    // yet claims the transaction was submitted.
    expect(intentIndex(memory.entries, txHash)).toBeGreaterThanOrEqual(0);
    expect(submittedIndex(memory.entries, txHash)).toBe(-1);

    const intentAt = intentIndex(memory.entries, txHash);
    await bridge.familyJournal.append({
      sequence: 0,
      identity: "family-evidence",
      sourceStage: "none",
      targetStage: "step01",
      action: "submitInit",
      phase: "submitted",
      txHash,
    });
    expect(submittedIndex(memory.entries, txHash)).toBeGreaterThan(intentAt);

    await bridge.reconcile("step01");
    expect(confirmedIndex(memory.entries, txHash)).toBeGreaterThan(
      submittedIndex(memory.entries, txHash),
    );
    expect(await bridge.familyJournal.load("family-evidence")).toEqual([
      expect.objectContaining({
        identity: "family-evidence",
        targetStage: "step01",
        txHash,
      }),
    ]);
  });

  it("reuses an identical crash intent and rejects transaction substitution without journalling it", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    const txHash = "5".repeat(64);
    await bridge.begin("submitStep02", "family-evidence", "step02", "step03");
    const boundary = bridge.boundary(
      "submitStep02",
      "family-evidence",
      "step02",
      "step03",
    );
    await boundary({ txHash, referenceScripts: [] } as never);
    const afterFirst = memory.entries.length;
    // A replayed boundary after a crash must reuse the same durable intent
    // rather than opening a second one.
    await boundary({ txHash, referenceScripts: [] } as never);
    expect(memory.entries).toHaveLength(afterFirst);
    expect(
      memory.entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(1);

    const substitute = "6".repeat(64);
    await expect(
      boundary({ txHash: substitute, referenceScripts: [] } as never),
    ).rejects.toThrow(/identity changed across restart/u);
    // The refused substitution must leave no durable trace at all.
    expect(memory.entries).toHaveLength(afterFirst);
    expect(intentIndex(memory.entries, substitute)).toBe(-1);
  });

  it("refuses a completion that has no durable pre-submit intent", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    await expect(
      bridge.familyJournal.append({
        sequence: 0,
        identity: "family-evidence",
        sourceStage: "step05",
        targetStage: "proven",
        action: "submitStep05",
        phase: "submitted",
        txHash: "7".repeat(64),
      }),
    ).rejects.toThrow(/without its exact pre-submit intent/u);
    expect(memory.entries).toEqual([]);
  });

  it("refuses a confirmed hash paired with a substituted authenticated stage", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    const txHash = "8".repeat(64);
    await bridge.begin("submitStep01", "family-evidence", "step01", "step02");
    await bridge.boundary(
      "submitStep01",
      "family-evidence",
      "step01",
      "step02",
    )({ txHash, referenceScripts: [] } as never);
    // The same intent reconciles cleanly against the stage it actually
    // targeted, so the refusal below is caused only by the substituted stage.
    await expect(bridge.reconcile("step03")).rejects.toThrow(
      /identity substitution/u,
    );
    expect(confirmedIndex(memory.entries, txHash)).toBe(-1);

    const clean = store();
    const cleanBridge = adapter(clean.value);
    await cleanBridge.begin(
      "submitStep01",
      "family-evidence",
      "step01",
      "step02",
    );
    await cleanBridge.boundary(
      "submitStep01",
      "family-evidence",
      "step01",
      "step02",
    )({ txHash, referenceScripts: [] } as never);
    await cleanBridge.reconcile("step02");
    expect(confirmedIndex(clean.entries, txHash)).toBeGreaterThanOrEqual(0);
  });

  it("abandons an unconfirmed intent only when raw L1 still authenticates its source stage", async () => {
    const memory = store();
    const bridge = adapter(memory.value, {
      transactionConfirmed: async () => false,
    });
    const txHash = "9".repeat(64);
    await bridge.begin("submitStep03", "family-evidence", "step03", "scanning");
    await bridge.boundary(
      "submitStep03",
      "family-evidence",
      "step03",
      "proven",
    )({ txHash, referenceScripts: [] } as never);
    await bridge.reconcile("step03");
    expect(memory.entries.at(-1)?.event).toEqual(
      expect.objectContaining({ kind: "reconciled", outcome: "not_found" }),
    );
    expect(confirmedIndex(memory.entries, txHash)).toBe(-1);

    // An unconfirmed transaction observed at neither the source nor the target
    // stage is a substitution, not an abandonment.
    const drifted = store();
    const driftedBridge = adapter(drifted.value, {
      transactionConfirmed: async () => false,
    });
    await driftedBridge.begin(
      "submitStep03",
      "family-evidence",
      "step03",
      "scanning",
    );
    await driftedBridge.boundary(
      "submitStep03",
      "family-evidence",
      "step03",
      "proven",
    )({ txHash, referenceScripts: [] } as never);
    await expect(driftedBridge.reconcile("step05")).rejects.toThrow(
      /identity substitution/u,
    );
  });

  it("reconciles an exact submitted transaction after a directory-backed process restart", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-spend-input-signer-journal-"),
    );
    try {
      const first = adapter(
        new DirectoryFraudProofWorkflowJournalStore(directory),
      );
      const txHash = "a".repeat(64);
      await first.begin("submitInit", "family-evidence", "none", "step01");
      await first.boundary(
        "submitInit",
        "family-evidence",
        "none",
        "step01",
      )({ txHash, referenceScripts: [] } as never);
      await first.familyJournal.append({
        sequence: 0,
        identity: "family-evidence",
        sourceStage: "none",
        targetStage: "step01",
        action: "submitInit",
        phase: "submitted",
        txHash,
      });

      // A fresh process reads the same durable journal from disk: the exact
      // intent must be found again and reconciled, not re-opened.
      const restarted = adapter(
        new DirectoryFraudProofWorkflowJournalStore(directory),
      );
      await restarted.reconcile("step01");
      await expect(
        restarted.familyJournal.load("family-evidence"),
      ).resolves.toEqual([
        expect.objectContaining({ targetStage: "step01", txHash }),
      ]);

      // A restarted process that observes a different hash for the same
      // unresolved action must refuse rather than adopt it.
      const substituting = adapter(
        new DirectoryFraudProofWorkflowJournalStore(directory),
      );
      await substituting.begin(
        "submitStep02",
        "family-evidence",
        "step01",
        "step02",
      );
      const boundary = substituting.boundary(
        "submitStep02",
        "family-evidence",
        "step01",
        "step02",
      );
      await boundary({ txHash: "b".repeat(64), referenceScripts: [] } as never);
      await expect(
        adapter(
          new DirectoryFraudProofWorkflowJournalStore(directory),
        ).boundary(
          "submitStep02",
          "family-evidence",
          "step01",
          "step02",
        )({ txHash: "c".repeat(64), referenceScripts: [] } as never),
      ).rejects.toThrow(/identity changed across restart/u);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("journals and authenticates maximum-carriage prerequisite transactions without advancing the family stage", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    const captured: string[] = [];
    const txHash = "c".repeat(64);
    await bridge.auxiliaryBoundary(
      "certificate",
      "family-evidence",
      "step02",
      captured,
    )({ txHash, referenceScripts: [] } as never);
    expect(captured).toEqual([txHash]);
    expect(intentIndex(memory.entries, txHash)).toBeGreaterThanOrEqual(0);
    await bridge.confirmAuxiliary(txHash);
    expect(confirmedIndex(memory.entries, txHash)).toBeGreaterThan(
      intentIndex(memory.entries, txHash),
    );
    // An auxiliary transaction is never a family stage transition.
    expect(await bridge.familyJournal.load("family-evidence")).toEqual([]);
  });
});
