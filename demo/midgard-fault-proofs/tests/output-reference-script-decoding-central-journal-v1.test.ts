import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { createOutputReferenceScriptDecodingCentralJournalAdapter } from "../src/output-reference-script-decoding/central-journal-v1.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "../src/workflow/journal-v1.js";
import { DirectoryFraudProofWorkflowJournalStore } from "../src/workflow/journal-v1.js";

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

const adapter = (
  value: FraudProofWorkflowJournalStore,
  transactionConfirmed: (txHash: string) => Promise<boolean> = async () => true,
) =>
  createOutputReferenceScriptDecodingCentralJournalAdapter({
    store: value,
    deploymentFingerprint: "1".repeat(64),
    headerHash: "2".repeat(56),
    decisionDigest: "3".repeat(64),
    transactionConfirmed,
    testOnlyJournalCategoryAlias: "fieldItemWidthIllegal",
  });

describe("outputReferenceScriptDecoding central journal adapter", () => {
  it("persists intent before submitted/reconciled/confirmed state", async () => {
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
    expect(memory.entries.map(({ event }) => event.kind)).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
    ]);
    await bridge.familyJournal.append({
      sequence: 0,
      identity: "family-evidence",
      sourceStage: "none",
      targetStage: "step01",
      action: "submitInit",
      phase: "submitted",
      txHash,
    });
    expect(memory.entries.map(({ event }) => event.kind)).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
      "submitted",
    ]);
    await bridge.reconcile("step01");
    expect(memory.entries.map(({ event }) => event.kind)).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
      "submitted",
      "reconciled",
      "confirmed",
    ]);
    expect(await bridge.familyJournal.load("family-evidence")).toEqual([
      expect.objectContaining({
        identity: "family-evidence",
        targetStage: "step01",
        txHash,
      }),
    ]);
  });

  it("reuses an identical crash intent and rejects transaction substitution", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    await bridge.begin(
      "submitStep02",
      "family-evidence",
      "step02",
      "outputScan",
    );
    const boundary = bridge.boundary(
      "submitStep02",
      "family-evidence",
      "step02",
      "outputScan",
    );
    await boundary({ txHash: "5".repeat(64), referenceScripts: [] } as never);
    await boundary({ txHash: "5".repeat(64), referenceScripts: [] } as never);
    expect(memory.entries).toHaveLength(4);
    await expect(
      boundary({ txHash: "6".repeat(64), referenceScripts: [] } as never),
    ).rejects.toThrow(/identity changed across restart/u);
  });

  it("refuses a completion that has no durable pre-submit intent", async () => {
    const bridge = adapter(store().value);
    await expect(
      bridge.familyJournal.append({
        sequence: 0,
        identity: "family-evidence",
        sourceStage: "step06",
        targetStage: "proven",
        action: "submitStep06",
        phase: "submitted",
        txHash: "7".repeat(64),
      }),
    ).rejects.toThrow(/without its exact pre-submit intent/u);
  });

  it("refuses a confirmed hash paired with a substituted authenticated stage", async () => {
    const memory = store();
    const bridge = adapter(memory.value);
    await bridge.begin("submitStep01", "family-evidence", "step01", "step02");
    await bridge.boundary(
      "submitStep01",
      "family-evidence",
      "step01",
      "step02",
    )({ txHash: "8".repeat(64), referenceScripts: [] } as never);
    await expect(bridge.reconcile("outputScan")).rejects.toThrow(
      /identity substitution/u,
    );
  });

  it("abandons an unconfirmed intent only when raw L1 still authenticates its source stage", async () => {
    const memory = store();
    const bridge = adapter(memory.value, async () => false);
    await bridge.begin(
      "submitOutputScan",
      "family-evidence",
      "outputScan",
      "referenceBind",
    );
    await bridge.boundary(
      "submitOutputScan",
      "family-evidence",
      "outputScan",
      "referenceBind",
    )({ txHash: "9".repeat(64), referenceScripts: [] } as never);
    await bridge.reconcile("outputScan");
    expect(memory.entries.at(-1)?.event).toEqual(
      expect.objectContaining({ kind: "reconciled", outcome: "not_found" }),
    );
  });

  it("reconciles an exact submitted transaction after a directory-backed process restart", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-output-reference-script-journal-"),
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

      const restarted = adapter(
        new DirectoryFraudProofWorkflowJournalStore(directory),
      );
      await restarted.reconcile("step01");
      await expect(
        restarted.familyJournal.load("family-evidence"),
      ).resolves.toEqual([
        expect.objectContaining({ targetStage: "step01", txHash }),
      ]);
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
    await bridge.confirmAuxiliary(txHash);
    expect(await bridge.familyJournal.load("family-evidence")).toEqual([]);
    expect(memory.entries.map(({ event }) => event.kind)).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
      "submitted",
      "reconciled",
      "confirmed",
    ]);
  });
});
