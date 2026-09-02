import { describe, expect, it } from "vitest";

import { createFieldPreimageLengthCentralJournalAdapterV1 } from "../src/field-preimage-length-mismatch/central-journal-v1.js";
import type { PreparedFieldPreimageLengthWorkflowV1 } from "../src/field-preimage-length-mismatch/workflow-v1.js";
import type {
  FraudProofWorkflowJournalEntryV1,
  FraudProofWorkflowJournalStoreV1,
} from "../src/workflow/journal-v1.js";

const prepared = (
  evidenceDigest = "4".repeat(64),
): PreparedFieldPreimageLengthWorkflowV1 => ({
  schemaVersion: "midgard-field-preimage-length-mismatch-workflow-v1",
  headerHash: "2".repeat(56),
  transactionId: "3".repeat(64),
  direction: "wrongfulAcceptance",
  fieldIndex: 0,
  declaredLength: 2,
  actualLength: 1,
  preimageHex: "00",
  carriage: "Inline",
  evidenceDigest,
});

const memoryStore = () => {
  const entries: FraudProofWorkflowJournalEntryV1[] = [];
  const store: FraudProofWorkflowJournalStoreV1 = {
    load: async () => entries,
    append: async (entry, expectedSequence) => {
      if (expectedSequence !== entries.length) throw new Error("conflict");
      entries.push(entry);
    },
  };
  return { entries, store };
};

const adapter = (store: FraudProofWorkflowJournalStoreV1, value = prepared()) =>
  createFieldPreimageLengthCentralJournalAdapterV1({
    store,
    deploymentFingerprint: "1".repeat(64),
    decisionDigest: "5".repeat(64),
    prepared: value,
    observeConfirmed: async () => true,
  });

describe("fieldPreimageLengthMismatch central journal bridge", () => {
  it("persists exact prepared evidence and intent before submission state", async () => {
    const memory = memoryStore();
    const bridge = adapter(memory.store);
    const txHash = "6".repeat(64);
    await bridge.boundary(
      "init",
      prepared(),
    )({
      txHash,
      referenceScripts: [],
    } as never);
    expect(memory.entries.map(({ event }) => event.kind)).toEqual([
      "prepared",
      "submission_intent",
    ]);
    await bridge.journal.save({
      prepared: prepared(),
      confirmed: ["init"],
      transactionIds: { init: txHash },
    });
    expect(memory.entries.map(({ event }) => event.kind)).toEqual([
      "prepared",
      "submission_intent",
      "submitted",
      "reconciled",
      "confirmed",
    ]);
  });

  it("reuses the same crash intent and refuses transaction substitution", async () => {
    const memory = memoryStore();
    const boundary = adapter(memory.store).boundary("dispatch", prepared());
    await boundary({ txHash: "7".repeat(64), referenceScripts: [] } as never);
    await boundary({ txHash: "7".repeat(64), referenceScripts: [] } as never);
    await expect(
      boundary({ txHash: "8".repeat(64), referenceScripts: [] } as never),
    ).rejects.toThrow(/identity changed across restart/u);
  });

  it("reconstructs confirmed state on restart and refuses evidence substitution", async () => {
    const memory = memoryStore();
    const first = adapter(memory.store);
    const txHash = "9".repeat(64);
    await first.boundary(
      "init",
      prepared(),
    )({
      txHash,
      referenceScripts: [],
    } as never);
    await first.journal.save({
      prepared: prepared(),
      confirmed: ["init"],
      transactionIds: { init: txHash },
    });
    await expect(adapter(memory.store).journal.load()).resolves.toMatchObject({
      confirmed: ["init"],
      transactionIds: { init: txHash },
    });
    await expect(
      adapter(memory.store, prepared("a".repeat(64))).journal.load(),
    ).rejects.toThrow(/digest differs/u);
  });

  it.each(["publication", "certificate"] as const)(
    "persists and reconciles %s carriage actuation across restart",
    async (kind) => {
      const memory = memoryStore();
      const first = adapter(memory.store);
      const txHash = kind === "publication" ? "a".repeat(64) : "b".repeat(64);
      const transaction = { txHash, referenceScripts: [] } as never;
      await first.auxiliaryBoundary(kind)(transaction);

      const restarted = adapter(memory.store);
      await restarted.auxiliaryBoundary(kind)(transaction);
      await restarted.auxiliaryConfirmed(kind, [txHash, txHash]);
      await restarted.auxiliaryConfirmed(kind, [txHash]);

      expect(memory.entries.map(({ event }) => event.kind)).toEqual([
        "prepared",
        "submission_intent",
        "submitted",
        "reconciled",
        "confirmed",
      ]);
      expect(memory.entries[1]?.event).toMatchObject({
        kind: "submission_intent",
        actionId: `fieldPreimageLengthMismatch:carriage:${kind}:${txHash}`,
        txHash,
      });
    },
  );

  it("keeps distinct durable identities for multiple chunk publications", async () => {
    const memory = memoryStore();
    const bridge = adapter(memory.store);
    const first = "c".repeat(64);
    const second = "d".repeat(64);
    await bridge.auxiliaryBoundary("publication")({
      txHash: first,
      referenceScripts: [],
    } as never);
    await bridge.auxiliaryBoundary("publication")({
      txHash: second,
      referenceScripts: [],
    } as never);
    await bridge.auxiliaryConfirmed("publication", [first, second]);

    expect(
      memory.entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(2);
    expect(
      memory.entries.filter(({ event }) => event.kind === "confirmed"),
    ).toHaveLength(2);
  });

  it("refuses certificate transaction substitution after a crash", async () => {
    const memory = memoryStore();
    const first = adapter(memory.store);
    await first.auxiliaryBoundary("certificate")({
      txHash: "e".repeat(64),
      referenceScripts: [],
    } as never);
    await expect(
      adapter(memory.store).auxiliaryBoundary("certificate")({
        txHash: "f".repeat(64),
        referenceScripts: [],
      } as never),
    ).rejects.toThrow(/identity changed across restart/u);
  });
});
