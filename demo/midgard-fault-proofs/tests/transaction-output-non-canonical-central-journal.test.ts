import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { createTransactionOutputNonCanonicalCentralJournalAdapter } from "../src/transaction-output-non-canonical/central-journal.js";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";

const familyDirectoryStore = (
  directory: string,
): FraudProofWorkflowJournalStore => {
  const file = join(directory, "journal.json");
  const load = async (): Promise<FraudProofWorkflowJournalEntry[]> => {
    try {
      return JSON.parse(
        await readFile(file, "utf8"),
      ) as FraudProofWorkflowJournalEntry[];
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code === "ENOENT") return [];
      throw error;
    }
  };
  return {
    load,
    append: async (entry, expected) => {
      const entries = await load();
      if (entries.length !== expected) throw new Error("sequence conflict");
      await writeFile(file, JSON.stringify([...entries, entry]), "utf8");
    },
  };
};

const memoryStore = () => {
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const store: FraudProofWorkflowJournalStore = {
    load: async () => entries,
    append: async (entry, expected) => {
      if (expected !== entries.length) throw new Error("sequence conflict");
      entries.push(entry);
    },
  };
  return { entries, store };
};
const bridge = (
  store: FraudProofWorkflowJournalStore,
  confirmed = async (_txHash: string) => true,
) =>
  createTransactionOutputNonCanonicalCentralJournalAdapter({
    store,
    deploymentFingerprint: "1".repeat(64),
    headerHash: "2".repeat(56),
    decisionDigest: "3".repeat(64),
    transactionConfirmed: confirmed,
  });

describe("transactionOutputNonCanonical durable journal", () => {
  it("writes exact intent before submission and refuses tx substitution", async () => {
    const memory = memoryStore();
    const journal = bridge(memory.store);
    await journal.begin("submitStep03", "evidence", "step03", "step04");
    const boundary = journal.boundary(
      "submitStep03",
      "evidence",
      "step03",
      "step04",
    );
    await boundary({ txHash: "4".repeat(64), referenceScripts: [] } as never);
    expect(memory.entries.map(({ event }) => event.kind)).toEqual([
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
    ]);
    await expect(
      boundary({ txHash: "5".repeat(64), referenceScripts: [] } as never),
    ).rejects.toThrow(/identity changed across restart/u);
  });

  it("persists and reconciles the fourth physical stage across restart", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-output-noncanonical-"),
    );
    try {
      const txHash = "6".repeat(64);
      const first = bridge(familyDirectoryStore(directory));
      await first.begin("submitStep04", "evidence", "step04", "proven");
      await first.boundary(
        "submitStep04",
        "evidence",
        "step04",
        "proven",
      )({ txHash, referenceScripts: [] } as never);
      await first.familyJournal.append({
        sequence: 0,
        identity: "evidence",
        stage: "proven",
        txHash,
        outputReference: null,
      });
      const restarted = bridge(familyDirectoryStore(directory));
      await restarted.reconcile("proven");
      await expect(restarted.familyJournal.load("evidence")).resolves.toEqual([
        expect.objectContaining({ stage: "proven", txHash }),
      ]);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("journals certified-carriage actuation without advancing the thread", async () => {
    const memory = memoryStore();
    const journal = bridge(memory.store);
    const hashes: string[] = [];
    await journal.auxiliaryBoundary(
      "certificate",
      "evidence",
      "step03",
      hashes,
    )({ txHash: "7".repeat(64), referenceScripts: [] } as never);
    await journal.confirmAuxiliary(hashes[0]!);
    expect(await journal.familyJournal.load("evidence")).toEqual([]);
    expect(memory.entries.map(({ event }) => event.kind)).toContain(
      "confirmed",
    );
  });
});
