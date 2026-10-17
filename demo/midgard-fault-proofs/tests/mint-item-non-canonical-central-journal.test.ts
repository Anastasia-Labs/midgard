import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it, vi } from "vitest";

import { createMintItemNonCanonicalCentralJournalAdapter } from "../src/mint-item-non-canonical/central-journal.js";
import * as funding from "../src/workflow/funding-reservation-permit.js";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import { inspectSignedWorkflowTransaction } from "../src/workflow/signed-transaction-reconciliation.js";
import {
  signedRecoveryObservation,
  signedWorkflowTransactionFixture,
} from "./support/signed-workflow-transaction.js";

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
  createMintItemNonCanonicalCentralJournalAdapter({
    store,
    deploymentFingerprint: "1".repeat(64),
    headerHash: "2".repeat(56),
    decisionDigest: "3".repeat(64),
    transactionConfirmed: confirmed,
  });

describe("mintItemNonCanonical durable journal", () => {
  it("keeps a depth-zero mempool intent reserved and reopens its same receipt after a canonical rollback", async () => {
    const fixture = await signedWorkflowTransactionFixture({ ttl: null });
    // Present in the mempool at depth zero: canonical recovery reports pending.
    const observe = vi.fn(async (signed: typeof fixture.input) =>
      signedRecoveryObservation(signed, "pending", { depth: 0 }),
    );
    const memory = memoryStore();
    const txHash = fixture.input.transactionHash;
    const inspected = inspectSignedWorkflowTransaction(fixture.input);
    const read = vi
      .spyOn(funding, "readWorkflowFundingRecovery")
      .mockResolvedValue({
        transition: {
          actionKind: "proof.init",
          transactionHash: txHash,
          signedTransactionCborHex: fixture.input.signedTransactionCborHex,
          transactionBodySha256: createHash("sha256")
            .update(Buffer.from(inspected.body.to_cbor_hex(), "hex"))
            .digest("hex"),
          consumedOutRefs: inspected.inputOutRefs,
          producedInputs: [],
        },
        submissionHandoff: null,
        abandonmentHandoff: null,
        completionHandoff: null,
      });
    let confirmed = false;
    const restart = () =>
      createMintItemNonCanonicalCentralJournalAdapter({
        store: memory.store,
        deploymentFingerprint: "1".repeat(64),
        headerHash: "2".repeat(56),
        decisionDigest: "3".repeat(64),
        transactionConfirmed: async () => confirmed,
        observeSignedTransaction: observe,
      });
    try {
      const first = restart();
      await first.begin("submitStep01", "evidence", "none", "step01");
      await first.boundary(
        "submitStep01",
        "evidence",
        "none",
        "step01",
      )({ txHash, signed: fixture.signed, referenceScripts: [] });
      await expect(restart().reconcile("none")).rejects.toThrow(
        "remains unresolved: pending",
      );
      expect(
        memory.entries.some(
          ({ event }) =>
            event.kind === "reconciled" && event.outcome === "not_found",
        ),
      ).toBe(false);
      await expect(
        restart().begin("submitStep01", "evidence", "none", "step01"),
      ).rejects.toThrow("must reconcile before another build");
      confirmed = true;
      await restart().reconcile("step01");
      confirmed = false;
      await expect(restart().reconcile("none")).rejects.toThrow(
        "remains unresolved: pending",
      );
      expect(
        memory.entries.filter(
          ({ event }) => event.kind === "submission_intent",
        ),
      ).toHaveLength(1);
      expect(memory.entries.at(-1)?.event).toEqual({
        kind: "reobserved",
        actionId: "mintItemNonCanonical:submitStep01",
        txHash,
      });
      expect(observe).toHaveBeenCalled();
      for (const [signed] of observe.mock.calls)
        expect(signed).toEqual(fixture.input);
    } finally {
      read.mockRestore();
    }
  });

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

  it.each(["step02", "step03"] as const)(
    "appends and recovers repeated %s continuations",
    async (stage) => {
      const memory = memoryStore();
      const action = stage === "step02" ? "submitStep02" : "submitStep03";
      for (const byte of ["a", "b"]) {
        const journal = bridge(memory.store);
        await journal.begin(action, "evidence", stage, stage);
        const txHash = byte.repeat(64);
        await journal.boundary(
          action,
          "evidence",
          stage,
          stage,
        )({ txHash, referenceScripts: [] } as never);
        await journal.familyJournal.append({
          sequence: 0,
          identity: "evidence",
          stage,
          txHash,
          outputReference: `${txHash}#0`,
        });
        await bridge(memory.store).reconcile(stage);
      }
      expect(
        (await bridge(memory.store).familyJournal.load("evidence")).map(
          (entry) => entry.stage,
        ),
      ).toEqual([stage, stage]);
    },
  );

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
