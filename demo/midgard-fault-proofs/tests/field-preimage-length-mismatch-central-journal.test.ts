import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { CML, type TxSigned } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createFieldPreimageLengthCentralJournalAdapter } from "../src/field-preimage-length-mismatch/central-journal.js";
import type { PreparedFieldPreimageLengthWorkflow } from "../src/field-preimage-length-mismatch/workflow.js";
import * as removal from "../src/remove-fraudulent-block.js";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import { DirectoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";

const prepared = (
  evidenceDigest = "4".repeat(64),
): PreparedFieldPreimageLengthWorkflow => ({
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
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const store: FraudProofWorkflowJournalStore = {
    load: async () => entries,
    append: async (entry, expectedSequence) => {
      if (expectedSequence !== entries.length) throw new Error("conflict");
      entries.push(entry);
    },
  };
  return { entries, store };
};

const adapter = (store: FraudProofWorkflowJournalStore, value = prepared()) =>
  createFieldPreimageLengthCentralJournalAdapter({
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
      "started",
      "prepared",
      "preflight_passed",
      "submission_intent",
    ]);
    await bridge.journal.save({
      prepared: prepared(),
      confirmed: ["init"],
      transactionIds: { init: txHash },
    });
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
        "started",
        "prepared",
        "preflight_passed",
        "submission_intent",
        "submitted",
        "reconciled",
        "confirmed",
      ]);
      expect(memory.entries[3]?.event).toMatchObject({
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
    await bridge.auxiliaryConfirmed("publication", [first]);
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

describe("field-preimage-length validating directory initialization", () => {
  it("loads a new journal and retains a valid signed intent through a directory restart", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "field-preimage-first-run-"),
    );
    try {
      const store = new DirectoryFraudProofWorkflowJournalStore(directory);
      const first = adapter(store);
      await expect(first.journal.load()).resolves.toMatchObject({
        confirmed: [],
        transactionIds: {},
      });
      const body = CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        CML.TransactionOutputList.new(),
        0n,
      );
      const transaction = CML.Transaction.new(
        body,
        CML.TransactionWitnessSet.new(),
        true,
        undefined,
      );
      const txHash = CML.hash_transaction(body).to_hex();
      const signed = {
        toTransaction: () => transaction,
        toHash: () => txHash,
      } as TxSigned;
      await first.boundary(
        "init",
        prepared(),
      )({ txHash, signed, referenceScripts: [] });
      const restartedStore = new DirectoryFraudProofWorkflowJournalStore(
        directory,
      );
      const restarted = adapter(restartedStore);
      await expect(restarted.journal.load()).resolves.toMatchObject({
        transactionIds: { init: txHash },
      });
      expect(
        (await restartedStore.load(first.workflowId)).map(
          ({ event }) => event.kind,
        ),
      ).toEqual([
        "started",
        "prepared",
        "preflight_passed",
        "submission_intent",
      ]);
      await restarted.journal.save({
        prepared: prepared(),
        confirmed: ["init"],
        transactionIds: { init: txHash },
      });
      await expect(
        adapter(
          new DirectoryFraudProofWorkflowJournalStore(directory),
        ).journal.load(),
      ).resolves.toMatchObject({ confirmed: ["init"] });
      await expect(
        adapter(restartedStore, prepared("a".repeat(64))).journal.load(),
      ).rejects.toThrow(/digest differs/u);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});

describe("field-preimage-length removal funding binding", () => {
  afterEach(() => vi.restoreAllMocks());
  it.each([true, false])(
    "uses exact signed removal authority and rejects its absence (%s)",
    async (admitted) => {
      const directory = await mkdtemp(join(tmpdir(), "field-preimage-remove-"));
      try {
        const store = new DirectoryFraudProofWorkflowJournalStore(directory);
        const bridge = adapter(store);
        await bridge.journal.load();
        const body = CML.TransactionBody.new(
          CML.TransactionInputList.new(),
          CML.TransactionOutputList.new(),
          0n,
        );
        const transaction = CML.Transaction.new(
          body,
          CML.TransactionWitnessSet.new(),
          true,
          undefined,
        );
        const txHash = CML.hash_transaction(body).to_hex();
        const signed = {
          toTransaction: () => transaction,
          toHash: () => txHash,
        } as TxSigned;
        const authority: removal.FraudSlashFundingAuthority = {
          deploymentFingerprint: "1".repeat(64),
          economicsPolicyDigest: "0".repeat(64),
          category: "fieldPreimageLengthMismatch",
          headerHash: prepared().headerHash,
          fraudProofOutRef: `${"3".repeat(64)}#1`,
          removedStateQueueOutRef: `${"4".repeat(64)}#2`,
          operatorOutRef: `${"5".repeat(64)}#0`,
          operatorBondLovelace: "4000000",
          tranche: "full",
          exactFeeLovelace: "2000000",
          rewardLovelace: "2000000",
          rewardAddress: "test",
          transactionHash: txHash,
          transactionBodySha256: "6".repeat(64),
          signedTransactionCborHex: transaction.to_cbor_hex(),
          inputs: [],
        };
        // The removal builder alone mints this opaque authority. This test checks
        // the central bridge's consumption of it, not the builder's validation.
        const readAuthority = vi
          .spyOn(removal, "readFraudSlashFundingAuthority")
          .mockImplementation((candidate) => {
            expect(candidate).toBe(signed);
            return admitted ? authority : null;
          });
        const pending = bridge.boundary(
          "remove",
          prepared(),
        )({ txHash, signed, referenceScripts: [] });
        if (admitted) {
          await pending;
          expect(
            (await store.load(bridge.workflowId)).at(-1)?.event,
          ).toMatchObject({
            kind: "submission_intent",
            actionId: "fieldPreimageLengthMismatch:remove",
            actionInput: {
              actionKind: "remove",
              nextRemovalOutRef: authority.removedStateQueueOutRef,
              fraudProofOutRef: authority.fraudProofOutRef,
            },
          });
        } else {
          await expect(pending).rejects.toThrow(
            "removal omitted authenticated slash funding authority",
          );
          expect(
            (await store.load(bridge.workflowId)).map(
              ({ event }) => event.kind,
            ),
          ).toEqual(["started", "prepared"]);
        }
        expect(readAuthority).toHaveBeenCalledOnce();
      } finally {
        await rm(directory, { recursive: true, force: true });
      }
    },
  );
});
