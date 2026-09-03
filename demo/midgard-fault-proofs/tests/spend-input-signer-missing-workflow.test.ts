import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type {
  SpendInputSignerActuator,
  SpendInputSignerJournal,
  SpendInputSignerJournalEntry,
  SpendInputSignerMissingEvidence,
  SpendInputSignerStage,
} from "../src/spend-input-signer-missing/index.js";
import {
  runSpendInputSignerMissingWorkflow,
  spendInputSignerWorkflowEvidenceIdentity,
} from "../src/spend-input-signer-missing/index.js";

const evidence: SpendInputSignerMissingEvidence = Object.freeze({
  subject: acceptedVerdictSubject("11".repeat(32)),
  inputIndex: 0,
  canonicalTransactionCborHex: "80",
  inputFieldPreimageHex: "80",
  addressWitnessFieldPreimageHex: "80",
  witnessSetHashHex: "22".repeat(32),
  resolved: Object.freeze({
    priorRoot: "33".repeat(32),
    transactionId: "44".repeat(32),
    outputIndex: 0,
    descriptorCborHex: "80",
    outputCborHex: "80",
    membershipProofCborHex: "80",
    membershipProof: [],
  }),
  paymentCredentialHex: "55".repeat(28),
  validSignerHashes: Object.freeze([]),
  signerMissing: true,
  inputCarriage: "Inline",
  witnessCarriage: "Inline",
  checkpoints: Object.freeze([{ cursor: 0, signerPresent: false }]),
});

const memoryJournal = () => {
  const entries: SpendInputSignerJournalEntry[] = [];
  const journal: SpendInputSignerJournal = {
    load: async () => entries,
    append: async (entry) => {
      entries.push(entry);
    },
  };
  return { entries, journal };
};

describe("spendInputSignerMissing durable workflow", () => {
  it("reconciles an exact intent after a fresh-process restart", async () => {
    const identity = spendInputSignerWorkflowEvidenceIdentity(evidence);
    const { entries, journal } = memoryJournal();
    entries.push({
      sequence: 0,
      identity,
      sourceStage: "step03",
      targetStage: "scanning",
      action: "submitStep03",
      phase: "intent",
      txHash: "66".repeat(32),
    });
    let stage: SpendInputSignerStage = "scanning";
    const actuator: SpendInputSignerActuator = {
      observe: async () => stage,
      transactionConfirmed: async (hash) =>
        hash === "66".repeat(32) || hash === "77".repeat(32),
      build: async ({ action }) => {
        if (action === "submitScan") stage = "step05";
        else if (action === "submitStep05") stage = "proven";
        else throw new Error(`unexpected ${action}`);
        return {
          txHash: "77".repeat(32),
          targetStage: stage,
          submit: async () => "77".repeat(32),
        };
      },
    };
    await expect(
      runSpendInputSignerMissingWorkflow({ evidence, journal, actuator }),
    ).rejects.toThrow(/removal requires/u);
    expect(entries[1]).toMatchObject({
      phase: "confirmed",
      txHash: "66".repeat(32),
    });
  });

  it("refuses confirmed-hash stage substitution", async () => {
    const identity = spendInputSignerWorkflowEvidenceIdentity(evidence);
    const { entries, journal } = memoryJournal();
    entries.push({
      sequence: 0,
      identity,
      sourceStage: "step02",
      targetStage: "step03",
      action: "submitStep02",
      phase: "intent",
      txHash: "88".repeat(32),
    });
    await expect(
      runSpendInputSignerMissingWorkflow({
        evidence,
        journal,
        actuator: {
          observe: async () => "step05",
          transactionConfirmed: async () => true,
          build: async () => {
            throw new Error("must not build");
          },
        },
      }),
    ).rejects.toThrow(/stage\/transaction identity substitution/u);
  });

  it("writes intent before submit and refuses tx-hash substitution", async () => {
    const { entries, journal } = memoryJournal();
    await expect(
      runSpendInputSignerMissingWorkflow({
        evidence,
        journal,
        actuator: {
          observe: async () => "none",
          transactionConfirmed: async () => false,
          build: async () => ({
            txHash: "99".repeat(32),
            targetStage: "step01",
            submit: async () => "aa".repeat(32),
          }),
        },
      }),
    ).rejects.toThrow(/changed exact transaction identity/u);
    expect(entries).toHaveLength(1);
    expect(entries[0]?.phase).toBe("intent");
  });

  it.each(["step01", "step02", "step03", "scanning", "step05"] as const)(
    "selects cancel from nonterminal %s when the production actuator requests it",
    async (sourceStage) => {
      const { entries, journal } = memoryJournal();
      const actuator: SpendInputSignerActuator = {
        observe: async () => "cancelled",
        transactionConfirmed: async () => true,
        build: async () => {
          throw new Error("confirmed cancellation must not rebuild");
        },
      };
      // The deterministic happy-path selector cannot invent cancellation;
      // cancellation is a concrete actuator choice and is journaled identically.
      const cancelledIntent: SpendInputSignerJournalEntry = {
        sequence: 0,
        identity: spendInputSignerWorkflowEvidenceIdentity(evidence),
        sourceStage,
        targetStage: "cancelled",
        action: "cancel",
        phase: "intent",
        txHash: "bb".repeat(32),
      };
      entries.push(cancelledIntent);
      await expect(
        runSpendInputSignerMissingWorkflow({ evidence, journal, actuator }),
      ).resolves.toBe("cancelled");
    },
  );
});
