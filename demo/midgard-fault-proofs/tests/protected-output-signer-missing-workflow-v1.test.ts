import { acceptedVerdictSubjectV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import type {
  ProtectedOutputSignerActuatorV1,
  ProtectedOutputSignerJournalEntryV1,
  ProtectedOutputSignerJournalV1,
  ProtectedOutputSignerStageV1,
} from "../src/protected-output-signer-missing/index.js";
import {
  protectedOutputSignerEvidenceIdentityV1,
  runProtectedOutputSignerMissingWorkflowV1,
} from "../src/protected-output-signer-missing/index.js";

const evidence = Object.freeze({
  subject: acceptedVerdictSubjectV1("11".repeat(32)),
  outputIndex: 0,
  canonicalTransactionCborHex: "80",
  outputCborHex: "a0",
  paymentCredentialHex: "22".repeat(28),
  witnessSetHashHex: "33".repeat(32),
  addressWitnessFieldPreimageHex: "80",
  validSignerHashes: Object.freeze([]),
  signerPresent: false,
  outputCarriage: "Inline" as const,
  witnessCarriage: "Inline" as const,
  checkpoints: Object.freeze([]),
});

const memoryJournal = () => {
  const entries: ProtectedOutputSignerJournalEntryV1[] = [];
  const journal: ProtectedOutputSignerJournalV1 = {
    load: async () => entries,
    append: async (entry) => {
      entries.push(entry);
    },
  };
  return { entries, journal };
};

describe("protectedOutputSignerMissing durable workflow", () => {
  it("reconciles the exact pre-submit intent after restart", async () => {
    const identity = protectedOutputSignerEvidenceIdentityV1(evidence);
    const { entries, journal } = memoryJournal();
    entries.push({
      sequence: 0,
      identity,
      sourceStage: "step03",
      targetStage: "scanning",
      action: "submitStep03",
      phase: "intent",
      txHash: "44".repeat(32),
    });
    let stage: ProtectedOutputSignerStageV1 = "scanning";
    const actuator: ProtectedOutputSignerActuatorV1 = {
      observe: async () => stage,
      transactionConfirmed: async (hash) =>
        hash === "44".repeat(32) || hash === "55".repeat(32),
      build: async ({ action }) => {
        if (action === "submitScan") stage = "step05";
        else if (action === "submitStep05") stage = "proven";
        else throw new Error(`unexpected ${action}`);
        return {
          txHash: "55".repeat(32),
          targetStage: stage,
          submit: async () => "55".repeat(32),
        };
      },
    };
    await expect(
      runProtectedOutputSignerMissingWorkflowV1({
        evidence,
        journal,
        actuator,
      }),
    ).rejects.toThrow(/removal requires/u);
    expect(entries[1]?.phase).toBe("confirmed");
    expect(entries[1]?.txHash).toBe("44".repeat(32));
  });

  it("refuses confirmed-hash stage substitution", async () => {
    const identity = protectedOutputSignerEvidenceIdentityV1(evidence);
    const { entries, journal } = memoryJournal();
    entries.push({
      sequence: 0,
      identity,
      sourceStage: "step02",
      targetStage: "step03",
      action: "submitStep02",
      phase: "intent",
      txHash: "66".repeat(32),
    });
    await expect(
      runProtectedOutputSignerMissingWorkflowV1({
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

  it("records intent before submit and rejects submitter substitution", async () => {
    const { entries, journal } = memoryJournal();
    await expect(
      runProtectedOutputSignerMissingWorkflowV1({
        evidence,
        journal,
        actuator: {
          observe: async () => "none",
          transactionConfirmed: async () => false,
          build: async () => ({
            txHash: "77".repeat(32),
            targetStage: "step01",
            submit: async () => "88".repeat(32),
          }),
        },
      }),
    ).rejects.toThrow(/changed exact transaction identity/u);
    expect(entries).toHaveLength(1);
    expect(entries[0]?.phase).toBe("intent");
  });
});
