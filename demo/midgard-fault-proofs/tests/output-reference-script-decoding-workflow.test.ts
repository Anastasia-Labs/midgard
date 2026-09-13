import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  cancelOutputReferenceScriptDecodingWorkflow,
  nextOutputReferenceScriptDecodingAction,
  type OutputReferenceScriptDecodingJournalEntry,
  runOutputReferenceScriptDecodingWorkflow,
} from "../src/output-reference-script-decoding/index.js";

const evidence = {
  subject: acceptedVerdictSubject("11".repeat(32)),
  outputIndex: 0,
  canonicalTransactionCborHex: "aa",
  outputFieldPreimageHex: "bb",
  outputCborHex: "cc",
  outputLength: 1,
  outputHashHex: "11".repeat(32),
  outputChunkHashes: ["11".repeat(32)],
  outputScanControls: [],
  referenceScriptItemHex: "dd",
  referenceScriptItemCommitmentHex: "22".repeat(32),
  initialControlCbor: "",
  resultClass: 0,
  accusedClass: -1,
  carriage: "Inline",
  chunkProofCount: 1,
} as const;

describe("outputReferenceScriptDecoding durable workflow", () => {
  it("selects deterministic resume actions", () => {
    expect(
      [
        "none",
        "step01",
        "step02",
        "outputScan",
        "referenceBind",
        "scan",
        "step06",
        "proven",
        "removed",
      ].map((stage) => nextOutputReferenceScriptDecodingAction(stage as never)),
    ).toEqual([
      "submitInit",
      "submitStep01",
      "submitStep02",
      "submitOutputScan",
      "submitReferenceBind",
      "submitStructuralScan",
      "submitStep06",
      "removeDescendants",
      "done",
    ]);
  });

  it("reconciles exact crash intent and refuses stage substitution", async () => {
    const entries: OutputReferenceScriptDecodingJournalEntry[] = [];
    let stage: "none" | "step01" = "none";
    let builds = 0;
    const journal = {
      load: async () => entries,
      append: async (entry: OutputReferenceScriptDecodingJournalEntry) => {
        entries.push(entry);
      },
    };
    let crash = true;
    const actuator = {
      observe: async () => stage,
      transactionConfirmed: async () => true,
      build: async () => {
        builds += 1;
        if (builds > 1) throw new Error("stop after recovery");
        return {
          txHash: "ab".repeat(32),
          targetStage: "step01" as const,
          submit: async () => {
            stage = "step01";
            if (crash) {
              crash = false;
              throw new Error("crash after apply");
            }
            return "ab".repeat(32);
          },
        };
      },
    };
    await expect(
      runOutputReferenceScriptDecodingWorkflow({
        evidence,
        journal,
        actuator,
      }),
    ).rejects.toThrow(/crash after apply/u);
    expect(builds).toBe(1);
    expect(entries[0]?.phase).toBe("intent");
    await expect(
      runOutputReferenceScriptDecodingWorkflow({
        evidence,
        journal,
        actuator,
      }),
    ).rejects.toThrow(/stop after recovery/u);
    expect(entries.at(-1)?.phase).toBe("confirmed");
    entries.splice(1);
    stage = "none";
    await expect(
      runOutputReferenceScriptDecodingWorkflow({
        evidence,
        journal,
        actuator,
      }),
    ).rejects.toThrow(/substitution/u);
  });

  it.each(["outputScan", "referenceBind"] as const)(
    "cancels the separated %s step with exact identity",
    async (initial) => {
      const entries: OutputReferenceScriptDecodingJournalEntry[] = [];
      let stage: typeof initial | "cancelled" = initial;
      const journal = {
        load: async () => entries,
        append: async (entry: OutputReferenceScriptDecodingJournalEntry) => {
          entries.push(entry);
        },
      };
      const actuator = {
        observe: async () => stage,
        transactionConfirmed: async () => true,
        build: async ({ action }: { action: string }) => ({
          txHash: "cd".repeat(32),
          targetStage: "cancelled" as const,
          submit: async () => {
            expect(action).toBe("cancel");
            stage = "cancelled";
            return "cd".repeat(32);
          },
        }),
      };
      await expect(
        cancelOutputReferenceScriptDecodingWorkflow({
          evidence,
          journal,
          actuator,
        }),
      ).resolves.toBe("cancelled");
      expect(entries.map((entry) => entry.phase)).toEqual([
        "intent",
        "submitted",
        "confirmed",
      ]);
    },
  );
});
