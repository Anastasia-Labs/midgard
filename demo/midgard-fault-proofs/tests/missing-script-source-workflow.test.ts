import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import type { MissingScriptSourceEvidence } from "../src/missing-script-source/family.js";
import {
  cancelMissingScriptSourceWorkflow,
  type MissingScriptSourceCursor,
  type MissingScriptSourceJournal,
  type MissingScriptSourceJournalEntry,
  type MissingScriptSourceTransactionPort,
  type MissingScriptSourceWorkflowAction,
  type MissingScriptSourceWorkflowStage,
  runMissingScriptSourceWorkflow,
} from "../src/missing-script-source/workflow.js";

const nextFor: Record<
  MissingScriptSourceWorkflowAction,
  MissingScriptSourceWorkflowStage
> = {
  submitInit: "step01",
  submitStep01: "step02",
  submitStep02: "step03",
  submitStep03: "scan",
  submitScanOrResume: "scan",
  submitStep05: "proven",
  removeDescendants: "removed",
  cancel: "cancelled",
};
const evidence = (forced: boolean) =>
  ({
    finding: {
      subject: {
        transaction_id: (forced ? "22" : "11").repeat(32),
        direction: forced ? 1n : 0n,
      },
      executionIndex: 0,
      purposeKind: 0,
      purposeIndex: 0,
    },
    executionLeafHex: "33".repeat(32),
    itemCommitmentHex: "44".repeat(32),
  }) as unknown as MissingScriptSourceEvidence;

/**
 * A fake transaction port over a scan that needs `scanBatches` self-loop
 * transactions before the finalizer becomes the successor; every checkpoint
 * is the digest of the cursor it advances to, as the chain commits it.
 */
const harness = (
  initial: MissingScriptSourceWorkflowStage = "none",
  scanBatches = 2,
) => {
  const entries: MissingScriptSourceJournalEntry[] = [];
  let scanned = 0;
  let cursor: MissingScriptSourceCursor = {
    stage: initial,
    threadOutRef: `${initial}#0`,
    checkpointHash: "00".repeat(32),
    controlCbor: "",
  };
  const confirmed = new Set<string>();
  let nonce = 0;
  const journal: MissingScriptSourceJournal = {
    load: async () => entries,
    append: async (entry) => {
      entries.push(entry);
    },
  };
  const transactions: MissingScriptSourceTransactionPort = {
    observe: async () => cursor,
    capture: async ({ action }) => {
      const targetStage =
        action === "submitScanOrResume" && scanned + 1 >= scanBatches
          ? "proven"
          : nextFor[action];
      const target: MissingScriptSourceCursor = {
        stage: targetStage,
        threadOutRef: `${targetStage}#${String(nonce)}`,
        checkpointHash: createHash("sha256")
          .update(`${action}:${String(scanned)}`)
          .digest("hex"),
        controlCbor: "",
      };
      const txHash = createHash("sha256")
        .update(`${action}:${String(nonce++)}`)
        .digest("hex");
      return {
        txHash,
        target,
        submit: async () => {
          if (action === "submitScanOrResume") scanned += 1;
          cursor = target;
          confirmed.add(txHash);
          return txHash;
        },
      };
    },
    transactionConfirmed: async (txHash) => confirmed.has(txHash),
  };
  return { entries, journal, transactions, stage: () => cursor.stage };
};

describe("missingScriptSource durable workflow", () => {
  it.each([false, true])(
    "runs direction forced=%s through the resumable scan to permanent mint and removal",
    async (forced) => {
      const h = harness("none", 3);
      for (let turns = 0; turns < 24 && h.stage() !== "removed"; turns += 1)
        await runMissingScriptSourceWorkflow({
          evidence: evidence(forced),
          journal: h.journal,
          transactions: h.transactions,
        });
      expect(h.stage()).toBe("removed");
      expect(
        h.entries
          .filter((entry) => entry.phase === "intent")
          .map((entry) => entry.action),
      ).toEqual([
        "submitInit",
        "submitStep01",
        "submitStep02",
        "submitStep03",
        "submitScanOrResume",
        "submitScanOrResume",
        "submitScanOrResume",
        "removeDescendants",
      ]);
      // Every intent is followed by its submitted record for the same hash.
      const intents = h.entries.filter((entry) => entry.phase === "intent");
      for (const intent of intents)
        expect(
          h.entries.some(
            (entry) =>
              entry.phase === "submitted" && entry.txHash === intent.txHash,
          ),
        ).toBe(true);
    },
  );

  it("reconciles an interrupted scan batch against the live checkpoint after restart", async () => {
    const h = harness("scan", 3);
    await runMissingScriptSourceWorkflow({
      evidence: evidence(false),
      journal: h.journal,
      transactions: h.transactions,
    });
    expect(h.stage()).toBe("scan");
    const submitted = h.entries.at(-1);
    expect(submitted?.phase).toBe("submitted");
    expect(submitted?.action).toBe("submitScanOrResume");
    // A fresh process reads the journal's unconfirmed intent, confirms it
    // against the chain, and appends the confirmation without resubmitting.
    const restarted = { ...h.transactions };
    expect(
      await runMissingScriptSourceWorkflow({
        evidence: evidence(false),
        journal: h.journal,
        transactions: restarted,
      }),
    ).toBe("scan");
    expect(h.entries.at(-1)?.phase).toBe("confirmed");
    expect(h.entries.at(-1)?.txHash).toBe(submitted?.txHash);
  });

  it("refuses a restart whose live checkpoint differs from the captured target", async () => {
    const h = harness("scan", 3);
    await runMissingScriptSourceWorkflow({
      evidence: evidence(false),
      journal: h.journal,
      transactions: h.transactions,
    });
    const substituted: MissingScriptSourceTransactionPort = {
      ...h.transactions,
      observe: async () => ({
        ...(await h.transactions.observe("")),
        checkpointHash: "ff".repeat(32),
      }),
    };
    await expect(
      runMissingScriptSourceWorkflow({
        evidence: evidence(false),
        journal: h.journal,
        transactions: substituted,
      }),
    ).rejects.toThrow(/checkpoint substitution/u);
  });

  it("refuses a provider that substitutes the captured transaction identity", async () => {
    const h = harness();
    const lying: MissingScriptSourceTransactionPort = {
      ...h.transactions,
      capture: async (input) => {
        const captured = await h.transactions.capture(input);
        return { ...captured, submit: async () => "00".repeat(32) };
      },
    };
    await expect(
      runMissingScriptSourceWorkflow({
        evidence: evidence(false),
        journal: h.journal,
        transactions: lying,
      }),
    ).rejects.toThrow(/substituted transaction identity/u);
  });

  it.each(["step01", "step02", "step03", "scan"] as const)(
    "cancels from nonterminal stage %s",
    async (stage) => {
      const h = harness(stage);
      await expect(
        cancelMissingScriptSourceWorkflow({
          evidence: evidence(false),
          journal: h.journal,
          transactions: h.transactions,
        }),
      ).resolves.toBe("cancelled");
      expect(h.stage()).toBe("cancelled");
    },
  );

  it.each(["none", "proven", "removed", "cancelled"] as const)(
    "refuses to cancel from %s",
    async (stage) => {
      const h = harness(stage);
      await expect(
        cancelMissingScriptSourceWorkflow({
          evidence: evidence(false),
          journal: h.journal,
          transactions: h.transactions,
        }),
      ).rejects.toThrow(/cannot cancel/u);
    },
  );
});
