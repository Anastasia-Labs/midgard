import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import type { UnusedRedeemerEvidence } from "../src/unused-redeemer/family-v1.js";
import {
  cancelUnusedRedeemerWorkflow,
  runUnusedRedeemerWorkflow,
  type UnusedRedeemerActuator,
  type UnusedRedeemerCursor,
  type UnusedRedeemerJournal,
  type UnusedRedeemerJournalEntry,
  type UnusedRedeemerWorkflowAction,
  type UnusedRedeemerWorkflowStage,
} from "../src/unused-redeemer/workflow-v1.js";

const stages: readonly UnusedRedeemerWorkflowStage[] = [
  "none",
  "step01",
  "step02",
  "step02a",
  "step02b",
  "step02c",
  "step03",
  "step04",
  "step05",
  "step06",
  "proven",
  "removed",
];
const nextFor: Record<
  UnusedRedeemerWorkflowAction,
  UnusedRedeemerWorkflowStage
> = {
  submitInit: "step01",
  submitStep01: "step02",
  submitStep02: "step02a",
  submitStep02a: "step02b",
  submitStep02b: "step02c",
  submitStep02c: "step03",
  submitStep03: "step04",
  submitStep04: "step05",
  submitStep05: "step06",
  submitStep06: "proven",
  removeDescendants: "removed",
  cancel: "cancelled",
};
const evidence = (forced: boolean) =>
  ({
    finding: {
      subject: { transaction_id: (forced ? "22" : "11").repeat(32) },
      redeemerIndex: 0,
    },
    targetRedeemerLeafHex: "33".repeat(32),
    checkpointDigest: "44".repeat(32),
  }) as unknown as UnusedRedeemerEvidence;
const harness = (initial: UnusedRedeemerWorkflowStage = "none") => {
  const entries: UnusedRedeemerJournalEntry[] = [];
  let cursor: UnusedRedeemerCursor = {
    stage: initial,
    threadOutRef: `${initial}#0`,
    checkpointDigest: "44".repeat(32),
  };
  const confirmed = new Set<string>();
  let nonce = 0;
  const journal: UnusedRedeemerJournal = {
    load: async () => entries,
    append: async (entry) => {
      entries.push(entry);
    },
  };
  const actuator: UnusedRedeemerActuator = {
    observe: async () => cursor,
    capture: async ({ action }) => {
      const targetStage = nextFor[action];
      const target = {
        stage: targetStage,
        threadOutRef: `${targetStage}#0`,
        checkpointDigest: cursor.checkpointDigest,
      };
      const txHash = createHash("sha256")
        .update(`${action}:${String(nonce++)}`)
        .digest("hex");
      return {
        txHash,
        target,
        submit: async () => {
          cursor = target;
          confirmed.add(txHash);
          return txHash;
        },
      };
    },
    transactionConfirmed: async (txHash) => confirmed.has(txHash),
  };
  return { entries, journal, actuator, stage: () => cursor.stage };
};

describe("unusedRedeemer durable nine-script workflow", () => {
  it.each([false, true])(
    "runs direction forced=%s through permanent mint and canonical removal",
    async (forced) => {
      const h = harness();
      for (let turns = 0; turns < 24 && h.stage() !== "removed"; turns += 1)
        await runUnusedRedeemerWorkflow({
          evidence: evidence(forced),
          journal: h.journal,
          actuator: h.actuator,
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
        "submitStep02a",
        "submitStep02b",
        "submitStep02c",
        "submitStep03",
        "submitStep04",
        "submitStep05",
        "submitStep06",
        "removeDescendants",
      ]);
    },
  );

  it("reconciles an exact captured checkpoint after restart", async () => {
    const h = harness("step02b");
    await runUnusedRedeemerWorkflow({
      evidence: evidence(false),
      journal: h.journal,
      actuator: h.actuator,
    });
    expect(h.stage()).toBe("step02c");
    const restarted: UnusedRedeemerActuator = { ...h.actuator };
    expect(
      await runUnusedRedeemerWorkflow({
        evidence: evidence(false),
        journal: h.journal,
        actuator: restarted,
      }),
    ).toBe("step02c");
    expect(h.entries.at(-1)?.phase).toBe("confirmed");
  });

  it.each(stages.slice(1, 10))(
    "cancels from nonterminal physical %s",
    async (stage) => {
      const h = harness(stage);
      await expect(
        cancelUnusedRedeemerWorkflow({
          evidence: evidence(false),
          journal: h.journal,
          actuator: h.actuator,
        }),
      ).resolves.toBe("cancelled");
      expect(h.stage()).toBe("cancelled");
    },
  );
});
