import { expect, it } from "vitest";

import {
  MemoryFraudProofWorkflowJournalStore,
  validateFraudProofWorkflowJournal,
} from "../src/workflow/journal.js";
import { computeFraudProofRawL1PointId } from "../src/workflow/raw-l1-snapshot.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  run,
} from "./workflow.make-adapter.js";
it("accepts an independent historical retirement receipt after final completion while still refusing new protocol progress", async () => {
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const result = await run({
    evidence: await canonicalEvidence(),
    adapter: makeAdapter(),
    journal,
  });
  expect(result.kind).toBe("completed");
  if (result.kind !== "completed") throw new Error("Expected final completion");
  const saved = result.entries.at(-1)!;
  const boundary = { slot: "1000", blockNo: "50", blockHash: "ab".repeat(32) };
  const tip = { slot: "10000", blockNo: "2211", blockHash: "cd".repeat(32) };
  const retirement = {
    transactionHash: PROOF_TX_HASH,
    reason: "invalidated" as const,
    releaseFinalPoint: {
      ...boundary,
      pointId: computeFraudProofRawL1PointId(boundary),
    },
    canonicalPoint: { ...tip, pointId: computeFraudProofRawL1PointId(tip) },
  };
  const entries = [
    ...result.entries,
    {
      ...saved,
      sequence: result.entries.length,
      event: {
        kind: "signed_attempt_retired" as const,
        txHash: PROOF_TX_HASH,
        retirement,
      },
    },
  ];
  expect(() =>
    validateFraudProofWorkflowJournal({
      workflowId: saved.workflowId,
      expectedIdentity: saved.identity,
      entries,
    }),
  ).not.toThrow();
  expect(() =>
    validateFraudProofWorkflowJournal({
      workflowId: saved.workflowId,
      expectedIdentity: saved.identity,
      entries: [
        ...entries,
        {
          ...saved,
          sequence: entries.length,
          event: {
            kind: "reobserved",
            actionId: "proof.init",
            txHash: PROOF_TX_HASH,
          },
        },
      ],
    }),
  ).toThrow("after terminal completion");
});
