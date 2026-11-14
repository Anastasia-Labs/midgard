import { describe, expect, it } from "vitest";

import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
  terminal,
} from "./workflow.make-adapter.js";

describe("canonical reobservation with pending descendants", () => {
  it("closes an included descendant before reopening its still-included parent", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let parentIncluded = false;
    let childSubmitted = false;
    let childConfirmed = false;
    const reconciled: string[] = [];
    const adapter = makeAdapter({
      submit: async ({ preflight }) => {
        if (preflight.txHash === REMOVAL_TX_HASH) childSubmitted = true;
        return { kind: "submitted", txHash: preflight.txHash };
      },
      reconcile: async ({
        txHash,
        reconciliationOnly,
        signedTransactionCborHex,
        authorizeResubmission,
      }) => {
        if (reconciliationOnly) {
          expect(signedTransactionCborHex).toBeUndefined();
          expect(authorizeResubmission).toBeUndefined();
        }
        reconciled.push(txHash!);
        if (txHash === PROOF_TX_HASH) parentIncluded = true;
        else childConfirmed = true;
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    adapter.observe = async () =>
      childConfirmed
        ? { kind: "completed", terminal: terminal(evidence.headerHash) }
        : {
            kind: "action_required",
            // Inclusion of the closing child consumes the parent's effects.
            // Its journal must catch up before deriving the next action.
            action:
              parentIncluded && !childSubmitted
                ? { actionId: "remove", input: { step: 1 } }
                : { actionId: "prove", input: { step: 0 } },
          };
    const result = await run({ evidence, adapter, journal });
    expect(result.kind).toBe("completed");
    expect(reconciled).toEqual([PROOF_TX_HASH, PROOF_TX_HASH, REMOVAL_TX_HASH]);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(
      result.kind === "completed" &&
        result.entries.some(
          ({ event }) =>
            event.kind === "reobserved" && event.actionId === "prove",
        ),
    ).toBe(false);
  });
});
