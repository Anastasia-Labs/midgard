import { describe, expect, it, vi } from "vitest";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import type { FraudProofWorkflowTerminalVerifier } from "../src/workflow/orchestrator.js";
import {
  canonicalEvidence,
  DEPLOYMENT_FINGERPRINT,
  makeAdapter,
  PROOF_TX_HASH,
  RELEASE_FINALITY_POLICY,
  REMOVAL_TX_HASH,
  run,
  terminal,
  terminalVerifier,
} from "./workflow.make-adapter.js";

describe("provisional fault-proof completion", () => {
  it("advances on inclusion and anchors the same execution after restart", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter();
    const observe = adapter.observe;
    let depth = 1;
    adapter.observe = async (context) => {
      const result = await observe(context);
      return result.kind === "completed"
        ? {
            ...result,
            terminal: {
              ...result.terminal,
              observedAt: {
                ...result.terminal.observedAt,
                confirmationDepth: depth,
              },
            },
          }
        : result;
    };
    const verify = vi.fn(
      async ({
        candidate,
      }: Parameters<FraudProofWorkflowTerminalVerifier["verify"]>[0]) =>
        candidate,
    );
    const verifyIncluded = vi.fn(
      async ({
        candidate,
      }: Parameters<FraudProofWorkflowTerminalVerifier["verify"]>[0]) =>
        candidate,
    );
    const verifier = { ...terminalVerifier, verify, verifyIncluded };
    const included = await run({ evidence, adapter, journal, verifier });
    expect(included.kind).toBe("terminal_included");
    expect(verify).not.toHaveBeenCalled();
    expect(verifyIncluded).toHaveBeenCalledOnce();
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    for (depth of [
      30,
      RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth,
      RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 1,
    ]) {
      expect((await run({ evidence, adapter, journal, verifier })).kind).toBe(
        "terminal_included",
      );
      expect(verify).not.toHaveBeenCalled();
    }
    depth = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;
    const anchored = await run({ evidence, adapter, journal, verifier });
    expect(anchored.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(verify).toHaveBeenCalledOnce();
  });

  it.each(["terminal_included", "completed"] as const)(
    "reobserves a shallow %s terminal after rollback and reconciles prior actions",
    async (legacyKind) => {
      const evidence = await canonicalEvidence();
      let journal = new MemoryFraudProofWorkflowJournalStore();
      const onChain = new Set<string>();
      const adapter = makeAdapter({
        reconcile: async ({ txHash }) => {
          onChain.add(txHash!);
          return { kind: "confirmed", txHash: txHash! };
        },
      });
      adapter.observe = async () =>
        onChain.has(REMOVAL_TX_HASH)
          ? {
              kind: "completed",
              terminal: {
                ...terminal(evidence.headerHash),
                observedAt: {
                  ...terminal(evidence.headerHash).observedAt,
                  confirmationDepth: 30,
                },
              },
            }
          : {
              kind: "action_required",
              action: onChain.has(PROOF_TX_HASH)
                ? { actionId: "remove", input: { step: 1 } }
                : { actionId: "prove", input: { step: 0 } },
            };
      const verifier = {
        ...terminalVerifier,
        verifyIncluded: terminalVerifier.verify,
      };
      expect((await run({ evidence, adapter, journal, verifier })).kind).toBe(
        "terminal_included",
      );
      if (legacyKind === "completed") {
        const retained = await journal.load(
          computeFraudProofWorkflowId({
            schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
            deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
            category: "doubleSpend",
            target: {
              kind: "state_queue_header",
              headerHash: evidence.headerHash,
            },
          }),
        );
        journal = new MemoryFraudProofWorkflowJournalStore();
        for (const entry of retained)
          await journal.append(
            {
              ...entry,
              event:
                entry.event.kind === "terminal_included"
                  ? { ...entry.event, kind: "completed" }
                  : entry.event,
            },
            entry.sequence,
          );
      }
      onChain.clear();
      const resumed = await run({ evidence, adapter, journal, verifier });
      expect(resumed.kind).toBe("terminal_included");
      if (resumed.kind !== "terminal_included") return;
      expect(
        resumed.entries
          .filter(({ event }) => event.kind === "reobserved")
          .map(({ event }) => "actionId" in event && event.actionId),
      ).toEqual(["prove", "remove"]);
      expect(adapter.preflight).toHaveBeenCalledTimes(2);
      expect(adapter.submit).toHaveBeenCalledTimes(2);
    },
  );
});
