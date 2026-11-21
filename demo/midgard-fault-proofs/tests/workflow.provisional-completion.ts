import { describe, expect, it, vi } from "vitest";

import type { FraudProofWorkflowTerminalVerifier } from "../src/workflow/orchestrator.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  chainOf,
  makeAdapter,
  PROOF_TX_HASH,
  RELEASE_FINALITY_POLICY,
  REMOVAL_TX_HASH,
  run,
  terminalVerifier,
} from "./workflow.make-adapter.js";

describe("provisional fault-proof completion", () => {
  it("advances on inclusion and anchors the same removal after restarts, sending nothing again", async () => {
    const evidence = await canonicalEvidence();
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
    const included = await run({
      evidence,
      adapter,
      process: createTestWorkflowProcess(),
      verifier,
    });
    expect(included.kind).toBe("terminal_included");
    expect(verify).not.toHaveBeenCalled();
    expect(verifyIncluded).toHaveBeenCalledOnce();
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    for (depth of [
      30,
      RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth,
      RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 1,
    ]) {
      // Each run is a fresh process: a restart.
      const process = createTestWorkflowProcess();
      expect((await run({ evidence, adapter, process, verifier })).kind).toBe(
        "terminal_included",
      );
      expect(verify).not.toHaveBeenCalled();
    }
    depth = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;
    const anchored = await run({
      evidence,
      adapter,
      process: createTestWorkflowProcess(),
      verifier,
    });
    expect(anchored.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(verify).toHaveBeenCalledOnce();
  });

  it("holds a shallow terminal after a rollback on the transactions it has in flight, building nothing again", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter();
    const chain = chainOf(adapter);
    const observe = adapter.observe;
    adapter.observe = async (context) => {
      const result = await observe(context);
      return result.kind === "completed"
        ? {
            ...result,
            terminal: {
              ...result.terminal,
              observedAt: {
                ...result.terminal.observedAt,
                confirmationDepth: 30,
              },
            },
          }
        : result;
    };
    const verifier = {
      ...terminalVerifier,
      verifyIncluded: terminalVerifier.verify,
    };
    const process = createTestWorkflowProcess();
    expect((await run({ evidence, adapter, process, verifier })).kind).toBe(
      "terminal_included",
    );
    // A rollback drops both: they are absent but may land again.
    chain.status.set(PROOF_TX_HASH, "rebroadcast");
    chain.status.set(REMOVAL_TX_HASH, "rebroadcast");
    const held = await run({ evidence, adapter, process, verifier });
    expect(held).toMatchObject({ kind: "pending", awaitingActionId: "prove" });
    chain.land(PROOF_TX_HASH);
    chain.land(REMOVAL_TX_HASH);
    const relanded = await run({ evidence, adapter, process, verifier });
    expect(relanded.kind).toBe("terminal_included");
    expect(adapter.preflight).toHaveBeenCalledTimes(2);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });
});
