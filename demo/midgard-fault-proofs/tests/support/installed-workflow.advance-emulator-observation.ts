import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type Emulator } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";

const isTerminalIncluded = (result: {
  readonly kind: string;
}): result is Extract<
  FraudProofWorkflowRunResult,
  { kind: "terminal_included" }
> => result.kind === "terminal_included";

/** Advances the real emulator, never a raw snapshot's reported depth. */
export const installedWorkflowEmulatorClock = (emulator: Emulator) => {
  const horizon = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
  let retained: string | undefined;
  let observedRecoveryEdge = false;
  const inspect = async (
    result: Extract<
      FraudProofWorkflowRunResult,
      { kind: "terminal_included" | "completed" }
    >,
  ) => {
    const status = await emulator.getTransactionStatus(
      result.terminal.correction.removalTxHash,
    );
    if (status.status !== "confirmed")
      throw new Error("terminal removal is not actually emulator-confirmed");
    const depth = status.confirmation.confirmations;
    if (depth === undefined)
      throw new Error("actual confirmation depth missing");
    expect(result.terminal.observedAt.confirmationDepth).toBe(depth);
    const identity = JSON.stringify({
      proofToken: result.terminal.proofToken,
      correction: result.terminal.correction,
      economics: result.terminal.economics,
      intents: result.entries.flatMap(({ event }) =>
        event.kind === "submission_intent" ? [event] : [],
      ),
    });
    if (retained === undefined) retained = identity;
    else
      expect(
        identity,
        "same signed intents and terminal outrefs across restart",
      ).toBe(retained);
    return depth;
  };
  return {
    // Initial header classification requires the signed release depth. Empty
    // blocks are explicit fixture setup, not capture-time finality fabrication.
    awaitReleaseDepth: () =>
      emulator.awaitBlock(DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth),
    // Any runner's result: only its terminal_included shape is read.
    advance: async (result: { readonly kind: string }) => {
      if (!isTerminalIncluded(result)) {
        emulator.awaitBlock();
        return;
      }
      const depth = await inspect(result);
      expect(depth).toBeLessThanOrEqual(horizon + 1);
      if (depth === horizon + 1) {
        observedRecoveryEdge = true;
        emulator.awaitBlock();
      } else emulator.awaitBlock(horizon + 1 - depth);
    },
    kind: async (result: FraudProofWorkflowRunResult) => {
      if (result.kind === "completed") {
        expect(
          observedRecoveryEdge,
          "reopened journal held at the recovery edge",
        ).toBe(true);
        expect(await inspect(result)).toBe(horizon + 2);
      }
      return result.kind;
    },
  };
};
