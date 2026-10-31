import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowTerminal,
} from "@al-ft/midgard-fault-proofs";
import { expect } from "vitest";

import type { createEmulatorChainTransport } from "./emulator-chain-transport.js";

/** Advance genuine native frames only after the installed workflow proves inclusion. */
export const createTerminalRelease = (
  chain: Pick<Awaited<ReturnType<typeof createEmulatorChainTransport>>, "grow">,
) => {
  const releaseDepth =
    DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 2;
  let advanced = false;
  return {
    observe: async (entries: readonly FraudProofWorkflowJournalEntry[]) => {
      const event = entries.at(-1)?.event;
      if (advanced || event?.kind !== "terminal_included") return;
      expect(event.terminal.observedAt.confirmationDepth).toBeLessThan(
        releaseDepth,
      );
      expect(entries.some(({ event }) => event.kind === "completed")).toBe(
        false,
      );
      await chain.grow(releaseDepth - 1);
      advanced = true;
    },
    assertCompleted: (terminal: FraudProofWorkflowTerminal) => {
      expect(advanced).toBe(true);
      expect(terminal.observedAt.confirmationDepth).toBeGreaterThanOrEqual(
        releaseDepth,
      );
    },
  };
};
