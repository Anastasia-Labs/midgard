import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect } from "vitest";

import type { WatcherOperationsMetrics } from "../../src/runtime/operations-observability.js";
import type { createEmulatorChainTransport } from "./emulator-chain-transport.js";

/**
 * Advance genuine native frames only after the installed workflow reports
 * its removal included but not yet final: the proof step's latest status is
 * `confirmed`, and none is `completed`.
 */
export const createTerminalRelease = (
  chain: Pick<Awaited<ReturnType<typeof createEmulatorChainTransport>>, "grow">,
) => {
  const releaseDepth =
    DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth + 2;
  let advanced = false;
  return {
    observe: async (steps: WatcherOperationsMetrics["proofSteps"]) => {
      if (advanced || steps.confirmed === "0") return;
      expect(steps.completed).toBe("0");
      await chain.grow(releaseDepth - 1);
      advanced = true;
    },
    /** A completion reported only after the chain grew past its release. */
    assertCompleted: () => {
      expect(advanced).toBe(true);
    },
  };
};
