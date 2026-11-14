import { expect, vi } from "vitest";

import type { WatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";

export const waitForFaultProofSupervisorIdle = async (
  supervisor: WatcherFaultProofSupervisor,
) =>
  vi.waitFor(
    async () => {
      if (supervisor.status().phase === "blocked") await supervisor.done;
      expect(supervisor.status().phase).toBe("accepting");
      expect(supervisor.status().activeJob).toBeNull();
      expect(supervisor.status().queuedJobCount).toBe(0);
    },
    {
      // Real journal persistence exceeded 1 s in 3/20 contended runs. Explicit
      // barriers cover ordering; this only bounds invocation drain time.
      timeout: 10_000,
    },
  );
