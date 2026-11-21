/**
 * An objective whose correction is included but not yet final. Only a
 * rollback that takes its inclusion off the chain, or the inclusion reaching
 * completion depth, can change its outcome: other observations, and
 * rollbacks above the inclusion, leave it alone.
 */
import { expect, it } from "vitest";

import type { WatcherProofRetention } from "../../src/l1-follower/proof-retention.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

export const registerTerminalStandingTests = (
  setupScenario: typeof setup,
): void => {
  it("re-examines an included objective only once its inclusion leaves the chain or reaches completion depth", async () => {
    const test = await setupScenario(undefined, false);
    await test.fixture.run(await test.fixture.recover());
    await test.writeTerminal(true);
    const k = test.raw.binding.releaseFinality.policy.automaticRecoveryMaxDepth;
    const inclusion = test.terminal.observedAt;
    /** The follower's chain: the inclusion block's depth, or off the chain. */
    let depth: number | null = 1;
    const retention: WatcherProofRetention = {
      ...storelessProofRetention,
      securityParameter: k,
      blockDepth: async ({ slot, blockHash }) =>
        slot === inclusion.slot && blockHash === inclusion.blockHash
          ? depth
          : null,
    };
    const supervisor = test.createSupervisor(retention);
    /** One follower change; the removal took the header off the queue. */
    const observe = async (
      generation: number,
      inclusionDepth: number | null,
    ) => {
      depth = inclusionDepth;
      await test.request(supervisor, generation, test.fixture.fresh, false)
        .accepted;
      await test.idle(supervisor);
      return test.runOrResume.mock.calls.length;
    };
    // The chain grows, and a rollback above the inclusion: nothing changes.
    expect(await observe(1, 1)).toBe(0);
    expect(await observe(2, 2)).toBe(0);
    expect(await observe(3, k + 1)).toBe(0);
    // A rollback takes the inclusion block off the chain.
    expect(await observe(4, null)).toBe(1);
    expect(test.runOrResume.mock.calls[0]![0].mode).toBe("resume");
    // The inclusion stands again, and reaches completion depth.
    expect(await observe(5, k + 2)).toBe(2);
    expect(supervisor.status().unfinishedObjectiveCount).toBe(1);
  });
};
