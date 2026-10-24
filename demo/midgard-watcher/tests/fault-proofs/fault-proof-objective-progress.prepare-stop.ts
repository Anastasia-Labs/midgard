import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { expect, it, vi } from "vitest";

import { prepareThroughPermit } from "../funding/superseded-attempt-replacement.js";
import { FixtureProcessStop } from "../support/fault-proof-funding-fixture.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

/** W2: the prepare commits a pending attempt with its preflight and intent,
 * so a stop before its submission leaves an attempt the objective drives,
 * past its latest safe start or with its header gone. */
export const registerPrepareStopTests = (setupScenario: typeof setup): void => {
  it.each([
    ["its latest safe start passes", true],
    ["its header leaves the queue", false],
  ] as const)(
    "reconciles the signed attempt a stop right after its prepare commit left once %s",
    async (_case, deadlinePasses) => {
      vi.useFakeTimers({ toFake: ["Date"] });
      try {
        const startedAt = Date.now();
        // Its latest safe start is a minute away.
        const test = await setupScenario(
          BigInt(
            startedAt -
              (MIDGARD_RETENTION_WINDOW.maturityMs -
                MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs) +
              60_000,
          ),
          true,
          true,
        );
        const supervisor = test.createSupervisor();
        await test.request(supervisor, 2).accepted;
        await test.idle(supervisor);
        expect(test.runOrResume).toHaveBeenCalledOnce();
        await supervisor.close();
        // The prepare commits its pending attempt with the preflight and
        // intent; the process stops before submitting it.
        test.fixture.useUnspentPendingInputs();
        test.fixture.stopAt("prepareTransition", "after");
        await expect(prepareThroughPermit(test.fixture)).rejects.toBeInstanceOf(
          FixtureProcessStop,
        );
        const attempt = (await test.fixture.records())[0]!.pendingTransition!;
        await test.fixture.restartStore();
        if (deadlinePasses) vi.setSystemTime(startedAt + 120_000);
        const restarted = test.createSupervisor();
        await test.request(restarted, 3, test.fixture.fresh, !deadlinePasses)
          .accepted;
        await test.idle(restarted);
        expect(test.runOrResume).toHaveBeenCalledTimes(2);
        expect(test.fixture.adapter.reconcile).toHaveBeenCalledWith(
          expect.objectContaining({ txHash: attempt.transactionHash }),
        );
        expect(restarted.status()).toMatchObject({
          phase: "accepting",
          journalDecisionMissing: [],
        });
        expect(test.executionKept()).toBe(true);
        expect(await test.fixture.records()).toMatchObject([
          { state: "active", pendingTransition: null },
        ]);
        expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
      } finally {
        vi.useRealTimers();
      }
    },
  );
};
