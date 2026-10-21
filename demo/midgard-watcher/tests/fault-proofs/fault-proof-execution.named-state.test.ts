/** A pending run in a state its family named for readiness: the watcher
 * holds the objective under that name, logs it at error level only when the
 * family marks it an alarm, and clears it once a later run reports
 * something else. */
import { afterEach, describe, expect, it, vi } from "vitest";

import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

describe("supervisor execution adapter with a family's named run state", () => {
  it.each([
    {
      name: "validation-trace-one-step-required",
      alarm: false,
      reason:
        "validation-trace-one-step-required: thread a at b#1 is ReadyForOneStep",
    },
    {
      name: "validation-trace-move-fee-over-cap",
      alarm: true,
      reason:
        "validation-trace-move-fee-over-cap: the timeout move's minimum fee 1500000 passes the per-move cap 1200000",
    },
  ])(
    "holds the objective under $name (alarm: $alarm) until a later run reports something else",
    async ({ name, alarm, reason }) => {
      const test = await setupFaultProofExecution();
      const before = await test.fixture.records();
      const logged: string[] = [];
      const stderr = vi
        .spyOn(process.stderr, "write")
        .mockImplementation((line) => logged.push(String(line)) > 0);
      const hold = {
        reason: name,
        detail: `doubleSpend/${test.input.job.headerHash}: ${reason}`,
      };
      try {
        test.runOrResume.mockResolvedValueOnce({
          kind: "pending",
          resumeOnObservation: true,
          reason,
          namedState: { name, alarm },
        });
        expect(await test.execution.execute(test.input)).toEqual({
          kind: "pending",
          resume: "await_observation",
          reason,
        });
        expect(test.execution.readiness()).toEqual([hold]);
        expect(logged).toEqual(
          alarm
            ? [
                `${JSON.stringify({ packageName: "midgard-watcher", level: "error", ...hold })}\n`,
              ]
            : [],
        );
        expect(test.recordProofStep).toHaveBeenLastCalledWith(
          expect.objectContaining({ status: "reconciling" }),
        );
        expect(test.setAlert).not.toHaveBeenCalled();
        expect(await test.fixture.records()).toEqual(before);
        test.runOrResume.mockResolvedValueOnce({
          kind: "pending",
          reason: "signed transaction awaiting inclusion",
        });
        await test.execution.execute(test.input);
      } finally {
        stderr.mockRestore();
      }
      expect(test.execution.readiness()).toEqual([]);
    },
  );

  it("holds nothing for a pending run whose named state is malformed", async () => {
    const test = await setupFaultProofExecution();
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      reason: "transaction for walk is pending",
      namedState: { name: "", alarm: true },
    });
    await test.execution.execute(test.input);
    expect(test.execution.readiness()).toEqual([]);
  });
});
