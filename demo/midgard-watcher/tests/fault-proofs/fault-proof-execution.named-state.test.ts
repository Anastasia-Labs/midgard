/** A pending run in a state its family named for readiness: the watcher
 * holds the objective under that name, logs it at error level only when the
 * family marks it an alarm, and clears it once a later run reports
 * something else. A run whose script re-execution is held is retried under
 * the hold's own reason. */
import {
  VALIDATION_TRACE_REPLAY_HELD,
  ValidationTraceReplayHeldError,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

describe("supervisor execution adapter with a family's named run state", () => {
  it.each([
    {
      name: "validation-trace-thread-lost",
      alarm: false,
      reason:
        "validation-trace-thread-lost: thread a at b#1 ran out of the challenger's clock",
    },
    {
      name: "validation-trace-move-fee-over-cap",
      alarm: true,
      reason:
        "validation-trace-move-fee-over-cap: the timeout move's fee 9000000 passes the funding permit's per-transaction fee ceiling 8700000",
    },
    {
      name: "validation-trace-claim-build-held",
      alarm: false,
      reason:
        "validation-trace-claim-build-held: spend: Plutus evaluation stopped without a verdict",
    },
    {
      name: "validation-trace-move-unbuilt",
      alarm: true,
      reason:
        "validation-trace-move-unbuilt: the reveal move on thread a#1 of case c does not build, and must land by 1000: Transaction size exceeds the limit",
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

  it("raises every action a run skipped as unbuilt, even when the run went on to land another move under a state that is no alarm", async () => {
    const test = await setupFaultProofExecution();
    const logged: string[] = [];
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation((line) => logged.push(String(line)) > 0);
    const entry =
      "validation-trace-one-step-required: the enter_resolution move on thread a#1 of case c does not build, and must land by 1000: Transaction size exceeds the limit";
    const close =
      "validation-trace-move-unbuilt: the close_thread move on thread d#1 of case c does not build, and must land by 2000: Transaction size exceeds the limit";
    const reason = "transaction for validation-trace-reveal:b#1 is pending";
    const hold = {
      reason: "validation-trace-thread-lost",
      detail: `doubleSpend/${test.input.job.headerHash}: ${reason}; ${entry}; ${close}`,
    };
    try {
      test.runOrResume.mockResolvedValueOnce({
        kind: "pending",
        resumeOnObservation: true,
        reason,
        namedState: { name: "validation-trace-thread-lost", alarm: false },
        alarms: [
          { actionId: "validation-trace-enter-resolution:a#1", reason: entry },
          { actionId: "validation-trace-close-thread:d#1", reason: close },
        ],
      });
      expect(await test.execution.execute(test.input)).toEqual({
        kind: "pending",
        resume: "await_observation",
        reason,
      });
      expect(test.execution.readiness()).toEqual([hold]);
      expect(logged).toEqual([
        `${JSON.stringify({ packageName: "midgard-watcher", level: "error", ...hold })}\n`,
      ]);
    } finally {
      stderr.mockRestore();
    }
  });

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

describe("supervisor execution adapter with a held script re-execution", () => {
  it.each([
    ["the hold", (held: Error) => held],
    [
      "an error wrapping the hold",
      (held: Error) => new Error("workflow run failed", { cause: held }),
    ],
  ])(
    "retries the objective under the hold's reason for %s, after its retry time, until a later run reports something else",
    async (_, thrown) => {
      const test = await setupFaultProofExecution();
      const before = await test.fixture.records();
      const { headerHash } = test.input.job;
      const held = new ValidationTraceReplayHeldError(
        headerHash,
        [
          {
            headerHash,
            position: 0,
            txId: "0c".repeat(32),
            attempts: 2,
            retryAtMs: Date.now() + 60_000,
            detail: "spend: Plutus evaluation stopped without a verdict",
            stuck: false,
          },
        ],
        0,
        [],
      );
      test.runOrResume.mockRejectedValueOnce(thrown(held));
      const outcome = await test.execution.execute(test.input);
      expect(outcome).toMatchObject({
        kind: "retryable",
        resume: "backoff",
        reason: held.message,
      });
      expect(
        (outcome as { readonly retryAfterMs: number }).retryAfterMs,
      ).toBeGreaterThan(50_000);
      expect(test.execution.readiness()).toEqual([
        {
          reason: VALIDATION_TRACE_REPLAY_HELD,
          detail: `doubleSpend/${headerHash}: ${held.message}`,
        },
      ]);
      expect(test.setAlert).not.toHaveBeenCalled();
      expect(await test.fixture.records()).toEqual(before);
      test.runOrResume.mockResolvedValueOnce({
        kind: "pending",
        reason: "signed transaction awaiting inclusion",
      });
      await test.execution.execute(test.input);
      expect(test.execution.readiness()).toEqual([]);
    },
  );
});
