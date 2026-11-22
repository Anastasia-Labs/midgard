import { describe, expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";

const setup = setupFaultProofExecution;

describe("supervisor execution adapter quarantining one objective", () => {
  it.each([
    [{ kind: "awaiting_counterparty" }, "invalid execution outcome"],
    [
      { kind: "no_fault_detected", classification: {} },
      "the family's own classification returned no_fault_detected for an admitted fault",
    ],
    [
      { kind: "unprovable_gap", classification: {} },
      "the family's own classification returned unprovable_gap for an admitted fault",
    ],
    [{ kind: "unknown_kind", reason: "x" }, "x"],
  ])(
    "quarantines %j to its own objective under a named cause",
    async (result, cause) => {
      const test = await setup();
      test.runOrResume.mockResolvedValue(result);
      const stderr = vi
        .spyOn(process.stderr, "write")
        .mockImplementation(() => true);
      try {
        for (let run = 0; run < 2; run += 1)
          expect(await test.execution.execute(test.input)).toEqual({
            kind: "retryable",
            resume: "backoff",
            reason: `${WATCHER_FAULT_PROOF_QUARANTINED}: workflow did not progress: ${cause}`,
            retryAfterMs: 1000,
          });
      } finally {
        stderr.mockRestore();
      }
      expect(test.execution.readiness()).toEqual([
        {
          reason: WATCHER_FAULT_PROOF_QUARANTINED,
          detail: expect.stringContaining(
            `: workflow did not progress: ${cause}`,
          ),
        },
      ]);
      expect(
        test.setAlert.mock.calls.filter(
          ([alert]) =>
            (alert as { code: string }).code === "proof_submission_failure",
        ),
      ).toEqual([[expect.objectContaining({ active: true })]]);
    },
  );

  it("quarantines a workflow stall outside preflight to its own objective and clears it on the next run that moves on", async () => {
    const test = await setup();
    test.runOrResume.mockResolvedValueOnce({
      kind: "stalled",
      reason: "confirmed action still required",
    });
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      expect(await test.execution.execute(test.input)).toMatchObject({
        kind: "retryable",
        resume: "backoff",
        retryAfterMs: 1000,
      });
    } finally {
      stderr.mockRestore();
    }
    expect(test.execution.readiness()).toEqual([
      {
        reason: WATCHER_FAULT_PROOF_QUARANTINED,
        detail: expect.stringContaining(
          ": workflow stalled: confirmed action still required",
        ),
      },
    ]);
    const alarms = () =>
      test.setAlert.mock.calls
        .map(([alert]) => alert as { code: string; active: boolean })
        .filter(({ code }) => code === "proof_submission_failure")
        .map(({ active }) => active);
    expect(alarms()).toEqual([true]);
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      reason: "transaction for walk is pending",
    });
    await test.execution.execute(test.input);
    expect(test.execution.readiness()).toEqual([]);
    expect(alarms()).toEqual([true, false]);
  });

  it("quarantines an authentication failure of the objective's own run", async () => {
    const test = await setup();
    test.runOrResume.mockRejectedValueOnce(
      new Error("authenticated proof datum changed"),
    );
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      expect(await test.execution.execute(test.input)).toMatchObject({
        kind: "retryable",
        resume: "backoff",
      });
    } finally {
      stderr.mockRestore();
    }
    expect(test.execution.readiness()).toEqual([
      {
        reason: WATCHER_FAULT_PROOF_QUARANTINED,
        detail: expect.stringContaining(": authenticated proof datum changed"),
      },
    ]);
    expect(test.setAlert).toHaveBeenCalledWith(
      expect.objectContaining({
        code: "proof_submission_failure",
        active: true,
      }),
    );
  });

  it("does not classify provider message strings as transport authority", async () => {
    const test = await setup();
    test.runOrResume.mockRejectedValueOnce(
      new Error("provider says ECONNRESET"),
    );
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      // Not a transport backoff: the objective is quarantined under it.
      expect(await test.execution.execute(test.input)).toMatchObject({
        reason: `${WATCHER_FAULT_PROOF_QUARANTINED}: provider says ECONNRESET`,
      });
    } finally {
      stderr.mockRestore();
    }
    expect(test.execution.readiness()).toEqual([
      expect.objectContaining({ reason: WATCHER_FAULT_PROOF_QUARANTINED }),
    ]);
    expect(test.setAlert).toHaveBeenCalledWith(
      expect.objectContaining({ active: true }),
    );
  });
});
