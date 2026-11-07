import { afterEach, describe, expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

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

  it("keeps the journal bounded while a held objective is re-checked on the same stall", async () => {
    const test = await setup();
    const journal = () =>
      test.fixture.journal.load(test.fixture.initial.workflowId);
    const before = (await journal()).length;
    vi.mocked(test.fixture.adapter.reconcile).mockRejectedValue(
      new Error("the follower answered with a foreign block"),
    );
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      for (let recheck = 0; recheck < 50; recheck += 1)
        expect(await test.execution.execute(test.input)).toMatchObject({
          kind: "retryable",
          reason: expect.stringContaining("reconciliation failed for init"),
        });
    } finally {
      stderr.mockRestore();
    }
    // One stall records the cause; re-checking it adds nothing more.
    const entries = await journal();
    expect(entries).toHaveLength(before + 1);
    expect(entries.at(-1)!.event).toMatchObject({ kind: "stalled" });
    expect(test.execution.readiness()).toEqual([
      expect.objectContaining({ reason: WATCHER_FAULT_PROOF_QUARANTINED }),
    ]);
    // A different cause is recorded once.
    vi.mocked(test.fixture.adapter.reconcile).mockRejectedValue(
      new Error("the follower answered with a stale block"),
    );
    stderr.mockImplementation(() => true);
    for (let recheck = 0; recheck < 3; recheck += 1)
      await test.execution.execute(test.input);
    stderr.mockRestore();
    expect(await journal()).toHaveLength(before + 2);
  });

  it("quarantines an authentication failure of the objective's own run and preserves the signed reservation", async () => {
    const test = await setup();
    const before = await test.fixture.records();
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
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).toHaveBeenCalledWith(
      expect.objectContaining({
        code: "proof_submission_failure",
        active: true,
      }),
    );
  });
});
