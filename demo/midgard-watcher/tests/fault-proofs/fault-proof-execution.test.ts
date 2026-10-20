import {
  FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED,
  FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_CONTINUATION_STALLED } from "../../src/fault-proofs/fault-proof-continuation-stall.js";
import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import { WatcherProofDecisionMissingError } from "../../src/fault-proofs/watcher-decision-hold.js";
import { WatcherRetainedDaTransportUnavailableError } from "../../src/storage/retained-da-transport-unavailable.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

const setup = setupFaultProofExecution;

describe("supervisor execution adapter with durable funding", () => {
  it("restores the signed objective without querying fresh wallet inputs", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "pending",
      resume: "await_observation",
    });
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.getUtxosByOutRef).not.toHaveBeenCalled();
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect((await test.fixture.records())[0]!.reservationId).toBe(
      before[0]!.reservationId,
    );
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("returns revoked authority before funding or runner work", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    test.controller.revoke("new rollback generation");
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "authority_revoked",
    });
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("waits for a fresh observation when a typed canonical capture changes", async () => {
    const test = await setup();
    test.runOrResume.mockRejectedValueOnce(
      new FraudProofL1CheckpointChangedError("checkpoint changed"),
    );
    expect(await test.execution.execute(test.input)).toEqual({
      kind: "pending",
      resume: "await_observation",
      reason: "checkpoint changed",
    });
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it.each([
    { kind: "completed" },
    { kind: "terminal_included" },
    { kind: "pending", reason: "signed transaction awaiting inclusion" },
    { kind: "stalled", phase: "preflight", reason: "observed action changed" },
  ])("returns $kind as an explicit supervisor outcome", async (result) => {
    const test = await setup();
    test.runOrResume.mockResolvedValueOnce(result);
    const outcome = await test.execution.execute(test.input);
    expect(outcome.kind).toBe(
      result.kind === "stalled" ? "pending" : result.kind,
    );
    expect(test.setAlert).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
  });

  it("waits on a fresh observation while a dispute awaits its counterparty", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    const reason =
      "validationTraceDispute awaiting counterparty until 1767225600000";
    test.runOrResume.mockResolvedValueOnce({
      kind: "awaiting_counterparty",
      reason,
      responseDeadline: 1_767_225_600_000,
    });
    expect(await test.execution.execute(test.input)).toEqual({
      kind: "pending",
      resume: "await_observation",
      reason,
    });
    expect(test.recordProofStep).toHaveBeenLastCalledWith(
      expect.objectContaining({ status: "reconciling" }),
    );
    expect(test.setAlert).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
  });

  it("names a stalled session on readiness and logs it at error level until a later run journals progress", async () => {
    const test = await setup();
    const reason = `${FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED}: no journaled progress for 1800000 ms over 1800 polls; last pending: transaction for walk is pending`;
    const detail = `doubleSpend/${test.input.job.headerHash}: ${reason}`;
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      resumeOnObservation: true,
      reason,
    });
    const logged: string[] = [];
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation((line) => logged.push(String(line)) > 0);
    try {
      expect(await test.execution.execute(test.input)).toEqual({
        kind: "pending",
        resume: "await_observation",
        reason,
      });
    } finally {
      stderr.mockRestore();
    }
    const stalled = {
      reason: WATCHER_FAULT_PROOF_CONTINUATION_STALLED,
      detail,
    };
    expect(test.execution.readiness()).toEqual([stalled]);
    expect(logged).toContain(
      `${JSON.stringify({ packageName: "midgard-watcher", level: "error", ...stalled })}\n`,
    );
    // The next run waits on L1 at first, then its first journaled progress
    // clears the alarm before the run ends.
    const during: unknown[] = [];
    const progressThenPend = async (invocation: WorkflowAdapterRunnerInput) => {
      during.push(test.execution.readiness());
      invocation.onProgress?.();
      during.push(test.execution.readiness());
      return { kind: "pending", reason: "transaction for walk is pending" };
    };
    test.runOrResume.mockImplementationOnce(progressThenPend);
    await test.execution.execute(test.input);
    // A hold under another name is left to the run's outcome.
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      reason: FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
    });
    await test.execution.execute(test.input);
    test.runOrResume.mockImplementationOnce(progressThenPend);
    await test.execution.execute(test.input);
    const resubmission = expect.objectContaining({
      reason: "fault_proof_resubmission_required",
    });
    expect(during).toEqual([[stalled], [], [resubmission], [resubmission]]);
    expect(test.execution.readiness()).toEqual([]);
  });

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

  it("returns structured read-only provider transport failures for bounded backoff", async () => {
    const test = await setup(true);
    const before = await test.fixture.records();
    const cause = Object.assign(new Error("socket reset"), {
      code: "ECONNRESET",
    });
    test.getUtxos.mockRejectedValueOnce(
      new TypeError("fetch failed", { cause }),
    );
    const outcome = await test.execution.execute({
      ...test.input,
      job: { ...test.input.job, mode: "run" },
      admission: { mode: "run", funding: "create_or_resume" },
    });
    expect(outcome).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.getUtxos).toHaveBeenCalledOnce();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("keeps initial funding scarcity pending without a failed-submission alert", async () => {
    const test = await setup(true);
    const before = await test.fixture.records();
    test.getUtxos.mockResolvedValueOnce([]);
    const outcome = await test.execution.execute({
      ...test.input,
      job: { ...test.input.job, mode: "run" },
      admission: { mode: "run", funding: "create_or_resume" },
    });
    expect(outcome).toMatchObject({
      kind: "pending",
      resume: "await_observation",
    });
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).not.toHaveBeenCalled();
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

  it("backs off raw-source reconciliation outages without changing signed attempts or submitting", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    const entries = await test.fixture.journal.load(
      test.fixture.initial.workflowId,
    );
    vi.mocked(test.fixture.adapter.reconcile).mockRejectedValueOnce(
      new FraudProofL1UnavailableError("the follower has no cursor yet"),
    );
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.setAlert).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(
      await test.fixture.journal.load(test.fixture.initial.workflowId),
    ).toEqual(entries);
  });

  it.each([
    [
      "a refused provider connection",
      () =>
        Object.assign(new TypeError("fetch failed"), {
          cause: Object.assign(new Error("connect ECONNREFUSED"), {
            code: "ECONNREFUSED",
          }),
        }),
    ],
    [
      "a public-DA transport that did not start",
      () =>
        new Error("workflow runtime loader failed", {
          cause: new WatcherRetainedDaTransportUnavailableError(
            new Error("listen EADDRINUSE: address already in use"),
            1_000,
          ),
        }),
    ],
  ])(
    "backs off %s from the provider layer without submitting",
    async (_label, failure) => {
      const test = await setup();
      const before = await test.fixture.records();
      // The error escapes the workflow as the provider raised it.
      test.runOrResume.mockRejectedValueOnce(failure());
      expect(await test.execution.execute(test.input)).toMatchObject({
        kind: "retryable",
        resume: "backoff",
        retryAfterMs: 1000,
      });
      expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
      expect(test.setAlert).not.toHaveBeenCalled();
      expect(await test.fixture.records()).toEqual(before);
    },
  );

  it("passes a missing recorded decision to the supervisor's hold without a failure alert", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    const missing = new WatcherProofDecisionMissingError({
      kind: "objective",
      category: "doubleSpend",
      headerHash: test.fixture.old.headerHash,
      decisionDigest: test.fixture.old.decisionDigest,
      detail: "workflow recovery has no unique original fault decision",
    });
    test.runOrResume.mockRejectedValueOnce(missing);
    await expect(test.execution.execute(test.input)).rejects.toBe(missing);
    expect(test.setAlert).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
  });

  it("backs off completed verification transport outages without funding or journal changes", async () => {
    const test = await setup();
    const entries = await test.fixture.journal.load(
      test.fixture.initial.workflowId,
    );
    const before = await test.fixture.records();
    test.verifyCompleted.mockRejectedValueOnce(
      new FraudProofL1UnavailableError("the follower has no cursor yet"),
    );
    const request = {
      job: test.input.job,
      actuationPermit: test.controller.permit,
      entries,
      terminal: fundingTerminal(
        test.fixture.old.headerHash,
        test.fixture.transactionHash,
        "bb".repeat(32),
      ),
    };
    expect(await test.execution.verifyCompleted(request)).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(
      await test.fixture.journal.load(test.fixture.initial.workflowId),
    ).toEqual(entries);
    test.verifyCompleted.mockRejectedValueOnce(
      new Error("authenticated terminal mismatch"),
    );
    await expect(test.execution.verifyCompleted(request)).rejects.toThrow(
      "authenticated terminal mismatch",
    );
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
