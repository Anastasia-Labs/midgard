import {
  FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED,
  FraudProofL1CheckpointChangedError,
  FraudProofL1RefusedError,
  FraudProofL1UnavailableError,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { assertWorkflowActuationPermitIdentity } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { WorkflowFundingUnavailableError } from "@al-ft/midgard-fault-proofs/workflow/workflow-funding-shortfall";
import { describe, expect, it, vi } from "vitest";

import {
  WATCHER_COUNTERPARTY_DEADLINE_SLACK_MS,
  WATCHER_FAULT_PROOF_CONTINUATION_STALLED,
  WATCHER_FAULT_PROOF_COUNTERPARTY_DEADLINE_PASSED,
} from "../../src/fault-proofs/fault-proof-continuation-stall.js";
import { WatcherProofDecisionMissingError } from "../../src/fault-proofs/watcher-decision-hold.js";
import { WatcherRetainedDaTransportUnavailableError } from "../../src/storage/retained-da-transport-unavailable.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";

const setup = setupFaultProofExecution;

/** A dispute's response deadline, and the reason its run awaits it under. */
const DEADLINE = 1_767_225_600_000;
const AWAITING = `validationTraceDispute awaiting counterparty until ${DEADLINE.toString()}`;

describe("supervisor execution adapter", () => {
  it("resumes the objective through its runner with the objective's own wallet", async () => {
    const test = await setup();
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "pending",
      resume: "await_observation",
    });
    expect(test.runOrResume).toHaveBeenCalledOnce();
    const [invocation] = test.runOrResume.mock.calls[0]!;
    expect(invocation).toMatchObject({
      category: "doubleSpend",
      headerHash: test.input.job.headerHash,
      decisionDigest: test.input.job.decisionDigest,
      actuationPermit: test.controller.permit,
      objectiveWallet: {
        category: "doubleSpend",
        headerHash: test.input.job.headerHash,
      },
    });
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("returns revoked authority before runner work", async () => {
    const test = await setup();
    test.controller.revoke("new rollback generation");
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "authority_revoked",
    });
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("returns revoked authority when the decision is revoked while the run reads the chain", async () => {
    const test = await setup();
    test.runOrResume.mockImplementationOnce(async (invocation) => {
      test.controller.revoke("new rollback generation");
      // The run's next permit check, as at its next chain read.
      assertWorkflowActuationPermitIdentity({
        permit: invocation.actuationPermit,
        category: invocation.category,
        rollbackGeneration: "2",
      });
      throw new Error("a revoked run must not continue");
    });
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "authority_revoked",
    });
    expect(test.runOrResume).toHaveBeenCalledOnce();
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
  });

  it.each([
    {
      name: "before its response deadline, healthy",
      nowMs: DEADLINE - 1,
      responseDeadline: DEADLINE,
      holds: [],
    },
    {
      name: "within the slack after its response deadline, healthy",
      nowMs: DEADLINE + WATCHER_COUNTERPARTY_DEADLINE_SLACK_MS,
      responseDeadline: DEADLINE,
      holds: [],
    },
    {
      name: "past its response deadline and the slack, alarmed",
      nowMs: DEADLINE + WATCHER_COUNTERPARTY_DEADLINE_SLACK_MS + 1,
      responseDeadline: DEADLINE,
      holds: [
        `awaiting its counterparty 1 ms past the response deadline ${DEADLINE.toString()} and its slack: ${AWAITING}`,
      ],
    },
    {
      name: "with no response deadline, alarmed",
      nowMs: DEADLINE - 1,
      responseDeadline: undefined,
      holds: [
        `awaiting its counterparty with no response deadline: ${AWAITING}`,
      ],
    },
  ])(
    "waits on a fresh observation while a dispute awaits its counterparty $name",
    async ({ nowMs, responseDeadline, holds }) => {
      const test = await setup();
      vi.useFakeTimers({ toFake: ["Date"], now: nowMs });
      const logged: string[] = [];
      const stderr = vi
        .spyOn(process.stderr, "write")
        .mockImplementation((line) => logged.push(String(line)) > 0);
      try {
        test.runOrResume.mockResolvedValueOnce({
          kind: "awaiting_counterparty",
          reason: AWAITING,
          ...(responseDeadline === undefined ? {} : { responseDeadline }),
        });
        expect(await test.execution.execute(test.input)).toEqual({
          kind: "pending",
          resume: "await_observation",
          reason: AWAITING,
        });
      } finally {
        stderr.mockRestore();
        vi.useRealTimers();
      }
      const alarms = holds.map((detail) => ({
        reason: WATCHER_FAULT_PROOF_COUNTERPARTY_DEADLINE_PASSED,
        detail: `doubleSpend/${test.input.job.headerHash}: ${detail}`,
      }));
      expect(test.execution.readiness()).toEqual(alarms);
      expect(logged).toEqual(
        alarms.map(
          (alarm) =>
            `${JSON.stringify({ packageName: "midgard-watcher", level: "error", ...alarm })}\n`,
        ),
      );
      expect(test.recordProofStep).toHaveBeenLastCalledWith(
        expect.objectContaining({ status: "reconciling" }),
      );
      expect(test.setAlert).not.toHaveBeenCalled();
    },
  );

  it("clears the counterparty alarm once a later run reports something else", async () => {
    const test = await setup();
    vi.useFakeTimers({ toFake: ["Date"], now: DEADLINE + 3_600_000 });
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      test.runOrResume.mockResolvedValueOnce({
        kind: "awaiting_counterparty",
        reason: AWAITING,
        responseDeadline: DEADLINE,
      });
      await test.execution.execute(test.input);
      expect(test.execution.readiness()).toHaveLength(1);
      test.runOrResume.mockResolvedValueOnce({
        kind: "pending",
        reason: "signed transaction awaiting inclusion",
      });
      await test.execution.execute(test.input);
    } finally {
      stderr.mockRestore();
      vi.useRealTimers();
    }
    expect(test.execution.readiness()).toEqual([]);
  });

  it("names a stalled session on readiness and logs it at error level until a later run makes progress", async () => {
    const test = await setup();
    const reason = `${FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED}: no progress for 1800000 ms over 1800 polls; last pending: transaction for walk is pending`;
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
    // The next run waits on L1 at first, then its first progress clears the
    // alarm before the run ends.
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
      reason: "script re-execution is held",
      namedState: { name: "validation_trace_replay_held", alarm: false },
    });
    await test.execution.execute(test.input);
    test.runOrResume.mockImplementationOnce(progressThenPend);
    await test.execution.execute(test.input);
    const named = expect.objectContaining({
      reason: "validation_trace_replay_held",
    });
    expect(during).toEqual([[stalled], [], [named], [named]]);
    expect(test.execution.readiness()).toEqual([]);
  });

  it("returns structured read-only provider transport failures for bounded backoff", async () => {
    const test = await setup();
    const cause = Object.assign(new Error("socket reset"), {
      code: "ECONNRESET",
    });
    // The run's wallet read met the transport failure.
    test.runOrResume.mockRejectedValueOnce(
      new TypeError("fetch failed", { cause }),
    );
    const outcome = await test.execution.execute({
      ...test.input,
      job: { ...test.input.job, mode: "run" },
      admission: test.runAdmission,
    });
    expect(outcome).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("keeps initial funding scarcity pending without a failed-submission alert", async () => {
    const test = await setup();
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      resumeOnObservation: true,
      reason: new WorkflowFundingUnavailableError().message,
    });
    const outcome = await test.execution.execute({
      ...test.input,
      job: { ...test.input.job, mode: "run" },
      admission: test.runAdmission,
    });
    expect(outcome).toMatchObject({
      kind: "pending",
      resume: "await_observation",
    });
    // A funding pause, never a failed submission.
    expect(
      test.setAlert.mock.calls.map(([{ code, active }]) => ({ code, active })),
    ).toEqual([{ code: "prover_funding_unavailable", active: true }]);
  });

  it.each([
    [
      "an unavailable L1 read",
      () => new FraudProofL1UnavailableError("the follower is catching up"),
      {
        kind: "retryable",
        resume: "backoff",
        retryAfterMs: 1000,
        reason: "the follower is catching up",
      },
    ],
    [
      "a refused L1 read",
      () => new FraudProofL1RefusedError("beyond_retention", "pruned"),
      {
        kind: "pending",
        resume: "await_observation",
        reason: "fault_proof_l1_refused:beyond_retention: pruned",
      },
    ],
  ] as const)(
    "waits on %s met while building the next move, run after run",
    async (_label, failure, waiting) => {
      const test = await setup();
      // A run admitted to build a fresh move.
      const input = {
        ...test.input,
        job: { ...test.input.job, mode: "run" },
        admission: test.runAdmission,
      } as const;
      test.runOrResume.mockImplementation(async () => {
        throw failure();
      });
      for (let run = 1; run <= 3; run += 1) {
        expect(await test.execution.execute(input)).toMatchObject(waiting);
        expect(test.runOrResume).toHaveBeenCalledTimes(run);
      }
      expect(test.setAlert).not.toHaveBeenCalled();
    },
  );

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
      // The error escapes the workflow as the provider raised it.
      test.runOrResume.mockRejectedValueOnce(failure());
      expect(await test.execution.execute(test.input)).toMatchObject({
        kind: "retryable",
        resume: "backoff",
        retryAfterMs: 1000,
      });
      expect(test.setAlert).not.toHaveBeenCalled();
    },
  );

  it("passes a held objective to the supervisor's hold without a failure alert", async () => {
    const test = await setup();
    const missing = new WatcherProofDecisionMissingError({
      kind: "objective",
      category: "doubleSpend",
      headerHash: test.fresh.headerHash,
      decisionDigest: test.fresh.decisionDigest,
      detail: `doubleSpend/${test.fresh.headerHash}`,
      readiness: "fault_proof_start_deadline_passed",
    });
    test.runOrResume.mockRejectedValueOnce(missing);
    await expect(test.execution.execute(test.input)).rejects.toBe(missing);
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("backs off completed verification transport outages", async () => {
    const test = await setup();
    test.verifyCompleted.mockRejectedValueOnce(
      new FraudProofL1UnavailableError("the follower has no cursor yet"),
    );
    const request = {
      job: test.input.job,
      actuationPermit: test.controller.permit,
      terminal: fundingTerminal(
        test.fresh.headerHash,
        "aa".repeat(32),
        "bb".repeat(32),
      ),
    };
    expect(await test.execution.verifyCompleted(request)).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.runOrResume).not.toHaveBeenCalled();
    test.verifyCompleted.mockRejectedValueOnce(
      new Error("authenticated terminal mismatch"),
    );
    await expect(test.execution.verifyCompleted(request)).rejects.toThrow(
      "authenticated terminal mismatch",
    );
  });
});
