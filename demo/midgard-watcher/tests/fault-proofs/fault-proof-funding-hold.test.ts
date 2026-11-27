/**
 * An objective paused because the prover wallet cannot fund its next
 * transaction is named on readiness (`prover_funding_unavailable`), and its
 * `prover_funding_unavailable` alert is active, until a later run meets
 * something else, as one does after a top-up.
 */
import { WORKFLOW_FUNDING_SHORTFALL } from "@al-ft/midgard-fault-proofs/workflow/workflow-funding-shortfall";
import { describe, expect, it, vi } from "vitest";

import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";

const HOLD = "prover_funding_unavailable";
const SHORTFALL = `${WORKFLOW_FUNDING_SHORTFALL}: the prover wallet cannot fund init:00#0 from its plain-ADA coins (a coin holding a token is never spent): Your wallet does not have enough funds`;
const UNAVAILABLE =
  "prover funding is temporarily unavailable for a fresh transaction";
const OTHER = "transaction for walk is pending";

describe("a fault-proof objective paused on prover funding", () => {
  it("is named on readiness, with an active alert, until a later run meets something else", async () => {
    const test = await setupFaultProofExecution();
    const objective = `doubleSpend/${test.input.job.headerHash}`;
    const pend = async (reason: string) => {
      test.runOrResume.mockResolvedValueOnce({
        kind: "pending",
        resumeOnObservation: true,
        reason,
      });
      expect(await test.execution.execute(test.input)).toEqual({
        kind: "pending",
        resume: "await_observation",
        reason,
      });
      return test.execution.readiness();
    };
    const logged: string[] = [];
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation((line) => logged.push(String(line)) > 0);
    try {
      expect(await pend(SHORTFALL)).toEqual([
        { reason: HOLD, detail: `${objective}: ${SHORTFALL}` },
      ]);
      expect(await pend(UNAVAILABLE)).toEqual([
        { reason: HOLD, detail: `${objective}: ${UNAVAILABLE}` },
      ]);
    } finally {
      stderr.mockRestore();
    }
    // One active alert while it stays paused; no error line, since a top-up
    // resumes it.
    expect(logged).toEqual([]);
    const alert = (active: boolean) => ({
      code: HOLD,
      subjectDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
      active,
      observedAtMs: expect.any(String),
    });
    expect(test.setAlert.mock.calls).toEqual([[alert(true)]]);
    expect(await pend(OTHER)).toEqual([]);
    expect(test.setAlert.mock.calls).toEqual([[alert(true)], [alert(false)]]);
    // The same subject clears it.
    const [[raised], [cleared]] = test.setAlert.mock.calls;
    expect(cleared.subjectDigest).toBe(raised.subjectDigest);
  });

  it("is named on readiness while a script-decode record transaction is unfunded, until a top-up", async () => {
    const test = await setupFaultProofExecution();
    const objective = `doubleSpend/${test.input.job.headerHash}`;
    const underfunded =
      "script-decode-record-underfunded: the prover wallet cannot fund bind from its plain-ADA coins (a coin holding a token is never spent): Your wallet does not have enough funds";
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      resumeOnObservation: true,
      reason: underfunded,
    });
    await test.execution.execute(test.input);
    expect(test.execution.readiness()).toEqual([
      { reason: HOLD, detail: `${objective}: ${underfunded}` },
    ]);
    // After a top-up the record transaction lands and the run moves on.
    test.runOrResume.mockResolvedValueOnce({ kind: "pending", reason: OTHER });
    await test.execution.execute(test.input);
    expect(test.execution.readiness()).toEqual([]);
  });

  it("keeps a wait that only mentions funding off readiness", async () => {
    const test = await setupFaultProofExecution();
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      reason: `signed transaction ${"ab".repeat(32)} awaiting inclusion: ${UNAVAILABLE}`,
    });
    await test.execution.execute(test.input);
    expect(test.execution.readiness()).toEqual([]);
  });
});
