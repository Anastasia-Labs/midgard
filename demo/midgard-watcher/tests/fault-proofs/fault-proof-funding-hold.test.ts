/**
 * An objective paused because the prover wallet cannot fund its next
 * transaction is named on readiness (`prover_funding_unavailable`) until a
 * later run meets something else, as one does after a top-up.
 */
import { WORKFLOW_FUNDING_SHORTFALL } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WatcherProverFundingOrphanedReservationError } from "../../src/funding/prover-funding-recovery.orphaned-reservation.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import {
  cleanupFundingRecoveryFixtures,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

const HOLD = "prover_funding_unavailable";
const SHORTFALL = `${WORKFLOW_FUNDING_SHORTFALL}: the prover wallet cannot fund init:00#0 from its plain-ADA coins (a coin holding a token is never spent): Your wallet does not have enough funds`;
const RESERVATION =
  "prover funding is temporarily unavailable for a fresh transaction";
const OTHER = "transaction for walk is pending";

describe("a fault-proof objective paused on prover funding", () => {
  it("is named on readiness until a later run meets something else", async () => {
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
      expect(await pend(RESERVATION)).toEqual([
        { reason: HOLD, detail: `${objective}: ${RESERVATION}` },
      ]);
    } finally {
      stderr.mockRestore();
    }
    // A pause a top-up resumes is no alarm on its own.
    expect(logged).toEqual([]);
    expect(await pend(OTHER)).toEqual([]);
    expect(test.setAlert).not.toHaveBeenCalled();
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

  it("is named on readiness while the wallet cannot fund a new reservation", async () => {
    const test = await setupFaultProofExecution(true);
    const run = {
      ...test.input,
      job: { ...test.input.job, mode: "run" },
      admission: { mode: "run", funding: "create_or_resume" },
    } as const;
    test.getUtxos.mockResolvedValueOnce([]);
    expect(await test.execution.execute(run)).toMatchObject({
      kind: "pending",
    });
    expect(test.execution.readiness()).toEqual([
      {
        reason: HOLD,
        detail: expect.stringContaining(
          `doubleSpend/${test.input.job.headerHash}: `,
        ),
      },
    ]);
    // A top-up: fresh plain-ada coins the existing reservation does not hold.
    test.getUtxos.mockResolvedValue(
      [0, 1, 2, 3].map((outputIndex) => ({
        txHash: "aa".repeat(32),
        outputIndex,
        address: walletAddress,
        assets: { lovelace: 2_000_000_000n },
      })),
    );
    test.runOrResume.mockResolvedValueOnce({ kind: "pending", reason: OTHER });
    await test.execution.execute(run);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.execution.readiness()).toEqual([]);
  });

  it("keeps a reconciliation wait that only mentions funding off readiness", async () => {
    const test = await setupFaultProofExecution();
    test.runOrResume.mockResolvedValueOnce({
      kind: "pending",
      reason: `signed transaction ${"ab".repeat(32)} requires canonical reconciliation: ${RESERVATION}`,
    });
    await test.execution.execute(test.input);
    expect(test.execution.readiness()).toEqual([]);
  });

  it("names a wait on signed funding no workflow drives on readiness and alerts until it ends", async () => {
    const test = await setupFaultProofExecution();
    const objective = `doubleSpend/${test.input.job.headerHash}`;
    const orphaned =
      "reservation aa has no workflow to drive its signed attempts: attempt bb reads pending";
    const alerts = (active: boolean) =>
      test.setAlert.mock.calls.filter(
        ([alert]) =>
          alert.code === "proof_submission_failure" &&
          alert.subjectDigest === test.input.job.decisionDigest &&
          alert.active === active,
      ).length;
    for (const attempt of [1, 2]) {
      test.runOrResume.mockRejectedValueOnce(
        new WatcherProverFundingOrphanedReservationError([orphaned]),
      );
      expect(await test.execution.execute(test.input)).toMatchObject({
        kind: "pending",
        resume: "await_observation",
      });
      expect(test.execution.readiness()).toEqual([
        {
          reason: "prover_funding_orphaned_reservation",
          detail: `${objective}: ${orphaned}`,
        },
      ]);
      // Raised once on entering, however often the wait is met again.
      expect([alerts(true), alerts(false), attempt]).toEqual([1, 0, attempt]);
    }
    test.runOrResume.mockResolvedValueOnce({ kind: "pending", reason: OTHER });
    await test.execution.execute(test.input);
    expect(test.execution.readiness()).toEqual([]);
    expect([alerts(true), alerts(false)]).toEqual([1, 1]);
  });
});
