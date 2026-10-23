/** A header removed against another record while a thread of ours still
 * stands on it: the run reclaims that thread before it resolves. The
 * watcher keeps the objective while the reclaim is unfunded, unavailable,
 * refused by the funding permit or in flight, alarms on the first three by
 * name, and ends the objective, never as our conviction, only once the run
 * resolves with the reclaim done. Readiness is then restored. */
import {
  FRAUD_PROOF_THREAD_RECLAIM_REFUSED,
  FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE,
  FRAUD_PROOF_THREAD_RECLAIM_UNFUNDED,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

const THREAD = `${"d7".repeat(32)}#1`;
const RECLAIM_TX = "d8".repeat(32);

const held = (name: string, reason: string) => ({
  kind: "pending",
  resumeOnObservation: true,
  reason,
  namedState: { name, alarm: true },
});

describe("supervisor execution adapter with our thread on a header another record removed", () => {
  it.each([
    {
      name: FRAUD_PROOF_THREAD_RECLAIM_UNFUNDED,
      reason: `${FRAUD_PROOF_THREAD_RECLAIM_UNFUNDED}: the prover wallet cannot fund reclaiming our thread ${THREAD}; the header is already removed, so no deadline applies; top up the wallet's plain-ADA coins`,
    },
    {
      name: FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE,
      reason: `${FRAUD_PROOF_THREAD_RECLAIM_UNAVAILABLE}: our thread ${THREAD} still holds funds on the removed header and cannot be reclaimed: no reclaim is wired in front of this family's workflow`,
    },
    {
      name: FRAUD_PROOF_THREAD_RECLAIM_REFUSED,
      reason: `${FRAUD_PROOF_THREAD_RECLAIM_REFUSED}: the funding permit refused the cancel of our thread ${THREAD}; it is built again at the next observation: thread reclaim differs from its exact binding: pays only our wallet`,
    },
  ])(
    "holds the objective under $name, keeps it through the reclaim, and ends it only once the run resolves",
    async ({ name, reason }) => {
      const test = await setupFaultProofExecution();
      const logged: string[] = [];
      const stderr = vi
        .spyOn(process.stderr, "write")
        .mockImplementation((line) => logged.push(String(line)) > 0);
      const hold = {
        reason: name,
        detail: `doubleSpend/${test.input.job.headerHash}: ${reason}`,
      };
      const statuses = () =>
        test.recordProofStep.mock.calls.map(
          ([step]) => (step as { status: string }).status,
        );
      try {
        // The thread cannot be reclaimed yet: held and alarmed by name,
        // never ended, and retried at each observation.
        for (let pass = 0; pass < 2; pass += 1) {
          test.runOrResume.mockResolvedValueOnce(held(name, reason));
          expect(await test.execution.execute(test.input)).toEqual({
            kind: "pending",
            resume: "await_observation",
            reason,
          });
          expect(test.execution.readiness()).toEqual([hold]);
        }
        expect(logged).toContain(
          `${JSON.stringify({ packageName: "midgard-watcher", level: "error", ...hold })}\n`,
        );
        // The reclaim is submitted and settles: the objective still runs.
        for (const inFlight of [
          `reclaim ${RECLAIM_TX} of our thread ${THREAD} awaits confirmation`,
          `reclaim ${RECLAIM_TX} of our thread ${THREAD} awaits release finality`,
        ]) {
          test.runOrResume.mockResolvedValueOnce({
            kind: "pending",
            resumeOnObservation: true,
            reason: inFlight,
          });
          expect(await test.execution.execute(test.input)).toMatchObject({
            kind: "pending",
            resume: "await_observation",
          });
          expect(test.execution.readiness()).toEqual([]);
        }
        expect(statuses()).not.toContain("cancelled");
        expect(statuses()).not.toContain("completed");
      } finally {
        stderr.mockRestore();
      }
      // Resolved with the reclaim done: the objective ends, not as ours.
      test.runOrResume.mockResolvedValueOnce({
        kind: "resolved_by_another_record",
        resolution: {
          removalTxHash: "c5".repeat(32),
          referencedProofTokenOutRef: `${"c4".repeat(32)}#0`,
          confirmationDepth: 2162,
        },
        reclaimed: [
          {
            actionId: `reclaim:${THREAD}`,
            txHash: RECLAIM_TX,
            threadOutRef: THREAD,
            lovelace: "9000000",
          },
        ],
        entries: [],
      });
      expect(await test.execution.execute(test.input)).toMatchObject({
        kind: "resolved_by_another_record",
      });
      expect(statuses().at(-1)).toBe("cancelled");
      expect(statuses()).not.toContain("completed");
      expect(test.execution.readiness()).toEqual([]);
      expect(test.setAlert).not.toHaveBeenCalled();
    },
  );
});
