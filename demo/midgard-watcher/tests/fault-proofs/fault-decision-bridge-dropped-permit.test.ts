import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";

afterEach(cleanupFundingRecoveryFixtures);

describe("a fault target the bridge stops using", () => {
  it.each([
    ["an earlier fault is found", "earlier_fault"],
    ["another proof locks the earlier fault", "earlier_locked"],
  ] as const)(
    "refuses the pending retry of the old target when %s, with no quarantine or alert",
    async (_transition, change) => {
      const test = await setupFaultProofExecution();
      // What a retry meets once the bridge has dropped its decision's replay
      // context, as the drill's transitionTrace retry did.
      test.runOrResume.mockRejectedValue(
        new Error(
          "doubleSpend application requires replayContext, which the host did not supply",
        ),
      );
      const current = observation([headerFixture("01"), headerFixture("02")]);
      const [earlier, later] = current.finalizedHeaders;
      let earlierFault = false;
      const h = harness({
        current,
        categoryByHeader: {
          [earlier!.headerHash]: "doubleSpend",
          [later!.headerHash]: "doubleSpend",
        },
        pendingAvailabilityHeaders: () => new Set(),
        classifyOverride: (fresh) =>
          !earlierFault && fresh.headerHash === earlier!.headerHash
            ? { ...fresh, decision: "healthy" }
            : fresh,
        actuationControllerFor: (decision) =>
          decision.headerHash === later!.headerHash
            ? test.controller
            : undefined,
      });
      await h.bridge.reconcileAndDispatch(current);
      const [first] = h.progressRequests;
      expect(first!.fault?.decision.headerHash).toBe(later!.headerHash);
      // The old target's run returned retryable: the supervisor's retry timer
      // holds the permit the bridge handed it.
      const retryPermit = first!.fault!.actuationPermit;
      expect(retryPermit).toBe(test.controller.permit);

      earlierFault = true;
      const next =
        change === "earlier_fault"
          ? { ...current, observationDigest: "41".repeat(32) }
          : {
              ...observation([headerFixture("01"), headerFixture("02")], {
                Locked: {
                  target_header_hash: earlier!.headerHash,
                  correction_identity: {
                    FraudProof: {
                      fraud_proof_asset_name: `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend}${earlier!.headerHash}`,
                    },
                  },
                },
              }),
              observationDigest: "42".repeat(32),
            };
      h.admitted.add(next);
      const switched = await h.bridge.reconcileAndDispatch(next);
      expect(switched.target?.headerHash).toBe(earlier!.headerHash);

      // The retry fires after the switch.
      const retried = await test.execution.execute({
        ...test.input,
        actuationPermit: retryPermit,
      });
      expect(retried).toMatchObject({ kind: "authority_revoked" });
      expect(test.runOrResume).not.toHaveBeenCalled();
      expect(test.execution.readiness()).not.toContainEqual(
        expect.objectContaining({ reason: WATCHER_FAULT_PROOF_QUARANTINED }),
      );
      expect(
        test.setAlert.mock.calls.filter(
          ([alert]) =>
            (alert as { code: string }).code === "proof_submission_failure",
        ),
      ).toEqual([]);
    },
  );
});
