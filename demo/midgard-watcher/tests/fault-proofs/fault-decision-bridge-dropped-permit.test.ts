import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it } from "vitest";

import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import { WatcherRetainedHeaderAttestationPendingError } from "../../src/indexers/authenticated-state-queue-observation.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  type decision,
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

  it("refuses the pending retry of a target whose header left the queue, revoked before its replay context goes", async () => {
    const test = await setupFaultProofExecution();
    // What a retry meets once the bridge has dropped its replay context.
    test.runOrResume.mockRejectedValue(
      new Error(
        "doubleSpend application requires replayContext, which the host did not supply",
      ),
    );
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [earlier, targeted] = current.finalizedHeaders;
    // The context the bridge still retained when it revoked the permit.
    let retainedAtRevoke: string | null | undefined;
    const h = harness({
      current,
      categoryByHeader: {
        [earlier!.headerHash]: "doubleSpend",
        [targeted!.headerHash]: "doubleSpend",
      },
      pendingAvailabilityHeaders: () => new Set(),
      classifyOverride: (fresh) =>
        fresh.headerHash === earlier!.headerHash
          ? { ...fresh, decision: "healthy" }
          : fresh,
      actuationControllerFor: (decision) =>
        decision.headerHash === targeted!.headerHash
          ? {
              ...test.controller,
              revoke: (reason: string) => {
                retainedAtRevoke = h.retainedDecisionAuthorities.at(-1);
                test.controller.revoke(reason);
              },
            }
          : undefined,
    });
    const target = (await h.bridge.reconcileAndDispatch(current)).target;
    expect(target?.headerHash).toBe(targeted!.headerHash);
    // Its run returned retryable: the retry holds the permit it was handed.
    const retryPermit = h.progressRequests[0]!.fault!.actuationPermit;
    const journaled = () =>
      test.fixture.workflowJournal
        .readExecution(test.fixture.objective)
        ?.entries.map(({ event }) => event.kind);
    const before = journaled();

    // Our removal landed: the header left the queue.
    const departed = {
      ...observation([headerFixture("01")]),
      observationDigest: "45".repeat(32),
    };
    h.admitted.add(departed);
    expect((await h.bridge.reconcileAndDispatch(departed)).target).toBeNull();
    expect(retainedAtRevoke).toBe(target?.decisionDigest);
    expect(h.retainedDecisionAuthorities.at(-1)).toBeNull();

    // The retry fires after the departure.
    const retried = await test.execution.execute({
      ...test.input,
      actuationPermit: retryPermit,
    });
    expect(retried).toMatchObject({ kind: "authority_revoked" });
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(journaled()).toEqual(before);
    expect(test.execution.readiness()).not.toContainEqual(
      expect.objectContaining({ reason: WATCHER_FAULT_PROOF_QUARANTINED }),
    );
    expect(
      test.setAlert.mock.calls.filter(
        ([alert]) =>
          (alert as { code: string }).code === "proof_submission_failure",
      ),
    ).toEqual([]);
  });
});

describe("a fault target a pass cannot classify", () => {
  it.each([
    ["the provider resets while the header before it replays", "earlier"],
    ["its own predecessor's DA attachment is still pending", "target"],
  ] as const)(
    "keeps the target's permit, generation and replay context when %s",
    async (_deferral, where) => {
      const current = observation([headerFixture("01"), headerFixture("02")]);
      const [earlier, targeted] = current.finalizedHeaders as [
        (typeof current.finalizedHeaders)[number],
        (typeof current.finalizedHeaders)[number],
      ];
      let deferring = false;
      const h = harness({
        current,
        categoryByHeader: {
          [earlier.headerHash]: "invalidRange",
          [targeted.headerHash]: "doubleSpend",
        },
        pendingAvailabilityHeaders: () => new Set(),
        classifyOverride: (fresh: ReturnType<typeof decision>) => {
          if (fresh.headerHash !== earlier.headerHash) return fresh;
          if (deferring && where === "earlier")
            throw Object.assign(new Error("socket reset"), {
              code: "ECONNRESET",
            });
          return { ...fresh, decision: "healthy" as const };
        },
        resolvePredecessorOverride: async (header) => {
          if (
            deferring &&
            where === "target" &&
            header.headerHash === targeted.headerHash
          )
            throw new WatcherRetainedHeaderAttestationPendingError(
              earlier.headerHash,
            );
          return undefined;
        },
      });
      const target = (await h.bridge.reconcileAndDispatch(current)).target;
      expect(target?.headerHash).toBe(targeted.headerHash);
      const permit = h.progressRequests[0]!.fault!.actuationPermit;
      const generation = h.progressRequests[0]!.rollbackGeneration;
      // Every controller and every dispatch carries the target's original
      // rollback generation, however many dispatches there were.
      const keepsGeneration = () => {
        expect(new Set(h.controllerGenerations)).toEqual(new Set([generation]));
        expect(new Set(h.enqueuedGenerations)).toEqual(new Set([generation]));
      };

      // A later block: the target is classified again, and cannot be.
      deferring = true;
      const later = { ...current, observationDigest: "43".repeat(32) };
      h.admitted.add(later);
      expect((await h.bridge.reconcileAndDispatch(later)).target).toEqual(
        target,
      );
      expect(h.revocations).toEqual([]);
      keepsGeneration();
      expect(h.retainedDecisionAuthorities.at(-1)).toBe(target?.decisionDigest);
      expect(h.progressRequests.at(-1)?.fault?.actuationPermit).toBe(permit);

      // Once the deferral clears, the same proof carries on unreissued.
      deferring = false;
      const cleared = { ...current, observationDigest: "44".repeat(32) };
      h.admitted.add(cleared);
      const dispatchedBefore = h.progressRequests.length;
      expect((await h.bridge.reconcileAndDispatch(cleared)).target).toEqual(
        target,
      );
      expect(h.revocations).toEqual([]);
      keepsGeneration();
      // The proof is dispatched again, with the permit it started with.
      const continued = h.progressRequests.slice(dispatchedBefore).at(-1);
      expect(continued?.fault?.actuationPermit).toBe(permit);
      expect(continued?.rollbackGeneration).toBe(generation);
    },
  );
});
