import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import {
  journeyTimingForCategory,
  stackJourneyTimeoutMs,
} from "./journey-timing.js";

/** Preprod's cadence: 1 s slots, 5% active, 30-block finality. */
const preprod = {
  slotLengthSeconds: 1,
  activeSlotsCoeff: 0.05,
  confirmationDepth: 30,
};

describe("an attached stack's per-family time limit", () => {
  const profile = DEPLOYMENT_PROFILES["preprod-rehearsal"].timing;
  // The refusal-only families end at the committee's refusal, never at a
  // proof; transitionTrace has its own audited plan.
  it.each([
    "daHashPreimage",
    "canonicalDecodability",
    "l2TxMistag",
    "transitionTrace",
  ])(
    "gives %s its computed budget and two of operator-2's shifts at rehearsal cadence",
    (category) => {
      const timing = journeyTimingForCategory(category, {
        ...preprod,
        actionDepth: 1,
      });
      const limitMs = stackJourneyTimeoutMs(timing, profile);
      // Up to one shift waiting for operator-2's turn and one holding its
      // staging, on top of everything the family's plan waits for.
      expect(limitMs).toBe(
        timing.journeyTimeoutMs + 2 * profile.operator_shift_ms,
      );
      // A refused fault ages out one attestation timeout after its block,
      // and its removal lands within one more
      // (stack-da-publication.ts, refusal).
      expect(
        2 * profile.operator_shift_ms + 2 * profile.da_attestation_timeout_ms,
      ).toBeLessThanOrEqual(limitMs);
    },
  );

  // Operator-2's node is stopped through the fault phase, so nobody defends
  // the case the watcher opens; the case validator lets it close Convicted
  // only past its defense deadline, the side clock C plus the longest move
  // validity range L_d after the case starts (validation-case-v1), and the
  // watcher journals nothing while it waits.
  it("gives validationTraceDispute its computed budget plus the undefended case's defense window at rehearsal cadence", () => {
    const cadence = { ...preprod, actionDepth: 1 };
    const generic = journeyTimingForCategory("zeroInput", cadence);
    const timing = journeyTimingForCategory("validationTraceDispute", {
      ...cadence,
      disputeTiming: profile,
    });
    const defenseWindowMs =
      profile.dispute_side_clock_ms + profile.dispute_max_validity_range_ms;
    // One day and two minutes on the rehearsal profile.
    expect(defenseWindowMs).toBe(86_520_000);
    // The window the progress gate holds the journal's silence open for,
    // from the case's start; the Close then gets one transaction allowance.
    expect(timing.allowances.undefendedCaseMs).toBe(defenseWindowMs);
    expect(timing.transactionAllowanceMs).toBe(generic.transactionAllowanceMs);
    expect(timing.correctionTimeoutMs).toBe(
      generic.correctionTimeoutMs + defenseWindowMs,
    );
    expect(stackJourneyTimeoutMs(timing, profile)).toBe(
      generic.journeyTimeoutMs +
        defenseWindowMs +
        2 * profile.operator_shift_ms,
    );
    // Every other family has no such window.
    expect(generic.allowances.undefendedCaseMs).toBe(0);
  });

  it("refuses validationTraceDispute's budget without the deployment's dispute clocks", () => {
    expect(() =>
      journeyTimingForCategory("validationTraceDispute", {
        ...preprod,
        actionDepth: 1,
      }),
    ).toThrow(/dispute clocks/u);
  });

  it("refuses a limit no timer can hold", () => {
    expect(() =>
      stackJourneyTimeoutMs(
        { journeyTimeoutMs: 2_147_483_647 - profile.operator_shift_ms },
        profile,
      ),
    ).toThrow(/timer range/u);
  });
});

describe("a family the deployment's profile leaves out of its catalogue", () => {
  // The Preprod testing profile, as an attached twin stack's finalized
  // manifest carries it: 10-block finality, the watcher acting at inclusion.
  const testing = DEPLOYMENT_PROFILES["preprod-testing"];
  const cadence = {
    ...preprod,
    confirmationDepth: testing.l1_finality.confirmation_depth,
    actionDepth: 1,
    disputeTiming: testing.timing,
    deploymentProfile: testing,
  };

  it("refuses it by name and profile before any family is staged", () => {
    expect(testing.excluded_fraud_proof_categories).toContain(
      "validationTraceDispute",
    );
    expect(() =>
      journeyTimingForCategory("validationTraceDispute", cadence),
    ).toThrow(/validationTraceDispute.*preprod-testing/u);
  });

  it("refuses every family a profile leaves out and budgets every family it installs", () => {
    const mainnet = DEPLOYMENT_PROFILES.mainnet;
    const mainnetCadence = {
      ...cadence,
      disputeTiming: mainnet.timing,
      deploymentProfile: mainnet,
    };
    expect(mainnet.excluded_fraud_proof_categories.length).toBeGreaterThan(1);
    for (const category of mainnet.excluded_fraud_proof_categories)
      expect(() => journeyTimingForCategory(category, mainnetCadence)).toThrow(
        new RegExp(`${category}.*mainnet`, "u"),
      );
    for (const category of ["transitionTrace", "doubleWithdraw", "zeroInput"])
      expect(
        journeyTimingForCategory(category, cadence).journeyTimeoutMs,
      ).toBeGreaterThan(0);
    // The rehearsal profile installs the execution dispute.
    const rehearsal = DEPLOYMENT_PROFILES["preprod-rehearsal"];
    expect(
      journeyTimingForCategory("validationTraceDispute", {
        ...cadence,
        disputeTiming: rehearsal.timing,
        deploymentProfile: rehearsal,
      }).allowances.undefendedCaseMs,
    ).toBeGreaterThan(0);
  });
});
