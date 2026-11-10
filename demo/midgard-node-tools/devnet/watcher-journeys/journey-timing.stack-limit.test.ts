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

  it("refuses a limit no timer can hold", () => {
    expect(() =>
      stackJourneyTimeoutMs(
        { journeyTimeoutMs: 2_147_483_647 - profile.operator_shift_ms },
        profile,
      ),
    ).toThrow(/timer range/u);
  });
});
