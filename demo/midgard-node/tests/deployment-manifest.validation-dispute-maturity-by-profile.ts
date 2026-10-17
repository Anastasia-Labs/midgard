import "./deployment-manifest.v1-deployment-manifest.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  isNonInteractiveTestingProfile,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import { validationDisputeMaturityFitsProfile } from "../src/deployment-manifest.js";

describe("validation-dispute maturity by profile", () => {
  // Both testing profiles: 32 rounds, a 60,000 ms response window, so the
  // whole interactive schedule is (2 * 32 + 2) * 60,000 = 3,960,000 ms and the
  // interactive floor is twice that.
  const TESTING_LIMITS = {
    maxValidationBisectionRounds: 32,
    validationDisputeResponseWindowMs: 60_000,
    minValidationDisputeMaturityMs: 7_920_000,
  };

  it("keeps every name outside the closed testing allowlist", () => {
    expect(isNonInteractiveTestingProfile("preprod-testing")).toBe(true);
    for (const name of [
      "preprod-public",
      "mainnet",
      "foo-testing",
      "local-devnet",
      "testing",
      "",
    ]) {
      expect(isNonInteractiveTestingProfile(name)).toBe(false);
    }
  });

  it("treats local-devnet-testing exactly like preprod-testing", () => {
    for (const name of ["preprod-testing", "local-devnet-testing"]) {
      expect(isNonInteractiveTestingProfile(name)).toBe(true);
      expect(
        validationDisputeMaturityFitsProfile(name, 900_000, TESTING_LIMITS),
      ).toBe(true);
      expect(
        validationDisputeMaturityFitsProfile(name, 7_920_000, TESTING_LIMITS),
      ).toBe(false);
    }
  });

  it("refuses a testing profile whose maturity admits an interactive dispute", () => {
    for (const maturityMs of [3_960_000, 7_920_000]) {
      expect(
        validationDisputeMaturityFitsProfile(
          "preprod-testing",
          maturityMs,
          TESTING_LIMITS,
        ),
      ).toBe(false);
    }
    expect(
      validationDisputeMaturityFitsProfile(
        "preprod-testing",
        3_959_999,
        TESTING_LIMITS,
      ),
    ).toBe(true);
  });

  it("holds every other profile to the interactive floor", () => {
    for (const name of ["preprod-public", "mainnet", "foo-testing"]) {
      expect(
        validationDisputeMaturityFitsProfile(name, 900_000, TESTING_LIMITS),
      ).toBe(false);
      expect(
        validationDisputeMaturityFitsProfile(name, 7_919_999, TESTING_LIMITS),
      ).toBe(false);
      expect(
        validationDisputeMaturityFitsProfile(name, 7_920_000, TESTING_LIMITS),
      ).toBe(true);
    }
  });

  it("accepts the selected profile at its own canonical maturity", () => {
    expect(
      validationDisputeMaturityFitsProfile(
        SELECTED_DEPLOYMENT_PROFILE.name,
        MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
        MIDGARD_CONSENSUS_PROFILE.limits,
      ),
    ).toBe(true);
  });
});
