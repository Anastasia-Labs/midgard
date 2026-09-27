import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import { canonicalJson } from "../src/canonical-json.js";
import {
  daBondManifestAmounts,
  DEPLOYMENT_PROFILES,
  requireSelectedDeploymentProfile,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
  verifyDeploymentProfileBinding,
} from "../src/deployment-profile.js";

describe("deployment profile binding", () => {
  it("uses the SHA-256 digest of the canonical resolved profile", () => {
    expect(
      createHash("sha256")
        .update(canonicalJson(SELECTED_DEPLOYMENT_PROFILE, "profile"))
        .digest("hex"),
    ).toBe(SELECTED_DEPLOYMENT_PROFILE_DIGEST);
  });
  it("accepts the selected profile and rejects the other Preprod deployment", () => {
    expect(
      requireSelectedDeploymentProfile(SELECTED_DEPLOYMENT_PROFILE.name),
    ).toBe(SELECTED_DEPLOYMENT_PROFILE);
    expect(() =>
      verifyDeploymentProfileBinding(
        SELECTED_DEPLOYMENT_PROFILE,
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        SELECTED_DEPLOYMENT_PROFILE.network,
      ),
    ).not.toThrow();
    expect(() =>
      verifyDeploymentProfileBinding(
        DEPLOYMENT_PROFILES["preprod-public"],
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        "Preprod",
      ),
    ).toThrow();
  });

  it("rejects changed contents even when supplied with the expected digest", () => {
    const profile = structuredClone(SELECTED_DEPLOYMENT_PROFILE);
    expect(() =>
      verifyDeploymentProfileBinding(
        { ...profile, timing: { ...profile.timing, block_maturity_ms: 1 } },
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        profile.network,
      ),
    ).toThrow();
    expect(() =>
      verifyDeploymentProfileBinding(profile, "00".repeat(32), profile.network),
    ).toThrow();
    expect(() =>
      verifyDeploymentProfileBinding(
        profile,
        SELECTED_DEPLOYMENT_PROFILE_DIGEST,
        "Mainnet",
      ),
    ).toThrow();
    expect(() => requireSelectedDeploymentProfile(undefined)).toThrow();
    expect(() => requireSelectedDeploymentProfile("preprod-public")).toThrow();
  });

  it("maps each DA bond amount to its own manifest key", () => {
    // The testing profiles set the minimum top-up equal to the pool floor, so
    // only a public profile, where every amount differs, catches a swap.
    expect(daBondManifestAmounts(DEPLOYMENT_PROFILES.mainnet)).toEqual({
      daBondLovelace: 100_000_000_000,
      daSlashPenaltyLovelace: 25_000_000_000,
      daBondMinTopUpLovelace: 1_000_000_000,
      daBondPoolFloorLovelace: 5_000_000,
      challengeRecordLovelace: 27_000_000,
    });
  });
});
