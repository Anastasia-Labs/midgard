import "./deployment-manifest.v1-deployment-manifest.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import {
  DEPLOYMENT_PROFILES,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import { describe, expect, it } from "vitest";

import {
  computeDeploymentManifestId,
  parseDeploymentManifestValue,
  validationDisputeMaturityFitsProfile,
} from "../src/deployment-manifest.js";
import { canonicalManifest } from "./deployment-manifest.canonical-identity.js";

describe("validation-dispute maturity by profile", () => {
  // Public: D_case = 2 * 86,400,000 + 2 * (2 * 32 + 3) * 120,000 + 21,600,000
  // = 210,480,000 (validation-dispute-v1.ak), so the floor is 2 D_case + 1.
  const PUBLIC_LIMITS = { minValidationDisputeMaturityMs: 420_960_001 };
  const installed = { excluded_fraud_proof_categories: [] };
  const excluded = {
    excluded_fraud_proof_categories: ["validationTraceDispute"],
  };

  it("holds a profile that installs the execution dispute to 2 D_case + 1, exactly", () => {
    for (const profile of [installed, DEPLOYMENT_PROFILES["preprod-public"]]) {
      expect(
        validationDisputeMaturityFitsProfile(
          profile,
          420_960_000,
          PUBLIC_LIMITS,
        ),
      ).toBe(false);
      expect(
        validationDisputeMaturityFitsProfile(
          profile,
          420_960_001,
          PUBLIC_LIMITS,
        ),
      ).toBe(true);
    }
  });

  it("passes any maturity when the profile excludes the execution dispute", () => {
    for (const profile of [
      excluded,
      DEPLOYMENT_PROFILES.mainnet,
      DEPLOYMENT_PROFILES["preprod-testing"],
      DEPLOYMENT_PROFILES["local-devnet-testing"],
    ]) {
      for (const maturityMs of [900_000, 420_960_000]) {
        expect(
          validationDisputeMaturityFitsProfile(
            profile,
            maturityMs,
            PUBLIC_LIMITS,
          ),
        ).toBe(true);
      }
    }
  });

  it("loads the selected 900 s profile, which excludes the execution dispute", () => {
    expect(SELECTED_DEPLOYMENT_PROFILE.timing.block_maturity_ms).toBe(900_000);
    expect(SELECTED_DEPLOYMENT_PROFILE.excluded_fraud_proof_categories).toEqual(
      ["validationTraceDispute"],
    );
    // Its own floor (D_case = 10,780,000) is far above fifteen minutes.
    expect(
      MIDGARD_CONSENSUS_PROFILE.limits.minValidationDisputeMaturityMs,
    ).toBe(21_560_001);
    expect(
      validationDisputeMaturityFitsProfile(
        SELECTED_DEPLOYMENT_PROFILE,
        MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
        MIDGARD_CONSENSUS_PROFILE.limits,
      ),
    ).toBe(true);
    expect(parseDeploymentManifestValue(canonicalManifest())).toEqual(
      canonicalManifest(),
    );
    // A manifest that installs 00000006 is not this profile's deployment: the
    // node refuses it at startup with a plain error rather than running a
    // dispute its maturity cannot hold.
    const { manifestId: _manifestId, ...identity } = canonicalManifest();
    const withDispute = structuredClone(identity) as unknown as {
      contracts: {
        fraudProofCatalogueMint: {
          fraudProofCatalogue: {
            categories: Record<string, { categoryId: string }>;
          };
        };
      };
    };
    const categories =
      withDispute.contracts.fraudProofCatalogueMint.fraudProofCatalogue
        .categories;
    categories.validationTraceDispute = {
      ...categories.doubleSpend!,
      categoryId: "00000006",
    };
    expect(() =>
      parseDeploymentManifestValue({
        ...withDispute,
        manifestId: computeDeploymentManifestId(
          withDispute as unknown as typeof identity,
        ),
      }),
    ).toThrow(
      "Deployment manifest contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.validationTraceDispute is unexpected",
    );
  });
});
