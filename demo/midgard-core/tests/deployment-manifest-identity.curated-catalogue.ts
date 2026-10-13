import "./deployment-manifest-identity.finalized-manifest.js";

import { describe, expect, it } from "vitest";

import { verifyDeploymentManifestFraudProofCatalogueIdentity } from "../src/deployment-manifest-identity.js";
import {
  DEPLOYMENT_PROFILES,
  deploymentFraudProofCatalogueCategories,
} from "../src/deployment-profile.js";
import {
  buildCatalogueFixture,
  catalogueFixture,
} from "./deployment-manifest-identity.finalized-manifest.js";

describe("curated deployment fraud-proof catalogue", () => {
  it("checks a curated deployment's catalogue against the profile's set", async () => {
    const curated = deploymentFraudProofCatalogueCategories(
      DEPLOYMENT_PROFILES["preprod-public"],
    );
    expect(curated).not.toContain("validationTraceDispute");
    expect(curated).not.toContain("unusedRedeemer");
    expect(curated).toContain("doubleSpend");
    const curatedCatalogue = await buildCatalogueFixture(curated);
    expect(
      verifyDeploymentManifestFraudProofCatalogueIdentity(
        curatedCatalogue,
        curated,
      ),
    ).toEqual(curatedCatalogue);
    // A catalogue carrying an excluded family is not the deployment's.
    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity(
        catalogueFixture(),
        curated,
      ),
    ).toThrow(/validationTraceDispute is unexpected/u);
    // The selected testing profile carries the whole catalogue.
    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity(curatedCatalogue),
    ).toThrow(/validationTraceDispute is required/u);
  });
});
