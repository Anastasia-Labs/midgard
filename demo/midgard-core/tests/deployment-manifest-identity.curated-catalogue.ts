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
    // The public catalogue carries the execution dispute and every family.
    expect(curated).toContain("validationTraceDispute");
    expect(curated).toContain("unusedRedeemer");
    expect(curated).toContain("doubleSpend");
    const curatedCatalogue = await buildCatalogueFixture(curated);
    expect(
      verifyDeploymentManifestFraudProofCatalogueIdentity(
        curatedCatalogue,
        curated,
      ),
    ).toEqual(curatedCatalogue);
    // A catalogue missing a category the profile installs is not the
    // deployment's: the selected testing profile's catalogue lacks the
    // execution dispute.
    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity(
        catalogueFixture(),
        curated,
      ),
    ).toThrow(/\.validationTraceDispute is required/u);
    // The selected 900 s testing profile excludes the execution dispute,
    // which the public catalogue carries.
    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity(curatedCatalogue),
    ).toThrow(/\.validationTraceDispute is unexpected/u);
  });
});
