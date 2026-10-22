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
    // The execution dispute is in the public catalogue; the families with no
    // installed off-chain workflow are not.
    expect(curated).toContain("validationTraceDispute");
    expect(curated).not.toContain("unusedRedeemer");
    expect(curated).toContain("doubleSpend");
    const excludedFamily = (verdict: string) =>
      new RegExp(
        `\\.(${DEPLOYMENT_PROFILES["preprod-public"].excluded_fraud_proof_categories.join("|")}) is ${verdict}`,
        "u",
      );
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
    ).toThrow(excludedFamily("unexpected"));
    // The selected 900 s testing profile excludes the execution dispute,
    // which the public catalogue carries.
    expect(() =>
      verifyDeploymentManifestFraudProofCatalogueIdentity(curatedCatalogue),
    ).toThrow(/\.validationTraceDispute is unexpected/u);
  });
});
