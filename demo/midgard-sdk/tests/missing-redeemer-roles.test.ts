import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkMissingRedeemerRoleOrder,
  MISSING_REDEEMER_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/missing-redeemer.js";

const manifestSteps = () => [...deploymentFamilyStepNames("missingRedeemer")];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("missing-redeemer role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(MISSING_REDEEMER_ROLE_ORDER.length);
    expect(() => checkMissingRedeemerRoleOrder(manifestSteps())).not.toThrow();
  });

  it("refuses a manifest that swaps the claim mint grammar and scan steps", () => {
    const grammar = MISSING_REDEEMER_ROLE_ORDER.indexOf("claimMintGrammar");
    const scan = MISSING_REDEEMER_ROLE_ORDER.indexOf("claimMintScan");
    expect(scan).toBe(grammar + 1);
    expect(() => checkMissingRedeemerRoleOrder(swapped(grammar, scan))).toThrow(
      `missing-redeemer role claimMintGrammar is fraudProofMissingRedeemerClaimMintGrammar, but manifest step ${grammar} is fraudProofMissingRedeemerClaimMintScan`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    const roles = MISSING_REDEEMER_ROLE_ORDER.length;
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() => checkMissingRedeemerRoleOrder(changed)).toThrow(
        `missing-redeemer has ${roles} roles; the manifest has ${changed.length} steps`,
      );
  });
});
