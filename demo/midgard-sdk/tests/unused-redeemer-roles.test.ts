import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkUnusedRedeemerRoleOrder,
  UNUSED_REDEEMER_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/unused-redeemer.js";

const manifestSteps = () => [...deploymentFamilyStepNames("unusedRedeemer")];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("unused-redeemer role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(UNUSED_REDEEMER_ROLE_ORDER.length);
    expect(() => checkUnusedRedeemerRoleOrder(manifestSteps())).not.toThrow();
  });

  it("refuses a manifest that swaps the claim mint grammar and scan steps", () => {
    const grammar = UNUSED_REDEEMER_ROLE_ORDER.indexOf("claimMintGrammar");
    const scan = UNUSED_REDEEMER_ROLE_ORDER.indexOf("claimMintScan");
    expect(scan).toBe(grammar + 1);
    expect(() => checkUnusedRedeemerRoleOrder(swapped(grammar, scan))).toThrow(
      `unused-redeemer role claimMintGrammar is fraudProofUnusedRedeemerClaimMintGrammar, but manifest step ${grammar} is fraudProofUnusedRedeemerClaimMintScan`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    const roles = UNUSED_REDEEMER_ROLE_ORDER.length;
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() => checkUnusedRedeemerRoleOrder(changed)).toThrow(
        `unused-redeemer has ${roles} roles; the manifest has ${changed.length} steps`,
      );
  });
});
