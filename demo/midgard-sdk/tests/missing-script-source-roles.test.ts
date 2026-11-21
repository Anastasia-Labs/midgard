import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkMissingScriptSourceRoleOrder,
  MISSING_SCRIPT_SOURCE_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/missing-script-source.js";

const manifestSteps = () => [
  ...deploymentFamilyStepNames("missingScriptSource"),
];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("missing-script-source role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(
      MISSING_SCRIPT_SOURCE_ROLE_ORDER.length,
    );
    expect(() =>
      checkMissingScriptSourceRoleOrder(manifestSteps()),
    ).not.toThrow();
  });

  it("refuses a manifest that swaps the claim mint grammar and scan steps", () => {
    const grammar =
      MISSING_SCRIPT_SOURCE_ROLE_ORDER.indexOf("claimMintGrammar");
    const scan = MISSING_SCRIPT_SOURCE_ROLE_ORDER.indexOf("claimMintScan");
    expect(scan).toBe(grammar + 1);
    expect(() =>
      checkMissingScriptSourceRoleOrder(swapped(grammar, scan)),
    ).toThrow(
      `missing-script-source role claimMintGrammar is fraudProofMissingScriptSourceClaimMintGrammar, but manifest step ${grammar} is fraudProofMissingScriptSourceClaimMintScan`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    const roles = MISSING_SCRIPT_SOURCE_ROLE_ORDER.length;
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() => checkMissingScriptSourceRoleOrder(changed)).toThrow(
        `missing-script-source has ${roles} roles; the manifest has ${changed.length} steps`,
      );
  });
});
