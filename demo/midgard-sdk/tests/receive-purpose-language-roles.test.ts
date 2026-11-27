import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkReceivePurposeLanguageRoleOrder,
  RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/receive-purpose-language.js";

const manifestSteps = () => [
  ...deploymentFamilyStepNames("receivePurposeLanguage"),
];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("receive-purpose-language role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(21);
    expect(manifestSteps()).toHaveLength(
      RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER.length,
    );
    expect(() =>
      checkReceivePurposeLanguageRoleOrder(manifestSteps()),
    ).not.toThrow();
  });

  it("refuses a manifest that swaps the lookup and the claimed redeemer grammar", () => {
    const lookup = RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER.indexOf("sourceLookup");
    const grammar = RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER.indexOf(
      "claimedRedeemerGrammar",
    );
    expect(grammar).toBe(lookup + 1);
    expect(() =>
      checkReceivePurposeLanguageRoleOrder(swapped(lookup, grammar)),
    ).toThrow(
      `receive-purpose-language role sourceLookup is fraudProofReceivePurposeLanguageSourceLookup, but manifest step ${lookup} is fraudProofReceivePurposeLanguageClaimedRedeemerGrammar`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    const roles = RECEIVE_PURPOSE_LANGUAGE_ROLE_ORDER.length;
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() => checkReceivePurposeLanguageRoleOrder(changed)).toThrow(
        `receive-purpose-language has ${roles} roles; the manifest has ${changed.length} steps`,
      );
  });
});
