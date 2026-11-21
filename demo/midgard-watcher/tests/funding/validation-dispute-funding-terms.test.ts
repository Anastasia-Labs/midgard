/** The validation-trace dispute's funding terms come from the signed
 * deployment: the bond its VerifySource posts is the manifest's dispute
 * bond, and any other category carries no dispute terms. */
import { describe, expect, it } from "vitest";

import { watcherValidationDisputeFundingTerms } from "../../src/funding/validation-dispute-funding-terms.js";
import { watcherDeploymentAvailabilityChallengeAuthority } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.build-watcher-deployment-authority-fixture.js";

const identity = makeWatcherDeploymentAuthorityFixture().result;

describe("validation-trace dispute funding terms from the signed deployment", () => {
  it("posts the manifest's dispute bond", () => {
    const bond = BigInt(
      watcherDeploymentAvailabilityChallengeAuthority(identity).parameters
        .challengerBondLovelace,
    );
    expect(bond).toBeGreaterThan(0n);
    expect(
      watcherValidationDisputeFundingTerms(identity, "validationTraceDispute"),
    ).toStrictEqual({ validationDispute: { bondLovelace: bond } });
  });

  it("carries no dispute terms for another category", () => {
    expect(
      watcherValidationDisputeFundingTerms(identity, "doubleSpend"),
    ).toStrictEqual({});
  });
});
