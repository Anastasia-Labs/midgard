/** The validation-trace dispute's funding terms come from the signed
 * deployment: the thread moves' terms name the boundary and each one-step
 * resolver a thread visits between the game and the award by the address
 * of the script the manifest applies under that holder's name, and a
 * policy for any other category carries no dispute terms. */
import { DEPLOYMENT_MEMBERS } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { watcherValidationDisputeFundingTerms } from "../../src/funding/prover-funding-authority.js";
import { watcherDeploymentAppliedScriptHashes } from "../../src/runtime/deployment-identity.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const identity = makeWatcherDeploymentAuthorityFixture().result;

/** The address of the script the signed manifest applies under `name`. */
const manifestAddress = (name: string) => {
  const scriptHash = watcherDeploymentAppliedScriptHashes(identity)[name];
  if (scriptHash === undefined) throw new Error(`no ${name}`);
  return credentialToAddress(
    identity.network,
    scriptHashToCredential(scriptHash),
  );
};

/** The dispute's spending contracts a thread is never paid to past the
 * boundary: the case's own and the game's, and the award. */
const OUTSIDE_RESOLUTION = new Set([
  "validationTraceDispute",
  "validationTraceDisputeSource",
  "validationTraceDisputeGame",
  "validationTraceDisputeBoundary",
  "validationTraceDisputeTimeout",
  "validationTraceDisputeAward",
  "validationTraceDisputeCase",
  "validationTraceDisputeBond",
  "validationTraceDisputeProofItem",
]);

/** The one-step resolvers no thread of ours is paid to: those whose step
 * the runner never plays because no honest dispute reaches it (a CEK
 * finish step, which a Plutus trace never emits; and the resolvers of an
 * input that does not exist, a missing script source and a missing
 * redeemer, each convicted by its own family), the value-and-mint
 * resolution, which a dispute opened on a Plutus execution failure never
 * reaches (decisions/2026-11-05-vtd-missing-phases.md), and the CEK context
 * finalize stages of a script that is not a Cardano spend, which the runner
 * does not drive. */
const NOT_PLAYED = new Set([
  "validationTraceDisputeCekFinishSemantic",
  "validationTraceDisputeResolveInputsNonMembershipSemantic",
  "validationTraceDisputeScriptSourcesStageNineMissingSemantic",
  "validationTraceDisputeScriptSourcesStageTenMissingSemantic",
  "validationTraceDisputeCekContextFinalizeMint",
  "validationTraceDisputeCekContextFinalizeWithdraw",
  "validationTraceDisputeCekContextFinalizeObserve",
  "validationTraceDisputeCekContextFinalizeMidgard",
]);

/** The deployment members the thread moves pay a thread to past the
 * boundary: every spending contract of the dispute's family but those
 * above and the value-and-mint resolution's. Its withdrawal contracts are
 * yields, invoked beside a move, never paid. */
const ONE_STEP_RESOLVERS = Object.entries(DEPLOYMENT_MEMBERS)
  .filter(
    ([name, { family, purpose }]) =>
      family === "validationTraceDispute" &&
      purpose === "spend" &&
      !OUTSIDE_RESOLUTION.has(name) &&
      !NOT_PLAYED.has(name) &&
      !name.startsWith("validationTraceDisputeValueAndMint"),
  )
  .map(([name]) => name);

describe("validation-trace dispute funding terms from the signed deployment", () => {
  it("names the boundary and each one-step resolver by the manifest's applied script", () => {
    const { validationDispute } = watcherValidationDisputeFundingTerms(
      identity,
      "validationTraceDispute",
    );
    const oneStep = validationDispute?.moves?.oneStep;
    expect(oneStep?.boundaryAddress).toBe(
      manifestAddress("validationTraceDisputeBoundary"),
    );
    const resolvers = oneStep?.resolverAddresses ?? [];
    // Each named once, and none the boundary's.
    expect(new Set([oneStep?.boundaryAddress, ...resolvers]).size).toBe(
      resolvers.length + 1,
    );
    expect([...resolvers].sort()).toStrictEqual(
      ONE_STEP_RESOLVERS.map(manifestAddress).sort(),
    );
  });

  it("carries no dispute terms for another category", () => {
    expect(
      watcherValidationDisputeFundingTerms(identity, "doubleSpend"),
    ).toStrictEqual({});
  });
});
