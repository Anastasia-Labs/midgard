import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyReceivePurposeLanguageScripts,
  RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES,
} from "../src/receive-purpose-language/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const missingTitles = RECEIVE_PURPOSE_LANGUAGE_BLUEPRINT_TITLES.filter(
  (title) =>
    !blueprint.validators.some((validator) => validator.title === title),
);

describe("receivePurposeLanguage signed publication fit", () => {
  /**
   * Discovery gate (§14). This file used to hang off `describe.runIf`, so a
   * blueprint that no longer carried the family reported a clean pass instead
   * of an unpublishable deployment. The family's presence is now a required
   * result of its own.
   */
  it("finds every declared receivePurposeLanguage validator in the deployed blueprint", () => {
    expect(missingTitles).toEqual([]);
  });

  it("publishes all three applied scripts below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyReceivePurposeLanguageScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    // Three is the deployment contract this file's name states: a short or
    // empty `steps` would otherwise walk the loop below too few times and
    // report a vacuous pass.
    expect(steps).toHaveLength(3);
    const sizes: number[] = [];
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `receive purpose language step ${(index + 1).toString()}`,
      });
      sizes.push(published.publicationMeasurement.completeSignedBytes);
      expect(
        published.publicationMeasurement.completeSignedBytes,
        `step ${(index + 1).toString()} signed publication bytes`,
      ).toBeLessThanOrEqual(15_872);
    }
    console.info(
      `[receive-purpose-language-publication] ${JSON.stringify(sizes)}`,
    );
  }, 600_000);
});
