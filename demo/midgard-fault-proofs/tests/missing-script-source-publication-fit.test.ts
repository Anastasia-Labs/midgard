import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyMissingScriptSourceScripts,
  MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES,
} from "../src/missing-script-source/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const missingTitles = MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES.filter(
  (title) =>
    !blueprint.validators.some((validator) => validator.title === title),
);

describe("missingScriptSource signed publication fit", () => {
  /**
   * Discovery gate (§14). This file used to hang off `describe.runIf`, so a
   * blueprint that no longer carried the family reported a clean pass instead
   * of an unpublishable deployment. The family's presence is now a required
   * result of its own.
   */
  it("finds every declared missingScriptSource validator in the deployed blueprint", () => {
    expect(missingTitles).toEqual([]);
  });

  it("publishes all six fully applied validators below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyMissingScriptSourceScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    // Six is the deployment contract this file's name states: a short or empty
    // `steps` would otherwise walk the loop below too few times and report a
    // vacuous pass.
    expect(steps).toHaveLength(6);
    const sizes: number[] = [];
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `missing script source step ${index + 1}`,
      });
      sizes.push(published.publicationMeasurement.completeSignedBytes);
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(15_872);
    }
    console.info(
      `[missing-script-source-publication] ${JSON.stringify(sizes)}`,
    );
  }, 600_000);
});
