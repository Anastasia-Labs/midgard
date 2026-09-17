import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyExecutionNativeScriptInvalidScripts,
  EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES,
  EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES,
} from "../src/execution-native-script-invalid/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const missingTitles = [
  ...EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES,
  ...EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES,
].filter(
  (title) =>
    !blueprint.validators.some((validator) => validator.title === title),
);

describe("executionNativeScriptInvalid signed publication fit", () => {
  /**
   * Discovery gate (§14). This file used to hang off `describe.runIf`, so a
   * blueprint that no longer carried the family reported a clean pass instead
   * of an unpublishable deployment. The family's presence is now a required
   * result of its own.
   */
  it("finds every declared executionNativeScriptInvalid validator in the deployed blueprint", () => {
    expect(missingTitles).toEqual([]);
  });

  it("publishes every logical and accepted-reconstruction script below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyExecutionNativeScriptInvalidScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
    });
    const sizes: number[] = [];
    const physical = [...steps, ...steps.acceptedPrelude];
    // Six logical steps plus the seven-script accepted-reconstruction
    // prelude: a short or empty `physical` would otherwise walk the loop
    // below too few times and report a vacuous pass.
    expect(steps).toHaveLength(6);
    expect(steps.acceptedPrelude).toHaveLength(7);
    for (const [index, step] of physical.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `execution native script invalid physical step ${index + 1}`,
      });
      sizes.push(published.publicationMeasurement.completeSignedBytes);
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(15_872);
    }
    console.info(
      `[execution-native-script-invalid-publication] ${JSON.stringify({ signedSizes: sizes })}`,
    );
  }, 600_000);
});
