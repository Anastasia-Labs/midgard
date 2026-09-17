import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyExecutionSourceScriptDecodingScripts,
  EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES,
} from "../src/execution-source-script-decoding/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const missingTitles = EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES.filter(
  (title) =>
    !blueprint.validators.some((validator) => validator.title === title),
);

describe("executionSourceScriptDecoding signed publication fit", () => {
  /**
   * Discovery gate (§14). This file used to hang off `describe.runIf`, so a
   * blueprint that no longer carried the family reported a clean pass instead
   * of an unpublishable deployment. The family's presence is now a required
   * result of its own.
   */
  it("finds every declared executionSourceScriptDecoding validator in the deployed blueprint", () => {
    expect(missingTitles).toEqual([]);
  });

  it("publishes all five applied scripts below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyExecutionSourceScriptDecodingScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    // Five is the deployment contract this file's name states: an empty or
    // short `steps` would otherwise walk the loop below zero or too few
    // times and report a vacuous pass.
    expect(steps).toHaveLength(5);
    const sizes: number[] = [];
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `execution source decoding step ${index + 1}`,
      });
      sizes.push(published.publicationMeasurement.completeSignedBytes);
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(15_872);
    }
    console.info(
      `[execution-source-script-decoding-publication] ${JSON.stringify(sizes)}`,
    );
  }, 600_000);
});
