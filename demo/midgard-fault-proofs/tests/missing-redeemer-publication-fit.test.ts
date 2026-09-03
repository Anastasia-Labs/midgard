import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyMissingRedeemerScripts,
  MISSING_REDEEMER_BLUEPRINT_TITLES,
} from "../src/missing-redeemer/contracts-v1.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const hasFamily = MISSING_REDEEMER_BLUEPRINT_TITLES.every((title) =>
  blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)("missingRedeemer signed publication fit", () => {
  it("publishes all seven applied scripts below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyMissingRedeemerScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    const sizes: number[] = [];
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `missing redeemer step ${index + 1}`,
      });
      sizes.push(published.publicationMeasurement.completeSignedBytes);
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(15_872);
    }
    console.info(`[missing-redeemer-publication] ${JSON.stringify(sizes)}`);
  }, 600_000);
});
