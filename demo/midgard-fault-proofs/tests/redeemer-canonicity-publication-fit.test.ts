import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyRedeemerCanonicityScripts,
  REDEEMER_CANONICITY_BLUEPRINT_TITLES,
} from "../src/redeemer-canonicity/contracts-v1.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);

describe("redeemerCanonicity signed publication fit", () => {
  it("publishes every fully applied step within ordinary L1 limits", async () => {
    expect(
      REDEEMER_CANONICITY_BLUEPRINT_TITLES.every((title) =>
        blueprint.validators.some((validator) => validator.title === title),
      ),
    ).toBe(true);
    const harness = await makeFaultProofEmulatorHarness();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyRedeemerCanonicityScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `redeemer canonicity step ${index + 1}`,
      });
      console.info(
        `[redeemer-canonicity-publication] ${JSON.stringify({ step: index + 1, scriptHash: step.spendingScriptHash, bytes: published.publicationMeasurement.completeSignedBytes })}`,
      );
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(16_384);
    }
  }, 600_000);
});
