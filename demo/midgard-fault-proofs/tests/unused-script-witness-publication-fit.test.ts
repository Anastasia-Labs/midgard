import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyUnusedScriptWitnessScripts,
  UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES,
} from "../src/unused-script-witness/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const publicationReserveBytes = 15_872;

describe("unusedScriptWitness signed publication fit", () => {
  it("publishes all six fully applied validators within ordinary L1 limits", async () => {
    expect(
      UNUSED_SCRIPT_WITNESS_BLUEPRINT_TITLES.every((title) =>
        blueprint.validators.some((validator) => validator.title === title),
      ),
    ).toBe(true);
    const harness = await makeFaultProofEmulatorHarness();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyUnusedScriptWitnessScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `unused-script-witness-step-${(index + 1).toString()}`,
      });
      console.info(
        `[unused-script-witness-publication] ${JSON.stringify({ step: index + 1, scriptHash: step.spendingScriptHash, bytes: published.publicationMeasurement.completeSignedBytes, publicationReserveMargin: publicationReserveBytes - published.publicationMeasurement.completeSignedBytes })}`,
      );
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(publicationReserveBytes);
    }
  }, 600_000);
});
