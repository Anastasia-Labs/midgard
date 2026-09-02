import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyUnusedRedeemerScriptsV1,
  UNUSED_REDEEMER_BLUEPRINT_TITLES_V1,
} from "../src/unused-redeemer/contracts-v1.js";
import {
  makeFaultProofEmulatorHarnessV1,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const publicationReserveBytes = 15_872;
describe("unusedRedeemer signed publication fit", () => {
  it("publishes all nine fully applied validators within ordinary L1 limits", async () => {
    expect(
      UNUSED_REDEEMER_BLUEPRINT_TITLES_V1.every((title) =>
        blueprint.validators.some((v) => v.title === title),
      ),
    ).toBe(true);
    const harness = await makeFaultProofEmulatorHarnessV1();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyUnusedRedeemerScriptsV1({
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
        label: `unused-redeemer-step-${String(index + 1)}`,
      });
      console.info(
        `[unused-redeemer-publication] ${JSON.stringify({ step: index + 1, bytes: published.publicationMeasurement.completeSignedBytes, margin: publicationReserveBytes - published.publicationMeasurement.completeSignedBytes })}`,
      );
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(publicationReserveBytes);
    }
  }, 600_000);
});
