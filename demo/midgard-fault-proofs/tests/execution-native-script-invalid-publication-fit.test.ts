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
const hasFamily = [
  ...EXECUTION_NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES,
  ...EXECUTION_NATIVE_SCRIPT_INVALID_ACCEPTED_PRELUDE_TITLES,
].every((title) =>
  blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)(
  "executionNativeScriptInvalid signed publication fit",
  () => {
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
      const rawSizes = physical.map(
        ({ spendingScript }) => spendingScript.script.length / 2,
      );
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
        `[execution-native-script-invalid-publication] ${JSON.stringify({ rawSizes, signedSizes: sizes })}`,
      );
    }, 600_000);
  },
);
