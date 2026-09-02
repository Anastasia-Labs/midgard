import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyExecutionSourceScriptDecodingScriptsV1,
  EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1,
} from "../src/execution-source-script-decoding/contracts-v1.js";
import {
  makeFaultProofEmulatorHarnessV1,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const hasFamily = EXECUTION_SOURCE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1.every(
  (title) =>
    blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)(
  "executionSourceScriptDecoding signed publication fit",
  () => {
    it("publishes all five applied scripts below the reliability reserve", async () => {
      const harness = await makeFaultProofEmulatorHarnessV1();
      const addressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
      );
      const steps = applyExecutionSourceScriptDecodingScriptsV1({
        blueprint,
        network: "Preprod",
        computationThreadPolicyId: harness.contracts.computationThread.policyId,
        fraudProofPolicyId: harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData: addressData,
        hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
      });
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
  },
);
