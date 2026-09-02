import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyDistinctAssetAccumulationLimitScriptsV1,
  DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES_V1,
} from "../src/distinct-asset-accumulation-limit/contracts-v1.js";
import {
  makeFaultProofEmulatorHarnessV1,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const hasFamily = DISTINCT_ASSET_ACCUMULATION_LIMIT_BLUEPRINT_TITLES_V1.every(
  (title) =>
    blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)(
  "distinctAssetAccumulationLimit signed publication fit",
  () => {
    it("publishes all six applied scripts below the reliability reserve", async () => {
      const harness = await makeFaultProofEmulatorHarnessV1();
      const proofAddressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
      );
      const steps = applyDistinctAssetAccumulationLimitScriptsV1({
        blueprint,
        network: "Preprod",
        computationThreadPolicyId: harness.contracts.computationThread.policyId,
        fraudProofPolicyId: harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData: proofAddressData,
        hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
      });
      for (const [index, step] of steps.entries()) {
        const published = await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `distinct asset accumulation limit step ${index + 1}`,
        });
        expect(
          published.publicationMeasurement.completeSignedBytes,
          `step ${(index + 1).toString()} signed publication bytes`,
        ).toBeLessThanOrEqual(15_872);
      }
    }, 600_000);
  },
);
