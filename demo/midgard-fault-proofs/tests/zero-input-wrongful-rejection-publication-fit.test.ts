import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyZeroInputScripts,
  ZERO_INPUT_BLUEPRINT_TITLES,
} from "../src/zero-input/contracts-v1.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const hasFamily = ZERO_INPUT_BLUEPRINT_TITLES.every((title) =>
  blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)(
  "zeroInput direction-complete signed publication fit",
  () => {
    it("publishes both applied scripts below the reliability reserve", async () => {
      const harness = await makeFaultProofEmulatorHarness();
      const proofAddressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
      );
      const steps = applyZeroInputScripts({
        blueprint,
        network: "Preprod",
        computationThreadPolicyId: harness.contracts.computationThread.policyId,
        fraudProofPolicyId: harness.contracts.fraudProof.policyId,
        fraudProofTokenAddressData: proofAddressData,
        fieldPreimageCertificatePolicyId:
          harness.contracts.fieldPreimageCertificate.policyId,
        hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
      });
      for (const [index, step] of steps.entries()) {
        const published = await publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `zero input direction complete step ${index + 1}`,
        });
        console.info(
          `[zero-input-publication] ${JSON.stringify({ step: index + 1, scriptHash: step.spendingScriptHash, bytes: published.publicationMeasurement.completeSignedBytes, memory: published.publicationMeasurement.executionMemory.toString(), cpu: published.publicationMeasurement.executionSteps.toString() })}`,
        );
        expect(
          published.publicationMeasurement.completeSignedBytes,
        ).toBeLessThanOrEqual(15_872);
      }
    }, 600_000);
  },
);
