import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyResolvedOutputNonCanonicalScripts,
  RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
} from "../src/resolved-output-non-canonical/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const hasFamily = RESOLVED_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES.every(
  (title) =>
    blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)(
  "resolvedOutputNonCanonical signed publication fit",
  () => {
    it("publishes every applied script below the reliability reserve", async () => {
      const harness = await makeFaultProofEmulatorHarness();
      const proofAddressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
      );
      const steps = applyResolvedOutputNonCanonicalScripts({
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
          label: `resolved output non-canonical step ${index + 1}`,
        });
        console.info(
          `[resolved-output-non-canonical-publication] ${JSON.stringify({ step: index + 1, hash: step.spendingScriptHash, bytes: published.publicationMeasurement.completeSignedBytes, memory: published.publicationMeasurement.executionMemory.toString(), cpu: published.publicationMeasurement.executionSteps.toString() })}`,
        );
        expect(
          published.publicationMeasurement.completeSignedBytes,
        ).toBeLessThanOrEqual(15_872);
      }
    }, 600_000);
  },
);
