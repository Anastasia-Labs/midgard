import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applySpendInputSignerMissingScriptsV1,
  SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES_V1,
} from "../src/spend-input-signer-missing/index.js";
import {
  makeFaultProofEmulatorHarnessV1,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);

describe("spendInputSignerMissing signed publication fit", () => {
  it("publishes all five fully applied scripts with ordinary L1 limits", async () => {
    expect(
      SPEND_INPUT_SIGNER_MISSING_BLUEPRINT_TITLES_V1.every((title) =>
        blueprint.validators.some((validator) => validator.title === title),
      ),
    ).toBe(true);
    const harness = await makeFaultProofEmulatorHarnessV1();
    const proofAddressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applySpendInputSignerMissingScriptsV1({
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
      const publication = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `spend-input-signer-missing-step-${String(index + 1)}`,
      });
      const measurement = publication.publicationMeasurement;
      console.info(
        `[spend-input-signer-missing-publication] ${JSON.stringify({ step: index + 1, hash: step.spendingScriptHash, bytes: measurement.completeSignedBytes, memory: measurement.executionMemory.toString(), cpu: measurement.executionSteps.toString() })}`,
      );
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 600_000);
});
