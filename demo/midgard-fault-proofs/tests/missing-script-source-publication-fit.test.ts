import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyMissingScriptSourceScriptsV1,
  MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES_V1,
} from "../src/missing-script-source/contracts-v1.js";
import {
  makeFaultProofEmulatorHarnessV1,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const hasFamily = MISSING_SCRIPT_SOURCE_BLUEPRINT_TITLES_V1.every((title) =>
  blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)("missingScriptSource signed publication fit", () => {
  it("publishes all six fully applied validators below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1();
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyMissingScriptSourceScriptsV1({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: addressData,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    const sizes: number[] = [];
    const digests: string[] = [];
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `missing script source step ${index + 1}`,
      });
      sizes.push(published.publicationMeasurement.completeSignedBytes);
      digests.push(
        createHash("sha256").update(step.spendingScript.script).digest("hex"),
      );
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(15_872);
    }
    console.info(
      `[missing-script-source-publication] ${JSON.stringify(sizes)}`,
    );
    console.info(`[missing-script-source-scripts] ${JSON.stringify(digests)}`);
  }, 600_000);
});
import { createHash } from "node:crypto";
