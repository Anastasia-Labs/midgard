import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyMintDeclaredAssetLimitScripts,
  MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES,
} from "../src/mint-declared-asset-limit/contracts.js";
import { createMeasuredFitRecorder } from "./support/measured-fit-ledger.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const measuredFit = createMeasuredFitRecorder(
  "mint-declared-asset-limit",
  "publication",
  "fully applied testnet physical validator",
);

const blueprint = readBlueprint(realBlueprintPath);
const missingTitles = MINT_DECLARED_ASSET_LIMIT_BLUEPRINT_TITLES.filter(
  (title) =>
    !blueprint.validators.some((validator) => validator.title === title),
);

describe("mintDeclaredAssetLimit signed publication fit", () => {
  /**
   * Discovery gate (§14). This file used to hang off `describe.runIf`, so a
   * blueprint that no longer carried the family reported a clean pass instead
   * of an unpublishable deployment. The family's presence is now a required
   * result of its own.
   */
  it("finds every declared mintDeclaredAssetLimit validator in the deployed blueprint", () => {
    expect(missingTitles).toEqual([]);
  });

  it("publishes all four applied scripts below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness();
    const proofAddressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyMintDeclaredAssetLimitScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: proofAddressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    // Four is the deployment contract this file's name states: a short or
    // empty `steps` would otherwise walk the loop below too few times and
    // report a vacuous pass.
    expect(steps).toHaveLength(4);
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `mint declared asset limit step ${index + 1}`,
      });
      measuredFit.record(
        `step-${index + 1}`,
        published.publicationMeasurement,
        "publication",
      );
      console.info(
        `[mint-declared-asset-limit-publication] ${JSON.stringify({ step: index + 1, bytes: published.publicationMeasurement.completeSignedBytes, memory: published.publicationMeasurement.executionMemory.toString(), cpu: published.publicationMeasurement.executionSteps.toString() })}`,
      );
      expect(
        published.publicationMeasurement.completeSignedBytes,
        `step ${(index + 1).toString()} signed publication bytes`,
      ).toBeLessThanOrEqual(15_872);
    }
  }, 600_000);
});
