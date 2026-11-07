import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyObserverOrderInvalidScripts,
  OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES,
} from "../src/observer-order-invalid/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const publicationReserveBytes = 15_872;
const expectedStepCount = 4;

describe("observerOrderInvalid signed publication fit", () => {
  it("publishes all four applied scripts below the reliability reserve", async () => {
    // Fail closed: a rebuilt blueprint that no longer carries the family must
    // report a failure, not a silently skipped suite (§14).
    expect(
      OBSERVER_ORDER_INVALID_BLUEPRINT_TITLES.filter(
        (title) =>
          !blueprint.validators.some((validator) => validator.title === title),
      ),
      "observerOrderInvalid validators missing from the rebuilt blueprint",
    ).toEqual([]);
    const harness = await makeFaultProofEmulatorHarness();
    const proofAddressData = await Effect.runPromise(
      addressDataFromBech32(
        harness.contracts.fraudProof.spendingScriptAddress,
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = applyObserverOrderInvalidScripts({
      blueprint,
      network: "Preprod",
      computationThreadPolicyId: harness.contracts.computationThread.policyId,
      fraudProofPolicyId: harness.contracts.fraudProof.policyId,
      fraudProofTokenAddressData: proofAddressData,
      fieldPreimageCertificatePolicyId:
        harness.contracts.fieldPreimageCertificate.policyId,
      hubOracleScriptHash: harness.contracts.hubOracle.spendingScriptHash,
    });
    expect(steps, "applied observerOrderInvalid steps").toHaveLength(
      expectedStepCount,
    );
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `observer order invalid asset limit step ${index + 1}`,
      });
      // The published UTxO must carry this step's applied script, not merely
      // some script ref.
      expect(
        published.utxo.scriptRef
          ? validatorToScriptHash(published.utxo.scriptRef)
          : null,
        `step ${(index + 1).toString()} published reference script identity`,
      ).toBe(step.spendingScriptHash);
      expect(
        published.publicationMeasurement.completeSignedBytes,
        `step ${(index + 1).toString()} signed publication bytes`,
      ).toBeLessThanOrEqual(publicationReserveBytes);
    }
  }, 600_000);
});
