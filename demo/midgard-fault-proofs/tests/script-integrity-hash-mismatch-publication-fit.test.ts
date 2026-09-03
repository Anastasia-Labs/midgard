import { AddressData, addressDataFromBech32 } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  applyScriptIntegrityHashMismatchScripts,
  SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES,
} from "../src/script-integrity-hash-mismatch/contracts.js";
import {
  makeFaultProofEmulatorHarness,
  publishPlainReferenceScriptUtxo,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const blueprint = readBlueprint(realBlueprintPath);
const hasFamily = SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES.every(
  (title) =>
    blueprint.validators.some((validator) => validator.title === title),
);

describe.runIf(hasFamily)(
  "scriptIntegrityHashMismatch signed publication fit",
  () => {
    it("publishes all five fully applied scripts below the reliability reserve", async () => {
      const harness = await makeFaultProofEmulatorHarness();
      const addressData = await Effect.runPromise(
        addressDataFromBech32(
          harness.contracts.fraudProof.spendingScriptAddress,
        ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
      );
      const steps = applyScriptIntegrityHashMismatchScripts({
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
          label: `script integrity hash mismatch step ${index + 1}`,
        });
        sizes.push(published.publicationMeasurement.completeSignedBytes);
        expect(
          published.publicationMeasurement.completeSignedBytes,
        ).toBeLessThanOrEqual(15_872);
        expect(published.publicationMeasurement.l1ByteMargin).toBeGreaterThan(
          0,
        );
      }
      expect(sizes).toEqual([14968, 12093, 1879, 5677, 2271]);
    }, 600_000);
  },
);
