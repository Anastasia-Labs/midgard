import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile";
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

/**
 * The Van Rossem reliability reserve: the smallest L1 `max_tx_size` Midgard
 * supports less the 512-byte publication reliability reserve. Derived from the
 * consensus profile, not transcribed.
 */
const RELIABILITY_RESERVE_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes -
  MIDGARD_ENVELOPE_MEASUREMENTS.proofItemEnvelopeReliabilityReserveBytes;

const blueprint = readBlueprint(realBlueprintPath);

/**
 * Fail-closed precondition (test-quality rule 14). The family's five step
 * validators are part of the shipped blueprint; a blueprint that does not
 * carry them cannot give this suite's claim any evidence, so the file fails
 * loudly instead of reporting a silent pass through `describe.runIf`.
 */
const missingTitles = SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES.filter(
  (title) =>
    !blueprint.validators.some((validator) => validator.title === title),
);
if (missingTitles.length > 0) {
  throw new Error(
    `blueprint at ${realBlueprintPath} is missing the scriptIntegrityHashMismatch validators ${missingTitles.join(", ")}; rebuild it with the pinned Aiken fork before running this suite`,
  );
}

describe("scriptIntegrityHashMismatch signed publication fit", () => {
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
    expect(steps).toHaveLength(
      SCRIPT_INTEGRITY_HASH_MISMATCH_BLUEPRINT_TITLES.length,
    );
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `script integrity hash mismatch step ${index + 1}`,
      });
      expect(
        published.publicationMeasurement.completeSignedBytes,
        `step ${String(index + 1)}`,
      ).toBeLessThanOrEqual(RELIABILITY_RESERVE_BYTES);
      expect(
        published.publicationMeasurement.l1ByteMargin,
        `step ${String(index + 1)}`,
      ).toBeGreaterThan(0);
    }
  }, 600_000);
});
