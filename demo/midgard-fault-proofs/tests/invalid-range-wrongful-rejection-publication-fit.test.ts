import { describe, expect, it } from "vitest";

import {
  makeFaultProofEmulatorHarnessV1,
  publishPlainReferenceScriptUtxo,
} from "./support/submit-init-emulator-shared.js";

describe("invalidRange direction-complete signed publication fit", () => {
  it("publishes both applied scripts below the reliability reserve", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1({
      contractOptions: { realInvalidRange: true },
    });
    const steps = harness.contracts.fraudProofContracts.invalidRange.steps;
    for (const [index, step] of steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `invalid range direction complete step ${index + 1}`,
      });
      console.info(
        `[invalid-range-publication] ${JSON.stringify({ step: index + 1, scriptHash: step.spendingScriptHash, bytes: published.publicationMeasurement.completeSignedBytes, memory: published.publicationMeasurement.executionMemory.toString(), cpu: published.publicationMeasurement.executionSteps.toString() })}`,
      );
      expect(
        published.publicationMeasurement.completeSignedBytes,
      ).toBeLessThanOrEqual(15_872);
    }
  }, 600_000);
});
