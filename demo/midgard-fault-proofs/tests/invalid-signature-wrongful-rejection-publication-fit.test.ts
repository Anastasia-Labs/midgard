import { describe, expect, it } from "vitest";

import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";
describe("invalidSignature signed publication fit", () => {
  it("publishes the applied direction-complete chain below the reserve", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { realInvalidSignature: true },
    });
    for (const [
      index,
      step,
    ] of harness.contracts.fraudProofContracts.invalidSignature.steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: step.spendingScript,
        label: `invalid signature step ${index + 1}`,
      });
      const measurement = published.publicationMeasurement;
      expect(measurement.completeSignedBytes).toBeLessThan(15_872);
    }
  }, 600_000);
});
