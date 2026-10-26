import { describe, expect, it } from "vitest";

import { makeCanonicalDecodabilityEmulatorHarness } from "./support/canonical-decodability-emulator.js";
import { publishPlainReferenceScriptUtxo } from "./support/submit-init-emulator-shared.js";

describe("canonicalDecodability signed publication fit", () => {
  it("publishes every step's reference script within the signed-transaction reserve", async () => {
    const harness = await makeCanonicalDecodabilityEmulatorHarness();
    for (const [index, step] of harness.canonicalDecodability.steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: step.spendingScript,
        label: `canonical-decodability step ${(index + 1).toString()}`,
      });
      const measurement = published.publicationMeasurement;
      console.info(
        `[canonical-decodability-publication] ${JSON.stringify({
          step: index + 1,
          scriptBytes: step.spendingScript.script.length / 2,
          signedBytes: measurement.completeSignedBytes,
        })}`,
      );
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(15_872);
    }
  }, 600_000);
});
