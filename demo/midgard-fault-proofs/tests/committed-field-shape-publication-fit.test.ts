import { describe, expect, it } from "vitest";

import { makeCommittedFieldShapeEmulatorHarness } from "./support/committed-field-shape-emulator.js";
import { publishPlainReferenceScriptUtxo } from "./support/submit-init-emulator-shared.js";

describe("committedFieldShape signed publication fit", () => {
  it("publishes every step's reference script within the signed-transaction reserve", async () => {
    const harness = await makeCommittedFieldShapeEmulatorHarness();
    for (const [index, step] of harness.committedFieldShape.steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.proverLucid,
        script: step.spendingScript,
        label: `committed-field-shape step ${(index + 1).toString()}`,
      });
      const measurement = published.publicationMeasurement;
      console.info(
        `[committed-field-shape-publication] ${JSON.stringify({
          step: index + 1,
          scriptBytes: step.spendingScript.script.length / 2,
          signedBytes: measurement.completeSignedBytes,
        })}`,
      );
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(15_872);
    }
  }, 600_000);
});
