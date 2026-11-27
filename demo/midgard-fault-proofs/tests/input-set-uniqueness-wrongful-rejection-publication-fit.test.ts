import { describe, expect, it } from "vitest";

import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.publish-operator-lifecycle-reference-scripts.js";
import { makeInputSetUniquenessEmulatorHarness } from "./support/input-set-uniqueness-emulator.build-input-set-uniqueness-fixture.js";

describe("input-set-uniqueness direction-complete publication fit", () => {
  it("publishes every applied physical validator below the reliability reserve", async () => {
    const harness = await makeInputSetUniquenessEmulatorHarness();
    expect(harness.family.steps).toHaveLength(4);
    for (const [index, step] of harness.family.steps.entries()) {
      const published = await publishPlainReferenceScriptUtxo({
        lucid: harness.funderLucid,
        script: step.spendingScript,
        label: `input-set-uniqueness-step-${(index + 1).toString()}`,
      });
      const measurement = published.publicationMeasurement;
      console.info(
        `[input-set-uniqueness-publication] ${JSON.stringify({
          step: index + 1,
          scriptHash: step.spendingScriptHash,
          bytes: measurement.completeSignedBytes,
          reserveMargin: 15_872 - measurement.completeSignedBytes,
          memory: measurement.executionMemory.toString(),
          cpu: measurement.executionSteps.toString(),
        })}`,
      );
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(15_872);
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 600_000);
});
