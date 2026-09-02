import { describe, expect, it } from "vitest";

import { makeFaultProofEmulatorHarnessV1 } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";

describe("field-item-width-illegal real publication fit", () => {
  it("publishes every applied physical validator under Van Rossem limits", async () => {
    const harness = await makeFaultProofEmulatorHarnessV1({
      contractOptions: {
        realFieldItemWidthIllegal: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const chain = harness.contracts.fraudProofContracts.fieldItemWidthIllegal;
    for (const [index, step] of chain.steps.entries()) {
      const capture = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `field-item-width-illegal-step-${(index + 1).toString()}`,
        }),
      );
      expect(capture.measurement.completeSignedBytes).toBeLessThanOrEqual(
        16_384,
      );
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
    }
  }, 120_000);
});
