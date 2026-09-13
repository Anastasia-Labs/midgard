import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";

describe("field-preimage-length-mismatch real publication fit", () => {
  it("publishes every applied physical validator under Van Rossem limits", async () => {
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: {
        realFieldPreimageLengthMismatch: true,
        alwaysFraudProofCatalogue: true,
      },
    });
    const chain =
      harness.contracts.fraudProofContracts.fieldPreimageLengthMismatch;
    if (chain === undefined) throw new Error("real length chain is absent");
    const ledger = [];
    for (const [index, step] of chain.steps.entries()) {
      const capture = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script: step.spendingScript,
          label: `field-preimage-length-mismatch-step-${(index + 1).toString()}`,
        }),
      );
      const measurement = capture.measurement;
      ledger.push({
        step: index + 1,
        signedBytes: measurement.completeSignedBytes,
        byteMargin:
          PROTOCOL_PARAMETERS_DEFAULT.maxTxSize -
          measurement.completeSignedBytes,
        executionMemory: measurement.executionMemory.toString(),
        executionSteps: measurement.executionSteps.toString(),
      });
      expect(measurement.completeSignedBytes).toBeLessThanOrEqual(
        PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
      );
      expect(measurement.l1ByteMargin).toBeGreaterThan(0);
    }
    console.info(
      `[field-preimage-length-fit-ledger] ${JSON.stringify(ledger)}`,
    );
  }, 120_000);
});
