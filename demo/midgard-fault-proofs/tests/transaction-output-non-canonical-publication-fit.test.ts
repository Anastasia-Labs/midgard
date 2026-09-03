import { describe, expect, it } from "vitest";

import { TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES } from "../src/transaction-output-non-canonical/contracts-v1.js";
import {
  applyCompiledScript,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";
import { makeSpendingValidator } from "./support/emulator/validators.js";

describe("transactionOutputNonCanonical real publication fit", () => {
  it("publishes every fully applied physical validator under ordinary limits", async () => {
    const blueprint = readBlueprint(realBlueprintPath);
    const h28 = "11".repeat(28);
    const scripts = Object.values(
      TRANSACTION_OUTPUT_NON_CANONICAL_BLUEPRINT_TITLES,
    ).map(
      (title, index) =>
        makeSpendingValidator(
          applyCompiledScript(
            blueprint,
            title,
            Array.from({ length: index === 2 ? 2 : 3 }, () => h28),
          ),
        ).spendingScript,
    );
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    for (const [index, script] of scripts.entries()) {
      const capture = await captureEmulatorSubmission(harness.emulator, () =>
        publishPlainReferenceScriptUtxo({
          lucid: harness.funderLucid,
          script,
          label: `transaction-output-non-canonical-step-${String(index + 1)}`,
        }),
      );
      expect(capture.measurement.completeSignedBytes).toBeLessThanOrEqual(
        16_384,
      );
      expect(capture.measurement.l1ByteMargin).toBeGreaterThan(0);
      if (process.env.MIDGARD_PRINT_FIT === "1") {
        console.info(
          JSON.stringify(
            { index: index + 1, ...capture.measurement },
            (_key, value: unknown) =>
              typeof value === "bigint" ? value.toString() : value,
          ),
        );
      }
    }
  }, 120_000);
});
