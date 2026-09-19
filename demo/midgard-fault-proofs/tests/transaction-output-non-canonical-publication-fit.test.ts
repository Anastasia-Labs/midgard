import {
  buildTransactionOutputNonCanonicalFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import { captureEmulatorSubmission } from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";

describe("transactionOutputNonCanonical real publication fit", () => {
  it("publishes every fully applied physical validator under ordinary limits", async () => {
    const blueprint = readBlueprint(realBlueprintPath);
    const {
      transactionOutputNonCanonical: { steps },
    } = await Effect.runPromise(
      buildTransactionOutputNonCanonicalFaultProofContracts({
        blueprint: parseFaultProofBlueprint(blueprint),
        network,
        hubOraclePolicyId: "55".repeat(28),
        fraudProofCataloguePolicyId: "66".repeat(28),
      }),
    );
    const harness = await makeFaultProofEmulatorHarness({
      contractOptions: { alwaysFraudProofCatalogue: true },
    });
    for (const [index, { spendingScript: script }] of steps.entries()) {
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
