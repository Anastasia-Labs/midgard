import { describe, expect, it } from "vitest";

import { CANONICAL_DATA_STEP_MEMORY_TARGET } from "../src/canonical-data-stream-plan.js";
import { planRedeemerData } from "../src/redeemer-canonicity/data-plan.js";
import { planResolvedOutputDatum } from "../src/resolved-output-non-canonical/datum-plan.js";
import { planTransactionOutputDatum } from "../src/transaction-output-non-canonical/datum-plan.js";
import {
  densePlutusData,
  type DensePlutusDataShape,
} from "./support/dense-plutus-data.js";
import {
  MAXIMUM_DATUM_BYTES,
  outputWithDatumBytes,
} from "./support/resolved-output-non-canonical-emulator.build-prior-ledger.js";
import { outputWithInlineDatum } from "./support/transaction-output-non-canonical-emulator.continue-raw.js";

/**
 * The step planner shared by the redeemer, resolved-output and output Data
 * steps: whatever the shape, no planned step's memory estimate exceeds the
 * per-step target, including steps whose nodes close containers of the
 * stored stack and so widen what the step reveals.
 */

const SHAPES = [
  "map-chain",
  "generic-constructors",
  "flat-integers",
  "chunked-bytes",
] as const satisfies readonly DensePlutusDataShape[];

const MAXIMUM_REDEEMER_DATA_BYTES = 32_743;
const MAXIMUM_OUTPUT_BYTES = 16_384;

/** A spend redeemer item carrying `data`, with a canonical envelope. */
const redeemerItem = (data: Buffer) =>
  Buffer.concat([
    Buffer.from("840000", "hex"),
    Buffer.from([0x59, data.length >> 8, data.length & 0xff]),
    data,
    Buffer.from("821a000f42401a3b9aca00", "hex"),
  ]);

const plans = (shape: DensePlutusDataShape) => {
  const resolved = outputWithDatumBytes(
    densePlutusData(shape, MAXIMUM_DATUM_BYTES),
  );
  const outputDatum = densePlutusData(shape, MAXIMUM_OUTPUT_BYTES - 45);
  const output = outputWithInlineDatum(outputDatum);
  return {
    redeemer: planRedeemerData(
      redeemerItem(densePlutusData(shape, MAXIMUM_REDEEMER_DATA_BYTES)),
    )!,
    resolvedOutput: planResolvedOutputDatum({
      output: resolved,
      datumOffset: resolved.length - MAXIMUM_DATUM_BYTES,
      datumLength: MAXIMUM_DATUM_BYTES,
    }),
    output: planTransactionOutputDatum({
      item: output,
      datumOffset: output.length - outputDatum.length,
      datumLength: outputDatum.length,
    }),
  };
};

describe("canonical Data step planner", () => {
  it.each(SHAPES)(
    "keeps every step of maximum %s data within the per-step memory target",
    (shape) => {
      for (const [family, plan] of Object.entries(plans(shape))) {
        expect(plan.canonical, family).toBe(true);
        expect(plan.steps.length, family).toBeGreaterThan(1);
        for (const step of plan.steps)
          expect(step.estimatedMemory, family).toBeLessThanOrEqual(
            CANONICAL_DATA_STEP_MEMORY_TARGET,
          );
      }
    },
    // Maximum-size shapes take seconds of pure planning; loaded CI shards
    // exceed the default 5 s.
    30_000,
  );
});
