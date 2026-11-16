import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import {
  absentEvidenceOf,
  commitForcedCoordinate,
  convictAbsent,
  maxShapeInputs,
} from "./support/forced-reason-resolved-output-coordinate.js";

/**
 * A forced InputSpentOutputNonCanonical coordinate past the last input of a
 * field at its consensus byte cap is closed by counting that field, and one
 * at a negative position reads no field; both convict within the per-step
 * execution budget.
 */

describe("forced InputSpentOutputNonCanonical coordinate at the consensus maximum", () => {
  it.each([
    { position: "past the last", past: true },
    { position: "at a negative position", past: false },
  ])(
    "convicts a coordinate $position of the most spend inputs field 0 holds",
    async ({ past }) => {
      const inputs = maxShapeInputs(
        MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes,
      );
      const inputIndex = past ? inputs.length : -1;
      const scenario = await commitForcedCoordinate(
        makeNativeTx({ spendInputCbors: inputs, fee: 9n, outputCbors: [] }),
        { sourceKind: 0, inputIndex },
      );
      const evidence = absentEvidenceOf(scenario);
      const { opened, minted } = await convictAbsent(scenario, evidence);
      // A negative position is absent without opening the field.
      expect(opened.result.carriageTier).toBe(past ? "Certified" : null);
      for (const { measurement } of [opened, minted]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent input ${inputIndex.toString()} (${inputs.length.toString()} spend inputs): step-02 mem=${opened.measurement.executionMemory.toString()} cpu=${opened.measurement.executionSteps.toString()}; step-05 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
      );
    },
    1_800_000,
  );
});
