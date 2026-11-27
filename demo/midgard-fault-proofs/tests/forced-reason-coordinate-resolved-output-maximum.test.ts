import {
  encodeMidgardFieldPreimage,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_CONSENSUS_LIMITS,
} from "@al-ft/midgard-core";
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
  outRefOf,
} from "./support/forced-reason-resolved-output-coordinate.js";

/**
 * A forced InputSpentOutputNonCanonical coordinate past the last input of a
 * field at its consensus byte cap is closed by counting that field, and one
 * at a negative position reads no field; both convict within the per-step
 * execution budget.
 */

const cases = [
  {
    field: "spend",
    sourceKind: 0,
    capBytes: MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes,
    nativeTx: (inputs: readonly Buffer[]) =>
      makeNativeTx({ spendInputCbors: inputs, fee: 9n, outputCbors: [] }),
  },
  {
    field: "reference",
    sourceKind: 1,
    capBytes: MIDGARD_CONSENSUS_LIMITS.maxReferenceInputsPreimageBytes,
    nativeTx: (inputs: readonly Buffer[]) => {
      const base = makeNativeTx({
        spendInputCbors: [outRefOf("56".repeat(32))],
        fee: 9n,
        outputCbors: [],
      });
      return materializeMidgardNativeTxFromCanonical({
        version: base.version,
        validity: base.validity,
        body: {
          ...base.body,
          referenceInputsPreimageCbor: encodeMidgardFieldPreimage(inputs),
        },
        witnessSet: base.witnessSet,
      });
    },
  },
] as const;

describe("forced InputSpentOutputNonCanonical coordinate at the consensus maximum", () => {
  it.each([
    { ...cases[0], position: "past the last", past: true },
    { ...cases[0], position: "at a negative position", past: false },
    { ...cases[1], position: "past the last", past: true },
  ])(
    "convicts a coordinate $position of the most $field inputs the field holds",
    async ({ sourceKind, capBytes, nativeTx, past }) => {
      const inputs = maxShapeInputs(capBytes);
      expect(encodeMidgardFieldPreimage(inputs).length).toBeLessThanOrEqual(
        capBytes,
      );
      const inputIndex = past ? inputs.length : -1;
      const scenario = await commitForcedCoordinate(nativeTx(inputs), {
        sourceKind,
        inputIndex,
      });
      const evidence = absentEvidenceOf(scenario);
      expect(evidence.namesInputField).toBe(past);
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
        `max-shape absent input (${sourceKind.toString()}, ${inputIndex.toString()}) over ${inputs.length.toString()} inputs: step-02 mem=${opened.measurement.executionMemory.toString()} cpu=${opened.measurement.executionSteps.toString()}; step-05 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
      );
    },
    1_800_000,
  );
});
