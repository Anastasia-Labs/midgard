import { decodeSingleCbor } from "@al-ft/midgard-core/codec";
import { expect } from "vitest";

import type { DeterministicValidationMachineTrace } from "../src/index.js";
import {
  type CountedStep,
  expectRowVerdictsTrace,
  type Row,
  stepVector,
} from "./emission-order.support.js";

/**
 * The counted steps of the emission-order rows: steps inside a row's
 * ValueAndMint walk whose successor's asset entry count the row pins.
 */

type Witness = DeterministicValidationMachineTrace["witnesses"][number];

/** The witness is the ValueAndMint step at `at`. */
const isStepAt = (at: CountedStep["at"], witness: Witness): boolean => {
  const auxiliary = witness.auxiliary;
  if (witness.phase !== "valueAndMint" || auxiliary === null) return false;
  if ("referenceInput" in at)
    return (
      auxiliary.kind === "resolvedInputReplay" &&
      auxiliary.sourceKind === "reference" &&
      auxiliary.itemIndex === at.referenceInput
    );
  if ("spendInput" in at)
    return (
      auxiliary.kind === "valueInputAsset" &&
      auxiliary.itemIndex === at.spendInput &&
      auxiliary.assetIndex === at.asset
    );
  if ("output" in at)
    return (
      auxiliary.kind === "valueOutputAsset" &&
      auxiliary.outputIndex === at.output &&
      auxiliary.assetIndex === at.asset
    );
  return auxiliary.kind === "valueMintAsset" && auxiliary.mintIndex === at.mint;
};

/** The asset entry count of a ValueAndMint work witness's accumulator. */
const assetEntryCount = (work: Buffer): bigint => {
  const control = decodeSingleCbor(work) as readonly unknown[];
  const accumulator = decodeSingleCbor(
    control[10] as Uint8Array,
  ) as readonly unknown[];
  return BigInt(accumulator[2] as number | bigint);
};

/**
 * A counted step as on-chain one-step input, with its successor's work
 * witness; the trace's successor holds the step's `entryCount`.
 */
export const countedStepVector = (
  trace: DeterministicValidationMachineTrace,
  step: CountedStep,
) => {
  const matches = trace.witnesses.flatMap((witness, index) =>
    isStepAt(step.at, witness) ? [index] : [],
  );
  expect(matches, step.name).toHaveLength(1);
  const stateIndex = matches[0]!;
  const successor = trace.witnesses[stateIndex + 1]!;
  expect(successor.phase, step.name).toBe("valueAndMint");
  expect(assetEntryCount(successor.cbor), step.name).toBe(
    BigInt(step.entryCount),
  );
  return {
    ...stepVector(trace, stateIndex),
    successorWork: successor.cbor.toString("hex"),
  };
};

/**
 * The producer's verdict, then the trace's, then the stopping step's, then
 * the asset entry count after each of the row's counted steps.
 */
export const expectCountedRowVerdicts = async (row: Row): Promise<void> => {
  const trace = await expectRowVerdictsTrace(row);
  for (const step of row.countedSteps ?? []) countedStepVector(trace, step);
};
