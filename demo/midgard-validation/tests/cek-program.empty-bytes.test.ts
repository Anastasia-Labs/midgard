import { DataB, DataConstr, DataI } from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekPlutusData,
} from "../src/cek-constant.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import {
  MidgardCekErrorCodes,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import { CardanoFlatRefusal } from "../src/cek-program.cardano-flat.js";
import {
  midgardEnvelopeDataFromCborIterative,
  plutusDataFromCborIterative,
} from "../src/plutus-data-iterative.decode.js";
import {
  EMPTY_BYTES_CONSTANTS,
  EMPTY_BYTES_REFUSED,
  EMPTY_BYTES_RUNS,
  EMPTY_BYTES_STRUCTURED_RUNS,
} from "./cek-program.empty-bytes.vectors.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

/**
 * Data that is an empty byte string, or ends in one (as the last list item,
 * constructor field or map value), against the Cardano evaluator's verdicts
 * in `cek-program.empty-bytes.vectors.ts`: both Data readers, the program
 * builder's constant witnesses and the structural executor's results and
 * budgets.
 */

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");
const bytes = (value: string): Buffer => Buffer.from(value, "hex");

const programView = (flat: Buffer) => {
  const program = buildCekProgramFromRawFlat(flat);
  return {
    constantWitnesses: [...program.constantWitnesses.entries()]
      .map(([root, value]) =>
        value.kind === "constant"
          ? `${root}:${hex(value.witness.typeCbor)}:${hex(value.witness.payloadCbor)}`
          : `${root}:semantic`,
      )
      .sort(),
    program,
  };
};

/** The detail of the `invalidData` refusal building the program raises. */
const invalidDataRefusal = (flat: Buffer): string => {
  try {
    buildCekProgramFromRawFlat(flat);
  } catch (error) {
    expect(error).toBeInstanceOf(CardanoFlatRefusal);
    expect((error as CardanoFlatRefusal).refusal).toBe("invalidData");
    return `${(error as CardanoFlatRefusal).bitOffset}: ${(error as CardanoFlatRefusal).detail}`;
  }
  throw new Error("the program was accepted");
};

/**
 * Runs the program in proof mode and checks every recorded step. A program
 * returning its builtin's result rather than unit halts with the non-unit
 * error after the whole run is charged; the result is read from the last
 * builtin step.
 */
const prove = (flat: Buffer) => {
  const { program } = programView(flat);
  const execution = executeMidgardCekStructuralProgram({
    root: program.termRoot,
    material: program.material.values(),
    constantWitnesses: program.constantWitnesses,
    maxSteps: 64,
  });
  expect(execution.terminalState.mode).toBe("haltError");
  expect(execution.terminalState.auxiliary).toBe(
    MidgardCekErrorCodes.NonUnitHalt,
  );
  expect(
    execution.steps.every((step) =>
      verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
    ),
  ).toBe(true);
  const builtinStep = [...execution.steps]
    .reverse()
    .find(
      (step) =>
        step.witness.kind === "executeBuiltinDirect" ||
        step.witness.kind === "executeBuiltinSemantic",
    );
  let result: unknown;
  if (
    builtinStep !== undefined &&
    (builtinStep.witness.kind === "executeBuiltinDirect" ||
      builtinStep.witness.kind === "executeBuiltinSemantic") &&
    builtinStep.witness.result.kind === "constant"
  ) {
    result = decodeMidgardCekConstantWitness(
      builtinStep.witness.result.witness,
    ).payload;
  }
  return {
    cpu: execution.terminalState.cpu,
    memory: execution.terminalState.memory,
    result,
  };
};

const cardanoResult = (result: unknown): string => {
  if (result instanceof DataB) return `(con bytestring #${hex(result.bytes)})`;
  if (result instanceof DataI) return `(con integer ${result.int})`;
  if (result instanceof DataConstr && result.fields.length === 0)
    return `(con bool ${result.constr === 1n ? "True" : "False"})`;
  throw new Error("unexpected builtin result");
};

// [payload, its canonical serialisation], the second as Cardano's
// serialiseData returns it for the payload read as a Data constant
// (cek-program.empty-bytes.vectors.ts).
const EMPTY_BYTES_PAYLOADS = [
  ["40", "40"],
  ["8140", "9f40ff"],
  ["824040", "9f4040ff"],
  ["d87a8140", "d87a9f40ff"],
  ["d8799f40ff", "d8799f40ff"],
  ["a10040", "a10040"],
  ["a14040", "a14040"],
  ["824000", "9f4000ff"],
  ["82405fff", "9f4040ff"],
  ["5fff", "40"],
  ["815fff", "9f40ff"],
] as const;

describe("Data that is, or ends in, an empty byte string", () => {
  it.each(EMPTY_BYTES_PAYLOADS)(
    "both Data readers read %s to Cardano's value",
    (payload, canonical) => {
      for (const read of [
        plutusDataFromCborIterative,
        midgardEnvelopeDataFromCborIterative,
      ]) {
        expect(hex(encodeMidgardCekPlutusData(read(bytes(payload))))).toBe(
          canonical,
        );
      }
    },
  );

  it.each(["41", "8141", "a10041", "d87a8141"])(
    "both Data readers refuse %s, whose last byte string head claims a missing byte",
    (payload) => {
      for (const read of [
        plutusDataFromCborIterative,
        midgardEnvelopeDataFromCborIterative,
      ]) {
        expect(() => read(bytes(payload))).toThrow(
          "CBOR Plutus Data ends before its item does",
        );
      }
    },
  );

  it.each(EMPTY_BYTES_CONSTANTS)(
    "the constant %s commits its canonical payload at Cardano's budget",
    (_label, flat, payload, budget) => {
      const { constantWitnesses } = programView(bytes(flat));
      expect(constantWitnesses).toHaveLength(1);
      expect(constantWitnesses[0]!.split(":")[2]).toBe(payload);
      const run = prove(bytes(flat));
      expect(run.result).toBeUndefined();
      expect(`${run.cpu}/${run.memory}`).toBe(budget);
    },
  );

  it.each(EMPTY_BYTES_RUNS)(
    "%s gives Cardano's result and budget",
    (_label, flat, result, budget) => {
      const run = prove(bytes(flat));
      expect(cardanoResult(run.result)).toBe(result);
      expect(`${run.cpu}/${run.memory}`).toBe(budget);
    },
  );

  it.each(EMPTY_BYTES_STRUCTURED_RUNS)(
    "%s gives Cardano's result and budget",
    (_label, flat, _cardano, result, budget) => {
      const { program } = programView(bytes(flat));
      const execution = executeMidgardCekStructuralProgram({
        root: program.termRoot,
        material: program.material.values(),
        constantWitnesses: program.constantWitnesses,
        maxSteps: 64,
      });
      expect(
        execution.steps.every((step) =>
          verifyMidgardCekCoreStep(step.pre, step.post, step.witness),
        ),
      ).toBe(true);
      const builtinStep = execution.steps.find(
        (step) =>
          step.witness.kind === "executeBuiltinDirect" ||
          step.witness.kind === "executeBuiltinSemantic",
      );
      if (result === null) {
        expect(builtinStep).toBeUndefined();
      } else {
        if (
          builtinStep === undefined ||
          (builtinStep.witness.kind !== "executeBuiltinDirect" &&
            builtinStep.witness.kind !== "executeBuiltinSemantic") ||
          builtinStep.witness.result.kind !== "constant"
        ) {
          throw new Error("the builtin did not return a constant");
        }
        const witness = builtinStep.witness.result.witness;
        expect(`${hex(witness.typeCbor)}:${hex(witness.payloadCbor)}`).toBe(
          result,
        );
      }
      expect(
        `${execution.terminalState.cpu}/${execution.terminalState.memory}`,
      ).toBe(budget);
    },
  );

  it.each(EMPTY_BYTES_REFUSED)(
    "%s is refused as Cardano refuses it",
    (_label, flat, _cardano, bitOffset, detail) => {
      expect(invalidDataRefusal(bytes(flat))).toBe(`${bitOffset}: ${detail}`);
    },
  );
});
