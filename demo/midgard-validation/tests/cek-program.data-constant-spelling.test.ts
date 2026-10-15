import { hashMidgardCekMachineState } from "@al-ft/midgard-core";
import { DataB, DataConstr, DataI } from "@harmoniclabs/plutus-data";
import {
  Application,
  Builtin,
  type ConstType,
  ConstTyTag,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  type UPLCTerm,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import { decodeMidgardCekConstantWitness } from "../src/cek-constant.js";
import { hashMidgardCekConstantWitness } from "../src/cek-constant.semantic-uplc-constant.js";
import { executeMidgardCekStructuralProgram } from "../src/cek-executor.js";
import {
  MidgardCekErrorCodes,
  verifyMidgardCekCoreStep,
} from "../src/cek-machine.js";
import {
  CardanoFlatRefusal,
  encodeMidgardCekCardanoFlatProgram,
} from "../src/cek-program.cardano-flat.js";
import {
  buildMidgardCanonicalCekProgram,
  buildMidgardCanonicalScriptArtifact,
} from "../src/cek-program.js";
import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import {
  DATA_DECODER_ACCEPTED,
  DATA_DECODER_REFUSED,
} from "./cek-program.data-constant-spelling.vectors.js";

/**
 * The Cardano ledger reads a Data constant inside a script's Flat bytes with
 * its Plutus Data decoder, which accepts several spellings of one value. A
 * script whose flat uses any of them is the same program as its canonical
 * twin, so it must build the same envelope and prove the same execution.
 */

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");
const bytes = (value: string): Buffer => Buffer.from(value, "hex");

/** Writes `(program 1.1.0 term)` with each Data constant spelled as given. */
class SpelledFlatEncoder extends UPLCEncoder {
  constructor(private readonly spellings: Uint8Array[]) {
    super();
  }

  override encodeConstValue(type: ConstType, value: unknown): void {
    if (type[0] === ConstTyTag.data) {
      const spelling = this.spellings.shift();
      if (spelling === undefined) throw new Error("missing Data spelling");
      this.encodeByteString(spelling);
      return;
    }
    super.encodeConstValue(type, value);
  }
}

const spelledFlat = (term: UPLCTerm, spellings: readonly string[]): Buffer =>
  Buffer.from(
    new SpelledFlatEncoder(spellings.map(bytes)).compile(
      new UPLCProgram([1, 1, 0], term),
    ),
  );

const canonicalFlat = (term: UPLCTerm): Buffer =>
  encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], term));

const serialiseDataTerm = (): UPLCTerm =>
  new Application(Builtin.serialiseData, UPLCConst.data(new DataI(0n)));

const equalsDataTerm = (): UPLCTerm =>
  new Application(
    new Application(Builtin.equalsData, UPLCConst.data(new DataI(0n))),
    UPLCConst.data(new DataI(0n)),
  );

const programView = (flat: Buffer) => {
  const program = buildMidgardCanonicalCekProgram(flat);
  return {
    envelopeHash: hex(program.envelopeHash),
    envelopeCbor: hex(program.envelopeCbor),
    material: [...program.material.values()]
      .map((entry) => `${entry.kind}:${hex(entry.root)}:${hex(entry.preimage)}`)
      .sort(),
    constantWitnesses: [...program.constantWitnesses.entries()]
      .map(([root, value]) =>
        value.kind === "constant"
          ? `${root}:${hex(value.witness.typeCbor)}:${hex(value.witness.payloadCbor)}`
          : `${root}:semantic`,
      )
      .sort(),
    credentialScriptHash: buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: flat,
    }).canonicalMidgardCredentialScriptHash,
    program,
  };
};

/** The detail of the `invalidData` refusal building the program raises. */
const invalidDataRefusal = (flat: Buffer): string => {
  try {
    buildMidgardCanonicalCekProgram(flat);
  } catch (error) {
    expect(error).toBeInstanceOf(CardanoFlatRefusal);
    expect((error as CardanoFlatRefusal).refusal).toBe("invalidData");
    return `${(error as CardanoFlatRefusal).bitOffset}: ${(error as CardanoFlatRefusal).detail}`;
  }
  throw new Error("the program was accepted");
};

/**
 * Runs the program in proof mode and checks every recorded step. Each program
 * returns its builtin's result rather than unit, so the machine charges the
 * whole run and then halts with the non-unit error a PlutusV3 script fails
 * with; the result is read from the last builtin step.
 */
const prove = (flat: Buffer) => {
  const { program } = programView(flat);
  const execution = executeMidgardCekStructuralProgram({
    root: program.envelope.termRoot,
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
    terminal: hex(hashMidgardCekMachineState(execution.terminalState)),
    cpu: execution.terminalState.cpu,
    memory: execution.terminalState.memory,
    steps: execution.steps.length,
    result,
  };
};

// [label, accepted non-canonical spelling, canonical spelling]
const SPELLINGS = [
  ["1 as a c2 bignum", "c24101", "01"],
  ["1 with a non-minimal head", "1801", "01"],
  ["-1 as a c3 bignum", "c34100", "20"],
  ["constructor 0 under tag 102", "d866820080", "d87980"],
  ["constructor 7 under tag 102", "d866820780", "d9050080"],
  [
    "2^64 with a zero-padded magnitude",
    "c24a00010000000000000000",
    "c249010000000000000000",
  ],
] as const;

describe("Data constants spelled as the Cardano ledger accepts them", () => {
  it.each(SPELLINGS)(
    "%s builds and proves exactly like its canonical twin",
    (_label, spelling, canonical) => {
      const spelled = spelledFlat(serialiseDataTerm(), [spelling]);
      const twin = spelledFlat(serialiseDataTerm(), [canonical]);
      // The twin is the canonical flat, and the spelled flat is a different
      // byte string.
      expect(twin).toEqual(
        canonicalFlat(
          new Application(
            Builtin.serialiseData,
            UPLCConst.data(plutusDataFromCborIterative(bytes(canonical))),
          ),
        ),
      );
      expect(spelled.equals(twin)).toBe(false);

      const spelledView = programView(spelled);
      const twinView = programView(twin);
      expect(spelledView.envelopeHash).toBe(twinView.envelopeHash);
      expect(spelledView.envelopeCbor).toBe(twinView.envelopeCbor);
      expect(spelledView.material).toEqual(twinView.material);
      expect(spelledView.constantWitnesses).toEqual(twinView.constantWitnesses);
      expect(spelledView.credentialScriptHash).toBe(
        twinView.credentialScriptHash,
      );

      const spelledRun = prove(spelled);
      const twinRun = prove(twin);
      expect(spelledRun).toEqual(twinRun);
      // serialiseData returns the canonical bytes, as on Cardano.
      expect(spelledRun.result).toBeInstanceOf(DataB);
      expect(hex((spelledRun.result as DataB).bytes)).toBe(canonical);
    },
  );

  it("equalsData compares a c3 bignum spelling of -1 with 20 as equal", () => {
    const run = prove(spelledFlat(equalsDataTerm(), ["c34100", "20"]));
    expect(run.result).toBeInstanceOf(DataConstr);
    expect((run.result as DataConstr).constr).toBe(1n);
    expect(run).toEqual(prove(spelledFlat(equalsDataTerm(), ["20", "20"])));
  });

  it.each([
    ["a text string", "6161", "invalid CBOR major type for Plutus Data"],
    [
      "tag 2 under a one-byte-extended head",
      "d80241" + "01",
      "unknown CBOR tag 2 in Plutus Data",
    ],
    [
      "a definite byte string over 64 bytes",
      "5841" + "00".repeat(65),
      "CBOR Plutus Data byte string exceeds 64 bytes",
    ],
    ["trailing bytes", "0100", "trailing bytes after CBOR Plutus Data"],
  ])(
    "refuses %s as the Plutus Data decoder does",
    (_label, refused, detail) => {
      expect(
        invalidDataRefusal(spelledFlat(serialiseDataTerm(), [refused])),
      ).toBe(`49: ${detail}`);
    },
  );

  it("refuses a constant witness that carries a non-canonical spelling", () => {
    const spelled = programView(spelledFlat(serialiseDataTerm(), ["c24101"]));
    const [constantRoot, witness] = [
      ...spelled.program.constantWitnesses.entries(),
    ].find(([, value]) => value.kind === "constant")!;
    if (witness.kind !== "constant") throw new Error("unreachable");
    expect(hex(witness.witness.typeCbor)).toBe("9f08ff");
    expect(hex(witness.witness.payloadCbor)).toBe("01");
    // The on-chain witness check pins the same root
    // (cek-constant-v1.test.ak, data_one_root).
    expect(constantRoot).toBe(
      "e057a14e2548433aa820c2162b36ac98168db657696f4a94fc20b450cc533a1b",
    );
    expect(hex(hashMidgardCekConstantWitness(witness.witness))).toBe(
      constantRoot,
    );
    for (const payload of ["c24101", "1801"]) {
      expect(() =>
        hashMidgardCekConstantWitness({
          typeCbor: witness.witness.typeCbor,
          payloadCbor: bytes(payload),
        }),
      ).toThrow(/not canonical/);
    }
  });

  it("commits a padded 2^64 to its canonical root", () => {
    const spelled = programView(
      spelledFlat(serialiseDataTerm(), ["c24a00010000000000000000"]),
    );
    const [constantRoot, witness] = [
      ...spelled.program.constantWitnesses.entries(),
    ].find(([, value]) => value.kind === "constant")!;
    if (witness.kind !== "constant") throw new Error("unreachable");
    expect(hex(witness.witness.payloadCbor)).toBe("c249010000000000000000");
    // The on-chain witness check pins the same root
    // (cek-constant-v1.test.ak, data_two_pow_64_root).
    expect(constantRoot).toBe(
      "30f014bc9812674691ef17db74211c691c1aaa0038cda14ef8fda815a6b49a84",
    );
  });

  it("admits a constant over the 9,215-byte bound only through padding", () => {
    const count = 1_100;
    const padded = `9f${"1b0000000000000001".repeat(count)}ff`;
    const canonical = `9f${"01".repeat(count)}ff`;
    expect(padded.length / 2).toBeGreaterThan(9_215);
    expect(canonical.length / 2).toBeLessThanOrEqual(9_215);
    const spelled = spelledFlat(serialiseDataTerm(), [padded]);
    const twin = spelledFlat(serialiseDataTerm(), [canonical]);
    expect(programView(spelled).envelopeHash).toBe(
      programView(twin).envelopeHash,
    );
    const spelledRun = prove(spelled);
    expect(spelledRun.terminal).toBe(prove(twin).terminal);
    expect(spelledRun.cpu).toBe(prove(twin).cpu);
  });
});

/**
 * Programs the Plutus V3 evaluator (plutus-core 1.71, protocol version 11) ran
 * through `deserialiseScript` on the raw Flat bytes below, with its result and
 * budget. The budget includes the 100/100 start-up charge, which the Midgard
 * machine charges too.
 */
// [label, raw Flat program, Cardano result, Cardano cpu/mem]
const CARDANO_RUNS = [
  [
    "serialiseData of tag 102 index 0",
    "0101003766980105d8668200800001",
    "(con bytestring #d87980)",
    "1856854/408",
  ],
  [
    "serialiseData of tag 102 index 6",
    "0101003766980105d8668206800001",
    "(con bytestring #d87f80)",
    "1856854/408",
  ],
  [
    "serialiseData of tag 102 index 127",
    "0101003766980106d86682187f800001",
    "(con bytestring #d9057880)",
    "1856854/408",
  ],
  [
    "serialiseData of bignum 1",
    "0101003766980103c241010001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of padded bignum 1",
    "0101003766980104c24200010001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of empty bignum 0",
    "0101003766980102c2400001",
    "(con bytestring #00)",
    "2070166/410",
  ],
  [
    "serialiseData of negative bignum -1",
    "0101003766980103c341000001",
    "(con bytestring #20)",
    "2070166/410",
  ],
  [
    "serialiseData of empty negative bignum -1",
    "0101003766980102c3400001",
    "(con bytestring #20)",
    "2070166/410",
  ],
  [
    "serialiseData of integer 1",
    "0101003766980101010001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of byte string 01",
    "010100376698010241010001",
    "(con bytestring #4101)",
    "2070166/410",
  ],
  [
    "serialiseData of an indefinite map",
    "0101003766980104bf0102ff0001",
    "(con bytestring #a10102)",
    "3989974/428",
  ],
  [
    "equalsData of bignum -1 and -1",
    "0101003375e98103c34100004c0101200001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of empty bignum -1 and -1",
    "0101003375e98102c340004c0101200001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of bignum -6 and -5",
    "0101003375e98103c34105004c0101240001",
    "(con bool False)",
    "1114643/601",
  ],
  [
    "equalsData of bignum -6 and -6",
    "0101003375e98103c34105004c0101250001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of padded bignum 1 and 1",
    "0101003375e98104c2420001004c0101010001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of tag 102 index 0 and tag 121",
    "0101003375e98105d866820080004c0103d879800001",
    "(con bool True)",
    "1087364/601",
  ],
  [
    "unIData of empty negative bignum -1",
    "010100375a980102c3400001",
    "(con integer -1)",
    "68844/432",
  ],
  [
    "serialiseData of bignum 2^64",
    "010100376698010bc2490100000000000000000001",
    "(con bytestring #c249010000000000000000)",
    "2283478/412",
  ],
  [
    "serialiseData of padded bignum 2^64",
    "010100376698010cc24a000100000000000000000001",
    "(con bytestring #c249010000000000000000)",
    "2283478/412",
  ],
  [
    "serialiseData of bignum 2^64-1",
    "010100376698010ac248ffffffffffffffff0001",
    "(con bytestring #1bffffffffffffffff)",
    "2070166/410",
  ],
] as const;

const cardanoResult = (result: unknown): string => {
  if (result instanceof DataB) return `(con bytestring #${hex(result.bytes)})`;
  if (result instanceof DataI) return `(con integer ${result.int})`;
  if (result instanceof DataConstr && result.fields.length === 0)
    return `(con bool ${result.constr === 1n ? "True" : "False"})`;
  throw new Error("unexpected builtin result");
};

describe("raw Flat programs the Cardano evaluator accepted", () => {
  it.each(CARDANO_RUNS)(
    "%s gives Cardano's result and budget",
    (_label, flat, result, budget) => {
      const run = prove(bytes(flat));
      expect(cardanoResult(run.result)).toBe(result);
      expect(`${run.cpu}/${run.memory}`).toBe(budget);
    },
  );
});

describe("Data decoder boundaries the Cardano evaluator drew", () => {
  it.each(DATA_DECODER_ACCEPTED)(
    "%s gives Cardano's result and budget",
    (_label, flat, result, budget) => {
      const run = prove(bytes(flat));
      expect(cardanoResult(run.result)).toBe(result);
      expect(`${run.cpu}/${run.memory}`).toBe(budget);
    },
  );

  it.each(DATA_DECODER_REFUSED)(
    "%s is refused as Cardano refuses it",
    (_label, flat, _cardano, bitOffset, detail) => {
      expect(invalidDataRefusal(bytes(flat))).toBe(`${bitOffset}: ${detail}`);
    },
  );
});
