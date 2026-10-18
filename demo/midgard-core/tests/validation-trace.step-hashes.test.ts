import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  encodeCbor,
  encodeCborArrayRaw,
  encodeMidgardValidationMachineState,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionReasonV1,
  hashMidgardValidationWorkWitness,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationMachineState,
  MidgardValidationPhase,
  type MidgardValidationPhaseName,
} from "../src/index.js";

/**
 * The work-witness commitment assembled from the generic codec: the witness
 * bytes wrapped as Plutus Data and re-serialised as Aiken serialises it.
 */
const genericWorkWitnessHash = (
  phase: MidgardValidationPhaseName,
  programCounter: number,
  witnessCbor: Uint8Array,
): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardValidationWorkWitnessV1", "utf8"),
        encodeCborArrayRaw([
          encodeCbor(BigInt(MidgardValidationPhase[phase])),
          encodeCbor(BigInt(programCounter)),
          Buffer.from(
            aikenSerialisedPlutusDataCborPreservingMapOrder(
              encodeCbor(Buffer.from(witnessCbor)).toString("hex"),
            ),
            "hex",
          ),
        ]),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");

const WITNESS_LENGTHS = [
  0, 1, 23, 24, 63, 64, 65, 127, 128, 129, 191, 192, 193, 255, 256, 1000, 4096,
  65_535, 65_536, 70_001,
];

const PROGRAM_COUNTERS = [
  0, 1, 23, 24, 255, 256, 65_535, 65_536, 0xffff_fffe, 0xffff_ffff,
];

const witness = (length: number): Buffer =>
  Buffer.from(
    Array.from({ length }, (_, index) => (index * 31 + length) & 0xff),
  );

describe("validation work-witness commitment", () => {
  it("equals the generic Data serialisation at every chunk and head boundary", () => {
    const phases = Object.keys(
      MidgardValidationPhase,
    ) as MidgardValidationPhaseName[];
    for (const length of WITNESS_LENGTHS) {
      for (const programCounter of PROGRAM_COUNTERS) {
        const phase = phases[(length + programCounter) % phases.length]!;
        const witnessCbor = witness(length);
        expect(
          hashMidgardValidationWorkWitness({
            phase,
            programCounter,
            witnessCbor,
          }).toString("hex"),
          `phase ${phase}, counter ${programCounter.toString()}, length ${length.toString()}`,
        ).toBe(genericWorkWitnessHash(phase, programCounter, witnessCbor));
      }
    }
    for (const phase of phases) {
      expect(
        hashMidgardValidationWorkWitness({
          phase,
          programCounter: 7,
          witnessCbor: witness(130),
        }).toString("hex"),
      ).toBe(genericWorkWitnessHash(phase, 7, witness(130)));
    }
  });

  it("hashes a witness given as a view into a larger buffer", () => {
    const backing = witness(400);
    const view = backing.subarray(37, 37 + 200);
    expect(
      hashMidgardValidationWorkWitness({
        phase: "cek",
        programCounter: 3,
        witnessCbor: new Uint8Array(view.buffer, view.byteOffset, view.length),
      }).toString("hex"),
    ).toBe(genericWorkWitnessHash("cek", 3, Buffer.from(view)));
  });

  it("refuses a program counter outside the step bound", () => {
    for (const programCounter of [-1, 0x1_0000_0000, 1.5]) {
      expect(() =>
        hashMidgardValidationWorkWitness({
          phase: "cek",
          programCounter,
          witnessCbor: witness(4),
        }),
      ).toThrow();
    }
  });
});

const genericStateHash = (state: MidgardValidationMachineState): string =>
  Buffer.from(
    blake2b(
      Buffer.concat([
        Buffer.from("MidgardValidationMachineStateV1", "utf8"),
        encodeMidgardValidationMachineState(state),
      ]),
      { dkLen: 32 },
    ),
  ).toString("hex");

const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

const REJECTION_HASH = hashMidgardValidationRejectionReasonV1({
  PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 1n },
});

const baseState: MidgardValidationMachineState = {
  machineVersion: 1,
  eventKeyHash: hash(1),
  transactionId: hash(2),
  transactionCommitment: hash(3),
  validationContextHash: hash(4),
  sourceKind: "normal",
  priorLedgerRoot: hash(5),
  phase: "canonicalDecode",
  programCounter: 0,
  workRoot: hash(6),
  executionCpu: 0n,
  executionMemory: 0n,
  verdict: "pending",
  rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
};

const BUDGETS = [
  0n,
  23n,
  24n,
  255n,
  256n,
  65_535n,
  65_536n,
  0xffff_ffffn,
  0x1_0000_0000n,
  0xffff_ffff_ffff_ffffn,
  0x1_0000_0000_0000_0000n,
  // Outside the unsigned range: the direct writer hands these to the generic
  // encoder, which encodes or refuses them.
  -1n,
  -25n,
];

describe("validation machine state commitment", () => {
  it("equals the generic state encoding, or refuses with it, across phases, verdicts and budgets", () => {
    const phases = Object.keys(
      MidgardValidationPhase,
    ) as MidgardValidationPhaseName[];
    const variants: MidgardValidationMachineState[] = [];
    for (const phase of phases) {
      variants.push({ ...baseState, phase, programCounter: phase.length });
    }
    for (const [index, cpu] of BUDGETS.entries()) {
      const memory = BUDGETS[BUDGETS.length - 1 - index]!;
      variants.push({
        ...baseState,
        executionCpu: cpu,
        executionMemory: memory,
        sourceKind: index % 2 === 0 ? "normal" : "forced",
      });
    }
    for (const programCounter of PROGRAM_COUNTERS) {
      variants.push({ ...baseState, programCounter });
    }
    variants.push(
      { ...baseState, verdict: "accepted", phase: "terminal" },
      {
        ...baseState,
        verdict: "rejected",
        phase: "terminal",
        rejectionCodeHash: REJECTION_HASH,
      },
    );
    for (const state of variants) {
      const label = JSON.stringify(state, (_, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      );
      let expected: string | null;
      try {
        expected = genericStateHash(state);
      } catch {
        expected = null;
      }
      if (expected === null) {
        expect(() => hashMidgardValidationMachineState(state), label).toThrow();
      } else {
        expect(
          hashMidgardValidationMachineState(state).toString("hex"),
          label,
        ).toBe(expected);
      }
    }
  });

  it("refuses the states the generic encoding refuses", () => {
    const refused: MidgardValidationMachineState[] = [
      { ...baseState, workRoot: Buffer.alloc(31) },
      { ...baseState, eventKeyHash: Buffer.alloc(33) },
      { ...baseState, programCounter: 0x1_0000_0000 },
      { ...baseState, programCounter: -1 },
      { ...baseState, verdict: "rejected" },
      { ...baseState, verdict: "accepted", rejectionCodeHash: REJECTION_HASH },
    ];
    for (const state of refused) {
      expect(() => encodeMidgardValidationMachineState(state)).toThrow();
      expect(() => hashMidgardValidationMachineState(state)).toThrow();
    }
  });
});
