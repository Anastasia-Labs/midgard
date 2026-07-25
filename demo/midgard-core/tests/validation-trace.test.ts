import { describe, expect, it } from "vitest";

import {
  buildMidgardValidationTraceTree,
  decodeMidgardValidationMachineStateV1,
  decodeMidgardValidationTraceDescriptorV1,
  encodeCbor,
  encodeMidgardValidationMachineStateV1,
  encodeMidgardValidationTraceDescriptorV1,
  hashMidgardValidationLedgerDeltaOperationV1,
  hashMidgardValidationLedgerDeltaV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  hashMidgardValidationWorkWitnessV1,
  MIDGARD_VALIDATION_MACHINE_V1_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationMachineStateV1,
  validationTraceDepthForStepCount,
  verifyMidgardValidationTraceProofV1,
} from "../src/index.js";

const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

const state = (
  overrides: Partial<MidgardValidationMachineStateV1> = {},
): MidgardValidationMachineStateV1 => ({
  machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
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
  ledgerDeltaRoot: hash(7),
  ...overrides,
});

describe("validation trace commitments", () => {
  it("matches Aiken's long-byte witness serialization exactly", () => {
    expect(
      hashMidgardValidationWorkWitnessV1({
        phase: "cek",
        programCounter: 17,
        witnessCbor: Buffer.from("820142abcd", "hex"),
      }).toString("hex"),
    ).toBe(
      "36d4e5e57f9cbcca2fc621a5f9411251be1c31a39378089dff8d44e2caa8e2bc",
    );
    expect(
      hashMidgardValidationWorkWitnessV1({
        phase: "canonicalDecode",
        programCounter: 0,
        witnessCbor: Buffer.alloc(200, 0xab),
      }).toString("hex"),
    ).toBe(
      "c0e76d5f16d1f18c9e27ce0744c2e02e84a00ef55af81ce392137b2fdc9b8a53",
    );
  });

  it("matches the L1 ledger-delta operation frontier vectors", () => {
    const deletion = {
      type: "delete" as const,
      key: Buffer.from("010203", "hex"),
    };
    const insertion = {
      type: "insert" as const,
      key: Buffer.from("0405", "hex"),
      value: Buffer.from("060708", "hex"),
    };
    expect(
      hashMidgardValidationLedgerDeltaOperationV1(deletion).toString("hex"),
    ).toBe(
      "ec29850954b571d525f5aa50102eb7c52b3411c559ea671cfbc2b649772105ef",
    );
    expect(
      hashMidgardValidationLedgerDeltaOperationV1(insertion).toString("hex"),
    ).toBe(
      "d0db5df4d5d20483cae1bbfa97fc7dda0bd605684e9a0023211b822cb35cecc1",
    );
    expect(
      hashMidgardValidationLedgerDeltaV1([deletion, insertion]).toString(
        "hex",
      ),
    ).toBe(
      "f930a92b68f2c321c138796f4a73d3d3155f76c51617eba6212ca6d1d43f04c7",
    );
  });

  it("round-trips and hashes exact machine states", () => {
    const expected = state({
      phase: "cek",
      programCounter: 17,
      executionCpu: 123n,
      executionMemory: 45n,
    });
    const encoded = encodeMidgardValidationMachineStateV1(expected);
    expect(decodeMidgardValidationMachineStateV1(encoded)).toEqual(expected);
    expect(hashMidgardValidationMachineStateV1(expected)).toHaveLength(32);
    expect(
      hashMidgardValidationMachineStateV1({
        ...expected,
        programCounter: 18,
      }),
    ).not.toEqual(hashMidgardValidationMachineStateV1(expected));
  });

  it("commits every state, pads with the terminal state, and verifies paths", () => {
    const stateHashes = [0, 1, 2, 3, 4].map((index) =>
      hashMidgardValidationMachineStateV1(
        state({
          phase: index === 4 ? "terminal" : "canonicalDecode",
          programCounter: index,
          verdict: index === 4 ? "accepted" : "pending",
        }),
      ),
    );
    const tree = buildMidgardValidationTraceTree(stateHashes, "accepted");
    expect(tree.descriptor.stepCount).toBe(4);
    expect(tree.paddedLeafCount).toBe(8);
    expect(validationTraceDepthForStepCount(4)).toBe(3);
    expect(
      tree.proofs.every((proof) =>
        verifyMidgardValidationTraceProofV1({
          descriptor: tree.descriptor,
          proof,
        }),
      ),
    ).toBe(true);

    expect(
      verifyMidgardValidationTraceProofV1({
        descriptor: tree.descriptor,
        proof: { ...tree.proofs[2]!, stateHash: hash(99) },
      }),
    ).toBe(false);
    expect(
      verifyMidgardValidationTraceProofV1({
        descriptor: tree.descriptor,
        proof: {
          ...tree.proofs[2]!,
          siblings: tree.proofs[2]!.siblings.slice(1),
        },
      }),
    ).toBe(false);
  });

  it("round-trips exact terminal descriptors", () => {
    const rejectionCodeHash =
      hashMidgardValidationRejectionCodeV1("E_FEE_TOO_LOW");
    const tree = buildMidgardValidationTraceTree(
      [hash(10), hash(11)],
      "rejected",
      rejectionCodeHash,
    );
    expect(
      decodeMidgardValidationTraceDescriptorV1(
        encodeMidgardValidationTraceDescriptorV1(tree.descriptor),
      ),
    ).toEqual(tree.descriptor);
  });

  it("fails closed for unknown versions, phases, verdicts, and malformed roots", () => {
    const encodedState = [
      BigInt(MIDGARD_VALIDATION_MACHINE_V1_VERSION),
      hash(1),
      hash(2),
      hash(3),
      hash(4),
      0n,
      hash(5),
      255n,
      0n,
      hash(6),
      0n,
      0n,
      0n,
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      hash(7),
    ];
    expect(() =>
      decodeMidgardValidationMachineStateV1(encodeCbor(encodedState)),
    ).toThrow(/Unknown validation phase/u);

    const tree = buildMidgardValidationTraceTree([hash(1)], "accepted");
    const descriptor = [
      1n,
      BigInt(MIDGARD_VALIDATION_MACHINE_V1_VERSION),
      tree.descriptor.traceRoot,
      0n,
      tree.descriptor.initialStateHash,
      tree.descriptor.terminalStateHash,
      0n,
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ];
    expect(() =>
      decodeMidgardValidationTraceDescriptorV1(encodeCbor(descriptor)),
    ).toThrow(/verdict must be terminal/u);
    expect(() =>
      decodeMidgardValidationTraceDescriptorV1(
        encodeCbor([2n, ...descriptor.slice(1)]),
      ),
    ).toThrow(/Unsupported validation trace descriptor version/u);
    expect(() =>
      decodeMidgardValidationTraceDescriptorV1(
        encodeCbor([
          1n,
          BigInt(MIDGARD_VALIDATION_MACHINE_V1_VERSION),
          Buffer.alloc(31),
          0n,
          tree.descriptor.initialStateHash,
          tree.descriptor.terminalStateHash,
          1n,
          MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
        ]),
      ),
    ).toThrow(/32 bytes/u);
  });

  it("binds terminal rejection codes to rejected verdicts", () => {
    const rejectionCodeHash =
      hashMidgardValidationRejectionCodeV1("E_FAILED_SCRIPT");
    expect(rejectionCodeHash).toHaveLength(32);
    expect(() =>
      buildMidgardValidationTraceTree(
        [hash(1), hash(2)],
        "accepted",
        rejectionCodeHash,
      ),
    ).toThrow(/inconsistent/u);
    expect(() =>
      buildMidgardValidationTraceTree([hash(1), hash(2)], "rejected"),
    ).toThrow(/inconsistent/u);
  });
});
