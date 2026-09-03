import { describe, expect, it } from "vitest";

import {
  buildMidgardValidationTraceTree,
  decodeMidgardValidationMachineState,
  decodeMidgardValidationTraceDescriptor,
  encodeCbor,
  encodeMidgardValidationMachineState,
  encodeMidgardValidationTraceDescriptor,
  hashMidgardValidationLedgerDelta,
  hashMidgardValidationLedgerDeltaOperation,
  hashMidgardValidationMachineState,
  hashMidgardValidationRejectionCode,
  hashMidgardValidationWorkWitness,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
  type MidgardValidationMachineState,
  validationTraceDepthForStepCount,
  verifyMidgardValidationTraceProof,
} from "../src/index.js";

const hash = (byte: number): Buffer => Buffer.alloc(32, byte);

const state = (
  overrides: Partial<MidgardValidationMachineState> = {},
): MidgardValidationMachineState => ({
  machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
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
      hashMidgardValidationWorkWitness({
        phase: "cek",
        programCounter: 17,
        witnessCbor: Buffer.from("820142abcd", "hex"),
      }).toString("hex"),
    ).toBe("36d4e5e57f9cbcca2fc621a5f9411251be1c31a39378089dff8d44e2caa8e2bc");
    expect(
      hashMidgardValidationWorkWitness({
        phase: "canonicalDecode",
        programCounter: 0,
        witnessCbor: Buffer.alloc(200, 0xab),
      }).toString("hex"),
    ).toBe("c0e76d5f16d1f18c9e27ce0744c2e02e84a00ef55af81ce392137b2fdc9b8a53");
  });

  it("matches the L1 ledger-delta operation frontier vectors", () => {
    const proofDescriptor = {
      version: 1 as const,
      frameCount: 0,
      terminalCursor: 0,
      frontier: { count: 0, peaks: [] },
    };
    const deletion = {
      type: "delete" as const,
      key: Buffer.from("010203", "hex"),
      proofDescriptor,
    };
    const insertion = {
      type: "insert" as const,
      key: Buffer.from("0405", "hex"),
      value: Buffer.from("060708", "hex"),
      proofDescriptor,
    };
    expect(
      hashMidgardValidationLedgerDeltaOperation(deletion).toString("hex"),
    ).toBe("d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589");
    expect(
      hashMidgardValidationLedgerDeltaOperation(insertion).toString("hex"),
    ).toBe("f8bc7029f5f58f0436ebdf6cbbb85bd9adac05d5f6dc1b9238c8166a517aa8db");
    expect(
      hashMidgardValidationLedgerDelta([deletion, insertion]).toString("hex"),
    ).toBe("b6d017c71f3fc974f620b22764385bf9ad56ee5627009e57dbeb9418e486dcb2");
  });

  it("round-trips and hashes exact machine states", () => {
    const expected = state({
      sourceKind: "forced",
      phase: "cek",
      programCounter: 17,
      executionCpu: 123n,
      executionMemory: 45n,
    });
    const encoded = encodeMidgardValidationMachineState(expected);
    expect(encoded.toString("hex")).toBe(
      "8f015820010101010101010101010101010101010101010101010101010101010101010158200202020202020202020202020202020202020202020202020202020202020202582003030303030303030303030303030303030303030303030303030303030303035820040404040404040404040404040404040404040404040404040404040404040401582005050505050505050505050505050505050505050505050505050505050505050b1158200606060606060606060606060606060606060606060606060606060606060606187b182d005820000000000000000000000000000000000000000000000000000000000000000058200707070707070707070707070707070707070707070707070707070707070707",
    );
    expect(decodeMidgardValidationMachineState(encoded)).toEqual(expected);
    expect(hashMidgardValidationMachineState(expected).toString("hex")).toBe(
      "fa9598fae21355bd529770b1c2c750ace65d721ada641bec6bd5f87a22c18088",
    );
    expect(
      hashMidgardValidationMachineState({
        ...expected,
        programCounter: 18,
      }),
    ).not.toEqual(hashMidgardValidationMachineState(expected));
  });

  it("commits every state, pads with the terminal state, and verifies paths", () => {
    const stateHashes = [0, 1, 2, 3, 4].map((index) =>
      hashMidgardValidationMachineState(
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
        verifyMidgardValidationTraceProof({
          descriptor: tree.descriptor,
          proof,
        }),
      ),
    ).toBe(true);

    expect(
      verifyMidgardValidationTraceProof({
        descriptor: tree.descriptor,
        proof: { ...tree.proofs[2]!, stateHash: hash(99) },
      }),
    ).toBe(false);
    expect(
      verifyMidgardValidationTraceProof({
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
      hashMidgardValidationRejectionCode("E_FEE_TOO_LOW");
    const tree = buildMidgardValidationTraceTree(
      [hash(10), hash(11)],
      "rejected",
      rejectionCodeHash,
    );
    expect(
      decodeMidgardValidationTraceDescriptor(
        encodeMidgardValidationTraceDescriptor(tree.descriptor),
      ),
    ).toEqual(tree.descriptor);

    expect(
      encodeMidgardValidationTraceDescriptor({
        schemaVersion: MIDGARD_VALIDATION_TRACE_DESCRIPTOR_VERSION,
        machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
        traceRoot: Buffer.from(
          "c6760a9266746c67578026b6d44e533ae8390264d227a9649e6558a3d70970eb",
          "hex",
        ),
        stepCount: 1,
        initialStateHash: Buffer.from(
          "fa9598fae21355bd529770b1c2c750ace65d721ada641bec6bd5f87a22c18088",
          "hex",
        ),
        terminalStateHash: hash(9),
        verdict: "rejected",
        rejectionCodeHash: hash(8),
      }).toString("hex"),
    ).toBe(
      "8801015820c6760a9266746c67578026b6d44e533ae8390264d227a9649e6558a3d70970eb015820fa9598fae21355bd529770b1c2c750ace65d721ada641bec6bd5f87a22c18088582009090909090909090909090909090909090909090909090909090909090909090258200808080808080808080808080808080808080808080808080808080808080808",
    );
  });

  it("fails closed for unknown versions, phases, verdicts, and malformed roots", () => {
    const encodedState = [
      BigInt(MIDGARD_VALIDATION_MACHINE_VERSION),
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
      decodeMidgardValidationMachineState(encodeCbor(encodedState)),
    ).toThrow(/Unknown validation phase/u);
    expect(() =>
      decodeMidgardValidationMachineState(
        encodeCbor([2n, ...encodedState.slice(1)]),
      ),
    ).toThrow(/Unsupported validation machine version/u);
    expect(() =>
      decodeMidgardValidationMachineState(
        encodeCbor(encodedState.slice(0, -1)),
      ),
    ).toThrow(/exactly 15 fields/u);

    const tree = buildMidgardValidationTraceTree([hash(1)], "accepted");
    const descriptor = [
      1n,
      BigInt(MIDGARD_VALIDATION_MACHINE_VERSION),
      tree.descriptor.traceRoot,
      0n,
      tree.descriptor.initialStateHash,
      tree.descriptor.terminalStateHash,
      0n,
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ];
    expect(() =>
      decodeMidgardValidationTraceDescriptor(encodeCbor(descriptor)),
    ).toThrow(/verdict must be terminal/u);
    expect(() =>
      decodeMidgardValidationTraceDescriptor(
        encodeCbor([2n, ...descriptor.slice(1)]),
      ),
    ).toThrow(/Unsupported validation trace descriptor version/u);
    expect(() =>
      decodeMidgardValidationTraceDescriptor(
        encodeCbor([
          1n,
          BigInt(MIDGARD_VALIDATION_MACHINE_VERSION),
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
      hashMidgardValidationRejectionCode("E_FAILED_SCRIPT");
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
