import { readdirSync, readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  buildMidgardValidationLedgerDeltaFrontier,
  commitMidgardValidationMerkleFrontier,
  encodeCbor,
  hashMidgardValidationLedgerDelta,
  hashMidgardValidationLedgerDeltaOperation,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import type { ValidationMachineWorkWitness } from "../src/validation-machine/index.js";
import {
  encodeValidationAuxiliaryWitnessCbor,
  validationAuxiliaryWitnessData,
} from "../src/validation-machine-data.js";

type Auxiliary = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;

const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");
const h32 = (byte: number): Buffer => Buffer.alloc(32, byte);
const digest = (value: Uint8Array): string =>
  Buffer.from(blake2b(value, { dkLen: 32 })).toString("hex");

const emptyFrontier = {
  count: 0,
  peaks: [],
} as const;
const mutationStep = {
  unit: Buffer.alloc(28, 0x31),
  quantityDelta: 5n,
  oldDelta: null,
  preAssetRoot: h32(0x32),
  postAssetRoot: h32(0x33),
  proofCbor: bytes("80"),
  postSeenAssetCount: 1,
  postNonzeroAssetCount: 1,
} as const;
const descriptor = {
  version: 1,
  frameCount: 0,
  terminalCursor: 0,
  frontier: emptyFrontier,
} as const;
const foldControl = {
  nextFrameIndex: 0,
  expectedNextCursor: 0,
  includingRoot: h32(0x34),
  excludingRoot: h32(0x35),
} as const;
const mutation = {
  operation: { type: "delete", key: bytes("01") } as const,
  preRoot: h32(0x36),
  postRoot: h32(0x37),
  proofFoldTrace: {
    descriptor,
    frames: [],
    initial: foldControl,
    steps: [],
    terminal: foldControl,
  },
} as const;
const operationMembership = {
  frontier: emptyFrontier,
  leafIndex: 0,
  leafHash: h32(0x38),
  siblings: [],
} as const;
const chunkProof = {
  version: 1,
  fieldIndex: 5,
  itemIndex: 0,
  totalLength: 1,
  chunkIndex: 0,
  chunk: bytes("12"),
  frontier: emptyFrontier,
  siblings: [],
} as const;
const proofFrame = {
  version: 1,
  frameIndex: 0,
  cursor: 0,
  nextCursor: 1,
  step: { kind: "branch", skip: 0, neighbors: Buffer.alloc(0) },
} as const;

const auxiliary = (value: Auxiliary): Auxiliary => value;

const tailAuxiliaryVectors = [
  [
    24,
    11,
    auxiliary({
      kind: "valueInputAsset",
      sourceKind: "spend",
      key: bytes("01"),
      nextScheduleHash: h32(0x41),
      descriptorCbor: bytes("80"),
      assetIndex: 0,
      policyId: Buffer.alloc(28, 0x42),
      assetName: bytes("abcd"),
      quantity: 5n,
      assetFrontier: emptyFrontier,
      assetSiblings: [],
      mutationStep,
    }),
  ],
  [
    25,
    9,
    auxiliary({
      kind: "valueOutputAsset",
      outputIndex: 1,
      descriptorCbor: bytes("80"),
      assetIndex: 0,
      policyId: Buffer.alloc(28, 0x43),
      assetName: bytes("beef"),
      quantity: 7n,
      assetFrontier: emptyFrontier,
      assetSiblings: [],
      mutationStep,
    }),
  ],
  [
    26,
    6,
    auxiliary({
      kind: "valueMintAsset",
      mintIndex: 2,
      policyId: Buffer.alloc(28, 0x44),
      assetName: bytes("cafe"),
      quantity: -5n,
      siblings: [],
      mutationStep,
    }),
  ],
  [
    27,
    4,
    auxiliary({
      kind: "ledgerDeltaReplay",
      sourceKind: "reference",
      key: bytes("02"),
      nextScheduleHash: h32(0x45),
      value: bytes("03"),
    }),
  ],
  [
    28,
    3,
    auxiliary({
      kind: "ledgerDeltaOutput",
      outputIndex: 3,
      descriptorCbor: bytes("8100"),
      siblings: [h32(0x46)],
    }),
  ],
  [
    34,
    2,
    auxiliary({
      kind: "ledgerDeltaProofFrame",
      frame: proofFrame,
      siblings: [h32(0x47)],
    }),
  ],
  [
    35,
    4,
    auxiliary({
      kind: "ledgerDeltaOperation",
      operationKind: "delete",
      key: bytes("01"),
      value: Buffer.alloc(0),
      mutationStep: mutation,
      operationMembership,
    }),
  ],
  [
    38,
    3,
    auxiliary({
      kind: "valueOutputDescriptor",
      outputIndex: 4,
      descriptorCbor: bytes("8101"),
      siblings: [h32(0x48)],
    }),
  ],
  [
    39,
    2,
    auxiliary({
      kind: "mintFoldAsset",
      chunkProof,
      nextChunkProof: null,
    }),
  ],
] as const;

const expectedTailArities: ReadonlyMap<number, number> = new Map(
  tailAuxiliaryVectors.map(([tag, arity]) => [tag, arity]),
);

const decodeExactTailAuxiliary = (cborHex: string): Constr<Data> => {
  const decoded = Data.from(cborHex);
  if (!(decoded instanceof Constr)) {
    throw new Error("validation tail auxiliary must be a constructor");
  }
  const expectedArity = expectedTailArities.get(decoded.index);
  if (expectedArity === undefined) {
    throw new Error("validation tail auxiliary tag is not canonical V1");
  }
  if (decoded.fields.length !== expectedArity) {
    throw new Error("validation tail auxiliary arity is not canonical V1");
  }
  if (Data.to(decoded) !== cborHex) {
    throw new Error("validation tail auxiliary CBOR is not canonical");
  }
  return decoded;
};

const encodeFrontier = (
  peaks: readonly {
    readonly height: number;
    readonly hash: Uint8Array;
  }[],
): readonly (readonly [bigint, Buffer])[] =>
  peaks.map(({ height, hash }) => [BigInt(height), Buffer.from(hash)]);

const nativeControlCbor = encodeCbor([
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  h32(0x51),
  0n,
  [],
  0n,
  h32(0x52),
  0n,
  [],
  0n,
  [],
  0n,
  [],
  0n,
  [],
  [],
  0n,
  [],
  0n,
  [],
  0n,
  0n,
  h32(0x53),
]);
const valueAccumulatorCbor = encodeCbor([7n, h32(0x54), 2n, 1n]);
const valueAndMintControlCbor = encodeCbor([
  nativeControlCbor,
  3n,
  h32(0x55),
  4n,
  5n,
  h32(0x56),
  h32(0x57),
  h32(0x58),
  6n,
  7n,
  8n,
  valueAccumulatorCbor,
]);

const proofDescriptorCbor = encodeCbor([1n, 0n, 0n, []]);
const pendingMutationCbor = encodeCbor([
  1n,
  0n,
  1n,
  bytes("0102"),
  bytes("0304"),
  proofDescriptorCbor,
  -1n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  0n,
]);
const ledgerDeltaControlCbor = encodeCbor([
  0n,
  h32(0x61),
  0n,
  [],
  1n,
  h32(0x62),
  0n,
  h32(0x63),
  h32(0x64),
  h32(0x65),
  0n,
  0n,
  pendingMutationCbor,
  [],
]);

const acceptanceFrontier = {
  count: 2,
  peaks: [{ height: 1, hash: h32(0x71) }],
} as const;
const acceptanceFrontierCbor = encodeCbor([
  BigInt(acceptanceFrontier.count),
  encodeFrontier(acceptanceFrontier.peaks),
]);
const terminalAcceptanceCbor = encodeCbor([
  1n,
  Buffer.alloc(0),
  h32(0x72),
  acceptanceFrontierCbor,
]);
const rejectionCode = Buffer.from("E_VALUE_NOT_PRESERVED", "ascii");
const terminalRejectionCbor = encodeCbor([
  2n,
  rejectionCode,
  h32(0x73),
  bytes("80"),
]);

const decodeExactTerminalWitness = (
  input: Uint8Array,
): {
  readonly outcome: "accepted" | "rejected";
  readonly ledgerRoot: Buffer;
} => {
  const encoded = Buffer.from(input);
  const decoded = decodeSingleCbor(encoded);
  if (!Array.isArray(decoded) || decoded.length !== 4) {
    throw new Error("terminal witness must contain exactly four fields");
  }
  const [outcome, code, ledgerRoot, deltaEvidence] = decoded;
  if (typeof outcome !== "bigint" && typeof outcome !== "number") {
    throw new Error("terminal witness outcome must be an integer");
  }
  const outcomeCode = typeof outcome === "bigint" ? outcome : BigInt(outcome);
  const exactCode = Buffer.from(code as Uint8Array);
  const exactRoot = Buffer.from(ledgerRoot as Uint8Array);
  const exactEvidence = Buffer.from(deltaEvidence as Uint8Array);
  if (exactRoot.length !== 32) {
    throw new Error("terminal witness ledger root must contain 32 bytes");
  }
  if (outcomeCode === 1n) {
    if (exactCode.length !== 0) {
      throw new Error("accepted terminal witness cannot carry a rejection");
    }
    const frontier = decodeSingleCbor(exactEvidence);
    if (
      !Array.isArray(frontier) ||
      frontier.length !== 2 ||
      (typeof frontier[0] !== "bigint" && typeof frontier[0] !== "number") ||
      !Array.isArray(frontier[1])
    ) {
      throw new Error("accepted terminal witness frontier is malformed");
    }
  } else if (outcomeCode === 2n) {
    if (exactCode.length === 0 || !exactEvidence.equals(bytes("80"))) {
      throw new Error("rejected terminal witness is misclassified");
    }
  } else {
    throw new Error("terminal witness outcome is not canonical V1");
  }
  if (!encodeCbor(decoded).equals(encoded)) {
    throw new Error("terminal witness CBOR is not canonical");
  }
  return {
    outcome: outcomeCode === 1n ? "accepted" : "rejected",
    ledgerRoot: exactRoot,
  };
};

const EXPECTED = {
  auxiliaryCorpusHash:
    "8916ad7c26d34eafe62c93ed9c36be30d880fb102b918bb37f0b6d3dc27111e1",
  valueAccumulatorCbor:
    "8407582054545454545454545454545454545454545454545454545454545454545454540201",
  valueAndMintControlHash:
    "d30dfeaa4f1f3323bf2824a1051ef943fee27a31779e171678abe4c05ba2b2e0",
  pendingMutationCbor: "8a01000142010242030445840100008020404000",
  ledgerDeltaControlHash:
    "92e07c0c935ac73750a521ed638aed060414828d766774495885b56a04f5481b",
  terminalAcceptanceCbor:
    "840140582072727272727272727272727272727272727272727272727272727272727272725827820281820158207171717171717171717171717171717171717171717171717171717171717171",
  terminalAcceptanceHash:
    "0b3defd802c8cc6ee1112724ef19532be5b8f61817ab0282a56db645a2b20948",
  terminalRejectionCbor:
    "840255455f56414c55455f4e4f545f505245534552564544582073737373737373737373737373737373737373737373737373737373737373734180",
  terminalRejectionHash:
    "6b15a4122dc6437ca54930248e9df11979d21dc484d5ea373373b43c489f1ce6",
} as const;

describe("canonical V1 validation tail controls", () => {
  it("freezes every V15/V16 auxiliary tag and arity with one corpus hash", () => {
    const corpus = Buffer.from(
      Data.to(
        tailAuxiliaryVectors.map(([, , value]) =>
          validationAuxiliaryWitnessData(value),
        ) as never,
      ),
      "hex",
    );
    for (const [tag, arity, value] of tailAuxiliaryVectors) {
      const cbor = encodeValidationAuxiliaryWitnessCbor(value);
      const decoded = decodeExactTailAuxiliary(cbor.toString("hex"));
      expect([decoded.index, decoded.fields.length]).toEqual([tag, arity]);
    }
    expect(digest(corpus)).toBe(EXPECTED.auxiliaryCorpusHash);

    expect(() => decodeExactTailAuxiliary(Data.to(new Constr(23, [])))).toThrow(
      /tag/u,
    );
    expect(() => decodeExactTailAuxiliary(Data.to(new Constr(40, [])))).toThrow(
      /tag/u,
    );
    expect(() =>
      decodeExactTailAuxiliary(Data.to(new Constr(27, [0n]))),
    ).toThrow(/arity/u);
  });

  it("freezes V15 accumulator and 12-field value-and-mint control bytes", () => {
    const accumulator = decodeSingleCbor(valueAccumulatorCbor);
    const control = decodeSingleCbor(valueAndMintControlCbor);
    expect(Array.isArray(accumulator) ? accumulator.length : -1).toBe(4);
    expect(Array.isArray(control) ? control.length : -1).toBe(12);
    expect(
      Array.isArray(control)
        ? (decodeSingleCbor(control[0] as Uint8Array) as unknown[]).length
        : -1,
    ).toBe(26);
    expect({
      valueAccumulatorCbor: valueAccumulatorCbor.toString("hex"),
      valueAndMintControlHash: digest(valueAndMintControlCbor),
    }).toEqual({
      valueAccumulatorCbor: EXPECTED.valueAccumulatorCbor,
      valueAndMintControlHash: EXPECTED.valueAndMintControlHash,
    });
  });

  it("freezes V16 pending mutation/control shapes and operation roots", () => {
    const pending = decodeSingleCbor(pendingMutationCbor);
    const control = decodeSingleCbor(ledgerDeltaControlCbor);
    expect(Array.isArray(pending) ? pending.length : -1).toBe(10);
    expect(Array.isArray(control) ? control.length : -1).toBe(14);
    expect({
      pendingMutationCbor: pendingMutationCbor.toString("hex"),
      ledgerDeltaControlHash: digest(ledgerDeltaControlCbor),
    }).toEqual({
      pendingMutationCbor: EXPECTED.pendingMutationCbor,
      ledgerDeltaControlHash: EXPECTED.ledgerDeltaControlHash,
    });

    const deletion = {
      type: "delete" as const,
      key: bytes("010203"),
      proofDescriptor: descriptor,
    };
    const insertion = {
      type: "insert" as const,
      key: bytes("0405"),
      value: bytes("060708"),
      proofDescriptor: descriptor,
    };
    const frontier = buildMidgardValidationLedgerDeltaFrontier([
      deletion,
      insertion,
    ]);
    expect(
      hashMidgardValidationLedgerDeltaOperation(deletion).toString("hex"),
    ).toBe("d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589");
    expect(
      hashMidgardValidationLedgerDeltaOperation(insertion).toString("hex"),
    ).toBe("f8bc7029f5f58f0436ebdf6cbbb85bd9adac05d5f6dc1b9238c8166a517aa8db");
    expect(
      commitMidgardValidationMerkleFrontier(frontier).toString("hex"),
    ).toBe("b6d017c71f3fc974f620b22764385bf9ad56ee5627009e57dbeb9418e486dcb2");
    expect(hashMidgardValidationLedgerDelta([deletion, insertion])).toEqual(
      commitMidgardValidationMerkleFrontier(frontier),
    );
  });

  it("freezes V17 accepted/rejected witnesses and rejects misclassification", () => {
    expect(decodeExactTerminalWitness(terminalAcceptanceCbor)).toEqual({
      outcome: "accepted",
      ledgerRoot: h32(0x72),
    });
    expect(decodeExactTerminalWitness(terminalRejectionCbor)).toEqual({
      outcome: "rejected",
      ledgerRoot: h32(0x73),
    });
    expect({
      terminalAcceptanceCbor: terminalAcceptanceCbor.toString("hex"),
      terminalAcceptanceHash: hashMidgardValidationWorkWitness({
        phase: "terminal",
        programCounter: 9,
        witnessCbor: terminalAcceptanceCbor,
      }).toString("hex"),
      terminalRejectionCbor: terminalRejectionCbor.toString("hex"),
      terminalRejectionHash: hashMidgardValidationWorkWitness({
        phase: "terminal",
        programCounter: 9,
        witnessCbor: terminalRejectionCbor,
      }).toString("hex"),
    }).toEqual({
      terminalAcceptanceCbor: EXPECTED.terminalAcceptanceCbor,
      terminalAcceptanceHash: EXPECTED.terminalAcceptanceHash,
      terminalRejectionCbor: EXPECTED.terminalRejectionCbor,
      terminalRejectionHash: EXPECTED.terminalRejectionHash,
    });

    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([1n, rejectionCode, h32(0x72), acceptanceFrontierCbor]),
      ),
    ).toThrow(/cannot carry a rejection/u);
    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([2n, Buffer.alloc(0), h32(0x73), bytes("80")]),
      ),
    ).toThrow(/misclassified/u);
    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([2n, rejectionCode, h32(0x73), acceptanceFrontierCbor]),
      ),
    ).toThrow(/misclassified/u);
    expect(() =>
      decodeExactTerminalWitness(
        encodeCbor([3n, Buffer.alloc(0), h32(0x73), bytes("80")]),
      ),
    ).toThrow(/outcome/u);
  });

  it("contains no retired V2/V3 validation-tail production identity", () => {
    const machineDirectory = fileURLToPath(
      new URL("../src/validation-machine/", import.meta.url),
    );
    const sources = [
      ...readdirSync(machineDirectory)
        .filter((name) => name.endsWith(".ts"))
        .sort()
        .map((name) => `../src/validation-machine/${name}`),
      "../src/validation-machine-data.ts",
      ...readdirSync(
        fileURLToPath(
          new URL(
            "../../../onchain/aiken/lib/midgard/validation-machine/",
            import.meta.url,
          ),
        ),
      )
        .filter((name) => name.endsWith(".ak"))
        .sort()
        .map(
          (name) =>
            `../../../onchain/aiken/lib/midgard/validation-machine/${name}`,
        ),
    ].map((path) =>
      readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8"),
    );
    const retired =
      /\b(?:ValueAccumulator|ValueAccumulatorUpdate|ValueAssetMutationWitness|ValueAndMintControl|LedgerDeltaControl|LedgerDeltaPendingMutation|LedgerDeltaOperationProof|TerminalAcceptanceWitness|TerminalRejectionWitness)V[23]\b/u;
    expect(sources.flatMap((source) => source.match(retired) ?? [])).toEqual(
      [],
    );
  });
});
