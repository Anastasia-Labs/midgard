import { encodeCbor } from "@al-ft/midgard-core";
import { encodeMidgardRejectionReasonV1Cbor } from "@al-ft/midgard-core/validation-trace";
import { fixtureBytes } from "@al-ft/midgard-test-support/hex";
import { Constr, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  encodeValidationTerminalWitnessCbor,
  type ValidationMachineWorkWitness,
} from "../src/index.js";

type Auxiliary = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;

export const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");

export const digest = (value: Uint8Array): string =>
  Buffer.from(blake2b(value, { dkLen: 32 })).toString("hex");

const emptyFrontier = {
  count: 0,
  peaks: [],
} as const;

const mutationStep = {
  unit: Buffer.alloc(28, 0x31),
  quantityDelta: 5n,
  oldDelta: null,
  preAssetRoot: fixtureBytes(0x32, 32),
  postAssetRoot: fixtureBytes(0x33, 32),
  proofCbor: bytes("80"),
  postSeenAssetCount: 1,
  postNonzeroAssetCount: 1,
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

const auxiliary = (value: Auxiliary): Auxiliary => value;

export const tailAuxiliaryVectors = [
  [
    24,
    11,
    auxiliary({
      kind: "valueInputAsset",
      sourceKind: "spend",
      key: bytes("01"),
      nextScheduleHash: fixtureBytes(0x41, 32),
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
    34,
    3,
    auxiliary({
      kind: "valueOutputDescriptor",
      outputIndex: 4,
      descriptorCbor: bytes("8101"),
      siblings: [fixtureBytes(0x48, 32)],
    }),
  ],
  [
    35,
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

export const decodeExactTailAuxiliary = (cborHex: string): Constr<Data> => {
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

const nativeControlCbor = encodeCbor([
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  fixtureBytes(0x51, 32),
  0n,
  [],
  0n,
  fixtureBytes(0x52, 32),
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
  fixtureBytes(0x53, 32),
]);

export const valueAccumulatorCbor = encodeCbor([
  7n,
  fixtureBytes(0x54, 32),
  2n,
  1n,
]);

export const valueAndMintControlCbor = encodeCbor([
  nativeControlCbor,
  3n,
  fixtureBytes(0x55, 32),
  4n,
  5n,
  fixtureBytes(0x56, 32),
  fixtureBytes(0x57, 32),
  fixtureBytes(0x58, 32),
  6n,
  7n,
  8n,
  valueAccumulatorCbor,
]);

export const terminalAcceptanceCbor = encodeValidationTerminalWitnessCbor({
  verdict: "accepted",
});

export const rejectionReasonCbor =
  encodeMidgardRejectionReasonV1Cbor("ValueNotPreserved");
