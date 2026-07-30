import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  encodeCbor,
  verifyMidgardBoundedItemChunkProofV1,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekContextPartsControlV1,
  encodeMidgardCekFinalContextControlV1,
  encodeMidgardCekRedeemerContextControlV1,
  encodeMidgardCekTxInfoAssemblyControlV1,
  hashMidgardCekContextPartsControlV1,
  hashMidgardCekFinalContextControlV1,
  hashMidgardCekRedeemerContextControlV1,
  hashMidgardCekTxInfoAssemblyControlV1,
  initialMidgardCekRedeemerContextControlV1,
} from "../src/cek-context.js";
import type { ValidationMachineWorkWitness } from "../src/validation-machine.js";
import {
  encodeValidationAuxiliaryWitnessCborV1,
  validationAuxiliaryWitnessDataV1,
} from "../src/validation-machine-data.js";

type AuxiliaryV1 = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;

const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");
const hash = (byte: number): Buffer => Buffer.alloc(32, byte);
const digest = (value: Uint8Array): string =>
  Buffer.from(blake2b(value, { dkLen: 32 })).toString("hex");

const emptyFrontier = {
  count: 0,
  peaks: [],
} as const;
const collectionProof = {
  version: 1,
  fieldIndex: 0,
  itemCount: 1,
  itemIndex: 0,
  itemLength: 1,
  itemCommitment: hash(0x11),
  frontier: emptyFrontier,
  siblings: [],
} as const;
const chunkProof = {
  version: 1,
  fieldIndex: 0,
  itemIndex: 0,
  totalLength: 1,
  chunkIndex: 0,
  chunk: bytes("12"),
  frontier: emptyFrontier,
  siblings: [],
} as const;
const signerProof = { kind: "none" } as const;
const emptySummary = {
  root: Buffer.alloc(0),
  cborLength: 0n,
  memory: 0n,
} as const;
const emptySequence = {
  root: Buffer.alloc(0),
  length: 0n,
  payloadCborLength: 0n,
  memory: 0n,
} as const;
const redeemerControl = initialMidgardCekRedeemerContextControlV1();
const contextPartsControl = {
  redeemerItems: emptySequence,
  redeemer: emptySummary,
  scriptInfo: emptySummary,
} as const;
const txInfoAssemblyControl = {
  tailFields: emptySequence,
  redeemer: emptySummary,
  scriptInfo: emptySummary,
} as const;
const finalContextControl = {
  txInfo: emptySummary,
  redeemer: emptySummary,
  scriptInfo: emptySummary,
} as const;
const redeemerItemControl = {
  version: 1,
  mode: 0,
  stage: 0,
  itemIndex: 0,
  itemCount: 1,
  totalLength: 1,
  itemCommitment: hash(0x13),
  expectedPurposeTag: 0,
  expectedPointerIndex: 0,
  purposeTag: 0,
  pointerIndex: 0,
  dataOffset: 0,
  dataLength: 0,
  executionMemory: 0n,
  executionSteps: 0n,
  traversal: null,
} as const;
const redeemerItemWitness = {
  action: { kind: "finishData" },
  chunkProof: null,
  nextChunkProof: null,
} as const;
const cekState = {
  mode: "compute",
  executionIndex: 0n,
  focusRoot: hash(0x14),
  environmentRoot: hash(0x15),
  continuationRoot: hash(0x16),
  auxiliary: 0n,
  cpu: 0n,
  memory: 0n,
} as const;
const cekStep = {
  pre: cekState,
  post: cekState,
  witness: { kind: "computeError" },
} as const;
const valueMutation = {
  unit: bytes("17"),
  quantityDelta: 1n,
  oldDelta: null,
  preAssetRoot: hash(0x18),
  postAssetRoot: hash(0x19),
  proofCbor: bytes("80"),
  postSeenAssetCount: 1,
  postNonzeroAssetCount: 1,
} as const;
const proofFrame = {
  version: 1,
  frameIndex: 0,
  cursor: 0,
  nextCursor: 1,
  step: { kind: "branch", skip: 0, neighbors: Buffer.alloc(0) },
} as const;
const proofDescriptor = {
  version: 1,
  frameCount: 0,
  terminalCursor: 0,
  frontier: emptyFrontier,
} as const;
const proofFoldControl = {
  nextFrameIndex: 0,
  expectedNextCursor: 0,
  includingRoot: hash(0x1a),
  excludingRoot: hash(0x1b),
} as const;
const ledgerMutation = {
  operation: { type: "delete", key: bytes("1c") },
  preRoot: hash(0x1d),
  postRoot: hash(0x1e),
  proofFoldTrace: {
    descriptor: proofDescriptor,
    frames: [],
    initial: proofFoldControl,
    steps: [],
    terminal: proofFoldControl,
  },
} as const;
const operationMembership = {
  frontier: emptyFrontier,
  leafIndex: 0,
  leafHash: hash(0x1f),
  siblings: [],
} as const;

const auxiliary = (value: AuxiliaryV1): AuxiliaryV1 => value;

const auxiliaryVectors = [
  [0, 0, null],
  [
    1,
    2,
    auxiliary({
      kind: "transactionFieldChunk",
      collectionProof,
      chunkProof,
    }),
  ],
  [
    2,
    3,
    auxiliary({
      kind: "requiredSignerItem",
      collectionProof,
      chunkProof,
      signerProof,
    }),
  ],
  [
    3,
    3,
    auxiliary({
      kind: "nativeScriptToken",
      chunkProof,
      nextChunkProof: null,
      signerProof,
    }),
  ],
  [
    4,
    1,
    auxiliary({
      kind: "nativeScriptFrame",
      frame: {
        tail: bytes("02"),
        kind: 1,
        childCount: 0,
        remaining: 0,
        validCount: 0,
        required: 0n,
      },
    }),
  ],
  [
    5,
    6,
    auxiliary({
      kind: "scheduledLedgerLookup",
      sourceKind: "spend",
      key: bytes("03"),
      nextScheduleHash: hash(0x20),
      value: bytes("04"),
      proofCbor: bytes("80"),
      signerProof,
    }),
  ],
  [
    6,
    4,
    auxiliary({
      kind: "scheduledLedgerLookup",
      sourceKind: "reference",
      key: bytes("05"),
      nextScheduleHash: hash(0x21),
      value: null,
      proofCbor: bytes("80"),
      signerProof,
    }),
  ],
  [
    7,
    4,
    auxiliary({
      kind: "resolvedInputReplay",
      sourceKind: "spend",
      key: bytes("06"),
      nextScheduleHash: hash(0x22),
      value: bytes("07"),
    }),
  ],
  [
    8,
    5,
    auxiliary({
      kind: "scriptPurposeScan",
      purposeKind: 0,
      purposeIndex: 0n,
      scriptHash: hash(0x23),
      subject: bytes("09"),
      siblings: [],
    }),
  ],
  [
    9,
    8,
    auxiliary({
      kind: "scriptSourceScan",
      sourceIndex: 0,
      originKind: "inline",
      sourceKey: bytes("0a"),
      scriptLanguageTag: 0,
      scriptHash: hash(0x24),
      scriptTotalLength: 1,
      scriptItemCommitment: hash(0x25),
      siblings: [],
    }),
  ],
  [
    10,
    5,
    auxiliary({
      kind: "redeemerScanBegin",
      itemIndex: 0,
      itemCount: 1,
      totalLength: 1,
      itemCommitment: hash(0x26),
      siblings: [],
    }),
  ],
  [
    11,
    16,
    auxiliary({
      kind: "nativeExecutionScan",
      executionIndex: 0,
      languageTag: 0,
      purpose: {
        purposeKind: 0,
        purposeIndex: 0n,
        scriptHash: hash(0x27),
        subject: bytes("0b"),
        siblings: [],
      },
      source: {
        sourceIndex: 0,
        originKind: "inline",
        sourceKey: bytes("0c"),
        scriptTotalLength: 1,
        scriptItemCommitment: hash(0x30),
        siblings: [],
      },
      redeemerLeaf: hash(0x28),
      executionSiblings: [],
      firstChunkProof: chunkProof,
    }),
  ],
  [
    12,
    1,
    auxiliary({
      kind: "cekCoreStep",
      step: cekStep,
    }),
  ],
  [
    13,
    5,
    auxiliary({
      kind: "cekResolvedContextItem",
      sourceKind: "spend",
      itemIndex: 0,
      key: bytes("0e"),
      descriptorCbor: bytes("0f"),
      siblings: [],
    }),
  ],
  [
    14,
    3,
    auxiliary({
      kind: "cekOutputContextItem",
      outputIndex: 0,
      descriptorCbor: bytes("10"),
      siblings: [],
    }),
  ],
  [
    15,
    4,
    auxiliary({
      kind: "cekSignerContextItem",
      frontier: emptyFrontier,
      signerIndex: 0,
      signerHash: hash(0x29),
      siblings: [],
    }),
  ],
  [
    16,
    5,
    auxiliary({
      kind: "cekMintContextItem",
      mintIndex: 0,
      policyId: bytes("11"),
      assetName: bytes("12"),
      quantity: 1n,
      siblings: [],
    }),
  ],
  [
    17,
    12,
    auxiliary({
      kind: "cekRedeemerContextSelect",
      control: redeemerControl,
      itemIndex: 0,
      itemCount: 1,
      totalLength: 1,
      itemCommitment: hash(0x2a),
      redeemerSiblings: [],
      purposeFrontierIndex: 0,
      purpose: {
        purposeKind: 0,
        purposeIndex: 0n,
        scriptHash: hash(0x2b),
        subject: bytes("13"),
        siblings: [],
      },
    }),
  ],
  [
    18,
    3,
    auxiliary({
      kind: "redeemerItemStep",
      redeemerControl: null,
      control: redeemerItemControl,
      witness: redeemerItemWitness,
    }),
  ],
  [
    19,
    1,
    auxiliary({
      kind: "cekContextFinalize",
      redeemerControl,
    }),
  ],
  [
    20,
    5,
    auxiliary({
      kind: "cekContextFinalizeSpend",
      redeemerControl,
      itemIndex: 0,
      key: bytes("14"),
      descriptorCbor: bytes("15"),
      siblings: [],
    }),
  ],
  [
    21,
    1,
    auxiliary({
      kind: "cekContextAssemble",
      control: contextPartsControl,
    }),
  ],
  [
    22,
    1,
    auxiliary({
      kind: "cekTxInfoFinalize",
      control: txInfoAssemblyControl,
    }),
  ],
  [
    23,
    1,
    auxiliary({
      kind: "cekContextSeed",
      control: finalContextControl,
    }),
  ],
  [
    24,
    11,
    auxiliary({
      kind: "valueInputAsset",
      sourceKind: "spend",
      key: bytes("16"),
      nextScheduleHash: hash(0x2c),
      descriptorCbor: bytes("17"),
      assetIndex: 0,
      policyId: bytes("18"),
      assetName: bytes("19"),
      quantity: 1n,
      assetFrontier: emptyFrontier,
      assetSiblings: [],
      mutationStep: valueMutation,
    }),
  ],
  [
    25,
    9,
    auxiliary({
      kind: "valueOutputAsset",
      outputIndex: 0,
      descriptorCbor: bytes("1a"),
      assetIndex: 0,
      policyId: bytes("1b"),
      assetName: bytes("1c"),
      quantity: 1n,
      assetFrontier: emptyFrontier,
      assetSiblings: [],
      mutationStep: valueMutation,
    }),
  ],
  [
    26,
    6,
    auxiliary({
      kind: "valueMintAsset",
      mintIndex: 0,
      policyId: bytes("1d"),
      assetName: bytes("1e"),
      quantity: 1n,
      siblings: [],
      mutationStep: valueMutation,
    }),
  ],
  [
    27,
    4,
    auxiliary({
      kind: "ledgerDeltaReplay",
      sourceKind: "reference",
      key: bytes("1f"),
      nextScheduleHash: hash(0x2d),
      value: bytes("20"),
    }),
  ],
  [
    28,
    3,
    auxiliary({
      kind: "ledgerDeltaOutput",
      outputIndex: 0,
      descriptorCbor: bytes("21"),
      siblings: [],
    }),
  ],
  [
    29,
    1,
    auxiliary({
      kind: "transactionRedeemerItemBegin",
      collectionProof,
    }),
  ],
  [
    30,
    2,
    auxiliary({
      kind: "transactionFieldItem",
      collectionProof,
      itemCbor: bytes("22"),
    }),
  ],
  [
    31,
    4,
    auxiliary({
      kind: "ledgerOutputProofBegin",
      outputIndex: 0,
      totalLength: 1,
      itemCommitment: hash(0x2e),
      siblings: [],
    }),
  ],
  [
    32,
    1,
    auxiliary({
      kind: "ledgerOutputProofStep",
      witness: null,
    }),
  ],
  [
    33,
    2,
    auxiliary({
      kind: "ledgerOutputProofFinalize",
      descriptorCbor: bytes("22"),
      signerProof,
    }),
  ],
  [
    34,
    2,
    auxiliary({
      kind: "ledgerDeltaProofFrame",
      frame: proofFrame,
      siblings: [],
    }),
  ],
  [
    35,
    4,
    auxiliary({
      kind: "ledgerDeltaOperation",
      operationKind: "delete",
      key: bytes("23"),
      value: bytes("24"),
      mutationStep: ledgerMutation,
      operationMembership,
    }),
  ],
  [
    36,
    2,
    auxiliary({
      kind: "scriptSourceHashBlock",
      chunkProof,
      nextChunkProof: null,
    }),
  ],
  [
    37,
    17,
    auxiliary({
      kind: "nativeExecutionDescriptor",
      executionIndex: 0,
      languageTag: 0,
      purpose: {
        purposeKind: 0,
        purposeIndex: 0n,
        scriptHash: hash(0x2f),
        subject: bytes("25"),
        siblings: [],
      },
      source: {
        sourceIndex: 0,
        originKind: "inline",
        sourceKey: bytes("26"),
        scriptTotalLength: 1,
        scriptItemCommitment: hash(0x30),
        siblings: [],
      },
      redeemerLeaf: hash(0x31),
      executionSiblings: [],
      firstChunkProof: null,
      signerFrontier: emptyFrontier,
    }),
  ],
  [
    38,
    3,
    auxiliary({
      kind: "valueOutputDescriptor",
      outputIndex: 0,
      descriptorCbor: bytes("27"),
      siblings: [],
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

const auxiliaryCorpusData = auxiliaryVectors.map(([, , value]) =>
  validationAuxiliaryWitnessDataV1(value),
);

const CANONICAL_AUXILIARY_CORPUS_CBOR =
  "9fd87980d87a9fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffd8799f010000010041128080ffffd87b9fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffd8799f010000010041128080ffd87980ffd87c9fd8799f010000010041128080ffd87a80d87980ffd87d9fd8799f41020100000000ffffd87e9f00410358202020202020202020202020202020202020202020202020202020202020202020410480d87980ffd87f9f0141055820212121212121212121212121212121212121212121212121212121212121212180ffd905009f004106582022222222222222222222222222222222222222222222222222222222222222224107ffd905019f000058202323232323232323232323232323232323232323232323232323232323232323410980ffd905029f0000410a0058202424242424242424242424242424242424242424242424242424242424242424015820252525252525252525252525252525252525252525252525252525252525252580ffd905039f0001015820262626262626262626262626262626262626262626262626262626262626262680ffd905049f0000000058202727272727272727272727272727272727272727272727272727272727272727410b800000410c0158203030303030303030303030303030303030303030303030303030303030303030805820282828282828282828282828282828282828282828282828282828282828282880d8799f010000010041128080ffffd905059fd8799fd8799f0000582014141414141414141414141414141414141414141414141414141414141414145820151515151515151515151515151515151515151515151515151515151515151558201616161616161616161616161616161616161616161616161616161616161616000000ffd8799f0000582014141414141414141414141414141414141414141414141414141414141414145820151515151515151515151515151515151515151515151515151515151515151558201616161616161616161616161616161616161616161616161616161616161616000000ffd87f80ffffd905069f0000410e410f80ffd905079f00411080ffd905089f80005820292929292929292929292929292929292929292929292929292929292929292980ffd905099f00411141120180ffd9050a9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffff00010158202a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a8000000058202b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b411380ffd9050b9fd87a80d8799f010000000101582013131313131313131313131313131313131313131313131313131313131313130000000000000000d87a80ffd8799fd87c80d87a80d87a80ffffd9050c9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffffffd9050d9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffff004114411580ffd9050e9fd8799fd8799f40000000ffd8799f400000ffd8799f400000ffffffd9050f9fd8799fd8799f40000000ffd8799f400000ffd8799f400000ffffffd905109fd8799fd8799f400000ffd8799f400000ffd8799f400000ffffffd905119f00411658202c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c41170041184119018080d8799fd879800080ffffd905129f00411a00411b411c018080d8799fd879800080ffffd905139f00411d411e0180d8799fd879800080ffffd905149f01411f58202d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d4120ffd905159f00412180ffd905169fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffffd905179fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ff4122ffd905189f000158202e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e80ffd905199fd87980ffd9051a9f4122d87980ffd9051b9fd8799f01000001d8799f0040ffff80ffd9051c9f0041234124d8799fd8799f01000080ff00800080ffffd9051d9fd8799f010000010041128080ffd87a80ffd9051e9f0000000058202f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f412580000041260158203030303030303030303030303030303030303030303030303030303030303030805820313131313131313131313131313131313131313131313131313131313131313180d87a8080ffd9051f9f00412780ffd905209fd8799f010000010041128080ffd87a80ffff";
const CANONICAL_AUXILIARY_CORPUS_HASH =
  "9e3d884b15fa7d04c150e26adac1f0d4415dc94c48f6e7585a58d080ba31980f";

const shared32 = hash(0x41);
const resolveInputsValues = [
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  shared32,
  hash(0x42),
  0n,
  hash(0x43),
  bytes("00"),
  hash(0x44),
] as const;
const CANONICAL_RESOLVE_INPUTS_CONTROL_CBOR =
  "8b41014102410341040058204141414141414141414141414141414141414141414141414141414141414141582042424242424242424242424242424242424242424242424242424242424242420058204343434343434343434343434343434343434343434343434343434343434343410058204444444444444444444444444444444444444444444444444444444444444444";

const scriptDiscoveryValues = [
  0n,
  0n,
  0n,
  -1n,
  -1n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  -1n,
  -1n,
  Buffer.alloc(0),
  0n,
  0n,
  0n,
  [],
  Buffer.alloc(0),
] as const;
const mintFoldValues = [
  -1n,
  0n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  0n,
  Buffer.alloc(0),
  0n,
  0n,
  0n,
  Buffer.alloc(0),
  0n,
  [],
] as const;
const receiveScanValues = [
  0n,
  [],
  0n,
  Buffer.alloc(0),
  Buffer.alloc(0),
  [],
] as const;
const observerScanValues = [0n, Buffer.alloc(0), 0n] as const;
const scriptSourcesValues = [
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  hash(0x45),
  0n,
  hash(0x46),
  [],
  0n,
  0n,
  [],
  0n,
  [],
  0n,
  hash(0x47),
  hash(0x48),
  0n,
  0n,
  [],
  0n,
  0n,
  [],
  0n,
  receiveScanValues,
  0n,
  0n,
  observerScanValues,
  mintFoldValues,
  hash(0x49),
] as const;
const CANONICAL_SCRIPT_DISCOVERY_CONTROL_CBOR =
  "8f000000202040402020400000008040";
const CANONICAL_SCRIPT_SOURCES_CONTROL_CBOR =
  "981e41014102410341040058204545454545454545454545454545454545454545454545454545454545454545005820464646464646464646464646464646464646464646464646464646464646464680000080008000582047474747474747474747474747474747474747474747474747474747474747475820484848484848484848484848484848484848484848484848484848484848484800008000008000860080004040800000830040008c20004040004000000040008058204949494949494949494949494949494949494949494949494949494949494949";

const nativeScriptsValues = [
  bytes("01"),
  bytes("02"),
  bytes("03"),
  bytes("04"),
  0n,
  hash(0x4a),
  0n,
  [],
  0n,
  hash(0x4b),
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
  hash(0x4c),
] as const;
const CANONICAL_NATIVE_SCRIPTS_CONTROL_CBOR =
  "981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c";
const CANONICAL_SCRIPT_INTEGRITY_WITNESS_CBORS = [
  "825883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c00",
  "825883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c01",
  "845883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c0258204d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d4d58204e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e4e",
  "845883981a41014102410341040058204a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a4a00800058204b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b4b00800080008000808000800080000058204c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c4c0358204f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f4f58205050505050505050505050505050505050505050505050505050505050505050",
] as const;

const nonemptySummary = {
  root: hash(0x51),
  cborLength: 1n,
  memory: 1n,
} as const;
const nonemptySequence = {
  root: hash(0x52),
  length: 1n,
  payloadCborLength: 1n,
  memory: 1n,
} as const;
const v14RedeemerControl = {
  cursor: 1,
  mapItems: nonemptySequence,
  activeScanHash: hash(0x53),
  activeRedeemerLeaf: hash(0x54),
  activePurpose: emptySummary,
  currentRedeemer: nonemptySummary,
} as const;
const v14FinalControl = {
  txInfo: nonemptySummary,
  redeemer: nonemptySummary,
  scriptInfo: nonemptySummary,
} as const;
const v14PartsControl = {
  redeemerItems: nonemptySequence,
  redeemer: nonemptySummary,
  scriptInfo: nonemptySummary,
} as const;
const v14AssemblyControl = {
  tailFields: nonemptySequence,
  redeemer: nonemptySummary,
  scriptInfo: nonemptySummary,
} as const;
const CANONICAL_V14_CONTROL_CBORS = [
  "8601845820525252525252525252525252525252525252525252525252525252525252525201010158205353535353535353535353535353535353535353535353535353535353535353582054545454545454545454545454545454545454545454545454545454545454548340000083582051515151515151515151515151515151515151515151515151515151515151510101",
  "83835820515151515151515151515151515151515151515151515151515151515151515101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
  "8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
  "8384582052525252525252525252525252525252525252525252525252525252525252520101018358205151515151515151515151515151515151515151515151515151515151515151010183582051515151515151515151515151515151515151515151515151515151515151510101",
] as const;
const CANONICAL_V14_CONTROL_HASHES = [
  "3dfab23fb96dece2da964d3b0b62ef26006400b04b676b6ccfc18ac5da438c10",
  "a4fdda392c9324034244f6b4674441a320d90d819521ccc9b62ff37c0dfdc10b",
  "1e8e3ea65ea7e762512207ea4276022ce321332ee4c2f6bdf1b7329bd1baa962",
  "7e848c60ea1a41e1d8d90d38c9034b78ce2c0b55e5e0cba7620bb0d3f909e674",
] as const;

const expectExactArray = (
  cbor: Uint8Array,
  expectedArity: number,
): readonly unknown[] => {
  const decoded = decodeSingleCbor(Buffer.from(cbor));
  if (!Array.isArray(decoded) || decoded.length !== expectedArity) {
    throw new Error(`expected exact V1 array arity ${expectedArity}`);
  }
  return decoded;
};

describe("canonical validation controls V1 ABI", () => {
  it("freezes all 40 ValidationAuxiliaryWitnessV1 tags and arities", () => {
    expect(auxiliaryVectors).toHaveLength(40);
    for (const [
      vectorIndex,
      [declaredTag, expectedArity, value],
    ] of auxiliaryVectors.entries()) {
      expect(declaredTag).toBe(vectorIndex);
      const cbor = encodeValidationAuxiliaryWitnessCborV1(value);
      const decoded = Data.from(cbor.toString("hex"));
      expect(decoded).toBeInstanceOf(Constr);
      expect((decoded as Constr<unknown>).index).toBe(vectorIndex);
      expect((decoded as Constr<unknown>).fields).toHaveLength(expectedArity);
    }
    const corpus = Data.to(auxiliaryCorpusData as never);
    expect(corpus).toBe(CANONICAL_AUXILIARY_CORPUS_CBOR);
    expect(digest(bytes(corpus))).toBe(CANONICAL_AUXILIARY_CORPUS_HASH);
  });

  it("keeps the descriptor-only native execution witness bounded at the chunk limit", () => {
    const nativeExecution = auxiliaryVectors[11]![2];
    if (nativeExecution?.kind !== "nativeExecutionScan") {
      throw new Error("canonical tag 11 must be nativeExecutionScan");
    }
    const maximumItem = buildMidgardBoundedItemV1({
      fieldIndex: 0,
      itemIndex: 0,
      bytes: Buffer.alloc(4_095, 0x5a),
    });
    const maximumChunkProof = buildMidgardBoundedItemChunkProofV1(
      maximumItem,
      0,
    );
    expect(
      verifyMidgardBoundedItemChunkProofV1({
        expectedCommitment: maximumItem.commitment,
        proof: maximumChunkProof,
      }),
    ).toBe(true);
    const maximumChunkWitness = {
      ...nativeExecution,
      source: {
        ...nativeExecution.source,
        scriptTotalLength: maximumItem.bytes.length,
        scriptItemCommitment: maximumItem.commitment,
      },
      firstChunkProof: maximumChunkProof,
    } satisfies AuxiliaryV1;
    const encoded = encodeValidationAuxiliaryWitnessCborV1(maximumChunkWitness);
    const decoded = Data.from(encoded.toString("hex"));
    expect(decoded).toBeInstanceOf(Constr);
    expect((decoded as Constr<unknown>).index).toBe(11);
    expect((decoded as Constr<unknown>).fields).toHaveLength(16);
    expect(encoded.length).toBeLessThan(16 * 1024);
    expect("script" in maximumChunkWitness.source).toBe(false);
    expect("signerHashes" in maximumChunkWitness).toBe(false);
  });

  it("matches exact V11 resolve-inputs and V12 script-source controls", () => {
    const resolveInputs = encodeCbor(resolveInputsValues);
    const discovery = encodeCbor(scriptDiscoveryValues);
    const scriptSources = encodeCbor(scriptSourcesValues);

    expect(resolveInputs.toString("hex")).toBe(
      CANONICAL_RESOLVE_INPUTS_CONTROL_CBOR,
    );
    expect(discovery.toString("hex")).toBe(
      CANONICAL_SCRIPT_DISCOVERY_CONTROL_CBOR,
    );
    expect(scriptSources.toString("hex")).toBe(
      CANONICAL_SCRIPT_SOURCES_CONTROL_CBOR,
    );
    expectExactArray(resolveInputs, 11);
    expectExactArray(discovery, 15);
    expectExactArray(scriptSources, 30);
  });

  it("matches the V13 native control and every integrity witness shape", () => {
    const control = encodeCbor(nativeScriptsValues);
    const wrappers = [
      encodeCbor([control, 0n]),
      encodeCbor([control, 1n]),
      encodeCbor([control, 2n, hash(0x4d), hash(0x4e)]),
      encodeCbor([control, 3n, hash(0x4f), hash(0x50)]),
    ];

    expect(control.toString("hex")).toBe(CANONICAL_NATIVE_SCRIPTS_CONTROL_CBOR);
    expect(wrappers.map((value) => value.toString("hex"))).toEqual(
      CANONICAL_SCRIPT_INTEGRITY_WITNESS_CBORS,
    );
    expectExactArray(control, 26);
    expectExactArray(wrappers[0]!, 2);
    expectExactArray(wrappers[1]!, 2);
    expectExactArray(wrappers[2]!, 4);
    expectExactArray(wrappers[3]!, 4);
  });

  it("matches every exported V14 encoder and domain-separated hash", () => {
    const encodings = [
      encodeMidgardCekRedeemerContextControlV1(v14RedeemerControl),
      encodeMidgardCekFinalContextControlV1(v14FinalControl),
      encodeMidgardCekContextPartsControlV1(v14PartsControl),
      encodeMidgardCekTxInfoAssemblyControlV1(v14AssemblyControl),
    ];
    const hashes = [
      hashMidgardCekRedeemerContextControlV1(v14RedeemerControl),
      hashMidgardCekFinalContextControlV1(v14FinalControl),
      hashMidgardCekContextPartsControlV1(v14PartsControl),
      hashMidgardCekTxInfoAssemblyControlV1(v14AssemblyControl),
    ];

    expect(encodings.map((value) => value.toString("hex"))).toEqual(
      CANONICAL_V14_CONTROL_CBORS,
    );
    expect(hashes.map((value) => value.toString("hex"))).toEqual(
      CANONICAL_V14_CONTROL_HASHES,
    );
    expectExactArray(encodings[0]!, 6);
    for (const encoding of encodings.slice(1)) {
      expectExactArray(encoding, 3);
    }
  });

  it("rejects adjacent tags, wrong arities, and malformed controls", () => {
    const assertAuxiliaryEnvelope = (cbor: string): void => {
      const decoded = Data.from(cbor);
      if (!(decoded instanceof Constr)) {
        throw new Error("V1 auxiliary witness must be a constructor");
      }
      const expectedArity = auxiliaryVectors[decoded.index]?.[1];
      if (
        expectedArity === undefined ||
        decoded.fields.length !== expectedArity
      ) {
        throw new Error("unknown V1 auxiliary tag or wrong arity");
      }
    };

    expect(() => assertAuxiliaryEnvelope("d9052180")).toThrow(
      "unknown V1 auxiliary tag",
    );
    expect(() => assertAuxiliaryEnvelope("d8799f00ff")).toThrow("wrong arity");
    expect(() => expectExactArray(bytes("8a00000000000000000000"), 11)).toThrow(
      "arity 11",
    );
    expect(() =>
      expectExactArray(bytes("8f000000000000000000000000000000"), 15),
    ).not.toThrow();
    expect(() =>
      expectExactArray(
        bytes("981d0000000000000000000000000000000000000000000000000000000000"),
        30,
      ),
    ).toThrow("arity 30");
    expect(() => expectExactArray(bytes("81ff"), 3)).toThrow();
  });
});
