import type { ValidationMachineWorkWitness } from "../../src/validation-machine.js";

type AuxiliaryV1 = NonNullable<ValidationMachineWorkWitness["auxiliary"]>;

const bytes = (hex: string): Buffer => Buffer.from(hex, "hex");
const hash = (byte: number): Buffer => Buffer.alloc(32, byte);
const emptyFrontier = {
  count: 0,
  peaks: [],
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
/**
 * #597. The four §8-door constructors name a `FieldCarriageV1` now. Tier-1
 * `Inline` is what the trace producer emits, so it is what this fixture pins;
 * the tier byte is the frozen part (Inline 0 / RawUtxo 1 / Certified 2) and the
 * preimage is the §5.1 envelope of one one-byte item.
 */
const carriage = {
  carriage: "Inline",
  preimage: bytes("81411a"),
} as const;
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
const emptyPairSummary = {
  root: bytes(
    "bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2",
  ),
  length: 0n,
  payloadCborLength: 0n,
  memory: 0n,
} as const;
const redeemerControl = {
  cursor: 0,
  mapItems: emptyPairSummary,
  activeScanHash: Buffer.alloc(0),
  activeRedeemerLeaf: Buffer.alloc(0),
  activePurpose: emptySummary,
  currentRedeemer: emptySummary,
} as const;
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

export const canonicalValidationAuxiliaryWitnesses = [
  [0, null],
  [
    1,
    auxiliary({
      kind: "transactionFieldChunk",
      fieldIndex: 0,
      itemIndex: 0,
      carriage,
    }),
  ],
  [
    2,
    auxiliary({
      kind: "requiredSignerItem",
      carriage,
      signerProof,
    }),
  ],
  [
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
    auxiliary({
      kind: "cekCoreStep",
      step: cekStep,
    }),
  ],
  [
    13,
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
    auxiliary({
      kind: "cekOutputContextItem",
      outputIndex: 0,
      descriptorCbor: bytes("10"),
      siblings: [],
    }),
  ],
  [
    15,
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
    auxiliary({
      kind: "redeemerItemStep",
      redeemerControl: null,
      control: redeemerItemControl,
      witness: redeemerItemWitness,
    }),
  ],
  [
    19,
    auxiliary({
      kind: "cekContextFinalize",
      redeemerControl,
    }),
  ],
  [
    20,
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
    auxiliary({
      kind: "cekContextAssemble",
      control: contextPartsControl,
    }),
  ],
  [
    22,
    auxiliary({
      kind: "cekTxInfoFinalize",
      control: txInfoAssemblyControl,
    }),
  ],
  [
    23,
    auxiliary({
      kind: "cekContextSeed",
      control: finalContextControl,
    }),
  ],
  [
    24,
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
    auxiliary({
      kind: "ledgerDeltaOutput",
      outputIndex: 0,
      descriptorCbor: bytes("21"),
      siblings: [],
    }),
  ],
  [
    29,
    auxiliary({
      kind: "transactionRedeemerItemBegin",
      carriage,
    }),
  ],
  [
    30,
    auxiliary({
      kind: "transactionFieldItem",
      carriage,
    }),
  ],
  [
    31,
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
    auxiliary({
      kind: "ledgerOutputProofStep",
      witness: null,
    }),
  ],
  [
    33,
    auxiliary({
      kind: "ledgerOutputProofFinalize",
      descriptorCbor: bytes("22"),
      signerProof,
    }),
  ],
  [
    34,
    auxiliary({
      kind: "ledgerDeltaProofFrame",
      frame: proofFrame,
      siblings: [],
    }),
  ],
  [
    35,
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
    auxiliary({
      kind: "scriptSourceHashBlock",
      chunkProof,
      nextChunkProof: null,
    }),
  ],
  [
    37,
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
    auxiliary({
      kind: "valueOutputDescriptor",
      outputIndex: 0,
      descriptorCbor: bytes("27"),
      siblings: [],
    }),
  ],
  [
    39,
    auxiliary({
      kind: "mintFoldAsset",
      chunkProof,
      nextChunkProof: null,
    }),
  ],
] as const;
