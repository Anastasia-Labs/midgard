import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  appendMidgardValidationMerkleLeafV1,
  buildMidgardBoundedCollectionItemProofV1,
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardValidationLedgerDeltaFrontierV1,
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipV1,
  buildMidgardValidationTraceTree,
  commitMidgardValidationMerkleFrontierV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardCekProgramEnvelopeV1,
  decodeMidgardCekProgramMaterialSidecarV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  deriveMidgardNativeFieldCollectionV1,
  deriveMidgardV1TxFieldPreimages,
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecarV1,
  hashMidgardCekMachineStateV1,
  hashMidgardMintAssetLeafV1,
  hashMidgardOutputLeafV1,
  hashMidgardRedeemerItemLeafV1,
  hashMidgardRedeemerLeafV1,
  hashMidgardResolvedContextItemLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardScriptSourceLeafV1,
  hashMidgardSignerLeafV1,
  hashMidgardValidationContextV1,
  hashMidgardValidationLedgerDeltaOperationV1,
  hashMidgardValidationLedgerDeltaV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  hashMidgardValidationWorkWitnessV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_VALIDATION_MACHINE_V1_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  midgardBoundedItemChunkCountV1,
  type MidgardBoundedCollectionItemProofV1,
  type MidgardBoundedItemChunkProofV1,
  type MidgardConsensusProfileV1,
  type MidgardValidationMachineStateV1,
  type MidgardValidationMerkleFrontierV1,
  type MidgardValidationPhaseName,
  type MidgardValidationTraceTree,
} from "@al-ft/midgard-core";
import {
  decodeMidgardAddressBytes,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxCompactV1,
  decodeMidgardTxOutput,
  hashMidgardVersionedScript,
  type MidgardValue,
  type MidgardVersionedScript,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import {
  readCborArrayHeader,
  readCborBytes,
  readCborInteger,
  readCborMapHeader,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { blake2b } from "@noble/hashes/blake2.js";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  composeMidgardCekContextSummaryV1,
  decodeMidgardCekContextV1,
  encodeMidgardCekValidationWitnessV1,
  hashMidgardCekContextPartsControlV1,
  hashMidgardCekFinalContextControlV1,
  hashMidgardCekRedeemerContextControlV1,
  hashMidgardCekTxInfoAssemblyControlV1,
  initialMidgardCekContextControlV1,
  initialMidgardCekRedeemerContextControlV1,
  type MidgardCekContextControlV1,
  type MidgardCekContextPartsControlV1,
  type MidgardCekFinalContextControlV1,
  type MidgardCekRedeemerContextControlV1,
  type MidgardCekTxInfoAssemblyControlV1,
  summarizeMidgardCekContextPartsV1,
  summarizeMidgardCekLucidDataV1,
} from "./cek-context.js";
import {
  buildMidgardCekDataScanTraceV1,
  hashMidgardCekDataScanControlV1,
  type MidgardCekDataScanControlV1,
  type MidgardCekDataScanStepV1,
} from "./cek-data-scan.js";
import {
  buildMidgardCekExecutionGraphV1,
  executeMidgardCekStructuralProgramV1,
  type MidgardCekExecutionGraphV1,
  type MidgardCekExecutionStepV1,
  type MidgardCekStructuralExecutionV1,
} from "./cek-executor.js";
import type { LocalScriptEvalResult } from "./local-script-eval.js";
import {
  cardanoScriptPurposeData,
  type DecodedMidgardRedeemer,
  decodeMidgardRedeemers,
  type MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "./midgard-redeemers.js";
import { validatePhaseASingle } from "./phase-a.js";
import { runPhaseBValidationWithPatch } from "./phase-b.js";
import {
  commitMidgardScriptContextTxInInfoV1,
  commitMidgardScriptContextTxOutV1,
  emptyMidgardCekDataPairSummaryV1,
  prependMidgardCekDataListSummaryV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekMapDataV1,
} from "./script-context-proof.js";
import type { QueuedTx, RejectCode, RejectedTx } from "./types.js";
import { RejectCodes } from "./types.js";

export type ValidationMachineLedgerEntry = {
  readonly outRef: Buffer;
  readonly output: Buffer;
};

export type ValidationMachineLedgerOp =
  | { readonly type: "delete"; readonly key: Buffer }
  | { readonly type: "insert"; readonly key: Buffer; readonly value: Buffer };

export type ValidationMachineLedgerMutationStep = {
  readonly operation: ValidationMachineLedgerOp;
  readonly preRoot: Buffer;
  readonly postRoot: Buffer;
  /** Exact MPF witness against preRoot, consumed by the L1 one-step verifier. */
  readonly proofCbor: Buffer;
};

export type ValidationMachineValueMutationStep = {
  readonly unit: Buffer;
  readonly quantityDelta: bigint;
  readonly oldDelta: bigint | null;
  readonly preAssetRoot: Buffer;
  readonly postAssetRoot: Buffer;
  /** Membership/non-membership witness for unit against preAssetRoot. */
  readonly proofCbor: Buffer;
  readonly postSeenAssetCount: number;
  readonly postNonzeroAssetCount: number;
};

export type ValidationMachineReplayInput = {
  readonly consensusProfile: MidgardConsensusProfileV1;
  readonly eventKeyCbor: Buffer;
  readonly transactionId: Buffer;
  readonly canonicalTransactionCbor: Buffer;
  readonly programMaterialSidecarCbor?: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly priorUtxosRoot: string;
  readonly postUtxosRoot: string;
  readonly ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  readonly expectedLedgerOps: readonly ValidationMachineLedgerOp[];
  readonly ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  readonly expectedVerdict: "accepted" | "rejected";
  readonly expectedRejectionCode: RejectCode | null;
  readonly blockEndTimeMs: number;
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly blockSlot: bigint;
};

const exactTrieRoot = (trie: Trie): Buffer =>
  trie.hash == null ? Buffer.alloc(32) : Buffer.from(trie.hash);

export const buildValidationMachineLedgerMutationSteps = async (input: {
  readonly initialEntries: readonly ValidationMachineLedgerEntry[];
  readonly operations: readonly ValidationMachineLedgerOp[];
}): Promise<readonly ValidationMachineLedgerMutationStep[]> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const entry of [...input.initialEntries].sort((left, right) =>
    Buffer.compare(left.outRef, right.outRef),
  )) {
    await trie.insert(entry.outRef, entry.output);
  }
  const steps: ValidationMachineLedgerMutationStep[] = [];
  for (const operation of input.operations) {
    const preRoot = exactTrieRoot(trie);
    const proofCbor = Buffer.from(
      (await trie.prove(operation.key, operation.type === "insert")).toCBOR(),
    );
    if (operation.type === "delete") {
      await trie.delete(operation.key);
    } else {
      await trie.insert(operation.key, operation.value);
    }
    steps.push({
      operation,
      preRoot,
      postRoot: exactTrieRoot(trie),
      proofCbor,
    });
  }
  return steps;
};

export type ValidationMachineWorkWitness = {
  readonly phase: MidgardValidationPhaseName;
  readonly programCounter: number;
  readonly cbor: Buffer;
  readonly auxiliary:
    | {
        readonly kind: "transactionFieldPreimage";
        readonly preimageCbor: Buffer;
      }
    | {
        readonly kind: "transactionFieldChunk";
        readonly collectionProof: MidgardBoundedCollectionItemProofV1;
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
      }
    | {
        readonly kind: "requiredSignerItem";
        readonly collectionProof: MidgardBoundedCollectionItemProofV1;
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "nativeScriptToken";
        readonly chunkProof: MidgardBoundedItemChunkProofV1;
        readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "nativeScriptFrame";
        readonly frame: ValidationMachineNativeScriptFrameV1;
      }
    | {
        readonly kind: "transactionFieldPairPreimage";
        readonly firstFieldIndex: number;
        readonly firstPreimageCbor: Buffer;
        readonly secondFieldIndex: number;
        readonly secondPreimageCbor: Buffer;
      }
    | {
        readonly kind: "scheduledLedgerLookup";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer | null;
        readonly proofCbor: Buffer;
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "resolvedInputReplay";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer;
      }
    | {
        readonly kind: "outputReplay";
        readonly outputIndex: number;
        readonly outputCbor: Buffer;
        readonly siblings: readonly Buffer[];
        readonly signerProof: ValidationMachineSignerSetProof;
      }
    | {
        readonly kind: "scriptPurposeScan";
        readonly purposeKind: 0 | 1 | 2 | 3;
        readonly purposeIndex: bigint;
        readonly scriptHash: Buffer;
        readonly subject: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "scriptSourceScan";
        readonly sourceIndex: number;
        readonly originKind: "inline" | "reference";
        readonly sourceKey: Buffer;
        readonly script: MidgardVersionedScript;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "redeemerScan";
        readonly redeemerIndex: number;
        readonly redeemer: DecodedMidgardRedeemer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "transactionRedeemerItem";
        readonly collectionProof: MidgardBoundedCollectionItemProofV1;
        readonly redeemer: DecodedMidgardRedeemer;
      }
    | {
        readonly kind: "nativeExecutionScan";
        readonly executionIndex: number;
        readonly languageTag: 0 | 3 | 128;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
        readonly source: {
          readonly sourceIndex: number;
          readonly originKind: "inline" | "reference";
          readonly sourceKey: Buffer;
          readonly script: MidgardVersionedScript;
          readonly siblings: readonly Buffer[];
        };
        readonly redeemerLeaf: Buffer;
        readonly executionSiblings: readonly Buffer[];
        readonly signerHashes: readonly Buffer[];
      }
    | {
        readonly kind: "cekCoreStep";
        readonly step: MidgardCekExecutionStepV1;
      }
    | {
        readonly kind: "cekResolvedContextItem";
        readonly sourceKind: "spend" | "reference";
        readonly itemIndex: number;
        readonly key: Buffer;
        readonly value: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekOutputContextItem";
        readonly outputIndex: number;
        readonly outputCbor: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekSignerContextItem";
        readonly frontier: MidgardValidationMerkleFrontierV1;
        readonly signerIndex: number;
        readonly signerHash: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekMintContextItem";
        readonly mintIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekRedeemerContextSelect";
        readonly control: MidgardCekRedeemerContextControlV1;
        readonly redeemerIndex: number;
        readonly redeemer: DecodedMidgardRedeemer;
        readonly redeemerSiblings: readonly Buffer[];
        readonly purposeFrontierIndex: number;
        readonly purpose: {
          readonly purposeKind: 0 | 1 | 2 | 3;
          readonly purposeIndex: bigint;
          readonly scriptHash: Buffer;
          readonly subject: Buffer;
          readonly siblings: readonly Buffer[];
        };
      }
    | {
        readonly kind: "cekDataScanStep";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1;
        readonly control: MidgardCekDataScanControlV1;
        readonly step: MidgardCekDataScanStepV1;
      }
    | {
        readonly kind: "cekContextFinalize";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1;
      }
    | {
        readonly kind: "cekContextFinalizeSpend";
        readonly redeemerControl: MidgardCekRedeemerContextControlV1;
        readonly itemIndex: number;
        readonly key: Buffer;
        readonly value: Buffer;
        readonly siblings: readonly Buffer[];
      }
    | {
        readonly kind: "cekContextAssemble";
        readonly control: MidgardCekContextPartsControlV1;
      }
    | {
        readonly kind: "cekTxInfoFinalize";
        readonly control: MidgardCekTxInfoAssemblyControlV1;
      }
    | {
        readonly kind: "cekContextSeed";
        readonly control: MidgardCekFinalContextControlV1;
      }
    | {
        readonly kind: "valueInputAsset";
        readonly sourceKind: "spend";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer;
        readonly assetIndex: number;
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "valueOutputAsset";
        readonly outputIndex: number;
        readonly outputCbor: Buffer;
        readonly siblings: readonly Buffer[];
        readonly assetIndex: number;
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "valueMintAsset";
        readonly mintIndex: number;
        readonly policyId: Buffer;
        readonly assetName: Buffer;
        readonly quantity: bigint;
        readonly siblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineValueMutationStep;
      }
    | {
        readonly kind: "ledgerDeltaReplay";
        readonly sourceKind: "spend" | "reference";
        readonly key: Buffer;
        readonly nextScheduleHash: Buffer;
        readonly value: Buffer;
        readonly mutationStep: ValidationMachineLedgerMutationStep | null;
      }
    | {
        readonly kind: "ledgerDeltaOutput";
        readonly outputIndex: number;
        readonly outputCbor: Buffer;
        readonly siblings: readonly Buffer[];
        readonly mutationStep: ValidationMachineLedgerMutationStep;
      }
    | null;
};

export type ValidationMachineSignerSetProof =
  | { readonly kind: "none" }
  | {
      readonly kind: "membership";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly signerIndex: number;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "empty";
      readonly frontier: MidgardValidationMerkleFrontierV1;
    }
  | {
      readonly kind: "belowFirst";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly firstSignerHash: Buffer;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "aboveLast";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly lastSignerHash: Buffer;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "between";
      readonly frontier: MidgardValidationMerkleFrontierV1;
      readonly lowerIndex: number;
      readonly lowerSignerHash: Buffer;
      readonly lowerSiblings: readonly Buffer[];
      readonly upperSignerHash: Buffer;
      readonly upperSiblings: readonly Buffer[];
    };

export type DeterministicValidationMachineTrace = {
  readonly states: readonly MidgardValidationMachineStateV1[];
  readonly witnesses: readonly ValidationMachineWorkWitness[];
  readonly tree: MidgardValidationTraceTree;
  readonly verdict: "accepted" | "rejected";
  readonly rejectionCode: RejectCode | null;
  readonly ledgerOps: readonly ValidationMachineLedgerOp[];
};

const ZERO_32 = Buffer.alloc(32);

const RESOLVED_INPUTS_ACCUMULATOR_DOMAIN = Buffer.from(
  "MidgardResolvedInputsAccumulatorV1",
  "ascii",
);
const INPUT_RESOLUTION_SCHEDULE_DOMAIN = Buffer.from(
  "MidgardInputResolutionScheduleV1",
  "ascii",
);
const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(Buffer.from(bytes), { dkLen: 32 }));

const NATIVE_SCRIPT_SCAN_FRAME_DOMAIN_V1 = Buffer.from(
  "MidgardNativeScriptScanFrameV1",
  "ascii",
);
const MAX_NATIVE_SCRIPT_SCAN_NODES_V1 = 16_384;
const MAX_NATIVE_SCRIPT_SCAN_DEPTH_V1 = 16_384;

type ValidationMachineNativeScriptTokenV1 = {
  readonly kind: 0 | 1 | 2 | 3 | 4 | 5;
  readonly nextOffset: number;
  readonly childCount: number;
  readonly required: bigint;
  readonly keyHash: Buffer;
  readonly slot: bigint;
};

type ValidationMachineNativeScriptTokenHeadV1 = {
  readonly kind: 0 | 1 | 2 | 3 | 4 | 5;
  readonly payloadOffset: number;
};

export type ValidationMachineNativeScriptFrameV1 = {
  readonly tail: Buffer;
  readonly kind: 1 | 2 | 3;
  readonly childCount: number;
  readonly remaining: number;
  readonly validCount: number;
  readonly required: bigint;
};

type ValidationMachineVersionedScriptHeaderV1 = {
  readonly languageTag: 0 | 3 | 128;
  readonly payloadOffset: number;
  readonly payloadLength: number;
};

const readValidationMachineVersionedScriptHeaderV1 = (
  item: Buffer,
): ValidationMachineVersionedScriptHeaderV1 => {
  const outer = readCborArrayHeader(item, 0, "versioned_script");
  if (outer.length !== 2) {
    throw new Error("versioned script must contain exactly two fields");
  }
  const language = readCborUnsigned(
    item,
    outer.nextOffset,
    "versioned_script.language",
  );
  const payload = readCborBytes(
    item,
    language.nextOffset,
    "versioned_script.payload",
  );
  if (
    (language.value !== 0n &&
      language.value !== 3n &&
      language.value !== 128n) ||
    payload.nextOffset !== item.length
  ) {
    throw new Error("versioned script has an invalid language or length");
  }
  return {
    languageTag: Number(language.value) as 0 | 3 | 128,
    payloadOffset: payload.nextOffset - payload.value.length,
    payloadLength: payload.value.length,
  };
};

const readValidationMachineNativeScriptTokenHeadV1 = (
  item: Buffer,
  offset: number,
): ValidationMachineNativeScriptTokenHeadV1 => {
  const outer = readCborArrayHeader(item, offset, "native_script");
  const tag = readCborUnsigned(
    item,
    outer.nextOffset,
    "native_script.tag",
  );
  if (tag.value < 0n || tag.value > 5n) {
    throw new Error("native script has an unsupported tag");
  }
  const kind = Number(tag.value) as 0 | 1 | 2 | 3 | 4 | 5;
  if (
    (kind === 3 && outer.length !== 3) ||
    (kind !== 3 && outer.length !== 2)
  ) {
    throw new Error("native script has an invalid outer shape");
  }
  return { kind, payloadOffset: tag.nextOffset };
};

const readValidationMachineNativeScriptPayloadV1 = (
  item: Buffer,
  offset: number,
  kind: 0 | 1 | 2 | 3 | 4 | 5,
): ValidationMachineNativeScriptTokenV1 => {
  if (kind === 0) {
    const keyHash = readCborBytes(
      item,
      offset,
      "native_script.key_hash",
    );
    if (keyHash.value.length !== 28) {
      throw new Error("native signature script has an invalid shape");
    }
    return {
      kind: 0,
      nextOffset: keyHash.nextOffset,
      childCount: 0,
      required: 0n,
      keyHash: keyHash.value,
      slot: 0n,
    };
  }
  if (kind === 1 || kind === 2) {
    const children = readCborArrayHeader(
      item,
      offset,
      "native_script.children",
    );
    if (children.length > MAX_NATIVE_SCRIPT_SCAN_NODES_V1) {
      throw new Error("native all/any script has an invalid shape");
    }
    return {
      kind,
      nextOffset: children.nextOffset,
      childCount: children.length,
      required: 0n,
      keyHash: Buffer.alloc(0),
      slot: 0n,
    };
  }
  if (kind === 3) {
    const required = readCborUnsigned(
      item,
      offset,
      "native_script.required",
    );
    const children = readCborArrayHeader(
      item,
      required.nextOffset,
      "native_script.children",
    );
    if (children.length > MAX_NATIVE_SCRIPT_SCAN_NODES_V1) {
      throw new Error("native at-least script has an invalid shape");
    }
    return {
      kind: 3,
      nextOffset: children.nextOffset,
      childCount: children.length,
      required: required.value,
      keyHash: Buffer.alloc(0),
      slot: 0n,
    };
  }
  if (kind === 4 || kind === 5) {
    const slot = readCborUnsigned(
      item,
      offset,
      "native_script.slot",
    );
    return {
      kind,
      nextOffset: slot.nextOffset,
      childCount: 0,
      required: 0n,
      keyHash: Buffer.alloc(0),
      slot: slot.value,
    };
  }
  throw new Error("native script payload has an unsupported tag");
};

const validationMachineNativeScriptFrameIsWellFormedV1 = (
  frame: ValidationMachineNativeScriptFrameV1,
): boolean => {
  const processed = frame.childCount - frame.remaining;
  return (
    (frame.tail.length === 0 || frame.tail.length === 32) &&
    frame.childCount > 0 &&
    frame.childCount <= MAX_NATIVE_SCRIPT_SCAN_NODES_V1 &&
    frame.remaining > 0 &&
    frame.remaining <= frame.childCount &&
    frame.validCount >= 0 &&
    frame.validCount <= processed &&
    (frame.kind === 3 ? frame.required >= 0n : frame.required === 0n)
  );
};

const hashValidationMachineNativeScriptFrameV1 = (
  frame: ValidationMachineNativeScriptFrameV1,
): Buffer => {
  if (!validationMachineNativeScriptFrameIsWellFormedV1(frame)) {
    throw new Error("cannot hash a malformed native-script frame");
  }
  return hash32(
    Buffer.concat([
      NATIVE_SCRIPT_SCAN_FRAME_DOMAIN_V1,
      encodeCbor([
        frame.tail,
        BigInt(frame.kind),
        BigInt(frame.childCount),
        BigInt(frame.remaining),
        BigInt(frame.validCount),
        frame.required,
      ]),
    ]),
  );
};

const canonicalCborArgumentHeaderSize = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("canonical CBOR argument must be a non-negative integer");
  }
  if (value < 24) return 1;
  if (value < 0x100) return 2;
  if (value < 0x1_0000) return 3;
  if (value < 0x1_0000_0000) return 5;
  return 9;
};

const canonicalFieldItemEncodedLength = (
  fieldIndex: number,
  itemLength: number,
): number => {
  if ([0, 1, 2, 3, 4, 6].includes(fieldIndex)) {
    return canonicalCborArgumentHeaderSize(itemLength) + itemLength;
  }
  if (fieldIndex === 7 || fieldIndex === 8) return itemLength;
  if (fieldIndex !== 5 || itemLength === 0) {
    throw new Error(
      `invalid canonical field item length at field ${fieldIndex.toString()}`,
    );
  }
  return itemLength - 1;
};

export const initialMidgardResolvedInputsAccumulatorV1 = (): Buffer =>
  hash32(RESOLVED_INPUTS_ACCUMULATOR_DOMAIN);

export const emptyMidgardInputResolutionScheduleV1 = (): Buffer =>
  hash32(INPUT_RESOLUTION_SCHEDULE_DOMAIN);

export const prependMidgardInputResolutionScheduleV1 = (input: {
  readonly sourceKind: "spend" | "reference";
  readonly key: Uint8Array;
  readonly nextHash: Uint8Array;
}): Buffer => {
  if (input.nextHash.length !== 32) {
    throw new Error("input-resolution schedule hash must contain 32 bytes");
  }
  return hash32(
    Buffer.concat([
      INPUT_RESOLUTION_SCHEDULE_DOMAIN,
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(Buffer.from(input.key)),
      Buffer.from(input.nextHash),
    ]),
  );
};

export const advanceMidgardResolvedInputsAccumulatorV1 = (input: {
  readonly accumulator: Uint8Array;
  readonly sourceKind: "spend" | "reference";
  readonly key: Uint8Array;
  readonly value: Uint8Array;
}): Buffer => {
  if (input.accumulator.length !== 32) {
    throw new Error("resolved-input accumulator must contain exactly 32 bytes");
  }
  return hash32(
    Buffer.concat([
      RESOLVED_INPUTS_ACCUMULATOR_DOMAIN,
      Buffer.from(input.accumulator),
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(Buffer.from(input.key)),
      encodeCbor(Buffer.from(input.value)),
    ]),
  );
};

const exactHash32 = (hex: string, field: string): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(hex)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
  return Buffer.from(hex, "hex");
};

const canonicalLedgerOps = (
  operations: readonly ValidationMachineLedgerOp[],
): Buffer =>
  encodeCbor(
    operations.map((operation) =>
      operation.type === "delete"
        ? [0n, operation.key]
        : [1n, operation.key, operation.value],
    ),
  );

const sameLedgerOps = (
  left: readonly ValidationMachineLedgerOp[],
  right: readonly ValidationMachineLedgerOp[],
): boolean => canonicalLedgerOps(left).equals(canonicalLedgerOps(right));

type ValidationValueAccumulator = {
  lovelaceDelta: bigint;
  assetRoot: Buffer;
  seenAssetCount: number;
  nonzeroAssetCount: number;
};

const emptyValidationValueAccumulator = (): ValidationValueAccumulator => ({
  lovelaceDelta: 0n,
  assetRoot: Buffer.alloc(32),
  seenAssetCount: 0,
  nonzeroAssetCount: 0,
});

const encodeValidationValueAccumulator = (
  accumulator: ValidationValueAccumulator,
): Buffer =>
  encodeCbor([
    accumulator.lovelaceDelta,
    accumulator.assetRoot,
    BigInt(accumulator.seenAssetCount),
    BigInt(accumulator.nonzeroAssetCount),
  ]);

type ValidationValueContribution = {
  readonly unit: Buffer;
  readonly quantityDelta: bigint;
};

const midgardValueContributions = (
  value: MidgardValue,
  multiplier: 1n | -1n,
): readonly ValidationValueContribution[] => {
  const contributions: ValidationValueContribution[] = [];
  for (const [policyId, policyAssets] of value.assets) {
    for (const [assetName, quantity] of policyAssets) {
      contributions.push({
        unit: Buffer.from(`${policyId}${assetName}`, "hex"),
        quantityDelta: quantity * multiplier,
      });
    }
  }
  return contributions;
};

const buildValidationValueMutationSteps = async (
  contributions: readonly ValidationValueContribution[],
): Promise<readonly ValidationMachineValueMutationStep[]> => {
  const assetStore = new Store(undefined);
  await assetStore.ready();
  const assetTrie = new Trie(assetStore);
  const deltas = new Map<string, bigint>();
  const steps: ValidationMachineValueMutationStep[] = [];

  for (const contribution of contributions) {
    if (contribution.quantityDelta === 0n) {
      throw new Error("value mutation quantity delta must be non-zero");
    }
    const unit = Buffer.from(contribution.unit);
    const unitHex = unit.toString("hex");
    const oldDelta = deltas.get(unitHex) ?? null;
    const preAssetRoot = exactTrieRoot(assetTrie);
    const proofCbor = Buffer.from(
      (await assetTrie.prove(unit, oldDelta === null)).toCBOR(),
    );
    const nextDelta = (oldDelta ?? 0n) + contribution.quantityDelta;

    if (oldDelta !== null) {
      await assetTrie.delete(unit);
    }
    await assetTrie.insert(unit, encodeCbor(nextDelta));
    deltas.set(unitHex, nextDelta);

    steps.push({
      unit,
      quantityDelta: contribution.quantityDelta,
      oldDelta,
      preAssetRoot,
      postAssetRoot: exactTrieRoot(assetTrie),
      proofCbor,
      postSeenAssetCount: deltas.size,
      postNonzeroAssetCount: [...deltas.values()].filter(
        (quantity) => quantity !== 0n,
      ).length,
    });
  }
  return steps;
};

const applyValidationValueMutationStep = (
  accumulator: ValidationValueAccumulator,
  step: ValidationMachineValueMutationStep,
): void => {
  if (!accumulator.assetRoot.equals(step.preAssetRoot)) {
    throw new Error(
      "value mutation step does not continue the authenticated root",
    );
  }
  accumulator.assetRoot = Buffer.from(step.postAssetRoot);
  accumulator.seenAssetCount = step.postSeenAssetCount;
  accumulator.nonzeroAssetCount = step.postNonzeroAssetCount;
};

const rejectionPhase = (rejection: RejectedTx): MidgardValidationPhaseName => {
  if (rejection.consensusPhase === undefined) {
    throw new Error(
      `V1 rejection ${rejection.code} is missing its exact consensus phase`,
    );
  }
  return rejection.consensusPhase;
};

const orderedPhases: readonly MidgardValidationPhaseName[] = [
  "canonicalDecode",
  "compactBinding",
  "staticLedgerRules",
  "inputSets",
  "signatures",
  "phaseANativeScripts",
  "phaseAScriptPreconditions",
  "resolveInputs",
  "scriptSources",
  "nativeScripts",
  "scriptIntegrity",
  "cek",
  "valueAndMint",
  "ledgerDelta",
];

const safeBlockEndTime = (value: number): bigint => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("blockEndTimeMs must be a non-negative safe integer");
  }
  return BigInt(value);
};

/**
 * Replays the exact Phase A/B implementation and commits every macro-machine
 * witness it consumed. This is deliberately strict: a supplied operator
 * verdict, rejection code, or ledger delta that differs from replay aborts
 * block construction.
 */
export const buildDeterministicValidationMachineTrace = (
  input: ValidationMachineReplayInput,
): Effect.Effect<DeterministicValidationMachineTrace, Error> =>
  Effect.gen(function* () {
    const contextCbor = encodeCbor([
      1n,
      Buffer.from(input.consensusProfile.profileId, "ascii"),
      safeBlockEndTime(input.blockEndTimeMs),
      input.expectedNetworkId,
      input.minFeeA,
      input.minFeeB,
      input.blockSlot,
    ]);
    const validationContextHash = hashMidgardValidationContextV1(contextCbor);
    const priorLedgerRoot = exactHash32(input.priorUtxosRoot, "priorUtxosRoot");
    const postLedgerRoot = exactHash32(input.postUtxosRoot, "postUtxosRoot");
    if (input.transactionId.length !== 32) {
      return yield* Effect.fail(
        new Error("transactionId must contain exactly 32 bytes"),
      );
    }

    const queued: QueuedTx = {
      txId: Buffer.from(input.transactionId),
      txCbor: Buffer.from(input.canonicalTransactionCbor),
      programMaterialSidecarCbor:
        input.programMaterialSidecarCbor === undefined
          ? encodeMidgardCekProgramMaterialSidecarV1([])
          : Buffer.from(input.programMaterialSidecarCbor),
      arrivalSeq: 0n,
      createdAt: new Date(input.blockEndTimeMs),
    };
    const phaseA = validatePhaseASingle(queued, {
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
      consensusProfile: input.consensusProfile,
    });

    const ledgerState = new Map<string, Buffer>();
    for (const entry of input.ledgerWitnessEntries) {
      const outRefHex = entry.outRef.toString("hex");
      if (ledgerState.has(outRefHex)) {
        return yield* Effect.fail(
          new Error(`duplicate ledger witness entry for out-ref ${outRefHex}`),
        );
      }
      ledgerState.set(outRefHex, Buffer.from(entry.output));
    }
    const phaseALedgerTx = "ledgerTx" in phaseA ? phaseA.ledgerTx : null;
    const scriptEvaluations: {
      readonly scriptBytes: Buffer;
      readonly contextCbor: Buffer;
      readonly result: LocalScriptEvalResult;
      readonly graph: MidgardCekExecutionGraphV1 | null;
      readonly execution: MidgardCekStructuralExecutionV1 | null;
    }[] = [];
    const programMaterial = decodeMidgardCekProgramMaterialSidecarV1(
      queued.programMaterialSidecarCbor ??
        encodeMidgardCekProgramMaterialSidecarV1([]),
    );

    let rejection: RejectedTx | null = null;
    let ledgerOps: readonly ValidationMachineLedgerOp[] = [];
    if (!("ledgerTx" in phaseA)) {
      rejection = phaseA;
    } else {
      const phaseB = yield* runPhaseBValidationWithPatch(
        [phaseA],
        ledgerState,
        {
          nowCardanoSlotNo: input.blockSlot,
          bucketConcurrency: 1,
          enforceScriptBudget: true,
          evaluateProofScript: (scriptBytes, scriptContextCbor) =>
            Effect.sync(() => {
              let graph: MidgardCekExecutionGraphV1 | null = null;
              let execution: MidgardCekStructuralExecutionV1 | null = null;
              let result: LocalScriptEvalResult;
              try {
                const envelope = decodeMidgardCekProgramEnvelopeV1(scriptBytes);
                graph = buildMidgardCekExecutionGraphV1(
                  envelope,
                  programMaterial,
                  scriptContextCbor,
                );
                execution = executeMidgardCekStructuralProgramV1({
                  root: graph.root,
                  material: graph.material.values(),
                  constantWitnesses: graph.constantWitnesses,
                  maxSteps:
                    input.consensusProfile.limits.maxValidationMachineStepCount,
                });
                result =
                  execution.terminalState.mode === "haltSuccess"
                    ? {
                        kind: "accepted",
                        budget: {
                          cpu: execution.terminalState.cpu,
                          memory: execution.terminalState.memory,
                        },
                      }
                    : {
                        kind: "script_invalid",
                        detail: `V1 CEK halted with error ${execution.terminalState.auxiliary.toString(10)}`,
                      };
              } catch (cause) {
                result = {
                  kind: "script_invalid",
                  detail: `V1 CEK execution failed closed: ${String(cause)}`,
                };
              }
              scriptEvaluations.push({
                scriptBytes: Buffer.from(scriptBytes),
                contextCbor: Buffer.from(scriptContextCbor),
                result,
                graph,
                execution,
              });
              return result;
            }),
        },
      );
      rejection = phaseB.rejected[0] ?? null;
      if (rejection === null) {
        ledgerOps = [
          ...phaseB.statePatch.deletedOutRefs.map((outRef) => ({
            type: "delete" as const,
            key: Buffer.from(outRef, "hex"),
          })),
          ...phaseB.statePatch.upsertedOutRefs.map(([outRef, output]) => ({
            type: "insert" as const,
            key: Buffer.from(outRef, "hex"),
            value: Buffer.from(output),
          })),
        ];
      }
    }

    const verdict = rejection === null ? "accepted" : "rejected";
    const rejectionCode = rejection?.code ?? null;
    if (
      verdict !== input.expectedVerdict ||
      rejectionCode !== input.expectedRejectionCode
    ) {
      return yield* Effect.fail(
        new Error(
          `validation replay disagrees with operator classification: expected=${input.expectedVerdict}/${input.expectedRejectionCode ?? "none"},actual=${verdict}/${rejectionCode ?? "none"},detail=${rejection?.detail ?? "none"}`,
        ),
      );
    }
    if (!sameLedgerOps(ledgerOps, input.expectedLedgerOps)) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger delta differs from block transition",
        ),
      );
    }
    if (
      input.ledgerMutationSteps.length !== ledgerOps.length ||
      input.ledgerMutationSteps.some(
        (step, index) => !sameLedgerOps([step.operation], [ledgerOps[index]!]),
      )
    ) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation steps differ from the exact ledger delta",
        ),
      );
    }
    let mutationRoot = priorLedgerRoot;
    for (const step of input.ledgerMutationSteps) {
      if (!step.preRoot.equals(mutationRoot)) {
        return yield* Effect.fail(
          new Error(
            "validation replay ledger-mutation roots are not contiguous",
          ),
        );
      }
      mutationRoot = step.postRoot;
    }
    if (!mutationRoot.equals(postLedgerRoot)) {
      return yield* Effect.fail(
        new Error(
          "validation replay ledger-mutation terminal root differs from the block transition",
        ),
      );
    }
    if (
      verdict === "rejected" &&
      (!priorLedgerRoot.equals(postLedgerRoot) || ledgerOps.length !== 0)
    ) {
      return yield* Effect.fail(
        new Error("a rejected transaction must commit an exact ledger no-op"),
      );
    }

    const ledgerDeltaFrontier =
      buildMidgardValidationLedgerDeltaFrontierV1(ledgerOps);
    const ledgerDeltaRoot = hashMidgardValidationLedgerDeltaV1(ledgerOps);
    const proofSource = deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(
      input.canonicalTransactionCbor,
    );
    const compactProofTransaction = decodeMidgardNativeTxCompactV1(
      proofSource.compactCbor,
    );
    const transactionCommitment =
      computeMidgardNativeTxProofCommitmentV1(proofSource);
    const fieldPreimages = deriveMidgardV1TxFieldPreimages(
      input.canonicalTransactionCbor,
    );
    const spendInputsCollection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: 0,
      preimageCbor: fieldPreimages[0]!.preimageCbor,
    });
    const referenceInputsCollection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: 1,
      preimageCbor: fieldPreimages[1]!.preimageCbor,
    });
    const requiredObserversCollection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: 3,
      preimageCbor: fieldPreimages[3]!.preimageCbor,
    });
    const requiredSignersCollection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: 4,
      preimageCbor: fieldPreimages[4]!.preimageCbor,
    });
    const addressWitnessesCollection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: 6,
      preimageCbor: fieldPreimages[6]!.preimageCbor,
    });
    const scriptWitnessesCollection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: 7,
      preimageCbor: fieldPreimages[7]!.preimageCbor,
    });
    const redeemerWitnessesCollection = deriveMidgardNativeFieldCollectionV1({
      fieldIndex: 8,
      preimageCbor: fieldPreimages[8]!.preimageCbor,
    });
    const inputSetScanItems = [
      ...spendInputsCollection.items.map((item) => ({
        sourceKind: "spend" as const,
        collection: spendInputsCollection,
        item,
      })),
      ...referenceInputsCollection.items.map((item) => ({
        sourceKind: "reference" as const,
        collection: referenceInputsCollection,
        item,
      })),
    ].sort((left, right) => Buffer.compare(left.item.bytes, right.item.bytes));
    const resolutionItems = inputSetScanItems.map(({ sourceKind, item }) => ({
      sourceKind,
      key: item.bytes,
    }));
    const resolutionScheduleNodes: {
      sourceKind: "spend" | "reference";
      key: Buffer;
      nextScheduleHash: Buffer;
      scheduleHash: Buffer;
      proofCbor: Buffer;
    }[] = new Array(resolutionItems.length);
    let resolutionScheduleHash = emptyMidgardInputResolutionScheduleV1();
    for (let index = resolutionItems.length - 1; index >= 0; index -= 1) {
      const item = resolutionItems[index]!;
      const nextScheduleHash = resolutionScheduleHash;
      resolutionScheduleHash = prependMidgardInputResolutionScheduleV1({
        sourceKind: item.sourceKind,
        key: item.key,
        nextHash: nextScheduleHash,
      });
      resolutionScheduleNodes[index] = {
        ...item,
        nextScheduleHash,
        scheduleHash: resolutionScheduleHash,
        proofCbor: Buffer.alloc(0),
      };
    }
    const resolutionProofs = yield* Effect.tryPromise({
      try: async () => {
        const store = new Store(undefined);
        await store.ready();
        const trie = new Trie(store);
        for (const entry of [...input.ledgerWitnessEntries].sort(
          (left, right) => Buffer.compare(left.outRef, right.outRef),
        )) {
          await trie.insert(entry.outRef, entry.output);
        }
        return await Promise.all(
          resolutionScheduleNodes.map(async (node) =>
            Buffer.from(
              (
                await trie.prove(
                  node.key,
                  !ledgerState.has(node.key.toString("hex")),
                )
              ).toCBOR(),
            ),
          ),
        );
      },
      catch: (cause) =>
        cause instanceof Error
          ? cause
          : new Error("failed to build input-resolution MPF witnesses"),
    });
    for (let index = 0; index < resolutionScheduleNodes.length; index += 1) {
      resolutionScheduleNodes[index]!.proofCbor = resolutionProofs[index]!;
    }
    const transactionContextWitnessCbor = encodeCbor([
      input.canonicalTransactionCbor,
      contextCbor,
    ]);
    const sourceContextWitnessCbor = encodeCbor([
      proofSource.compactCbor,
      proofSource.witnessSetCompactCbor,
      proofSource.fieldPreimageLengthsCbor,
      contextCbor,
    ]);
    const inputSetsWitnessCbor = (control: {
      readonly spendCount: number;
      readonly referenceCount: number;
      readonly spendSeen: number;
      readonly referenceSeen: number;
      readonly previousKey: Buffer;
      readonly resolutionScheduleHash: Buffer;
    }): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(control.spendCount),
        BigInt(control.referenceCount),
        BigInt(control.spendSeen),
        BigInt(control.referenceSeen),
        control.previousKey,
        control.resolutionScheduleHash,
      ]);
    const decodeAddressWitnessItem = (
      witnessCbor: Buffer,
    ): {
      readonly verificationKey: Buffer;
      readonly signature: Buffer;
      readonly signerHash: Buffer;
    } => {
      const header = readCborArrayHeader(
        witnessCbor,
        0,
        "address_witness",
      );
      if (header.length !== 2) {
        throw new Error("address witness must contain [vkey, signature]");
      }
      const verificationKey = readCborBytes(
        witnessCbor,
        header.nextOffset,
        "address_witness.vkey",
      );
      const signature = readCborBytes(
        witnessCbor,
        verificationKey.nextOffset,
        "address_witness.signature",
      );
      if (
        verificationKey.value.length !== 32 ||
        signature.value.length !== 64 ||
        signature.nextOffset !== witnessCbor.length
      ) {
        throw new Error("address witness has a non-canonical shape");
      }
      return {
        verificationKey: verificationKey.value,
        signature: signature.value,
        signerHash: Buffer.from(
          blake2b(verificationKey.value, { dkLen: 28 }),
        ),
      };
    };
    const addressWitnessScanItems = addressWitnessesCollection.items
      .map((item) => {
        const decoded = decodeAddressWitnessItem(item.bytes);
        return {
          item,
          decoded,
          orderKey: Buffer.concat([
            decoded.signerHash,
            item.bytes,
            encodeCbor(BigInt(item.itemIndex)),
          ]),
        };
      })
      .sort((left, right) => Buffer.compare(left.orderKey, right.orderKey));
    const canonicalSignerHashes = addressWitnessScanItems
      .map(({ decoded }) => decoded.signerHash)
      .sort(Buffer.compare)
      .filter(
        (hash, index, hashes) =>
          index === 0 || !hash.equals(hashes[index - 1]!),
      );
    const signerLeafHashes = canonicalSignerHashes.map((signerHash) =>
      hashMidgardSignerLeafV1(signerHash),
    );
    const signerFrontier =
      buildMidgardValidationMerkleFrontierV1(signerLeafHashes);
    const signerFrontierCommitment =
      commitMidgardValidationMerkleFrontierV1(signerFrontier);
    type ScriptSourceProofEntry = {
      readonly originKind: "inline" | "reference";
      readonly sourceKey: Buffer;
      readonly script: MidgardVersionedScript;
      readonly leaf: Buffer;
    };
    type ScriptPurposeProofEntry = {
      readonly purposeKind: 0 | 1 | 2 | 3;
      readonly purposeIndex: bigint;
      readonly scriptHash: Buffer;
      readonly subject: Buffer;
      readonly leaf: Buffer;
    };
    type ScriptExecutionProofEntry = {
      readonly purpose: ScriptPurposeProofEntry;
      readonly source: ScriptSourceProofEntry;
      readonly sourceIndex: number;
      readonly languageTag: 0 | 3 | 128;
      readonly redeemerLeaf: Buffer;
      readonly leaf: Buffer;
    };
    const scriptSourceEntries: ScriptSourceProofEntry[] = (
      phaseALedgerTx?.scriptWitnesses ?? []
    ).map((witness) => {
      const sourceKey = encodeCbor(BigInt(witness.index));
      return {
        originKind: "inline",
        sourceKey,
        script: witness.script,
        leaf: hashMidgardScriptSourceLeafV1({
          originKind: "inline",
          sourceKey,
          script: witness.script,
        }),
      };
    });
    const inlineScriptSourceLeafHashes = scriptSourceEntries.map(
      (entry) => entry.leaf,
    );
    const scriptPurposeEntries: ScriptPurposeProofEntry[] = [];
    const scriptExecutionEntries: ScriptExecutionProofEntry[] = [];
    const inlineScriptSourceFrontier = buildMidgardValidationMerkleFrontierV1(
      inlineScriptSourceLeafHashes,
    );
    const outputCbors = decodeMidgardNativeByteListPreimage(
      fieldPreimages[2]!.preimageCbor,
      "v1.outputs",
    );
    const outputLeafHashes = outputCbors.map((outputCbor, outputIndex) =>
      hashMidgardOutputLeafV1({ outputIndex, outputCbor }),
    );
    const outputFrontier =
      buildMidgardValidationMerkleFrontierV1(outputLeafHashes);
    const outputMembership = (outputIndex: number) =>
      buildMidgardValidationMerkleMembershipV1(outputLeafHashes, outputIndex);
    const decodedProofRedeemers = decodeMidgardRedeemers(
      fieldPreimages[8]!.preimageCbor,
    );
    const canonicalRedeemerWitnessCbors = decodedProofRedeemers.map(
      (redeemer) =>
        encodeCbor([
          BigInt(redeemer.tag),
          redeemer.index,
          Buffer.from(redeemer.dataCborHex, "hex"),
          [redeemer.exUnits.memory, redeemer.exUnits.steps],
        ]),
    );
    const redeemerLeafHashes = canonicalRedeemerWitnessCbors.map(
      (canonicalRedeemerWitnessCbor, redeemerIndex) =>
        hashMidgardRedeemerLeafV1({
          redeemerIndex,
          canonicalRedeemerWitnessCbor,
        }),
    );
    const redeemerFrontier = buildMidgardValidationMerkleFrontierV1(
      redeemerLeafHashes,
    );
    const encodeFrontierPeaks = (
      frontier: MidgardValidationMerkleFrontierV1,
    ): readonly (readonly [bigint, Buffer])[] =>
      frontier.peaks.map((peak) => [BigInt(peak.height), peak.hash]);
    const emptyValidationFrontier = buildMidgardValidationMerkleFrontierV1([]);
    type SignatureScanControl = {
      readonly stage: 0 | 1 | 2;
      readonly addressCount: number;
      readonly requiredCount: number;
      readonly addressSeen: number;
      readonly requiredSeen: number;
      readonly previousOrderKey: Buffer;
      readonly previousSignerHash: Buffer;
      readonly signerFrontier: MidgardValidationMerkleFrontierV1;
      readonly invalidSignatureSeen: 0 | 1;
    };
    const signaturesScanWitnessCbor = (
      control: SignatureScanControl,
    ): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(control.stage),
        BigInt(control.addressCount),
        BigInt(control.requiredCount),
        BigInt(control.addressSeen),
        BigInt(control.requiredSeen),
        control.previousOrderKey,
        control.previousSignerHash,
        BigInt(control.signerFrontier.count),
        encodeFrontierPeaks(control.signerFrontier),
        BigInt(control.invalidSignatureSeen),
      ]);
    const initialSignatureScanControl: SignatureScanControl = {
      stage: 0,
      addressCount:
        addressWitnessesCollection.items.length === 0 ? 0 : -1,
      requiredCount:
        requiredSignersCollection.items.length === 0 ? 0 : -1,
      addressSeen: 0,
      requiredSeen: 0,
      previousOrderKey: Buffer.alloc(0),
      previousSignerHash: Buffer.alloc(0),
      signerFrontier: emptyValidationFrontier,
      invalidSignatureSeen: 0,
    };
    type PhaseANativeScriptsScanControl = {
      readonly stage: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8;
      readonly scriptCount: number;
      readonly scriptSeen: number;
      readonly containsNonNativeScript: 0 | 1;
      readonly itemLength: number;
      readonly itemCommitment: Buffer;
      readonly cursor: number;
      readonly stackRoot: Buffer;
      readonly stackDepth: number;
      readonly nodeCount: number;
      readonly result: -1 | 0 | 1;
    };
    const phaseANativeScriptsScanWitnessCbor = (
      control: PhaseANativeScriptsScanControl,
    ): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(control.stage),
        BigInt(control.scriptCount),
        BigInt(control.scriptSeen),
        BigInt(control.containsNonNativeScript),
        BigInt(control.itemLength),
        control.itemCommitment,
        BigInt(control.cursor),
        control.stackRoot,
        BigInt(control.stackDepth),
        BigInt(control.nodeCount),
        BigInt(control.result),
        BigInt(signerFrontier.count),
        encodeFrontierPeaks(signerFrontier),
      ]);
    const resetPhaseANativeScriptsScanControl = (input: {
      readonly scriptCount: number;
      readonly scriptSeen: number;
      readonly containsNonNativeScript: 0 | 1;
    }): PhaseANativeScriptsScanControl => ({
      stage: 0,
      scriptCount: input.scriptCount,
      scriptSeen: input.scriptSeen,
      containsNonNativeScript: input.containsNonNativeScript,
      itemLength: 0,
      itemCommitment: Buffer.alloc(0),
      cursor: 0,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: 0,
      result: -1,
    });
    const initialPhaseANativeScriptsScanControl =
      resetPhaseANativeScriptsScanControl({
        scriptCount:
          scriptWitnessesCollection.items.length === 0 ? 0 : -1,
        scriptSeen: 0,
        containsNonNativeScript: 0,
      });
    let resolvedItemFrontier = emptyValidationFrontier;
    type ScriptDiscoveryTraceControl = {
      readonly purposeCursor: number;
      readonly sourceCursor: number;
      readonly redeemerCursor: number;
      readonly currentPurposeKind: -1 | 0 | 1 | 2 | 3;
      readonly currentPurposeIndex: bigint;
      readonly currentScriptHash: Buffer;
      readonly currentSubject: Buffer;
      readonly matchedSourceIndex: number;
      readonly matchedLanguageTag: -1 | 0 | 3 | 128;
      readonly matchedSourceLeaf: Buffer;
      readonly usedInlineBitmap: bigint;
      readonly usedRedeemerBitmap: bigint;
      readonly executionFrontier: MidgardValidationMerkleFrontierV1;
    };
    const emptyScriptDiscoveryControl: ScriptDiscoveryTraceControl = {
      purposeCursor: 0,
      sourceCursor: 0,
      redeemerCursor: 0,
      currentPurposeKind: -1,
      currentPurposeIndex: -1n,
      currentScriptHash: Buffer.alloc(0),
      currentSubject: Buffer.alloc(0),
      matchedSourceIndex: -1,
      matchedLanguageTag: -1,
      matchedSourceLeaf: Buffer.alloc(0),
      usedInlineBitmap: 0n,
      usedRedeemerBitmap: 0n,
      executionFrontier: emptyValidationFrontier,
    };
    const scriptDiscoveryControlCbor = (
      discovery: ScriptDiscoveryTraceControl,
    ): Buffer =>
      encodeCbor([
        BigInt(discovery.purposeCursor),
        BigInt(discovery.sourceCursor),
        BigInt(discovery.redeemerCursor),
        BigInt(discovery.currentPurposeKind),
        discovery.currentPurposeIndex,
        discovery.currentScriptHash,
        discovery.currentSubject,
        BigInt(discovery.matchedSourceIndex),
        BigInt(discovery.matchedLanguageTag),
        discovery.matchedSourceLeaf,
        discovery.usedInlineBitmap,
        discovery.usedRedeemerBitmap,
        BigInt(discovery.executionFrontier.count),
        encodeFrontierPeaks(discovery.executionFrontier),
      ]);
    const scriptSourcesWitnessCbor = (input: {
      readonly resolvedInputCount: number;
      readonly resolvedInputsAccumulator: Buffer;
      readonly stage: number;
      readonly sourceFrontier: MidgardValidationMerkleFrontierV1;
      readonly redeemerFrontier: MidgardValidationMerkleFrontierV1;
      readonly replayCursor?: number;
      readonly replayAccumulator?: Buffer;
      readonly replayRemainingScheduleHash?: Buffer;
      readonly spendIndex?: number;
      readonly purposeFrontier?: MidgardValidationMerkleFrontierV1;
      readonly outputCursor?: number;
      readonly outputFrontier?: MidgardValidationMerkleFrontierV1;
      readonly receiveHashes?: readonly Buffer[];
      readonly sourceTotalCount?: number;
      readonly redeemerTotalCount?: number;
      readonly discovery?: ScriptDiscoveryTraceControl;
    }): Buffer => {
      const fields: unknown[] = [
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        BigInt(input.resolvedInputCount),
        input.resolvedInputsAccumulator,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        encodeFrontierPeaks(resolvedItemFrontier),
        BigInt(input.stage),
        BigInt(input.sourceFrontier.count),
        encodeFrontierPeaks(input.sourceFrontier),
        BigInt(input.redeemerFrontier.count),
        encodeFrontierPeaks(input.redeemerFrontier),
        BigInt(input.replayCursor ?? 0),
        input.replayAccumulator ?? initialMidgardResolvedInputsAccumulatorV1(),
        input.replayRemainingScheduleHash ??
          emptyMidgardInputResolutionScheduleV1(),
        BigInt(input.spendIndex ?? 0),
        BigInt(input.purposeFrontier?.count ?? 0),
        encodeFrontierPeaks(input.purposeFrontier ?? emptyValidationFrontier),
        BigInt(input.outputCursor ?? 0),
        BigInt(input.outputFrontier?.count ?? 0),
        encodeFrontierPeaks(input.outputFrontier ?? emptyValidationFrontier),
        input.receiveHashes ?? [],
        BigInt(input.sourceTotalCount ?? input.sourceFrontier.count),
        BigInt(input.redeemerTotalCount ?? input.redeemerFrontier.count),
      ];
      if (input.stage >= 8) {
        fields.push(
          scriptDiscoveryControlCbor(
            input.discovery ?? emptyScriptDiscoveryControl,
          ),
        );
      }
      return encodeCbor(fields);
    };
    const signerMembership = (signerIndex: number) =>
      buildMidgardValidationMerkleMembershipV1(signerLeafHashes, signerIndex);
    const signerProofForHash = (
      signerHash: Buffer,
    ): ValidationMachineSignerSetProof => {
      const insertionIndex = canonicalSignerHashes.findIndex(
        (candidate) => Buffer.compare(candidate, signerHash) >= 0,
      );
      if (
        insertionIndex >= 0 &&
        canonicalSignerHashes[insertionIndex]!.equals(signerHash)
      ) {
        return {
          kind: "membership",
          frontier: signerFrontier,
          signerIndex: insertionIndex,
          siblings: signerMembership(insertionIndex).siblings,
        };
      }
      if (canonicalSignerHashes.length === 0) {
        return { kind: "empty", frontier: signerFrontier };
      }
      if (insertionIndex === 0) {
        return {
          kind: "belowFirst",
          frontier: signerFrontier,
          firstSignerHash: canonicalSignerHashes[0]!,
          siblings: signerMembership(0).siblings,
        };
      }
      if (insertionIndex === -1) {
        const lastIndex = canonicalSignerHashes.length - 1;
        return {
          kind: "aboveLast",
          frontier: signerFrontier,
          lastSignerHash: canonicalSignerHashes[lastIndex]!,
          siblings: signerMembership(lastIndex).siblings,
        };
      }
      return {
        kind: "between",
        frontier: signerFrontier,
        lowerIndex: insertionIndex - 1,
        lowerSignerHash: canonicalSignerHashes[insertionIndex - 1]!,
        lowerSiblings: signerMembership(insertionIndex - 1).siblings,
        upperSignerHash: canonicalSignerHashes[insertionIndex]!,
        upperSiblings: signerMembership(insertionIndex).siblings,
      };
    };
    const signerSetProof = (
      sourceKind: "spend" | "reference",
      value: Buffer | null,
    ): ValidationMachineSignerSetProof => {
      if (sourceKind === "reference" || value === null) {
        return { kind: "none" };
      }
      let signerHash: Buffer;
      try {
        const output = decodeMidgardTxOutput(value);
        const credential = decodeMidgardAddressBytes(
          output.address,
        ).paymentCredential;
        if (credential.kind === "Script") return { kind: "none" };
        signerHash = Buffer.from(credential.hash);
      } catch {
        return { kind: "none" };
      }
      return signerProofForHash(signerHash);
    };
    const protectedOutputSignerProof = (
      outputCbor: Buffer,
    ): ValidationMachineSignerSetProof => {
      const output = decodeMidgardTxOutput(outputCbor);
      const address = decodeMidgardAddressBytes(output.address);
      if (!address.protected || address.paymentCredential.kind === "Script") {
        return { kind: "none" };
      }
      return signerProofForHash(Buffer.from(address.paymentCredential.hash));
    };
    const phaseAScriptPreconditionsWitnessCbor = (
      control: {
        readonly containsNonNativeScript: 0 | 1;
        readonly observerCount: number;
        readonly observerSeen: number;
        readonly previousObserver: Buffer;
      },
    ): Buffer =>
      encodeCbor([
        proofSource.compactCbor,
        proofSource.witnessSetCompactCbor,
        proofSource.fieldPreimageLengthsCbor,
        contextCbor,
        resolutionScheduleHash,
        BigInt(signerFrontier.count),
        signerFrontierCommitment,
        BigInt(control.containsNonNativeScript),
        control.previousObserver,
        BigInt(control.observerCount),
        BigInt(control.observerSeen),
      ]);
    const macroWitnessByPhase = new Map<MidgardValidationPhaseName, Buffer>([
      [
        "compactBinding",
        encodeCbor([
          input.transactionId,
          transactionCommitment,
          proofSource.compactCbor,
          proofSource.witnessSetCompactCbor,
          proofSource.fieldPreimageLengthsCbor,
          contextCbor,
        ]),
      ],
      ["staticLedgerRules", sourceContextWitnessCbor],
      ["valueAndMint", transactionContextWitnessCbor],
      ["nativeScripts", transactionContextWitnessCbor],
      ["scriptIntegrity", transactionContextWitnessCbor],
      ["cek", transactionContextWitnessCbor],
      ["ledgerDelta", transactionContextWitnessCbor],
    ]);
    const macroAuxiliaryByPhase = new Map<
      MidgardValidationPhaseName,
      ValidationMachineWorkWitness["auxiliary"]
    >([
    ]);

    const terminalPhase =
      rejection === null ? "ledgerDelta" : rejectionPhase(rejection);
    const stopIndex = orderedPhases.indexOf(terminalPhase);
    if (stopIndex < 0) {
      return yield* Effect.fail(
        new Error(`unknown validation terminal phase ${terminalPhase}`),
      );
    }
    const witnesses: ValidationMachineWorkWitness[] = [];
    const witnessExecutionBudgets: {
      readonly cpu: bigint;
      readonly memory: bigint;
    }[] = [];
    let traceExecutionCpu = 0n;
    let traceExecutionMemory = 0n;
    const pushWitness = (
      phase: MidgardValidationPhaseName,
      cbor: Buffer,
      auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
    ): void => {
      witnesses.push({
        phase,
        programCounter: witnesses.length,
        cbor,
        auxiliary,
      });
      witnessExecutionBudgets.push({
        cpu: traceExecutionCpu,
        memory: traceExecutionMemory,
      });
    };
    const macroWitness = (phase: MidgardValidationPhaseName): Buffer => {
      const witness = macroWitnessByPhase.get(phase);
      if (witness === undefined) {
        throw new Error(`missing macro witness for ${phase}`);
      }
      return witness;
    };
    const macroAuxiliary = (
      phase: MidgardValidationPhaseName,
    ): ValidationMachineWorkWitness["auxiliary"] =>
      macroAuxiliaryByPhase.get(phase) ?? null;

    let stoppedAtRejection = false;
    let authenticatedNativeScriptsWitnessCbor: Buffer | null = null;
    let authenticatedNativeScriptsBaseFields: unknown[] | null = null;
    for (const field of fieldPreimages) {
      const collection = deriveMidgardNativeFieldCollectionV1({
        fieldIndex: field.fieldIndex,
        preimageCbor: field.preimageCbor,
      });
      if (collection.items.length === 0) {
        pushWitness(
          "canonicalDecode",
          encodeCbor([
            proofSource.compactCbor,
            proofSource.witnessSetCompactCbor,
            proofSource.fieldPreimageLengthsCbor,
            contextCbor,
            BigInt(field.fieldIndex),
            0n,
            0n,
            -1n,
            0n,
          ]),
        );
        continue;
      }
      let itemCount = -1;
      let encodedLength = 0;
      for (const item of collection.items) {
        const collectionProof =
          buildMidgardBoundedCollectionItemProofV1(
            collection,
            item.itemIndex,
          );
        const chunkCount = midgardBoundedItemChunkCountV1(item.bytes.length);
        for (let chunkIndex = 0; chunkIndex < chunkCount; chunkIndex += 1) {
          pushWitness(
            "canonicalDecode",
            encodeCbor([
              proofSource.compactCbor,
              proofSource.witnessSetCompactCbor,
              proofSource.fieldPreimageLengthsCbor,
              contextCbor,
              BigInt(field.fieldIndex),
              BigInt(item.itemIndex),
              BigInt(chunkIndex),
              BigInt(itemCount),
              BigInt(encodedLength),
            ]),
            {
              kind: "transactionFieldChunk",
              collectionProof,
              chunkProof: buildMidgardBoundedItemChunkProofV1(
                item,
                chunkIndex,
              ),
            },
          );
          if (itemCount === -1) {
            itemCount = collection.items.length;
            encodedLength = canonicalCborArgumentHeaderSize(itemCount);
          }
          if (chunkIndex + 1 === chunkCount) {
            encodedLength += canonicalFieldItemEncodedLength(
              field.fieldIndex,
              item.bytes.length,
            );
          }
        }
      }
    }
    if (
      rejection !== null &&
      terminalPhase === "canonicalDecode"
    ) {
      return yield* Effect.fail(
        new Error(
          `V1 canonical rejection ${rejection.code} is not representable by the bounded canonical source`,
        ),
      );
    }
    for (const phase of ["compactBinding", "staticLedgerRules"] as const) {
      if (stoppedAtRejection) break;
      pushWitness(phase, macroWitness(phase), macroAuxiliary(phase));
      if (rejection !== null && phase === terminalPhase) {
        stoppedAtRejection = true;
        break;
      }
    }

    if (!stoppedAtRejection) {
      let spendCount = spendInputsCollection.items.length === 0 ? 0 : -1;
      let referenceCount =
        referenceInputsCollection.items.length === 0 ? 0 : -1;
      let spendSeen = 0;
      let referenceSeen = 0;
      let previousKey = Buffer.alloc(0);
      let inputScheduleHash = emptyMidgardInputResolutionScheduleV1();
      const currentInputSetsWitness = (): Buffer =>
        inputSetsWitnessCbor({
          spendCount,
          referenceCount,
          spendSeen,
          referenceSeen,
          previousKey,
          resolutionScheduleHash: inputScheduleHash,
        });

      if (spendCount === 0) {
        pushWitness("inputSets", currentInputSetsWitness());
        if (
          terminalPhase !== "inputSets" ||
          rejectionCode !== RejectCodes.EmptyInputs
        ) {
          return yield* Effect.fail(
            new Error(
              `bounded input scan found no spend inputs but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
            ),
          );
        }
        stoppedAtRejection = true;
      } else {
        for (
          let index = inputSetScanItems.length - 1;
          index >= 0;
          index -= 1
        ) {
          const scan = inputSetScanItems[index]!;
          const key = scan.item.bytes;
          pushWitness("inputSets", currentInputSetsWitness(), {
            kind: "transactionFieldChunk",
            collectionProof: buildMidgardBoundedCollectionItemProofV1(
              scan.collection,
              scan.item.itemIndex,
            ),
            chunkProof: buildMidgardBoundedItemChunkProofV1(scan.item, 0),
          });
          if (previousKey.length > 0 && key.equals(previousKey)) {
            if (
              terminalPhase !== "inputSets" ||
              rejectionCode !== RejectCodes.DuplicateInputInTx
            ) {
              return yield* Effect.fail(
                new Error(
                  `bounded input scan found a duplicate but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          if (previousKey.length > 0 && Buffer.compare(key, previousKey) >= 0) {
            return yield* Effect.fail(
              new Error("bounded input scan is not strictly descending"),
            );
          }
          if (scan.sourceKind === "spend") {
            if (spendCount === -1) {
              spendCount = scan.collection.items.length;
            }
            spendSeen += 1;
          } else {
            if (referenceCount === -1) {
              referenceCount = scan.collection.items.length;
            }
            referenceSeen += 1;
          }
          previousKey = key;
          inputScheduleHash = prependMidgardInputResolutionScheduleV1({
            sourceKind: scan.sourceKind,
            key,
            nextHash: inputScheduleHash,
          });
        }
        if (!stoppedAtRejection) {
          if (spendCount <= 0 || referenceCount < 0) {
            return yield* Effect.fail(
              new Error("bounded input scan did not reveal both input counts"),
            );
          }
          if (
            spendSeen !== spendCount ||
            referenceSeen !== referenceCount
          ) {
            return yield* Effect.fail(
              new Error("bounded input scan did not reveal every input"),
            );
          }
          if (!inputScheduleHash.equals(resolutionScheduleHash)) {
            return yield* Effect.fail(
              new Error(
                `bounded input scan schedule ${inputScheduleHash.toString("hex")} differs from committed ${resolutionScheduleHash.toString("hex")}`,
              ),
            );
          }
          if (terminalPhase === "inputSets") {
            if (
              rejectionCode !== RejectCodes.InvalidValidityIntervalFormat
            ) {
              return yield* Effect.fail(
                new Error(
                  `bounded input scan cannot prove rejection ${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
          }
        }
      }
    }

    if (!stoppedAtRejection) {
      let signatureControl = initialSignatureScanControl;
      const pushSignatureWitness = (
        auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
      ): void => {
        pushWitness(
          "signatures",
          signaturesScanWitnessCbor(signatureControl),
          auxiliary,
        );
      };
      if (signatureControl.addressCount === 0) {
        pushSignatureWitness();
        signatureControl = { ...signatureControl, stage: 1 };
      } else {
        for (
          let index = 0;
          index < addressWitnessScanItems.length;
          index += 1
        ) {
          const scan = addressWitnessScanItems[index]!;
          pushSignatureWitness({
            kind: "transactionFieldChunk",
            collectionProof: buildMidgardBoundedCollectionItemProofV1(
              addressWitnessesCollection,
              scan.item.itemIndex,
            ),
            chunkProof: buildMidgardBoundedItemChunkProofV1(scan.item, 0),
          });
          if (
            signatureControl.previousOrderKey.length > 0 &&
            Buffer.compare(
              signatureControl.previousOrderKey,
              scan.orderKey,
            ) >= 0
          ) {
            return yield* Effect.fail(
              new Error("address-witness scan is not strictly ordered"),
            );
          }
          const newSigner =
            !scan.decoded.signerHash.equals(
              signatureControl.previousSignerHash,
            );
          const signerFrontier = newSigner
            ? appendMidgardValidationMerkleLeafV1(
                signatureControl.signerFrontier,
                hashMidgardSignerLeafV1(scan.decoded.signerHash),
              )
            : signatureControl.signerFrontier;
          let signatureIsValid = false;
          try {
            const publicKey = CML.PublicKey.from_bytes(
              scan.decoded.verificationKey,
            );
            const signature = CML.Ed25519Signature.from_raw_bytes(
              scan.decoded.signature,
            );
            try {
              signatureIsValid = publicKey.verify(
                input.transactionId,
                signature,
              );
            } finally {
              publicKey.free();
              signature.free();
            }
          } catch {
            signatureIsValid = false;
          }
          const addressSeen = signatureControl.addressSeen + 1;
          const addressCount =
            signatureControl.addressCount === -1
              ? addressWitnessesCollection.items.length
              : signatureControl.addressCount;
          signatureControl =
            addressSeen === addressCount
              ? {
                  ...signatureControl,
                  stage: 1,
                  addressCount,
                  addressSeen,
                  previousOrderKey: Buffer.alloc(0),
                  previousSignerHash: Buffer.alloc(0),
                  signerFrontier,
                  invalidSignatureSeen:
                    signatureControl.invalidSignatureSeen === 1 ||
                    !signatureIsValid
                      ? 1
                      : 0,
                }
              : {
                  ...signatureControl,
                  addressCount,
                  addressSeen,
                  previousOrderKey: scan.orderKey,
                  previousSignerHash: scan.decoded.signerHash,
                  signerFrontier,
                  invalidSignatureSeen:
                    signatureControl.invalidSignatureSeen === 1 ||
                    !signatureIsValid
                      ? 1
                      : 0,
                };
        }
      }
      if (signatureControl.stage !== 1) {
        return yield* Effect.fail(
          new Error("address-witness scan did not reach required signers"),
        );
      }
      if (signatureControl.requiredCount === 0) {
        pushSignatureWitness();
        if (signatureControl.invalidSignatureSeen === 1) {
          if (
            terminalPhase !== "signatures" ||
            rejectionCode !== RejectCodes.InvalidSignature
          ) {
            return yield* Effect.fail(
              new Error(
                `signature scan found an invalid signature but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
              ),
            );
          }
          stoppedAtRejection = true;
        } else {
          signatureControl = { ...signatureControl, stage: 2 };
        }
      } else {
        for (
          let index = 0;
          index < requiredSignersCollection.items.length;
          index += 1
        ) {
          const item = requiredSignersCollection.items[index]!;
          const signerProof = signerProofForHash(item.bytes);
          pushSignatureWitness({
            kind: "requiredSignerItem",
            collectionProof: buildMidgardBoundedCollectionItemProofV1(
              requiredSignersCollection,
              item.itemIndex,
            ),
            chunkProof: buildMidgardBoundedItemChunkProofV1(item, 0),
            signerProof,
          });
          if (signerProof.kind !== "membership") {
            if (
              terminalPhase !== "signatures" ||
              rejectionCode !== RejectCodes.MissingRequiredWitness
            ) {
              return yield* Effect.fail(
                new Error(
                  `required signer is absent but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          const requiredSeen = signatureControl.requiredSeen + 1;
          const requiredCount =
            signatureControl.requiredCount === -1
              ? requiredSignersCollection.items.length
              : signatureControl.requiredCount;
          signatureControl = {
            ...signatureControl,
            requiredCount,
            requiredSeen,
          };
          if (
            requiredSeen === requiredCount &&
            signatureControl.invalidSignatureSeen === 1
          ) {
            if (
              terminalPhase !== "signatures" ||
              rejectionCode !== RejectCodes.InvalidSignature
            ) {
              return yield* Effect.fail(
                new Error(
                  `signature scan found an invalid signature but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          if (requiredSeen === requiredCount) {
            signatureControl = { ...signatureControl, stage: 2 };
          }
        }
      }
      if (!stoppedAtRejection) {
        if (signatureControl.stage !== 2) {
          return yield* Effect.fail(
            new Error("required-signer scan did not reach its handoff"),
          );
        }
        pushSignatureWitness();
        if (terminalPhase === "signatures") {
          return yield* Effect.fail(
            new Error(
              `signature scan cannot prove rejection ${rejectionCode ?? "none"}`,
            ),
          );
        }
      }
    }

    let phaseANativeControl = initialPhaseANativeScriptsScanControl;
    if (!stoppedAtRejection) {
      const nativeScriptFrames: ValidationMachineNativeScriptFrameV1[] = [];
      const expectedPhaseANativeRejection = (code: RejectCode): boolean =>
        rejection !== null &&
        terminalPhase === "phaseANativeScripts" &&
        rejectionCode === code;
      const failUnexpectedPhaseANativeRejection = (
        actual: RejectCode,
      ): Effect.Effect<never, Error> =>
        Effect.fail(
          new Error(
            `bounded native-script scan found ${actual} at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor} but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
          ),
        );
      const pushPhaseANativeWitness = (
        auxiliary: ValidationMachineWorkWitness["auxiliary"] = null,
      ): void => {
        pushWitness(
          "phaseANativeScripts",
          phaseANativeScriptsScanWitnessCbor(phaseANativeControl),
          auxiliary,
        );
      };

      if (phaseANativeControl.scriptCount === 0) {
        pushPhaseANativeWitness();
      } else {
        for (const item of scriptWitnessesCollection.items) {
          const activeScriptCount =
            phaseANativeControl.scriptCount === -1
              ? scriptWitnessesCollection.items.length
              : phaseANativeControl.scriptCount;
          pushPhaseANativeWitness({
            kind: "transactionFieldChunk",
            collectionProof: buildMidgardBoundedCollectionItemProofV1(
              scriptWitnessesCollection,
              item.itemIndex,
            ),
            chunkProof: buildMidgardBoundedItemChunkProofV1(item, 0),
          });

          let header: ValidationMachineVersionedScriptHeaderV1;
          try {
            header = readValidationMachineVersionedScriptHeaderV1(
              item.bytes,
            );
          } catch {
            if (
              !expectedPhaseANativeRejection(
                RejectCodes.InvalidFieldType,
              )
            ) {
              return yield* failUnexpectedPhaseANativeRejection(
                RejectCodes.InvalidFieldType,
              );
            }
            stoppedAtRejection = true;
            break;
          }

          if (header.languageTag !== 0) {
            phaseANativeControl =
              resetPhaseANativeScriptsScanControl({
                scriptCount: activeScriptCount,
                scriptSeen: phaseANativeControl.scriptSeen + 1,
                containsNonNativeScript: 1,
              });
            continue;
          }

          phaseANativeControl = {
            ...phaseANativeControl,
            stage: 1,
            scriptCount: activeScriptCount,
            itemLength: item.bytes.length,
            itemCommitment: item.commitment,
            cursor: header.payloadOffset,
          };
          while (!stoppedAtRejection) {
            if (phaseANativeControl.stage === 1) {
              const chunkIndex = Math.floor(
                phaseANativeControl.cursor /
                  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
              );
              const chunkCount = midgardBoundedItemChunkCountV1(
                item.bytes.length,
              );
              let head: ValidationMachineNativeScriptTokenHeadV1 | null =
                null;
              try {
                head = readValidationMachineNativeScriptTokenHeadV1(
                  item.bytes,
                  phaseANativeControl.cursor,
                );
              } catch {
                // The authenticated token witness still proves the exact
                // malformed bytes to the one-step resolver.
              }
              pushPhaseANativeWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProofV1(
                  item,
                  chunkIndex,
                ),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProofV1(
                        item,
                        chunkIndex + 1,
                      )
                    : null,
                signerProof: { kind: "none" },
              });
              if (head === null) {
                if (
                  !expectedPhaseANativeRejection(
                    RejectCodes.InvalidFieldType,
                  )
                ) {
                  return yield* failUnexpectedPhaseANativeRejection(
                    RejectCodes.InvalidFieldType,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              const nextNodeCount =
                phaseANativeControl.nodeCount + 1;
              if (
                nextNodeCount >
                MAX_NATIVE_SCRIPT_SCAN_NODES_V1
              ) {
                if (
                  !expectedPhaseANativeRejection(
                    RejectCodes.NativeScriptNodeCount,
                  )
                ) {
                  return yield* failUnexpectedPhaseANativeRejection(
                    RejectCodes.NativeScriptNodeCount,
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              phaseANativeControl = {
                ...phaseANativeControl,
                stage: (head.kind + 3) as 3 | 4 | 5 | 6 | 7 | 8,
                cursor: head.payloadOffset,
                nodeCount: nextNodeCount,
              };
              continue;
            }

            if (phaseANativeControl.stage >= 3) {
              const kind = (phaseANativeControl.stage - 3) as
                | 0
                | 1
                | 2
                | 3
                | 4
                | 5;
              const chunkIndex = Math.floor(
                phaseANativeControl.cursor /
                  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
              );
              const chunkCount = midgardBoundedItemChunkCountV1(
                item.bytes.length,
              );
              let token: ValidationMachineNativeScriptTokenV1 | null =
                null;
              let payloadParseFailure = "none";
              try {
                token = readValidationMachineNativeScriptPayloadV1(
                  item.bytes,
                  phaseANativeControl.cursor,
                  kind,
                );
              } catch (cause) {
                payloadParseFailure = String(cause);
                // The authenticated payload witness proves the exact
                // malformed bytes to the selected one-step resolver.
              }
              const signerProof =
                token?.kind === 0
                  ? signerProofForHash(token.keyHash)
                  : ({ kind: "none" } as const);
              pushPhaseANativeWitness({
                kind: "nativeScriptToken",
                chunkProof: buildMidgardBoundedItemChunkProofV1(
                  item,
                  chunkIndex,
                ),
                nextChunkProof:
                  chunkIndex + 1 < chunkCount
                    ? buildMidgardBoundedItemChunkProofV1(
                        item,
                        chunkIndex + 1,
                      )
                    : null,
                signerProof,
              });
              if (token === null) {
                if (
                  !expectedPhaseANativeRejection(
                    RejectCodes.InvalidFieldType,
                  )
                ) {
                  return yield* Effect.fail(
                    new Error(
                      `bounded native-script payload failed at stage=${phaseANativeControl.stage},cursor=${phaseANativeControl.cursor},bytes=${item.bytes.subarray(phaseANativeControl.cursor).toString("hex")}: ${payloadParseFailure}`,
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }

              if (
                token.kind >= 1 &&
                token.kind <= 3 &&
                token.childCount > 0
              ) {
                const nextDepth =
                  phaseANativeControl.stackDepth + 1;
                if (
                  nextDepth >
                  MAX_NATIVE_SCRIPT_SCAN_DEPTH_V1
                ) {
                  if (
                    !expectedPhaseANativeRejection(
                      RejectCodes.NativeScriptDepth,
                    )
                  ) {
                    return yield* failUnexpectedPhaseANativeRejection(
                      RejectCodes.NativeScriptDepth,
                    );
                  }
                  stoppedAtRejection = true;
                  break;
                }
                const frame: ValidationMachineNativeScriptFrameV1 = {
                  tail: phaseANativeControl.stackRoot,
                  kind: token.kind as 1 | 2 | 3,
                  childCount: token.childCount,
                  remaining: token.childCount,
                  validCount: 0,
                  required: token.required,
                };
                nativeScriptFrames.push(frame);
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: 1,
                  cursor: token.nextOffset,
                  stackRoot:
                    hashValidationMachineNativeScriptFrameV1(frame),
                  stackDepth: nextDepth,
                };
                continue;
              }

              let valid: boolean;
              if (token.kind === 0) {
                valid = signerProof.kind === "membership";
              } else if (token.kind === 4) {
                valid =
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= 0n &&
                  compactProofTransaction.transactionBody
                    .validityIntervalStart >= token.slot;
              } else if (token.kind === 5) {
                valid =
                  compactProofTransaction.transactionBody
                    .validityIntervalEnd >= 0n &&
                  compactProofTransaction.transactionBody
                    .validityIntervalEnd <= token.slot;
              } else if (token.kind === 1) {
                valid = true;
              } else if (token.kind === 2) {
                valid = false;
              } else {
                valid = token.required === 0n;
              }
              phaseANativeControl = {
                ...phaseANativeControl,
                stage: 2,
                cursor: token.nextOffset,
                result: valid ? 1 : 0,
              };
              continue;
            }

            const frame =
              nativeScriptFrames[nativeScriptFrames.length - 1];
            if (frame !== undefined) {
              pushPhaseANativeWitness({
                kind: "nativeScriptFrame",
                frame,
              });
              const validCount =
                frame.validCount +
                (phaseANativeControl.result === 1 ? 1 : 0);
              if (frame.remaining === 1) {
                nativeScriptFrames.pop();
                const valid =
                  frame.kind === 1
                    ? validCount === frame.childCount
                    : frame.kind === 2
                      ? validCount > 0
                      : BigInt(validCount) >= frame.required;
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stackRoot: frame.tail,
                  stackDepth:
                    phaseANativeControl.stackDepth - 1,
                  result: valid ? 1 : 0,
                };
              } else {
                const nextFrame: ValidationMachineNativeScriptFrameV1 =
                  {
                    ...frame,
                    remaining: frame.remaining - 1,
                    validCount,
                  };
                nativeScriptFrames[nativeScriptFrames.length - 1] =
                  nextFrame;
                phaseANativeControl = {
                  ...phaseANativeControl,
                  stage: 1,
                  stackRoot:
                    hashValidationMachineNativeScriptFrameV1(
                      nextFrame,
                    ),
                  result: -1,
                };
              }
              continue;
            }

            pushPhaseANativeWitness();
            if (
              phaseANativeControl.cursor !==
              phaseANativeControl.itemLength
            ) {
              if (
                !expectedPhaseANativeRejection(
                  RejectCodes.InvalidFieldType,
                )
              ) {
                return yield* failUnexpectedPhaseANativeRejection(
                  RejectCodes.InvalidFieldType,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (phaseANativeControl.result === 0) {
              if (
                !expectedPhaseANativeRejection(
                  RejectCodes.NativeScriptInvalid,
                )
              ) {
                return yield* failUnexpectedPhaseANativeRejection(
                  RejectCodes.NativeScriptInvalid,
                );
              }
              stoppedAtRejection = true;
              break;
            }
            phaseANativeControl =
              resetPhaseANativeScriptsScanControl({
                scriptCount: activeScriptCount,
                scriptSeen: phaseANativeControl.scriptSeen + 1,
                containsNonNativeScript:
                  phaseANativeControl.containsNonNativeScript,
              });
            break;
          }
          if (stoppedAtRejection) break;
        }
      }

      if (
        !stoppedAtRejection &&
        terminalPhase === "phaseANativeScripts"
      ) {
        return yield* Effect.fail(
          new Error(
            `bounded native-script scan cannot prove rejection ${rejectionCode ?? "none"}`,
          ),
        );
      }
    }

    if (!stoppedAtRejection) {
      let observerCount = 0;
      let observerSeen = 0;
      let previousObserver = Buffer.alloc(0);
      const currentPreconditionsWitness = (): Buffer =>
        phaseAScriptPreconditionsWitnessCbor({
          containsNonNativeScript:
            phaseANativeControl.containsNonNativeScript,
          observerCount,
          observerSeen,
          previousObserver,
        });
      for (const observer of requiredObserversCollection.items) {
        pushWitness(
          "phaseAScriptPreconditions",
          currentPreconditionsWitness(),
          {
            kind: "transactionFieldChunk",
            collectionProof:
              buildMidgardBoundedCollectionItemProofV1(
                requiredObserversCollection,
                observer.itemIndex,
              ),
            chunkProof: buildMidgardBoundedItemChunkProofV1(observer, 0),
          },
        );
        if (
          observerSeen > 0 &&
          Buffer.compare(previousObserver, observer.bytes) >= 0
        ) {
          if (
            rejection === null ||
            terminalPhase !== "phaseAScriptPreconditions" ||
            rejection.code !== RejectCodes.InvalidFieldType
          ) {
            return yield* Effect.fail(
              new Error(
                "bounded observer scan found a duplicate or noncanonical ordering without the exact InvalidFieldType rejection",
              ),
            );
          }
          stoppedAtRejection = true;
          break;
        }
        if (observerCount === 0) {
          observerCount = requiredObserversCollection.items.length;
        }
        observerSeen += 1;
        previousObserver = observer.bytes;
      }
      if (!stoppedAtRejection) {
        pushWitness(
          "phaseAScriptPreconditions",
          currentPreconditionsWitness(),
        );
        if (
          rejection !== null &&
          terminalPhase === "phaseAScriptPreconditions"
        ) {
          stoppedAtRejection = true;
        }
      }
    }

    if (!stoppedAtRejection) {
      if (phaseALedgerTx === null) {
        return yield* Effect.fail(
          new Error(
            "V1 trace reached input resolution without a Phase A ledger transaction",
          ),
        );
      }
      let resolutionAccumulator = initialMidgardResolvedInputsAccumulatorV1();
      let remainingScheduleHash = resolutionScheduleHash;
      let resolutionCursor = 0;
      const pushResolutionWitness = (
        node: (typeof resolutionScheduleNodes)[number] | undefined,
      ): void => {
        const value =
          node === undefined
            ? null
            : (ledgerState.get(node.key.toString("hex")) ?? null);
        pushWitness(
          "resolveInputs",
          encodeCbor([
            proofSource.compactCbor,
            proofSource.witnessSetCompactCbor,
            proofSource.fieldPreimageLengthsCbor,
            contextCbor,
            BigInt(resolutionCursor),
            resolutionAccumulator,
            remainingScheduleHash,
            BigInt(signerFrontier.count),
            signerFrontierCommitment,
          ]),
          node === undefined
            ? null
            : {
                kind: "scheduledLedgerLookup",
                sourceKind: node.sourceKind,
                key: node.key,
                nextScheduleHash: node.nextScheduleHash,
                value,
                proofCbor: node.proofCbor,
                signerProof: signerSetProof(node.sourceKind, value),
              },
        );
      };

      pushResolutionWitness(undefined);
      if (
        terminalPhase === "resolveInputs" &&
        rejectionCode === RejectCodes.ValidityIntervalMismatch
      ) {
        stoppedAtRejection = true;
      } else {
        resolutionCursor = 1;
        pushResolutionWitness(resolutionScheduleNodes[0]);

        for (
          let index = 0;
          index < resolutionScheduleNodes.length;
          index += 1
        ) {
          const item = resolutionScheduleNodes[index]!;
          if (!remainingScheduleHash.equals(item.scheduleHash)) {
            return yield* Effect.fail(
              new Error(
                "input-resolution schedule diverged from its committed hash chain",
              ),
            );
          }
          const value = ledgerState.get(item.key.toString("hex"));
          if (value === undefined) {
            if (
              terminalPhase !== "resolveInputs" ||
              rejectionCode !== RejectCodes.InputNotFound
            ) {
              return yield* Effect.fail(
                new Error(
                  `input resolution found no ledger member but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          try {
            decodeMidgardTxOutput(value);
          } catch (cause) {
            if (
              terminalPhase !== "resolveInputs" ||
              rejectionCode !== RejectCodes.InvalidOutput
            ) {
              return yield* Effect.fail(
                new Error(
                  `input resolution found a malformed ledger output but replay rejected at ${terminalPhase}/${rejectionCode ?? "none"}: ${String(cause)}`,
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
          resolutionAccumulator = advanceMidgardResolvedInputsAccumulatorV1({
            accumulator: resolutionAccumulator,
            sourceKind: item.sourceKind,
            key: item.key,
            value,
          });
          remainingScheduleHash = item.nextScheduleHash;
          resolutionCursor += 1;
          pushResolutionWitness(resolutionScheduleNodes[index + 1]);
        }

        if (!stoppedAtRejection) {
          if (terminalPhase === "resolveInputs") {
            return yield* Effect.fail(
              new Error(
                `input-resolution rejection ${rejectionCode ?? "none"} has no exact V1 instruction`,
              ),
            );
          }
          const scriptSourceControl = {
            resolvedInputCount: resolutionItems.length,
            resolvedInputsAccumulator: resolutionAccumulator,
          };
          let authenticatedInlineSourceFrontier = emptyValidationFrontier;
          let inlineSourceTotalCount = 0;
          const currentInlineSourceWitness = (): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 0,
              sourceFrontier: authenticatedInlineSourceFrontier,
              redeemerFrontier: emptyValidationFrontier,
              sourceTotalCount: inlineSourceTotalCount,
              redeemerTotalCount: 0,
            });
          for (const item of scriptWitnessesCollection.items) {
            pushWitness(
              "scriptSources",
              currentInlineSourceWitness(),
              {
                kind: "transactionFieldChunk",
                collectionProof:
                  buildMidgardBoundedCollectionItemProofV1(
                    scriptWitnessesCollection,
                    item.itemIndex,
                  ),
                chunkProof: buildMidgardBoundedItemChunkProofV1(item, 0),
              },
            );
            if (inlineSourceTotalCount === 0) {
              inlineSourceTotalCount =
                scriptWitnessesCollection.items.length;
            }
            authenticatedInlineSourceFrontier =
              appendMidgardValidationMerkleLeafV1(
                authenticatedInlineSourceFrontier,
                inlineScriptSourceLeafHashes[item.itemIndex]!,
              );
          }
          pushWitness(
            "scriptSources",
            currentInlineSourceWitness(),
          );
          if (
            !commitMidgardValidationMerkleFrontierV1(
              authenticatedInlineSourceFrontier,
            ).equals(
              commitMidgardValidationMerkleFrontierV1(
                inlineScriptSourceFrontier,
              ),
            )
          ) {
            return yield* Effect.fail(
              new Error(
                "authenticated inline source fold diverged from the canonical source frontier",
              ),
            );
          }
          let authenticatedRedeemerFrontier = emptyValidationFrontier;
          let redeemerTotalCount = 0;
          const currentRedeemerWitness = (): Buffer =>
            scriptSourcesWitnessCbor({
              ...scriptSourceControl,
              stage: 1,
              sourceFrontier: inlineScriptSourceFrontier,
              redeemerFrontier: authenticatedRedeemerFrontier,
              sourceTotalCount: inlineSourceTotalCount,
              redeemerTotalCount,
            });
          for (const item of redeemerWitnessesCollection.items) {
            const redeemer = decodedProofRedeemers[item.itemIndex];
            const canonicalRedeemerWitnessCbor =
              canonicalRedeemerWitnessCbors[item.itemIndex];
            if (
              redeemer === undefined ||
              canonicalRedeemerWitnessCbor === undefined ||
              !item.bytes.equals(canonicalRedeemerWitnessCbor)
            ) {
              return yield* Effect.fail(
                new Error(
                  "bounded redeemer item diverged from its canonical decoded witness",
                ),
              );
            }
            pushWitness(
              "scriptSources",
              currentRedeemerWitness(),
              {
                kind: "transactionRedeemerItem",
                collectionProof:
                  buildMidgardBoundedCollectionItemProofV1(
                    redeemerWitnessesCollection,
                    item.itemIndex,
                  ),
                redeemer,
              },
            );
            if (redeemerTotalCount === 0) {
              redeemerTotalCount =
                redeemerWitnessesCollection.items.length;
            }
            authenticatedRedeemerFrontier =
              appendMidgardValidationMerkleLeafV1(
                authenticatedRedeemerFrontier,
                hashMidgardRedeemerItemLeafV1({
                  redeemerIndex: item.itemIndex,
                  itemCommitment: item.commitment,
                }),
              );
          }
          pushWitness(
            "scriptSources",
            currentRedeemerWitness(),
          );
          if (
            !commitMidgardValidationMerkleFrontierV1(
              authenticatedRedeemerFrontier,
            ).equals(
              commitMidgardValidationMerkleFrontierV1(
                redeemerFrontier,
              ),
            )
          ) {
            return yield* Effect.fail(
              new Error(
                "authenticated redeemer fold diverged from the canonical redeemer frontier",
              ),
            );
          }
          {
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 2,
                sourceFrontier: inlineScriptSourceFrontier,
                redeemerFrontier,
              }),
              {
                kind: "transactionFieldPairPreimage",
                firstFieldIndex: 0,
                firstPreimageCbor: fieldPreimages[0]!.preimageCbor,
                secondFieldIndex: 1,
                secondPreimageCbor: fieldPreimages[1]!.preimageCbor,
              },
            );
            let replayCursor = 0;
            let replayAccumulator = initialMidgardResolvedInputsAccumulatorV1();
            let replayRemainingScheduleHash = resolutionScheduleHash;
            let replaySpendIndex = 0;
            let replaySourceFrontier = inlineScriptSourceFrontier;
            let replayPurposeFrontier = emptyValidationFrontier;
            for (const node of resolutionScheduleNodes) {
              const value = ledgerState.get(node.key.toString("hex"));
              if (value === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay lost a previously authenticated ledger value",
                  ),
                );
              }
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 3,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                }),
                {
                  kind: "resolvedInputReplay",
                  sourceKind: node.sourceKind,
                  key: node.key,
                  nextScheduleHash: node.nextScheduleHash,
                  value,
                },
              );
              if (!replayRemainingScheduleHash.equals(node.scheduleHash)) {
                return yield* Effect.fail(
                  new Error(
                    "resolved-input replay schedule diverged from its committed hash chain",
                  ),
                );
              }
              const output = decodeMidgardTxOutput(value);
              if (
                node.sourceKind === "reference" &&
                output.script_ref !== undefined
              ) {
                const sourceEntry: ScriptSourceProofEntry = {
                  originKind: "reference",
                  sourceKey: node.key,
                  script: output.script_ref,
                  leaf: hashMidgardScriptSourceLeafV1({
                    originKind: "reference",
                    sourceKey: node.key,
                    script: output.script_ref,
                  }),
                };
                scriptSourceEntries.push(sourceEntry);
                replaySourceFrontier = appendMidgardValidationMerkleLeafV1(
                  replaySourceFrontier,
                  sourceEntry.leaf,
                );
              }
              if (node.sourceKind === "spend") {
                const credential = decodeMidgardAddressBytes(
                  output.address,
                ).paymentCredential;
                if (credential.kind === "Script") {
                  const purposeEntry: ScriptPurposeProofEntry = {
                    purposeKind: 0,
                    purposeIndex: BigInt(replaySpendIndex),
                    scriptHash: Buffer.from(credential.hash),
                    subject: node.key,
                    leaf: hashMidgardScriptPurposeLeafV1({
                      purposeKind: 0,
                      purposeIndex: BigInt(replaySpendIndex),
                      scriptHash: credential.hash,
                      subject: node.key,
                    }),
                  };
                  scriptPurposeEntries.push(purposeEntry);
                  replayPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                    replayPurposeFrontier,
                    purposeEntry.leaf,
                  );
                }
                replaySpendIndex += 1;
              }
              resolvedItemFrontier = appendMidgardValidationMerkleLeafV1(
                resolvedItemFrontier,
                hashMidgardResolvedContextItemLeafV1({
                  sourceKind: node.sourceKind,
                  itemIndex: replayCursor,
                  key: node.key,
                  outputCbor: value,
                }),
              );
              replayAccumulator = advanceMidgardResolvedInputsAccumulatorV1({
                accumulator: replayAccumulator,
                sourceKind: node.sourceKind,
                key: node.key,
                value,
              });
              replayRemainingScheduleHash = node.nextScheduleHash;
              replayCursor += 1;
            }
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 3,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayAccumulator,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: replayPurposeFrontier,
              }),
            );
            pushWitness(
              "scriptSources",
              scriptSourcesWitnessCbor({
                ...scriptSourceControl,
                stage: 4,
                sourceFrontier: replaySourceFrontier,
                redeemerFrontier,
                replayCursor,
                replayAccumulator,
                replayRemainingScheduleHash,
                spendIndex: replaySpendIndex,
                purposeFrontier: replayPurposeFrontier,
              }),
              {
                kind: "transactionFieldPreimage",
                preimageCbor: fieldPreimages[2]!.preimageCbor,
              },
            );
            let outputCursor = 0;
            let receiveHashes: Buffer[] = [];
            const protectedSignerRejection =
              rejection !== null &&
              terminalPhase === "scriptSources" &&
              rejection.code === RejectCodes.MissingRequiredWitness &&
              rejection.detail?.startsWith(
                "missing witness for protected output signer ",
              ) === true;
            for (const outputCbor of outputCbors) {
              const signerProof = protectedOutputSignerProof(outputCbor);
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveHashes,
                }),
                {
                  kind: "outputReplay",
                  outputIndex: outputCursor,
                  outputCbor,
                  siblings: outputMembership(outputCursor).siblings,
                  signerProof,
                },
              );
              const output = decodeMidgardTxOutput(outputCbor);
              const address = decodeMidgardAddressBytes(output.address);
              if (
                protectedSignerRejection &&
                address.protected &&
                address.paymentCredential.kind === "PubKey" &&
                signerProof.kind !== "membership"
              ) {
                stoppedAtRejection = true;
                break;
              }
              if (
                address.protected &&
                address.paymentCredential.kind === "Script"
              ) {
                receiveHashes = [
                  ...receiveHashes,
                  Buffer.from(address.paymentCredential.hash),
                ]
                  .sort(Buffer.compare)
                  .filter(
                    (hash, index, hashes) =>
                      index === 0 || !hash.equals(hashes[index - 1]!),
                  );
              }
              outputCursor += 1;
            }
            if (!stoppedAtRejection) {
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 5,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveHashes,
                }),
              );
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 6,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: replayPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveHashes,
                }),
                {
                  kind: "transactionFieldPreimage",
                  preimageCbor: fieldPreimages[5]!.preimageCbor,
                },
              );
              const mintPolicyIds = [...(phaseALedgerTx?.mint.assets ?? [])]
                .map((asset) => Buffer.from(asset.policyId))
                .sort(Buffer.compare)
                .filter(
                  (policyId, index, policies) =>
                    index === 0 || !policyId.equals(policies[index - 1]!),
                );
              let mintPurposeFrontier = replayPurposeFrontier;
              for (
                let policyIndex = 0;
                policyIndex < mintPolicyIds.length;
                policyIndex += 1
              ) {
                const policyId = mintPolicyIds[policyIndex]!;
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 1,
                  purposeIndex: BigInt(policyIndex),
                  scriptHash: policyId,
                  subject: policyId,
                  leaf: hashMidgardScriptPurposeLeafV1({
                    purposeKind: 1,
                    purposeIndex: BigInt(policyIndex),
                    scriptHash: policyId,
                    subject: policyId,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                mintPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                  mintPurposeFrontier,
                  purposeEntry.leaf,
                );
              }
              pushWitness(
                "scriptSources",
                scriptSourcesWitnessCbor({
                  ...scriptSourceControl,
                  stage: 7,
                  sourceFrontier: replaySourceFrontier,
                  redeemerFrontier,
                  replayCursor,
                  replayAccumulator,
                  replayRemainingScheduleHash,
                  spendIndex: replaySpendIndex,
                  purposeFrontier: mintPurposeFrontier,
                  outputCursor,
                  outputFrontier,
                  receiveHashes,
                }),
                {
                  kind: "transactionFieldPreimage",
                  preimageCbor: fieldPreimages[3]!.preimageCbor,
                },
              );
              const observerHashes = [
                ...(phaseALedgerTx?.requiredObserverHashes ?? []),
              ]
                .map((hash) => Buffer.from(hash))
                .sort(Buffer.compare);
              let observerPurposeFrontier = mintPurposeFrontier;
              for (
                let observerIndex = 0;
                observerIndex < observerHashes.length;
                observerIndex += 1
              ) {
                const observerHash = observerHashes[observerIndex]!;
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 2,
                  purposeIndex: BigInt(observerIndex),
                  scriptHash: observerHash,
                  subject: observerHash,
                  leaf: hashMidgardScriptPurposeLeafV1({
                    purposeKind: 2,
                    purposeIndex: BigInt(observerIndex),
                    scriptHash: observerHash,
                    subject: observerHash,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                observerPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                  observerPurposeFrontier,
                  purposeEntry.leaf,
                );
              }
              let allPurposeFrontier = observerPurposeFrontier;
              for (
                let receiveIndex = 0;
                receiveIndex < receiveHashes.length;
                receiveIndex += 1
              ) {
                const scriptHash = receiveHashes[receiveIndex]!;
                const purposeEntry: ScriptPurposeProofEntry = {
                  purposeKind: 3,
                  purposeIndex: BigInt(receiveIndex),
                  scriptHash,
                  subject: scriptHash,
                  leaf: hashMidgardScriptPurposeLeafV1({
                    purposeKind: 3,
                    purposeIndex: BigInt(receiveIndex),
                    scriptHash,
                    subject: scriptHash,
                  }),
                };
                scriptPurposeEntries.push(purposeEntry);
                allPurposeFrontier = appendMidgardValidationMerkleLeafV1(
                  allPurposeFrontier,
                  purposeEntry.leaf,
                );
              }
              {
                const sourceLeaves = scriptSourceEntries.map(
                  (entry) => entry.leaf,
                );
                const purposeLeaves = scriptPurposeEntries.map(
                  (entry) => entry.leaf,
                );
                const redeemerLeaves = redeemerLeafHashes;
                const discoveryWitnessCbor = (
                  stage: number,
                  discovery: ScriptDiscoveryTraceControl,
                ): Buffer =>
                  scriptSourcesWitnessCbor({
                    ...scriptSourceControl,
                    stage,
                    sourceFrontier: replaySourceFrontier,
                    redeemerFrontier,
                    replayCursor,
                    replayAccumulator,
                    replayRemainingScheduleHash,
                    spendIndex: replaySpendIndex,
                    purposeFrontier: allPurposeFrontier,
                    outputCursor,
                    outputFrontier,
                    receiveHashes: [],
                    discovery,
                  });
                const sourceMembership = (sourceIndex: number) =>
                  buildMidgardValidationMerkleMembershipV1(
                    sourceLeaves,
                    sourceIndex,
                  );
                const purposeMembership = (purposeIndex: number) =>
                  buildMidgardValidationMerkleMembershipV1(
                    purposeLeaves,
                    purposeIndex,
                  );
                const redeemerMembership = (redeemerIndex: number) =>
                  buildMidgardValidationMerkleMembershipV1(
                    redeemerLeaves,
                    redeemerIndex,
                  );
                const languageTag = (
                  script: MidgardVersionedScript,
                ): 0 | 3 | 128 => {
                  switch (script.language) {
                    case "NativeCardano":
                      return 0;
                    case "PlutusV3":
                      return 3;
                    case "MidgardV1":
                      return 128;
                  }
                };
                const purposeRedeemerTag = (
                  purposeKind: 0 | 1 | 2 | 3,
                ): number => [0, 1, 3, 6][purposeKind]!;
                const setDiscoveryBit = (
                  bitmap: bigint,
                  index: number,
                ): bigint => bitmap | (1n << BigInt(index));
                const resetCurrent = (
                  discovery: ScriptDiscoveryTraceControl,
                ): ScriptDiscoveryTraceControl => ({
                  ...discovery,
                  sourceCursor: 0,
                  redeemerCursor: 0,
                  currentPurposeKind: -1,
                  currentPurposeIndex: -1n,
                  currentScriptHash: Buffer.alloc(0),
                  currentSubject: Buffer.alloc(0),
                  matchedSourceIndex: -1,
                  matchedLanguageTag: -1,
                  matchedSourceLeaf: Buffer.alloc(0),
                });

                let discovery = emptyScriptDiscoveryControl;
                for (
                  let purposeCursor = 0;
                  purposeCursor < scriptPurposeEntries.length;
                  purposeCursor += 1
                ) {
                  const purpose = scriptPurposeEntries[purposeCursor]!;
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(8, discovery),
                    {
                      kind: "scriptPurposeScan",
                      purposeKind: purpose.purposeKind,
                      purposeIndex: purpose.purposeIndex,
                      scriptHash: purpose.scriptHash,
                      subject: purpose.subject,
                      siblings: purposeMembership(purposeCursor).siblings,
                    },
                  );
                  discovery = {
                    ...discovery,
                    sourceCursor: 0,
                    redeemerCursor: 0,
                    currentPurposeKind: purpose.purposeKind,
                    currentPurposeIndex: purpose.purposeIndex,
                    currentScriptHash: purpose.scriptHash,
                    currentSubject: purpose.subject,
                    matchedSourceIndex: -1,
                    matchedLanguageTag: -1,
                    matchedSourceLeaf: Buffer.alloc(0),
                  };

                  let matchedSource:
                    | {
                        readonly entry: ScriptSourceProofEntry;
                        readonly sourceIndex: number;
                        readonly languageTag: 0 | 3 | 128;
                      }
                    | undefined;
                  for (
                    let sourceIndex = 0;
                    sourceIndex < scriptSourceEntries.length;
                    sourceIndex += 1
                  ) {
                    const source = scriptSourceEntries[sourceIndex]!;
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(9, discovery),
                      {
                        kind: "scriptSourceScan",
                        sourceIndex,
                        originKind: source.originKind,
                        sourceKey: source.sourceKey,
                        script: source.script,
                        siblings: sourceMembership(sourceIndex).siblings,
                      },
                    );
                    const sourceHash = Buffer.from(
                      hashMidgardVersionedScript(source.script),
                      "hex",
                    );
                    discovery = {
                      ...discovery,
                      sourceCursor: sourceIndex + 1,
                    };
                    if (sourceHash.equals(purpose.scriptHash)) {
                      const exactLanguageTag = languageTag(source.script);
                      discovery = {
                        ...discovery,
                        matchedSourceIndex: sourceIndex,
                        matchedLanguageTag: exactLanguageTag,
                        matchedSourceLeaf: source.leaf,
                        usedInlineBitmap:
                          source.originKind === "inline"
                            ? setDiscoveryBit(
                                discovery.usedInlineBitmap,
                                sourceIndex,
                              )
                            : discovery.usedInlineBitmap,
                      };
                      matchedSource = {
                        entry: source,
                        sourceIndex,
                        languageTag: exactLanguageTag,
                      };
                      break;
                    }
                  }
                  if (matchedSource === undefined) {
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(9, discovery),
                    );
                    if (
                      rejection === null ||
                      terminalPhase !== "scriptSources" ||
                      rejection.code !== RejectCodes.MissingRequiredWitness
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 source scan reached an exact missing-source rejection that disagrees with validation",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }

                  if (matchedSource.languageTag === 0) {
                    const executionLeaf = hashMidgardScriptExecutionLeafV1({
                      languageTag: 0,
                      purposeLeaf: purpose.leaf,
                      sourceLeaf: matchedSource.entry.leaf,
                    });
                    scriptExecutionEntries.push({
                      purpose,
                      source: matchedSource.entry,
                      sourceIndex: matchedSource.sourceIndex,
                      languageTag: 0,
                      redeemerLeaf: Buffer.alloc(0),
                      leaf: executionLeaf,
                    });
                    discovery = resetCurrent({
                      ...discovery,
                      purposeCursor: purposeCursor + 1,
                      executionFrontier: appendMidgardValidationMerkleLeafV1(
                        discovery.executionFrontier,
                        executionLeaf,
                      ),
                    });
                    continue;
                  }

                  let matchedRedeemerIndex = -1;
                  for (
                    let redeemerIndex = 0;
                    redeemerIndex < decodedProofRedeemers.length;
                    redeemerIndex += 1
                  ) {
                    const redeemer = decodedProofRedeemers[redeemerIndex]!;
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(10, discovery),
                      {
                        kind: "redeemerScan",
                        redeemerIndex,
                        redeemer,
                        siblings: redeemerMembership(redeemerIndex).siblings,
                      },
                    );
                    discovery = {
                      ...discovery,
                      redeemerCursor: redeemerIndex + 1,
                    };
                    if (
                      redeemer.tag ===
                        purposeRedeemerTag(purpose.purposeKind) &&
                      redeemer.index === purpose.purposeIndex
                    ) {
                      matchedRedeemerIndex = redeemerIndex;
                      const executionLeaf = hashMidgardScriptExecutionLeafV1({
                        languageTag: matchedSource.languageTag,
                        purposeLeaf: purpose.leaf,
                        sourceLeaf: matchedSource.entry.leaf,
                        redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                      });
                      scriptExecutionEntries.push({
                        purpose,
                        source: matchedSource.entry,
                        sourceIndex: matchedSource.sourceIndex,
                        languageTag: matchedSource.languageTag,
                        redeemerLeaf: redeemerLeaves[redeemerIndex]!,
                        leaf: executionLeaf,
                      });
                      discovery = resetCurrent({
                        ...discovery,
                        purposeCursor: purposeCursor + 1,
                        usedRedeemerBitmap: setDiscoveryBit(
                          discovery.usedRedeemerBitmap,
                          redeemerIndex,
                        ),
                        executionFrontier: appendMidgardValidationMerkleLeafV1(
                          discovery.executionFrontier,
                          executionLeaf,
                        ),
                      });
                      break;
                    }
                  }
                  if (matchedRedeemerIndex < 0) {
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(10, discovery),
                    );
                    if (
                      rejection === null ||
                      terminalPhase !== "scriptSources" ||
                      rejection.code !== RejectCodes.MissingRequiredWitness
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 redeemer scan reached an exact missing-redeemer rejection that disagrees with validation",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                }

                if (!stoppedAtRejection) {
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(8, discovery),
                  );
                  discovery = resetCurrent({
                    ...discovery,
                    sourceCursor: 0,
                  });
                  for (
                    let sourceIndex = 0;
                    sourceIndex < scriptSourceEntries.length;
                    sourceIndex += 1
                  ) {
                    const source = scriptSourceEntries[sourceIndex]!;
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(11, discovery),
                      {
                        kind: "scriptSourceScan",
                        sourceIndex,
                        originKind: source.originKind,
                        sourceKey: source.sourceKey,
                        script: source.script,
                        siblings: sourceMembership(sourceIndex).siblings,
                      },
                    );
                    if (
                      source.originKind === "inline" &&
                      (discovery.usedInlineBitmap &
                        (1n << BigInt(sourceIndex))) ===
                        0n
                    ) {
                      if (
                        rejection === null ||
                        terminalPhase !== "scriptSources" ||
                        rejection.code !== RejectCodes.InvalidFieldType
                      ) {
                        return yield* Effect.fail(
                          new Error(
                            "V1 source audit found an extraneous inline script that disagrees with validation",
                          ),
                        );
                      }
                      stoppedAtRejection = true;
                      break;
                    }
                    discovery = {
                      ...discovery,
                      sourceCursor: sourceIndex + 1,
                    };
                  }
                }

                if (!stoppedAtRejection) {
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(11, discovery),
                  );
                  discovery = {
                    ...discovery,
                    redeemerCursor: 0,
                  };
                  for (
                    let redeemerIndex = 0;
                    redeemerIndex < decodedProofRedeemers.length;
                    redeemerIndex += 1
                  ) {
                    pushWitness(
                      "scriptSources",
                      discoveryWitnessCbor(12, discovery),
                      {
                        kind: "redeemerScan",
                        redeemerIndex,
                        redeemer: decodedProofRedeemers[redeemerIndex]!,
                        siblings: redeemerMembership(redeemerIndex).siblings,
                      },
                    );
                    if (
                      (discovery.usedRedeemerBitmap &
                        (1n << BigInt(redeemerIndex))) ===
                      0n
                    ) {
                      if (
                        rejection === null ||
                        terminalPhase !== "scriptSources" ||
                        rejection.code !== RejectCodes.InvalidFieldType
                      ) {
                        return yield* Effect.fail(
                          new Error(
                            "V1 redeemer audit found an extraneous redeemer that disagrees with validation",
                          ),
                        );
                      }
                      stoppedAtRejection = true;
                      break;
                    }
                    discovery = {
                      ...discovery,
                      redeemerCursor: redeemerIndex + 1,
                    };
                  }
                }

                if (!stoppedAtRejection) {
                  const nativeScriptBaseFields: unknown[] = [
                    proofSource.compactCbor,
                    proofSource.witnessSetCompactCbor,
                    proofSource.fieldPreimageLengthsCbor,
                    contextCbor,
                    BigInt(scriptSourceControl.resolvedInputCount),
                    scriptSourceControl.resolvedInputsAccumulator,
                    BigInt(replaySpendIndex),
                    encodeFrontierPeaks(resolvedItemFrontier),
                    BigInt(signerFrontier.count),
                    signerFrontierCommitment,
                    BigInt(replaySourceFrontier.count),
                    encodeFrontierPeaks(replaySourceFrontier),
                    BigInt(redeemerFrontier.count),
                    encodeFrontierPeaks(redeemerFrontier),
                    BigInt(allPurposeFrontier.count),
                    encodeFrontierPeaks(allPurposeFrontier),
                    BigInt(outputFrontier.count),
                    encodeFrontierPeaks(outputFrontier),
                    BigInt(discovery.executionFrontier.count),
                    encodeFrontierPeaks(discovery.executionFrontier),
                  ];
                  const nativeScriptFields: unknown[] = [
                    ...nativeScriptBaseFields,
                    0n,
                    0n,
                  ];
                  authenticatedNativeScriptsBaseFields = nativeScriptBaseFields;
                  authenticatedNativeScriptsWitnessCbor =
                    encodeCbor(nativeScriptFields);
                  pushWitness(
                    "scriptSources",
                    discoveryWitnessCbor(12, discovery),
                  );
                  if (rejection !== null && terminalPhase === "scriptSources") {
                    return yield* Effect.fail(
                      new Error(
                        "V1 validation reports a ScriptSources rejection but all exact discovery and audit instructions accepted",
                      ),
                    );
                  }
                }
              }
            }
          }
        }
      }
    }

    if (!stoppedAtRejection) {
      const nativeBaseFields = authenticatedNativeScriptsBaseFields;
      if (
        authenticatedNativeScriptsWitnessCbor === null ||
        nativeBaseFields === null
      ) {
        return yield* Effect.fail(
          new Error(
            "V1 did not authenticate the NativeScripts handoff witness",
          ),
        );
      }
      const nativeControlCbor = (
        executionCursor: number,
        languageBitmap: number,
      ): Buffer =>
        encodeCbor([
          ...nativeBaseFields,
          BigInt(executionCursor),
          BigInt(languageBitmap),
        ]);
      const executionLeaves = scriptExecutionEntries.map((entry) => entry.leaf);
      const sourceLeaves = scriptSourceEntries.map((entry) => entry.leaf);
      const purposeLeaves = scriptPurposeEntries.map((entry) => entry.leaf);
      const signerSet = new Set(
        canonicalSignerHashes.map((hash) => hash.toString("hex")),
      );
      let languageBitmap = 0;
      for (
        let executionIndex = 0;
        executionIndex < scriptExecutionEntries.length;
        executionIndex += 1
      ) {
        const execution = scriptExecutionEntries[executionIndex]!;
        pushWitness(
          "nativeScripts",
          nativeControlCbor(executionIndex, languageBitmap),
          {
            kind: "nativeExecutionScan",
            executionIndex,
            languageTag: execution.languageTag,
            purpose: {
              purposeKind: execution.purpose.purposeKind,
              purposeIndex: execution.purpose.purposeIndex,
              scriptHash: execution.purpose.scriptHash,
              subject: execution.purpose.subject,
              siblings: buildMidgardValidationMerkleMembershipV1(
                purposeLeaves,
                executionIndex,
              ).siblings,
            },
            source: {
              sourceIndex: execution.sourceIndex,
              originKind: execution.source.originKind,
              sourceKey: execution.source.sourceKey,
              script: execution.source.script,
              siblings: buildMidgardValidationMerkleMembershipV1(
                sourceLeaves,
                execution.sourceIndex,
              ).siblings,
            },
            redeemerLeaf: execution.redeemerLeaf,
            executionSiblings: buildMidgardValidationMerkleMembershipV1(
              executionLeaves,
              executionIndex,
            ).siblings,
            signerHashes:
              execution.languageTag === 0 ? canonicalSignerHashes : [],
          },
        );
        if (execution.languageTag === 0) {
          if (execution.source.script.language !== "NativeCardano") {
            return yield* Effect.fail(
              new Error(
                "V1 native execution language disagrees with its script source",
              ),
            );
          }
          const nativeValid =
            phaseALedgerTx !== null &&
            verifyMidgardNativeScript(execution.source.script.nativeScript, {
              validityIntervalStart: phaseALedgerTx.validityIntervalStart,
              validityIntervalEnd: phaseALedgerTx.validityIntervalEnd,
              witnessSigners: signerSet,
            });
          if (!nativeValid) {
            if (
              rejection === null ||
              terminalPhase !== "nativeScripts" ||
              rejection.code !== RejectCodes.NativeScriptInvalid
            ) {
              return yield* Effect.fail(
                new Error(
                  "V1 native script evaluation disagrees with validation",
                ),
              );
            }
            stoppedAtRejection = true;
            break;
          }
        } else if (execution.languageTag === 3) {
          languageBitmap |= 1;
        } else {
          languageBitmap |= 2;
        }
      }
      if (!stoppedAtRejection) {
        pushWitness(
          "nativeScripts",
          nativeControlCbor(scriptExecutionEntries.length, languageBitmap),
        );
        if (rejection !== null && terminalPhase === "nativeScripts") {
          return yield* Effect.fail(
            new Error(
              "V1 validation reports a NativeScripts rejection but every authenticated native execution accepted",
            ),
          );
        }
        const authenticatedNativeControlCbor = nativeControlCbor(
          scriptExecutionEntries.length,
          languageBitmap,
        );
        const scriptIntegrityWitnessCbor = encodeCbor([
          authenticatedNativeControlCbor,
          0n,
        ]);
        pushWitness("scriptIntegrity", scriptIntegrityWitnessCbor);
        if (rejection !== null && terminalPhase === "scriptIntegrity") {
          stoppedAtRejection = true;
        } else {
          const sourceLeaves = scriptSourceEntries.map((entry) => entry.leaf);
          const purposeLeaves = scriptPurposeEntries.map((entry) => entry.leaf);
          const redeemerLeaves = redeemerLeafHashes;
          const resolvedLeaves = resolutionScheduleNodes.map(
            (node, itemIndex) => {
              const value = ledgerState.get(node.key.toString("hex"));
              if (value === undefined) {
                throw new Error(
                  "CEK context construction lost an authenticated resolved input",
                );
              }
              return hashMidgardResolvedContextItemLeafV1({
                sourceKind: node.sourceKind,
                itemIndex,
                key: node.key,
                outputCbor: value,
              });
            },
          );
          const sameSummary = (
            left: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
            right: {
              readonly root: Uint8Array;
              readonly cborLength: bigint;
              readonly memory: bigint;
            },
          ): boolean =>
            Buffer.from(left.root).equals(Buffer.from(right.root)) &&
            left.cborLength === right.cborLength &&
            left.memory === right.memory;
          const sameSequence = (
            left: {
              readonly root: Uint8Array;
              readonly length: bigint;
              readonly payloadCborLength: bigint;
              readonly memory: bigint;
            },
            right: {
              readonly root: Uint8Array;
              readonly length: bigint;
              readonly payloadCborLength: bigint;
              readonly memory: bigint;
            },
          ): boolean =>
            Buffer.from(left.root).equals(Buffer.from(right.root)) &&
            left.length === right.length &&
            left.payloadCborLength === right.payloadCborLength &&
            left.memory === right.memory;
          const cekWitness = (input: {
            readonly contextControl?: MidgardCekContextControlV1;
            readonly executionCursor: number;
            readonly completedCpu: bigint;
            readonly completedMemory: bigint;
            readonly activeStateHash?: Uint8Array;
            readonly executionCpuLimit?: bigint;
            readonly executionMemoryLimit?: bigint;
          }): Buffer =>
            encodeMidgardCekValidationWitnessV1({
              nativeControlCbor: authenticatedNativeControlCbor,
              ...input,
            });
          const executionAuxiliary = (
            execution: ScriptExecutionProofEntry,
            executionIndex: number,
          ): NonNullable<ValidationMachineWorkWitness["auxiliary"]> => ({
            kind: "nativeExecutionScan",
            executionIndex,
            languageTag: execution.languageTag,
            purpose: {
              purposeKind: execution.purpose.purposeKind,
              purposeIndex: execution.purpose.purposeIndex,
              scriptHash: execution.purpose.scriptHash,
              subject: execution.purpose.subject,
              siblings: buildMidgardValidationMerkleMembershipV1(
                purposeLeaves,
                executionIndex,
              ).siblings,
            },
            source: {
              sourceIndex: execution.sourceIndex,
              originKind: execution.source.originKind,
              sourceKey: execution.source.sourceKey,
              script: execution.source.script,
              siblings: buildMidgardValidationMerkleMembershipV1(
                sourceLeaves,
                execution.sourceIndex,
              ).siblings,
            },
            redeemerLeaf: execution.redeemerLeaf,
            executionSiblings: buildMidgardValidationMerkleMembershipV1(
              executionLeaves,
              executionIndex,
            ).siblings,
            signerHashes: [],
          });
          const purposeForProof = (
            purpose: ScriptPurposeProofEntry,
          ): MidgardScriptPurpose => {
            const scriptHash = purpose.scriptHash.toString("hex");
            if (purpose.purposeKind === 0) {
              return {
                kind: "spend",
                scriptHash,
                outRefHex: purpose.subject.toString("hex"),
              };
            }
            if (purpose.purposeKind === 1) {
              return {
                kind: "mint",
                scriptHash,
                policyId: scriptHash,
              };
            }
            if (purpose.purposeKind === 2) {
              return { kind: "observe", scriptHash };
            }
            return { kind: "receive", scriptHash };
          };
          const purposeSummary = (
            purpose: ScriptPurposeProofEntry,
            languageTag: 3 | 128,
          ) =>
            summarizeMidgardCekLucidDataV1(
              (languageTag === 128
                ? midgardScriptPurposeData(purposeForProof(purpose))
                : cardanoScriptPurposeData(purposeForProof(purpose))) as never,
            );
          const selectedRedeemer = (
            execution: ScriptExecutionProofEntry,
          ): {
            readonly index: number;
            readonly value: DecodedMidgardRedeemer;
          } => {
            const index = redeemerLeaves.findIndex((leaf) =>
              leaf.equals(execution.redeemerLeaf),
            );
            if (index < 0) {
              throw new Error(
                "CEK execution does not select an authenticated redeemer",
              );
            }
            return { index, value: decodedProofRedeemers[index]! };
          };

          let evaluationIndex = 0;
          for (
            let executionIndex = 0;
            executionIndex < scriptExecutionEntries.length;
            executionIndex += 1
          ) {
            const executionEntry = scriptExecutionEntries[executionIndex]!;
            const completedCpu = traceExecutionCpu;
            const completedMemory = traceExecutionMemory;
            pushWitness(
              "cek",
              cekWitness({
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              executionAuxiliary(executionEntry, executionIndex),
            );

            if (
              executionEntry.languageTag === 3 &&
              executionEntry.purpose.purposeKind === 3
            ) {
              if (
                rejection === null ||
                terminalPhase !== "cek" ||
                rejection.code !== RejectCodes.PlutusScriptInvalid
              ) {
                throw new Error(
                  "PlutusV3 receive-purpose rejection disagrees with validation",
                );
              }
              stoppedAtRejection = true;
              break;
            }
            if (executionEntry.languageTag === 0) {
              continue;
            }

            const evaluation = scriptEvaluations[evaluationIndex++];
            if (
              evaluation === undefined ||
              !evaluation.scriptBytes.equals(
                executionEntry.source.script.scriptBytes,
              ) ||
              evaluation.graph === null
            ) {
              throw new Error(
                "CEK execution is missing its authenticated program graph",
              );
            }
            const exactExecution = executeMidgardCekStructuralProgramV1({
              root: evaluation.graph.root,
              material: evaluation.graph.material.values(),
              constantWitnesses: evaluation.graph.constantWitnesses,
              executionIndex: BigInt(executionIndex),
              maxSteps:
                input.consensusProfile.limits.maxValidationMachineStepCount,
            });
            const selected = selectedRedeemer(executionEntry);
            let contextControl = initialMidgardCekContextControlV1({
              languageTag: executionEntry.languageTag,
              programTermRoot: decodeMidgardCekProgramEnvelopeV1(
                executionEntry.source.script.scriptBytes,
              ).termRoot,
              purposeKind: executionEntry.purpose.purposeKind,
              purposeIndex: executionEntry.purpose.purposeIndex,
              scriptHash: executionEntry.purpose.scriptHash,
              subject: executionEntry.purpose.subject,
              redeemerLeaf: executionEntry.redeemerLeaf,
            });
            const decodedContext = decodeMidgardCekContextV1(
              evaluation.contextCbor,
            );
            const contextParts = summarizeMidgardCekContextPartsV1(
              decodedContext,
              executionEntry.languageTag,
            );

            let redeemerControl = initialMidgardCekRedeemerContextControlV1();
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "redeemerScan",
                redeemerIndex: selected.index,
                redeemer: selected.value,
                siblings: buildMidgardValidationMerkleMembershipV1(
                  redeemerLeaves,
                  selected.index,
                ).siblings,
              },
            );
            contextControl = {
              ...contextControl,
              stage: 1,
              executionMemoryLimit: selected.value.exUnits.memory,
              executionCpuLimit: selected.value.exUnits.steps,
              redeemerContextControlHash:
                hashMidgardCekRedeemerContextControlV1(redeemerControl),
            };

            const spendCount = resolutionScheduleNodes.filter(
              (node) => node.sourceKind === "spend",
            ).length;
            const addressEncoding =
              executionEntry.languageTag === 128 ? "midgard" : "cardano";
            for (
              let itemIndex = resolutionScheduleNodes.length - 1;
              itemIndex >= spendCount;
              itemIndex -= 1
            ) {
              const node = resolutionScheduleNodes[itemIndex]!;
              const value = ledgerState.get(node.key.toString("hex"))!;
              pushWitness(
                "cek",
                cekWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekResolvedContextItem",
                  sourceKind: "reference",
                  itemIndex,
                  key: node.key,
                  value,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    resolvedLeaves,
                    itemIndex,
                  ).siblings,
                },
              );
              const item = commitMidgardScriptContextTxInInfoV1(
                node.key.toString("hex"),
                decodeMidgardTxOutput(value),
                addressEncoding,
              );
              contextControl = {
                ...contextControl,
                referenceItems: prependMidgardCekDataListSummaryV1(
                  {
                    root: item.root,
                    cborLength: item.cborLength,
                    memory: item.memory,
                  },
                  contextControl.referenceItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 2 };
            if (
              !sameSequence(
                contextControl.referenceItems,
                contextParts.referenceItems,
              )
            ) {
              throw new Error(
                "CEK reference-input context differs from the evaluated context",
              );
            }

            for (
              let itemIndex = spendCount - 1;
              itemIndex >= 0;
              itemIndex -= 1
            ) {
              const node = resolutionScheduleNodes[itemIndex]!;
              const value = ledgerState.get(node.key.toString("hex"))!;
              pushWitness(
                "cek",
                cekWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekResolvedContextItem",
                  sourceKind: "spend",
                  itemIndex,
                  key: node.key,
                  value,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    resolvedLeaves,
                    itemIndex,
                  ).siblings,
                },
              );
              const item = commitMidgardScriptContextTxInInfoV1(
                node.key.toString("hex"),
                decodeMidgardTxOutput(value),
                addressEncoding,
              );
              contextControl = {
                ...contextControl,
                spendItems: prependMidgardCekDataListSummaryV1(
                  {
                    root: item.root,
                    cborLength: item.cborLength,
                    memory: item.memory,
                  },
                  contextControl.spendItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 3 };
            if (
              !sameSequence(contextControl.spendItems, contextParts.spendItems)
            ) {
              throw new Error(
                "CEK spend-input context differs from the evaluated context",
              );
            }

            for (
              let outputIndex = outputCbors.length - 1;
              outputIndex >= 0;
              outputIndex -= 1
            ) {
              const outputCbor = outputCbors[outputIndex]!;
              pushWitness(
                "cek",
                cekWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekOutputContextItem",
                  outputIndex,
                  outputCbor,
                  siblings: outputMembership(outputIndex).siblings,
                },
              );
              const item = commitMidgardScriptContextTxOutV1(
                decodeMidgardTxOutput(outputCbor),
                addressEncoding,
              );
              contextControl = {
                ...contextControl,
                outputItems: prependMidgardCekDataListSummaryV1(
                  {
                    root: item.root,
                    cborLength: item.cborLength,
                    memory: item.memory,
                  },
                  contextControl.outputItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 4 };
            if (
              !sameSequence(
                contextControl.outputItems,
                contextParts.outputItems,
              )
            ) {
              throw new Error(
                "CEK output context differs from the evaluated context",
              );
            }

            for (
              let signerIndex = canonicalSignerHashes.length - 1;
              signerIndex >= 0;
              signerIndex -= 1
            ) {
              const signerHash = canonicalSignerHashes[signerIndex]!;
              pushWitness(
                "cek",
                cekWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekSignerContextItem",
                  frontier: signerFrontier,
                  signerIndex,
                  signerHash,
                  siblings: signerMembership(signerIndex).siblings,
                },
              );
              contextControl = {
                ...contextControl,
                signerItems: prependMidgardCekDataListSummaryV1(
                  summarizeMidgardCekLucidDataV1(signerHash.toString("hex")),
                  contextControl.signerItems,
                ),
              };
            }
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
            );
            contextControl = { ...contextControl, stage: 5 };
            if (
              !sameSequence(
                contextControl.signerItems,
                contextParts.signerItems,
              )
            ) {
              throw new Error(
                "CEK signer context differs from the evaluated context",
              );
            }

            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "transactionFieldPreimage",
                preimageCbor: fieldPreimages[3]!.preimageCbor,
              },
            );
            contextControl = {
              ...contextControl,
              stage: 6,
              observerSummary: contextParts.observer,
            };

            const mintPreimage = fieldPreimages[5]!.preimageCbor;
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "transactionFieldPreimage",
                preimageCbor: mintPreimage,
              },
            );
            const mintPreimageHash = hash32(mintPreimage);
            type ScannedMintAsset = {
              readonly policyId: Buffer;
              readonly assetName: Buffer;
              readonly quantity: bigint;
            };
            const scannedMintAssets: ScannedMintAsset[] = [];
            if (mintPreimage.equals(Buffer.from("80", "hex"))) {
              contextControl = {
                ...contextControl,
                stage: 9,
                mintPreimageHash,
                mintPreimageLength: mintPreimage.length,
                mintOffset: mintPreimage.length,
                mintSummary: contextParts.mint,
              };
            } else {
              const header = readCborMapHeader(
                mintPreimage,
                0,
                "v1.cek.mint",
              );
              contextControl = {
                ...contextControl,
                stage: 7,
                mintPreimageHash,
                mintPreimageLength: mintPreimage.length,
                mintOffset: header.nextOffset,
                mintPolicyRemaining: header.length,
              };
              while (contextControl.stage === 7) {
                pushWitness(
                  "cek",
                  cekWitness({
                    contextControl,
                    executionCursor: executionIndex,
                    completedCpu,
                    completedMemory,
                  }),
                  {
                    kind: "transactionFieldPreimage",
                    preimageCbor: mintPreimage,
                  },
                );
                let policyId: Buffer;
                let assetName: Buffer;
                let quantity: bigint;
                let nextOffset: number;
                let nextPolicyRemaining = contextControl.mintPolicyRemaining;
                let nextAssetRemaining = contextControl.mintAssetRemaining;
                if (contextControl.mintAssetRemaining === 0) {
                  const policy = readCborBytes(
                    mintPreimage,
                    contextControl.mintOffset,
                    "v1.cek.mint.policy",
                  );
                  const assets = readCborMapHeader(
                    mintPreimage,
                    policy.nextOffset,
                    "v1.cek.mint.assets",
                  );
                  const asset = readCborBytes(
                    mintPreimage,
                    assets.nextOffset,
                    "v1.cek.mint.asset",
                  );
                  const amount = readCborInteger(
                    mintPreimage,
                    asset.nextOffset,
                    "v1.cek.mint.quantity",
                  );
                  policyId = Buffer.from(policy.value);
                  assetName = Buffer.from(asset.value);
                  quantity = amount.value;
                  nextOffset = amount.nextOffset;
                  nextPolicyRemaining -= 1;
                  nextAssetRemaining = assets.length - 1;
                } else {
                  const asset = readCborBytes(
                    mintPreimage,
                    contextControl.mintOffset,
                    "v1.cek.mint.asset",
                  );
                  const amount = readCborInteger(
                    mintPreimage,
                    asset.nextOffset,
                    "v1.cek.mint.quantity",
                  );
                  policyId = contextControl.currentMintPolicy;
                  assetName = Buffer.from(asset.value);
                  quantity = amount.value;
                  nextOffset = amount.nextOffset;
                  nextAssetRemaining -= 1;
                }
                scannedMintAssets.push({
                  policyId,
                  assetName,
                  quantity,
                });
                const finished =
                  nextPolicyRemaining === 0 && nextAssetRemaining === 0;
                const nextMintLeaves = scannedMintAssets.map((asset) =>
                  hashMidgardMintAssetLeafV1(asset),
                );
                contextControl = {
                  ...contextControl,
                  stage: finished ? 8 : 7,
                  mintOffset: nextOffset,
                  mintPolicyRemaining: nextPolicyRemaining,
                  mintAssetRemaining: nextAssetRemaining,
                  previousMintAsset: finished ? Buffer.alloc(0) : assetName,
                  mintCount: scannedMintAssets.length,
                  mintFrontier:
                    buildMidgardValidationMerkleFrontierV1(nextMintLeaves),
                  currentMintPolicy: finished ? Buffer.alloc(0) : policyId,
                };
              }

              for (
                let mintIndex = scannedMintAssets.length - 1;
                mintIndex >= 0;
                mintIndex -= 1
              ) {
                const asset = scannedMintAssets[mintIndex]!;
                pushWitness(
                  "cek",
                  cekWitness({
                    contextControl,
                    executionCursor: executionIndex,
                    completedCpu,
                    completedMemory,
                  }),
                  {
                    kind: "cekMintContextItem",
                    mintIndex,
                    policyId: asset.policyId,
                    assetName: asset.assetName,
                    quantity: asset.quantity,
                    siblings: buildMidgardValidationMerkleMembershipV1(
                      scannedMintAssets.map((item) =>
                        hashMidgardMintAssetLeafV1(item),
                      ),
                      mintIndex,
                    ).siblings,
                  },
                );
                const nextAssetSummary = prependMidgardCekDataPairSummaryV1(
                  summarizeMidgardCekLucidDataV1(
                    asset.assetName.toString("hex"),
                  ),
                  summarizeMidgardCekLucidDataV1(asset.quantity),
                  contextControl.currentMintAssets,
                );
                if (
                  contextControl.currentMintPolicy.length === 0 ||
                  contextControl.currentMintPolicy.equals(asset.policyId)
                ) {
                  contextControl = {
                    ...contextControl,
                    mintCursor: contextControl.mintCursor + 1,
                    currentMintPolicy: asset.policyId,
                    currentMintAssets: nextAssetSummary,
                  };
                } else {
                  const priorPolicy = prependMidgardCekDataPairSummaryV1(
                    summarizeMidgardCekLucidDataV1(
                      contextControl.currentMintPolicy.toString("hex"),
                    ),
                    summarizeMidgardCekMapDataV1(
                      contextControl.currentMintAssets,
                    ),
                    contextControl.mintPolicies,
                  );
                  contextControl = {
                    ...contextControl,
                    mintCursor: contextControl.mintCursor + 1,
                    currentMintPolicy: asset.policyId,
                    currentMintAssets: prependMidgardCekDataPairSummaryV1(
                      summarizeMidgardCekLucidDataV1(
                        asset.assetName.toString("hex"),
                      ),
                      summarizeMidgardCekLucidDataV1(asset.quantity),
                      emptyMidgardCekDataPairSummaryV1(),
                    ),
                    mintPolicies: priorPolicy,
                  };
                }
              }
              pushWitness(
                "cek",
                cekWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
              );
              const finalPolicies = prependMidgardCekDataPairSummaryV1(
                summarizeMidgardCekLucidDataV1(
                  contextControl.currentMintPolicy.toString("hex"),
                ),
                summarizeMidgardCekMapDataV1(contextControl.currentMintAssets),
                contextControl.mintPolicies,
              );
              contextControl = {
                ...contextControl,
                stage: 9,
                currentMintPolicy: Buffer.alloc(0),
                currentMintAssets: emptyMidgardCekDataPairSummaryV1(),
                mintPolicies: finalPolicies,
                mintSummary: summarizeMidgardCekMapDataV1(finalPolicies),
              };
            }
            if (!sameSummary(contextControl.mintSummary, contextParts.mint)) {
              throw new Error(
                "CEK mint context differs from the evaluated context",
              );
            }

            for (
              let redeemerIndex = decodedProofRedeemers.length - 1;
              redeemerIndex >= 0;
              redeemerIndex -= 1
            ) {
              const redeemer = decodedProofRedeemers[redeemerIndex]!;
              const purposeKind: -1 | 0 | 1 | 2 | 3 =
                redeemer.tag === 0
                  ? 0
                  : redeemer.tag === 1
                    ? 1
                    : redeemer.tag === 3
                      ? 2
                      : redeemer.tag === 6
                        ? 3
                        : -1;
              const purposeFrontierIndex = scriptPurposeEntries.findIndex(
                (purpose) =>
                  purpose.purposeKind === purposeKind &&
                  purpose.purposeIndex === redeemer.index,
              );
              if (purposeFrontierIndex < 0 || purposeKind < 0) {
                throw new Error(
                  "CEK redeemer does not select an authenticated purpose",
                );
              }
              const purpose = scriptPurposeEntries[purposeFrontierIndex]!;
              pushWitness(
                "cek",
                cekWitness({
                  contextControl,
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                }),
                {
                  kind: "cekRedeemerContextSelect",
                  control: redeemerControl,
                  redeemerIndex,
                  redeemer,
                  redeemerSiblings: buildMidgardValidationMerkleMembershipV1(
                    redeemerLeaves,
                    redeemerIndex,
                  ).siblings,
                  purposeFrontierIndex,
                  purpose: {
                    purposeKind: purpose.purposeKind,
                    purposeIndex: purpose.purposeIndex,
                    scriptHash: purpose.scriptHash,
                    subject: purpose.subject,
                    siblings: buildMidgardValidationMerkleMembershipV1(
                      purposeLeaves,
                      purposeFrontierIndex,
                    ).siblings,
                  },
                },
              );
              if (
                executionEntry.languageTag === 3 &&
                purpose.purposeKind === 3
              ) {
                redeemerControl = {
                  ...redeemerControl,
                  cursor: redeemerControl.cursor + 1,
                };
                contextControl = {
                  ...contextControl,
                  redeemerContextControlHash:
                    hashMidgardCekRedeemerContextControlV1(redeemerControl),
                };
                continue;
              }
              const semanticPurpose = purposeSummary(
                purpose,
                executionEntry.languageTag,
              );
              const scan = buildMidgardCekDataScanTraceV1(
                Buffer.from(redeemer.dataCborHex, "hex"),
              );
              redeemerControl = {
                ...redeemerControl,
                activeScanHash: hashMidgardCekDataScanControlV1(scan.initial),
                activeRedeemerLeaf: redeemerLeaves[redeemerIndex]!,
                activePurpose: semanticPurpose,
              };
              contextControl = {
                ...contextControl,
                redeemerContextControlHash:
                  hashMidgardCekRedeemerContextControlV1(redeemerControl),
              };
              for (
                let scanIndex = 0;
                scanIndex < scan.steps.length;
                scanIndex += 1
              ) {
                const scanStep = scan.steps[scanIndex]!;
                pushWitness(
                  "cek",
                  cekWitness({
                    contextControl,
                    executionCursor: executionIndex,
                    completedCpu,
                    completedMemory,
                  }),
                  {
                    kind: "cekDataScanStep",
                    redeemerControl,
                    control: scanStep.control,
                    step: scanStep.step,
                  },
                );
                const nextScan =
                  scan.steps[scanIndex + 1]?.control ?? scan.terminal;
                if (nextScan.result !== null) {
                  const nextCurrent = redeemerLeaves[redeemerIndex]!.equals(
                    executionEntry.redeemerLeaf,
                  )
                    ? nextScan.result
                    : redeemerControl.currentRedeemer;
                  redeemerControl = {
                    ...redeemerControl,
                    cursor: redeemerControl.cursor + 1,
                    mapItems: prependMidgardCekDataPairSummaryV1(
                      redeemerControl.activePurpose,
                      nextScan.result,
                      redeemerControl.mapItems,
                    ),
                    activeScanHash: Buffer.alloc(0),
                    activeRedeemerLeaf: Buffer.alloc(0),
                    activePurpose:
                      initialMidgardCekRedeemerContextControlV1().activePurpose,
                    currentRedeemer: nextCurrent,
                  };
                } else {
                  redeemerControl = {
                    ...redeemerControl,
                    activeScanHash: hashMidgardCekDataScanControlV1(nextScan),
                  };
                }
                contextControl = {
                  ...contextControl,
                  stage:
                    redeemerControl.cursor === decodedProofRedeemers.length
                      ? 10
                      : 9,
                  redeemerContextControlHash:
                    hashMidgardCekRedeemerContextControlV1(redeemerControl),
                };
              }
            }
            if (
              contextControl.stage !== 10 ||
              !sameSummary(
                redeemerControl.currentRedeemer,
                contextParts.redeemer,
              ) ||
              !sameSequence(
                redeemerControl.mapItems,
                contextParts.redeemerItems,
              )
            ) {
              throw new Error(
                "CEK redeemer context differs from the evaluated context",
              );
            }

            const partsControl: MidgardCekContextPartsControlV1 = {
              redeemerItems: redeemerControl.mapItems,
              redeemer: redeemerControl.currentRedeemer,
              scriptInfo: contextParts.scriptInfo,
            };
            const selectedSpendItem =
              executionEntry.languageTag === 3 &&
              executionEntry.purpose.purposeKind === 0
                ? resolutionScheduleNodes[
                    Number(executionEntry.purpose.purposeIndex)
                  ]
                : undefined;
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              selectedSpendItem === undefined
                ? {
                    kind: "cekContextFinalize",
                    redeemerControl,
                  }
                : {
                    kind: "cekContextFinalizeSpend",
                    redeemerControl,
                    itemIndex: Number(executionEntry.purpose.purposeIndex),
                    key: selectedSpendItem.key,
                    value: ledgerState.get(
                      selectedSpendItem.key.toString("hex"),
                    )!,
                    siblings: buildMidgardValidationMerkleMembershipV1(
                      resolvedLeaves,
                      Number(executionEntry.purpose.purposeIndex),
                    ).siblings,
                  },
            );
            contextControl = {
              ...contextControl,
              stage: 11,
              redeemerContextControlHash:
                hashMidgardCekContextPartsControlV1(partsControl),
            };
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekContextAssemble",
                control: partsControl,
              },
            );
            const assemblyControl: MidgardCekTxInfoAssemblyControlV1 = {
              tailFields: contextParts.tailFields,
              redeemer: contextParts.redeemer,
              scriptInfo: contextParts.scriptInfo,
            };
            contextControl = {
              ...contextControl,
              stage: 12,
              redeemerContextControlHash:
                hashMidgardCekTxInfoAssemblyControlV1(assemblyControl),
            };
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              {
                kind: "cekTxInfoFinalize",
                control: assemblyControl,
              },
            );
            const finalControl: MidgardCekFinalContextControlV1 = {
              txInfo: contextParts.txInfo,
              redeemer: contextParts.redeemer,
              scriptInfo: contextParts.scriptInfo,
            };
            contextControl = {
              ...contextControl,
              stage: 13,
              redeemerContextControlHash:
                hashMidgardCekFinalContextControlV1(finalControl),
            };
            if (
              !sameSummary(
                composeMidgardCekContextSummaryV1(finalControl),
                contextParts.context,
              )
            ) {
              throw new Error(
                "CEK final context composition differs from evaluation",
              );
            }
            pushWitness(
              "cek",
              cekWitness({
                contextControl,
                executionCursor: executionIndex,
                completedCpu,
                completedMemory,
              }),
              { kind: "cekContextSeed", control: finalControl },
            );
            const contextWitness = evaluation.graph.constantWitnesses.get(
              Buffer.from(evaluation.graph.contextValueRoot).toString("hex"),
            );
            if (
              !Buffer.from(exactExecution.initialState.focusRoot).equals(
                Buffer.from(evaluation.graph.root),
              ) ||
              exactExecution.initialState.executionIndex !==
                BigInt(executionIndex) ||
              contextWitness?.kind !== "semanticConstant" ||
              !sameSummary(
                contextWitness.witness.payload,
                contextParts.context,
              ) ||
              contextWitness.witness.memory !== contextParts.context.memory
            ) {
              throw new Error(
                "CEK execution does not begin at its authenticated context state",
              );
            }

            for (const step of exactExecution.steps) {
              pushWitness(
                "cek",
                cekWitness({
                  executionCursor: executionIndex,
                  completedCpu,
                  completedMemory,
                  activeStateHash: hashMidgardCekMachineStateV1(step.pre),
                  executionCpuLimit: selected.value.exUnits.steps,
                  executionMemoryLimit: selected.value.exUnits.memory,
                }),
                { kind: "cekCoreStep", step },
              );
              traceExecutionCpu = completedCpu + step.post.cpu;
              traceExecutionMemory = completedMemory + step.post.memory;
              const budgetExceeded =
                step.post.cpu > selected.value.exUnits.steps ||
                step.post.memory > selected.value.exUnits.memory;
              if (budgetExceeded || step.post.mode === "haltError") {
                if (
                  rejection === null ||
                  terminalPhase !== "cek" ||
                  rejection.code !== RejectCodes.PlutusScriptInvalid
                ) {
                  throw new Error(
                    "CEK failure transition disagrees with validation",
                  );
                }
                stoppedAtRejection = true;
                break;
              }
            }
            if (stoppedAtRejection) break;
            if (
              exactExecution.terminalState.mode !== "haltSuccess" ||
              evaluation.result.kind !== "accepted"
            ) {
              throw new Error(
                "CEK successful trace disagrees with local validation",
              );
            }
          }
          if (
            !stoppedAtRejection &&
            evaluationIndex !== scriptEvaluations.length
          ) {
            throw new Error(
              "CEK trace did not consume every local script evaluation",
            );
          }
          if (scriptExecutionEntries.length === 0) {
            pushWitness(
              "cek",
              cekWitness({
                executionCursor: 0,
                completedCpu: 0n,
                completedMemory: 0n,
              }),
            );
          }
        }

        if (!stoppedAtRejection) {
          const mintAssets = [...phaseALedgerTx!.mint.assets];
          const mintLeaves = mintAssets.map((asset) =>
            hashMidgardMintAssetLeafV1({
              policyId: asset.policyId,
              assetName: asset.assetName,
              quantity: asset.quantity,
            }),
          );
          const mintFrontier =
            buildMidgardValidationMerkleFrontierV1(mintLeaves);
          const valueContributions: ValidationValueContribution[] = [];
          for (const node of resolutionScheduleNodes) {
            if (node.sourceKind !== "spend") continue;
            const value = ledgerState.get(node.key.toString("hex"));
            if (value === undefined) {
              return yield* Effect.fail(
                new Error(
                  "value mutation planning lost a previously authenticated ledger value",
                ),
              );
            }
            valueContributions.push(
              ...midgardValueContributions(
                decodeMidgardTxOutput(value).value,
                1n,
              ),
            );
          }
          for (const outputCbor of outputCbors) {
            valueContributions.push(
              ...midgardValueContributions(
                decodeMidgardTxOutput(outputCbor).value,
                -1n,
              ),
            );
          }
          for (const asset of mintAssets) {
            valueContributions.push({
              unit: Buffer.concat([
                Buffer.from(asset.policyId),
                Buffer.from(asset.assetName),
              ]),
              quantityDelta: asset.quantity,
            });
          }
          const valueMutationSteps = yield* Effect.tryPromise({
            try: () => buildValidationValueMutationSteps(valueContributions),
            catch: (cause) =>
              cause instanceof Error
                ? cause
                : new Error("failed to build authenticated value mutations"),
          });
          const valueAccumulator = emptyValidationValueAccumulator();
          let valueReplayCursor = 0;
          let valueReplayAssetCursor = 0;
          let valueReplayValueHash = Buffer.alloc(32);
          let valueReplayAccumulator =
            initialMidgardResolvedInputsAccumulatorV1();
          let valueReplayRemainingScheduleHash =
            emptyMidgardInputResolutionScheduleV1();
          let valueOutputCursor = 0;
          let valueOutputAssetCursor = 0;
          let valueMintCursor = 0;
          let valueMutationCursor = 0;
          const valueAndMintControlCbor = (input: {
            readonly stage: number;
            readonly replayScheduleHash: Buffer;
            readonly replayCursor?: number;
            readonly replayAccumulator?: Buffer;
            readonly replayRemainingScheduleHash?: Buffer;
            readonly outputCursor?: number;
            readonly mintFrontier?: MidgardValidationMerkleFrontierV1;
            readonly mintCursor?: number;
          }): Buffer =>
            encodeCbor([
              authenticatedNativeControlCbor,
              BigInt(input.stage),
              input.replayScheduleHash,
              BigInt(input.replayCursor ?? valueReplayCursor),
              BigInt(valueReplayAssetCursor),
              valueReplayValueHash,
              input.replayAccumulator ?? valueReplayAccumulator,
              input.replayRemainingScheduleHash ??
                valueReplayRemainingScheduleHash,
              BigInt(input.outputCursor ?? valueOutputCursor),
              BigInt(valueOutputAssetCursor),
              BigInt(input.mintFrontier?.count ?? 0),
              encodeFrontierPeaks(
                input.mintFrontier ?? emptyValidationFrontier,
              ),
              BigInt(input.mintCursor ?? valueMintCursor),
              encodeValidationValueAccumulator(valueAccumulator),
            ]);

          pushWitness(
            "valueAndMint",
            valueAndMintControlCbor({
              stage: 0,
              replayScheduleHash: emptyMidgardInputResolutionScheduleV1(),
            }),
            {
              kind: "transactionFieldPairPreimage",
              firstFieldIndex: 0,
              firstPreimageCbor: fieldPreimages[0]!.preimageCbor,
              secondFieldIndex: 1,
              secondPreimageCbor: fieldPreimages[1]!.preimageCbor,
            },
          );
          valueReplayRemainingScheduleHash = resolutionScheduleHash;
          pushWitness(
            "valueAndMint",
            valueAndMintControlCbor({
              stage: 1,
              replayScheduleHash: resolutionScheduleHash,
            }),
            {
              kind: "transactionFieldPreimage",
              preimageCbor: fieldPreimages[5]!.preimageCbor,
            },
          );
          if (
            mintFrontier.count >
            input.consensusProfile.limits.maxDistinctAssetCount
          ) {
            if (
              rejection === null ||
              terminalPhase !== "valueAndMint" ||
              rejection.code !== RejectCodes.AssetCount
            ) {
              return yield* Effect.fail(
                new Error(
                  "V1 mint frontier exceeds the asset bound but validation did not reject it in ValueAndMint",
                ),
              );
            }
            stoppedAtRejection = true;
          }

          if (!stoppedAtRejection) {
            for (const node of resolutionScheduleNodes) {
              const value = ledgerState.get(node.key.toString("hex"));
              if (value === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "value replay lost a previously authenticated ledger value",
                  ),
                );
              }
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 2,
                  replayScheduleHash: resolutionScheduleHash,
                  mintFrontier,
                }),
                {
                  kind: "resolvedInputReplay",
                  sourceKind: node.sourceKind,
                  key: node.key,
                  nextScheduleHash: node.nextScheduleHash,
                  value,
                },
              );
              const decodedValue = decodeMidgardTxOutput(value).value;
              const contributions =
                node.sourceKind === "spend"
                  ? midgardValueContributions(decodedValue, 1n)
                  : [];
              if (node.sourceKind === "spend") {
                valueAccumulator.lovelaceDelta += decodedValue.lovelace;
              }
              if (contributions.length > 0) {
                valueReplayAssetCursor = 1;
                valueReplayValueHash = hash32(value);
                for (
                  let assetIndex = 0;
                  assetIndex < contributions.length;
                  assetIndex += 1
                ) {
                  const mutationStep = valueMutationSteps[valueMutationCursor];
                  if (mutationStep === undefined) {
                    return yield* Effect.fail(
                      new Error(
                        "value replay exhausted authenticated mutation steps",
                      ),
                    );
                  }
                  pushWitness(
                    "valueAndMint",
                    valueAndMintControlCbor({
                      stage: 2,
                      replayScheduleHash: resolutionScheduleHash,
                      mintFrontier,
                    }),
                    {
                      kind: "valueInputAsset",
                      sourceKind: "spend",
                      key: node.key,
                      nextScheduleHash: node.nextScheduleHash,
                      value,
                      assetIndex,
                      mutationStep,
                    },
                  );
                  if (
                    mutationStep.postSeenAssetCount >
                    input.consensusProfile.limits.maxDistinctAssetCount
                  ) {
                    if (
                      rejection === null ||
                      terminalPhase !== "valueAndMint" ||
                      rejection.code !== RejectCodes.AssetCount
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 spend-value replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                  applyValidationValueMutationStep(
                    valueAccumulator,
                    mutationStep,
                  );
                  valueMutationCursor += 1;
                  valueReplayAssetCursor += 1;
                }
              }
              if (stoppedAtRejection) break;
              valueReplayAssetCursor = 0;
              valueReplayValueHash = Buffer.alloc(32);
              valueReplayAccumulator =
                advanceMidgardResolvedInputsAccumulatorV1({
                  accumulator: valueReplayAccumulator,
                  sourceKind: node.sourceKind,
                  key: node.key,
                  value,
                });
              valueReplayRemainingScheduleHash = node.nextScheduleHash;
              valueReplayCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 2,
                replayScheduleHash: resolutionScheduleHash,
                mintFrontier,
              }),
            );
            for (
              let outputIndex = 0;
              outputIndex < outputCbors.length;
              outputIndex += 1
            ) {
              const outputCbor = outputCbors[outputIndex]!;
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 3,
                  replayScheduleHash: resolutionScheduleHash,
                  mintFrontier,
                }),
                {
                  kind: "outputReplay",
                  outputIndex,
                  outputCbor,
                  siblings: outputMembership(outputIndex).siblings,
                  signerProof: { kind: "none" },
                },
              );
              const decodedValue = decodeMidgardTxOutput(outputCbor).value;
              valueAccumulator.lovelaceDelta -= decodedValue.lovelace;
              const contributions = midgardValueContributions(
                decodedValue,
                -1n,
              );
              if (contributions.length > 0) {
                valueOutputAssetCursor = 1;
                for (
                  let assetIndex = 0;
                  assetIndex < contributions.length;
                  assetIndex += 1
                ) {
                  const mutationStep = valueMutationSteps[valueMutationCursor];
                  if (mutationStep === undefined) {
                    return yield* Effect.fail(
                      new Error(
                        "output replay exhausted authenticated value mutations",
                      ),
                    );
                  }
                  pushWitness(
                    "valueAndMint",
                    valueAndMintControlCbor({
                      stage: 3,
                      replayScheduleHash: resolutionScheduleHash,
                      mintFrontier,
                    }),
                    {
                      kind: "valueOutputAsset",
                      outputIndex,
                      outputCbor,
                      siblings: outputMembership(outputIndex).siblings,
                      assetIndex,
                      mutationStep,
                    },
                  );
                  if (
                    mutationStep.postSeenAssetCount >
                    input.consensusProfile.limits.maxDistinctAssetCount
                  ) {
                    if (
                      rejection === null ||
                      terminalPhase !== "valueAndMint" ||
                      rejection.code !== RejectCodes.AssetCount
                    ) {
                      return yield* Effect.fail(
                        new Error(
                          "V1 output-value replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                        ),
                      );
                    }
                    stoppedAtRejection = true;
                    break;
                  }
                  applyValidationValueMutationStep(
                    valueAccumulator,
                    mutationStep,
                  );
                  valueMutationCursor += 1;
                  valueOutputAssetCursor += 1;
                }
              }
              if (stoppedAtRejection) break;
              valueOutputAssetCursor = 0;
              valueOutputCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 3,
                replayScheduleHash: resolutionScheduleHash,
                mintFrontier,
              }),
            );
            for (
              let mintIndex = 0;
              mintIndex < mintAssets.length;
              mintIndex += 1
            ) {
              const asset = mintAssets[mintIndex]!;
              pushWitness(
                "valueAndMint",
                valueAndMintControlCbor({
                  stage: 4,
                  replayScheduleHash: resolutionScheduleHash,
                  mintFrontier,
                }),
                {
                  kind: "valueMintAsset",
                  mintIndex,
                  policyId: Buffer.from(asset.policyId),
                  assetName: Buffer.from(asset.assetName),
                  quantity: asset.quantity,
                  siblings: buildMidgardValidationMerkleMembershipV1(
                    mintLeaves,
                    mintIndex,
                  ).siblings,
                  mutationStep: valueMutationSteps[valueMutationCursor]!,
                },
              );
              const mutationStep = valueMutationSteps[valueMutationCursor];
              if (mutationStep === undefined) {
                return yield* Effect.fail(
                  new Error(
                    "mint replay exhausted authenticated value mutations",
                  ),
                );
              }
              if (
                mutationStep.postSeenAssetCount >
                input.consensusProfile.limits.maxDistinctAssetCount
              ) {
                if (
                  rejection === null ||
                  terminalPhase !== "valueAndMint" ||
                  rejection.code !== RejectCodes.AssetCount
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "V1 mint replay exceeds the asset bound but validation did not reject it in ValueAndMint",
                    ),
                  );
                }
                stoppedAtRejection = true;
                break;
              }
              applyValidationValueMutationStep(valueAccumulator, mutationStep);
              valueMutationCursor += 1;
              valueMintCursor += 1;
            }
          }

          if (!stoppedAtRejection) {
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 4,
                replayScheduleHash: resolutionScheduleHash,
                mintFrontier,
              }),
            );
            const valueIsPreserved =
              valueAccumulator.lovelaceDelta - phaseALedgerTx!.fee === 0n &&
              valueAccumulator.nonzeroAssetCount === 0;
            pushWitness(
              "valueAndMint",
              valueAndMintControlCbor({
                stage: 5,
                replayScheduleHash: resolutionScheduleHash,
                mintFrontier,
              }),
            );
            if (!valueIsPreserved) {
              if (
                rejection === null ||
                terminalPhase !== "valueAndMint" ||
                rejection.code !== RejectCodes.ValueNotPreserved
              ) {
                return yield* Effect.fail(
                  new Error(
                    "V1 value equation disagrees with validation",
                  ),
                );
              }
              stoppedAtRejection = true;
            } else {
              if (rejection !== null && terminalPhase === "valueAndMint") {
                return yield* Effect.fail(
                  new Error(
                    "V1 validation reports a ValueAndMint rejection but the authenticated value equation accepted",
                  ),
                );
              }
              let ledgerReplayCursor = 0;
              let ledgerReplayAccumulator =
                initialMidgardResolvedInputsAccumulatorV1();
              let ledgerReplayRemainingScheduleHash =
                emptyMidgardInputResolutionScheduleV1();
              let currentLedgerRoot = Buffer.from(priorLedgerRoot);
              let ledgerOutputCursor = 0;
              let operationFrontier = emptyValidationFrontier;
              let mutationIndex = 0;
              const ledgerDeltaControlCbor = (input: {
                readonly stage: number;
                readonly replayScheduleHash: Buffer;
              }): Buffer =>
                encodeCbor([
                  authenticatedNativeControlCbor,
                  BigInt(input.stage),
                  input.replayScheduleHash,
                  BigInt(ledgerReplayCursor),
                  ledgerReplayAccumulator,
                  ledgerReplayRemainingScheduleHash,
                  currentLedgerRoot,
                  BigInt(ledgerOutputCursor),
                  BigInt(operationFrontier.count),
                  encodeFrontierPeaks(operationFrontier),
                ]);
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 0,
                  replayScheduleHash: emptyMidgardInputResolutionScheduleV1(),
                }),
                {
                  kind: "transactionFieldPairPreimage",
                  firstFieldIndex: 0,
                  firstPreimageCbor: fieldPreimages[0]!.preimageCbor,
                  secondFieldIndex: 1,
                  secondPreimageCbor: fieldPreimages[1]!.preimageCbor,
                },
              );
              ledgerReplayRemainingScheduleHash = resolutionScheduleHash;
              for (const node of resolutionScheduleNodes) {
                const value = ledgerState.get(node.key.toString("hex"));
                if (value === undefined) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta replay lost a previously authenticated ledger value",
                    ),
                  );
                }
                const mutationStep =
                  node.sourceKind === "spend"
                    ? (input.ledgerMutationSteps[mutationIndex] ?? null)
                    : null;
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 1,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaReplay",
                    sourceKind: node.sourceKind,
                    key: node.key,
                    nextScheduleHash: node.nextScheduleHash,
                    value,
                    mutationStep,
                  },
                );
                if (node.sourceKind === "spend") {
                  if (
                    mutationStep === null ||
                    mutationStep.operation.type !== "delete" ||
                    !mutationStep.operation.key.equals(node.key) ||
                    !mutationStep.preRoot.equals(currentLedgerRoot)
                  ) {
                    return yield* Effect.fail(
                      new Error(
                        "ledger-delta deletion mutation does not match the authenticated spend schedule",
                      ),
                    );
                  }
                  currentLedgerRoot = Buffer.from(mutationStep.postRoot);
                  operationFrontier = appendMidgardValidationMerkleLeafV1(
                    operationFrontier,
                    hashMidgardValidationLedgerDeltaOperationV1(
                      mutationStep.operation,
                    ),
                  );
                  mutationIndex += 1;
                }
                ledgerReplayAccumulator =
                  advanceMidgardResolvedInputsAccumulatorV1({
                    accumulator: ledgerReplayAccumulator,
                    sourceKind: node.sourceKind,
                    key: node.key,
                    value,
                  });
                ledgerReplayRemainingScheduleHash = node.nextScheduleHash;
                ledgerReplayCursor += 1;
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 1,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
              for (
                let outputIndex = 0;
                outputIndex < outputCbors.length;
                outputIndex += 1
              ) {
                const outputCbor = outputCbors[outputIndex]!;
                const mutationStep = input.ledgerMutationSteps[mutationIndex];
                const outputKey = encodeCbor([
                  input.transactionId,
                  BigInt(outputIndex),
                ]);
                if (
                  mutationStep === undefined ||
                  mutationStep.operation.type !== "insert" ||
                  !mutationStep.operation.key.equals(outputKey) ||
                  !mutationStep.operation.value.equals(outputCbor) ||
                  !mutationStep.preRoot.equals(currentLedgerRoot)
                ) {
                  return yield* Effect.fail(
                    new Error(
                      "ledger-delta insertion mutation does not match the authenticated output frontier",
                    ),
                  );
                }
                pushWitness(
                  "ledgerDelta",
                  ledgerDeltaControlCbor({
                    stage: 2,
                    replayScheduleHash: resolutionScheduleHash,
                  }),
                  {
                    kind: "ledgerDeltaOutput",
                    outputIndex,
                    outputCbor,
                    siblings: outputMembership(outputIndex).siblings,
                    mutationStep,
                  },
                );
                currentLedgerRoot = Buffer.from(mutationStep.postRoot);
                operationFrontier = appendMidgardValidationMerkleLeafV1(
                  operationFrontier,
                  hashMidgardValidationLedgerDeltaOperationV1(
                    mutationStep.operation,
                  ),
                );
                ledgerOutputCursor += 1;
                mutationIndex += 1;
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 2,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
              if (
                mutationIndex !== input.ledgerMutationSteps.length ||
                !currentLedgerRoot.equals(postLedgerRoot) ||
                commitMidgardValidationMerkleFrontierV1(
                  operationFrontier,
                ).equals(ledgerDeltaRoot) === false
              ) {
                return yield* Effect.fail(
                  new Error(
                    "ledger-delta replay did not reach its committed roots",
                  ),
                );
              }
              pushWitness(
                "ledgerDelta",
                ledgerDeltaControlCbor({
                  stage: 3,
                  replayScheduleHash: resolutionScheduleHash,
                }),
              );
            }
          }
        }
      }
    }
    if (rejection !== null && !stoppedAtRejection) {
      return yield* Effect.fail(
        new Error(
          `V1 trace did not reach rejection phase ${terminalPhase}`,
        ),
      );
    }

    const terminalWitness: ValidationMachineWorkWitness = {
      phase: "terminal",
      programCounter: witnesses.length,
      cbor: encodeCbor([
        verdict === "accepted" ? 1n : 2n,
        rejectionCode === null
          ? Buffer.alloc(0)
          : Buffer.from(rejectionCode, "ascii"),
        postLedgerRoot,
        verdict === "accepted"
          ? encodeCbor([
              BigInt(ledgerDeltaFrontier.count),
              encodeFrontierPeaks(ledgerDeltaFrontier),
            ])
          : Buffer.from("80", "hex"),
      ]),
      auxiliary: null,
    };
    witnesses.push(terminalWitness);
    witnessExecutionBudgets.push({
      cpu: traceExecutionCpu,
      memory: traceExecutionMemory,
    });

    const eventKeyHash = hash32(input.eventKeyCbor);
    const rejectionCodeHash =
      rejectionCode === null
        ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
        : hashMidgardValidationRejectionCodeV1(rejectionCode);
    const states = witnesses.map((witness, index) => {
      const terminal = index === witnesses.length - 1;
      const budget = witnessExecutionBudgets[index]!;
      return {
        machineVersion: MIDGARD_VALIDATION_MACHINE_V1_VERSION,
        eventKeyHash,
        transactionId: Buffer.from(input.transactionId),
        transactionCommitment,
        validationContextHash,
        sourceKind: input.sourceKind,
        priorLedgerRoot,
        phase: witness.phase,
        programCounter: witness.programCounter,
        workRoot: hashMidgardValidationWorkWitnessV1({
          phase: witness.phase,
          programCounter: witness.programCounter,
          witnessCbor: witness.cbor,
        }),
        executionCpu: budget.cpu,
        executionMemory: budget.memory,
        verdict: terminal ? verdict : ("pending" as const),
        rejectionCodeHash: terminal
          ? rejectionCodeHash
          : MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
        ledgerDeltaRoot,
      } satisfies MidgardValidationMachineStateV1;
    });
    if (states.length === 0) {
      return yield* Effect.fail(new Error("validation trace has no states"));
    }
    const tree = buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineStateV1),
      verdict,
      rejectionCodeHash,
    );
    if (
      tree.descriptor.initialStateHash.equals(ZERO_32) ||
      tree.descriptor.terminalStateHash.equals(ZERO_32)
    ) {
      return yield* Effect.fail(
        new Error("validation trace endpoint hash must not be zero"),
      );
    }
    return {
      states,
      witnesses,
      tree,
      verdict,
      rejectionCode,
      ledgerOps,
    };
  });
