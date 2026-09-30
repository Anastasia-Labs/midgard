import {
  type MidgardBoundedItem,
  type MidgardBoundedItemChunkProof,
} from "./bounded-item.js";
import {
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataTraverse,
  isWellFormedMidgardCekDataTraverseControl,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
} from "./cek-data-traverse.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

export const MIDGARD_REDEEMER_ITEM_PROOF_VERSION = 1 as const;

export const MIDGARD_REDEEMER_ITEM_FIELD_INDEX = 8 as const;

export const MIDGARD_REDEEMER_ITEM_MAX_HEADER_SPAN = 28 as const;

export const MIDGARD_REDEEMER_ITEM_MAX_TAIL_SPAN = 19 as const;

export const MidgardRedeemerItemProofModes = Object.freeze({
  Descriptor: 0,
  Data: 1,
} as const);

export const MidgardRedeemerItemProofStages = Object.freeze({
  Header: 0,
  Tail: 1,
  Data: 2,
  Terminal: 3,
} as const);

export type MidgardRedeemerItemProofMode =
  (typeof MidgardRedeemerItemProofModes)[keyof typeof MidgardRedeemerItemProofModes];

export type MidgardRedeemerItemProofStage =
  (typeof MidgardRedeemerItemProofStages)[keyof typeof MidgardRedeemerItemProofStages];

export type MidgardRedeemerItemDescriptor = {
  readonly itemIndex: number;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly itemCommitment: Hash32;
  readonly purposeTag: number;
  readonly pointerIndex: number;
  readonly dataOffset: number;
  readonly dataLength: number;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
};

export type MidgardRedeemerItemProofControl = {
  readonly version: typeof MIDGARD_REDEEMER_ITEM_PROOF_VERSION;
  readonly mode: MidgardRedeemerItemProofMode;
  readonly stage: MidgardRedeemerItemProofStage;
  readonly itemIndex: number;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly itemCommitment: Hash32;
  readonly expectedPurposeTag: number;
  readonly expectedPointerIndex: number;
  readonly purposeTag: number;
  readonly pointerIndex: number;
  readonly dataOffset: number;
  readonly dataLength: number;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
  readonly traversal: MidgardCekDataTraverseControl | null;
};

export type MidgardRedeemerItemProofAction =
  | { readonly kind: "openHeader" }
  | { readonly kind: "openTail" }
  | {
      readonly kind: "traverseData";
      readonly action: MidgardCekDataTraverseAction;
    }
  | { readonly kind: "finishData" };

export type MidgardRedeemerItemProofWitness = {
  readonly action: MidgardRedeemerItemProofAction;
  readonly chunkProof: MidgardBoundedItemChunkProof | null;
  readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
};

export type MidgardRedeemerItemProofTraceStep = {
  readonly control: MidgardRedeemerItemProofControl;
  readonly witness: MidgardRedeemerItemProofWitness;
  readonly next: MidgardRedeemerItemProofControl;
};

export type MidgardRedeemerItemProofTrace = {
  readonly item: MidgardBoundedItem;
  readonly initial: MidgardRedeemerItemProofControl;
  readonly steps: readonly MidgardRedeemerItemProofTraceStep[];
  readonly terminal: MidgardRedeemerItemProofControl;
};

type CborHead = {
  readonly major: number;
  readonly value: number;
  readonly nextOffset: number;
};

export const CONTROL_DOMAIN = Buffer.from(
  "MidgardRedeemerItemProofControlV1",
  "ascii",
);

const exactSafeInt = (value: number, name: string): number => {
  if (!Number.isSafeInteger(value)) {
    throw new Error(`${name} must be a safe integer`);
  }
  return value;
};

const supportedPurposeTag = (tag: number): boolean =>
  tag === 0 || tag === 1 || tag === 3 || tag === 6;

export const readCanonicalHead = (
  bytes: Uint8Array,
  offset: number,
  expectedMajor: number,
): CborHead | null => {
  if (offset < 0 || offset >= bytes.length) return null;
  const initial = bytes[offset]!;
  const major = initial >>> 5;
  const additional = initial & 0x1f;
  if (major !== expectedMajor || additional === 31) return null;
  if (additional < 24) {
    return { major, value: additional, nextOffset: offset + 1 };
  }
  const width =
    additional === 24
      ? 1
      : additional === 25
        ? 2
        : additional === 26
          ? 4
          : additional === 27
            ? 8
            : 0;
  if (width === 0 || offset + 1 + width > bytes.length) return null;
  let value = 0n;
  for (let index = 0; index < width; index += 1) {
    value = (value << 8n) | BigInt(bytes[offset + 1 + index]!);
  }
  if (
    (width === 1 && value < 24n) ||
    (width === 2 && value <= 0xffn) ||
    (width === 4 && value <= 0xffffn) ||
    (width === 8 && value <= 0xffff_ffffn) ||
    value > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    return null;
  }
  return {
    major,
    value: Number(value),
    nextOffset: offset + 1 + width,
  };
};

const openedDescriptorIsWellFormed = (
  control: MidgardRedeemerItemProofControl,
): boolean => {
  const tailLength =
    control.totalLength - control.dataOffset - control.dataLength;
  return (
    supportedPurposeTag(control.purposeTag) &&
    control.pointerIndex >= 0 &&
    control.dataOffset > 0 &&
    control.dataLength > 0 &&
    control.dataOffset + control.dataLength < control.totalLength &&
    tailLength > 0 &&
    tailLength <= MIDGARD_REDEEMER_ITEM_MAX_TAIL_SPAN &&
    (control.expectedPurposeTag === -1 ||
      (control.purposeTag === control.expectedPurposeTag &&
        control.pointerIndex === control.expectedPointerIndex))
  );
};

export const isWellFormedMidgardRedeemerItemProofControl = (
  control: MidgardRedeemerItemProofControl,
): boolean => {
  try {
    const expectedAbsent =
      control.expectedPurposeTag === -1 && control.expectedPointerIndex === -1;
    const expectedPresent =
      supportedPurposeTag(control.expectedPurposeTag) &&
      control.expectedPointerIndex >= 0;
    const descriptorOpen = openedDescriptorIsWellFormed(control);
    const exUnitsOpen =
      control.executionMemory >= 0n && control.executionSteps >= 0n;
    const traversalOpen =
      control.traversal !== null &&
      isWellFormedMidgardCekDataTraverseControl(control.traversal) &&
      control.traversal.sourceStart === control.dataOffset &&
      control.traversal.sourceLength === control.dataLength;
    return (
      control.version === MIDGARD_REDEEMER_ITEM_PROOF_VERSION &&
      (control.mode === MidgardRedeemerItemProofModes.Descriptor ||
        control.mode === MidgardRedeemerItemProofModes.Data) &&
      control.stage >= MidgardRedeemerItemProofStages.Header &&
      control.stage <= MidgardRedeemerItemProofStages.Terminal &&
      control.itemIndex >= 0 &&
      control.itemCount > control.itemIndex &&
      control.totalLength > 0 &&
      control.itemCommitment.length === 32 &&
      (expectedAbsent || expectedPresent) &&
      (control.stage === MidgardRedeemerItemProofStages.Header
        ? control.purposeTag === -1 &&
          control.pointerIndex === -1 &&
          control.dataOffset === 0 &&
          control.dataLength === 0 &&
          control.executionMemory === -1n &&
          control.executionSteps === -1n &&
          control.traversal === null
        : control.stage === MidgardRedeemerItemProofStages.Tail
          ? descriptorOpen &&
            control.executionMemory === -1n &&
            control.executionSteps === -1n &&
            control.traversal === null
          : control.stage === MidgardRedeemerItemProofStages.Data
            ? control.mode === MidgardRedeemerItemProofModes.Data &&
              descriptorOpen &&
              exUnitsOpen &&
              traversalOpen
            : control.mode === MidgardRedeemerItemProofModes.Descriptor
              ? descriptorOpen && exUnitsOpen && control.traversal === null
              : descriptorOpen &&
                exUnitsOpen &&
                traversalOpen &&
                control.traversal!.stage ===
                  MidgardCekDataTraverseStages.Terminal &&
                finalizeMidgardCekDataTraverse(control.traversal!) !== null)
    );
  } catch {
    return false;
  }
};

export const initialMidgardRedeemerItemProofControl = ({
  mode,
  itemIndex,
  itemCount,
  totalLength,
  itemCommitment,
  expectedPurposeTag = -1,
  expectedPointerIndex = -1,
}: {
  readonly mode: MidgardRedeemerItemProofMode;
  readonly itemIndex: number;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly itemCommitment: Uint8Array;
  readonly expectedPurposeTag?: number;
  readonly expectedPointerIndex?: number;
}): MidgardRedeemerItemProofControl => {
  const control = {
    version: MIDGARD_REDEEMER_ITEM_PROOF_VERSION,
    mode,
    stage: MidgardRedeemerItemProofStages.Header,
    itemIndex: exactSafeInt(itemIndex, "itemIndex"),
    itemCount: exactSafeInt(itemCount, "itemCount"),
    totalLength: exactSafeInt(totalLength, "totalLength"),
    itemCommitment: ensureHash32(itemCommitment, "itemCommitment"),
    expectedPurposeTag: exactSafeInt(expectedPurposeTag, "expectedPurposeTag"),
    expectedPointerIndex: exactSafeInt(
      expectedPointerIndex,
      "expectedPointerIndex",
    ),
    purposeTag: -1,
    pointerIndex: -1,
    dataOffset: 0,
    dataLength: 0,
    executionMemory: -1n,
    executionSteps: -1n,
    traversal: null,
  } satisfies MidgardRedeemerItemProofControl;
  if (!isWellFormedMidgardRedeemerItemProofControl(control)) {
    throw new Error("Invalid V1 redeemer-item proof source");
  }
  return control;
};

export const optionalTraversalCbor = (
  traversal: MidgardCekDataTraverseControl | null,
): Buffer =>
  traversal === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardCekDataTraverseControl(traversal),
        Buffer.from([0xff]),
      ]);
