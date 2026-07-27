import { blake2b } from "@noble/hashes/blake2.js";

import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  type MidgardBoundedItemChunkProofV1,
  type MidgardBoundedItemV1,
  verifyMidgardBoundedItemChunkProofV1,
} from "./bounded-item-v1.js";
import {
  advanceMidgardCekDataTraverseV1,
  buildMidgardCekDataTraverseTraceV1,
  encodeMidgardCekDataTraverseControlV1,
  finalizeMidgardCekDataTraverseV1,
  hashMidgardCekDataTraverseControlV1,
  initialMidgardCekDataTraverseControlV1,
  isWellFormedMidgardCekDataTraverseControlV1,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
  type MidgardCekDataTraverseActionV1,
  type MidgardCekDataTraverseControlV1,
  MidgardCekDataTraverseStagesV1,
  nextMidgardCekDataTraverseSpanV1,
} from "./cek-data-traverse-v1.js";
import type { MidgardCekDataSummaryV1 } from "./cek-semantic.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

export const MIDGARD_REDEEMER_ITEM_PROOF_V1_VERSION = 1 as const;
export const MIDGARD_REDEEMER_ITEM_FIELD_INDEX_V1 = 8 as const;
export const MIDGARD_REDEEMER_ITEM_MAX_HEADER_SPAN_V1 = 28 as const;
export const MIDGARD_REDEEMER_ITEM_MAX_TAIL_SPAN_V1 = 19 as const;

export const MidgardRedeemerItemProofModesV1 = Object.freeze({
  Descriptor: 0,
  Data: 1,
} as const);

export const MidgardRedeemerItemProofStagesV1 = Object.freeze({
  Header: 0,
  Tail: 1,
  Data: 2,
  Terminal: 3,
} as const);

export type MidgardRedeemerItemProofModeV1 =
  (typeof MidgardRedeemerItemProofModesV1)[keyof typeof MidgardRedeemerItemProofModesV1];

export type MidgardRedeemerItemProofStageV1 =
  (typeof MidgardRedeemerItemProofStagesV1)[keyof typeof MidgardRedeemerItemProofStagesV1];

export type MidgardRedeemerItemDescriptorV1 = {
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

export type MidgardRedeemerItemProofControlV1 = {
  readonly version: typeof MIDGARD_REDEEMER_ITEM_PROOF_V1_VERSION;
  readonly mode: MidgardRedeemerItemProofModeV1;
  readonly stage: MidgardRedeemerItemProofStageV1;
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
  readonly traversal: MidgardCekDataTraverseControlV1 | null;
};

export type MidgardRedeemerItemProofActionV1 =
  | { readonly kind: "openHeader" }
  | { readonly kind: "openTail" }
  | {
      readonly kind: "traverseData";
      readonly action: MidgardCekDataTraverseActionV1;
    }
  | { readonly kind: "finishData" };

export type MidgardRedeemerItemProofWitnessV1 = {
  readonly action: MidgardRedeemerItemProofActionV1;
  readonly chunkProof: MidgardBoundedItemChunkProofV1 | null;
  readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
};

export type MidgardRedeemerItemProofTraceStepV1 = {
  readonly control: MidgardRedeemerItemProofControlV1;
  readonly witness: MidgardRedeemerItemProofWitnessV1;
  readonly next: MidgardRedeemerItemProofControlV1;
};

export type MidgardRedeemerItemProofTraceV1 = {
  readonly item: MidgardBoundedItemV1;
  readonly initial: MidgardRedeemerItemProofControlV1;
  readonly steps: readonly MidgardRedeemerItemProofTraceStepV1[];
  readonly terminal: MidgardRedeemerItemProofControlV1;
};

type CborHead = {
  readonly major: number;
  readonly value: number;
  readonly nextOffset: number;
};

const CONTROL_DOMAIN = Buffer.from(
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

const readCanonicalHead = (
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
  control: MidgardRedeemerItemProofControlV1,
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
    tailLength <= MIDGARD_REDEEMER_ITEM_MAX_TAIL_SPAN_V1 &&
    (control.expectedPurposeTag === -1 ||
      (control.purposeTag === control.expectedPurposeTag &&
        control.pointerIndex === control.expectedPointerIndex))
  );
};

export const isWellFormedMidgardRedeemerItemProofControlV1 = (
  control: MidgardRedeemerItemProofControlV1,
): boolean => {
  try {
    const expectedAbsent =
      control.expectedPurposeTag === -1 &&
      control.expectedPointerIndex === -1;
    const expectedPresent =
      supportedPurposeTag(control.expectedPurposeTag) &&
      control.expectedPointerIndex >= 0;
    const descriptorOpen = openedDescriptorIsWellFormed(control);
    const exUnitsOpen =
      control.executionMemory >= 0n && control.executionSteps >= 0n;
    const traversalOpen =
      control.traversal !== null &&
      isWellFormedMidgardCekDataTraverseControlV1(control.traversal) &&
      control.traversal.sourceStart === control.dataOffset &&
      control.traversal.sourceLength === control.dataLength;
    return (
      control.version === MIDGARD_REDEEMER_ITEM_PROOF_V1_VERSION &&
      (control.mode === MidgardRedeemerItemProofModesV1.Descriptor ||
        control.mode === MidgardRedeemerItemProofModesV1.Data) &&
      control.stage >= MidgardRedeemerItemProofStagesV1.Header &&
      control.stage <= MidgardRedeemerItemProofStagesV1.Terminal &&
      control.itemIndex >= 0 &&
      control.itemCount > control.itemIndex &&
      control.totalLength > 0 &&
      control.itemCommitment.length === 32 &&
      (expectedAbsent || expectedPresent) &&
      (control.stage === MidgardRedeemerItemProofStagesV1.Header
        ? control.purposeTag === -1 &&
          control.pointerIndex === -1 &&
          control.dataOffset === 0 &&
          control.dataLength === 0 &&
          control.executionMemory === -1n &&
          control.executionSteps === -1n &&
          control.traversal === null
        : control.stage === MidgardRedeemerItemProofStagesV1.Tail
          ? descriptorOpen &&
            control.executionMemory === -1n &&
            control.executionSteps === -1n &&
            control.traversal === null
          : control.stage === MidgardRedeemerItemProofStagesV1.Data
            ? control.mode === MidgardRedeemerItemProofModesV1.Data &&
              descriptorOpen &&
              exUnitsOpen &&
              traversalOpen
            : control.mode ===
                MidgardRedeemerItemProofModesV1.Descriptor
              ? descriptorOpen && exUnitsOpen && control.traversal === null
              : descriptorOpen &&
                exUnitsOpen &&
                traversalOpen &&
                control.traversal!.stage ===
                  MidgardCekDataTraverseStagesV1.Terminal &&
                finalizeMidgardCekDataTraverseV1(
                  control.traversal!,
                ) !== null)
    );
  } catch {
    return false;
  }
};

export const initialMidgardRedeemerItemProofControlV1 = ({
  mode,
  itemIndex,
  itemCount,
  totalLength,
  itemCommitment,
  expectedPurposeTag = -1,
  expectedPointerIndex = -1,
}: {
  readonly mode: MidgardRedeemerItemProofModeV1;
  readonly itemIndex: number;
  readonly itemCount: number;
  readonly totalLength: number;
  readonly itemCommitment: Uint8Array;
  readonly expectedPurposeTag?: number;
  readonly expectedPointerIndex?: number;
}): MidgardRedeemerItemProofControlV1 => {
  const control = {
    version: MIDGARD_REDEEMER_ITEM_PROOF_V1_VERSION,
    mode,
    stage: MidgardRedeemerItemProofStagesV1.Header,
    itemIndex: exactSafeInt(itemIndex, "itemIndex"),
    itemCount: exactSafeInt(itemCount, "itemCount"),
    totalLength: exactSafeInt(totalLength, "totalLength"),
    itemCommitment: ensureHash32(itemCommitment, "itemCommitment"),
    expectedPurposeTag: exactSafeInt(
      expectedPurposeTag,
      "expectedPurposeTag",
    ),
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
  } satisfies MidgardRedeemerItemProofControlV1;
  if (!isWellFormedMidgardRedeemerItemProofControlV1(control)) {
    throw new Error("Invalid V1 redeemer-item proof source");
  }
  return control;
};

const optionalTraversalCbor = (
  traversal: MidgardCekDataTraverseControlV1 | null,
): Buffer =>
  traversal === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardCekDataTraverseControlV1(traversal),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardRedeemerItemProofControlV1 = (
  control: MidgardRedeemerItemProofControlV1,
): Buffer => {
  if (!isWellFormedMidgardRedeemerItemProofControlV1(control)) {
    throw new Error("Invalid V1 redeemer-item proof control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(control.version)),
    encodeCbor(BigInt(control.mode)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.itemIndex)),
    encodeCbor(BigInt(control.itemCount)),
    encodeCbor(BigInt(control.totalLength)),
    encodeCbor(control.itemCommitment),
    encodeCbor(BigInt(control.expectedPurposeTag)),
    encodeCbor(BigInt(control.expectedPointerIndex)),
    encodeCbor(BigInt(control.purposeTag)),
    encodeCbor(BigInt(control.pointerIndex)),
    encodeCbor(BigInt(control.dataOffset)),
    encodeCbor(BigInt(control.dataLength)),
    encodeCbor(control.executionMemory),
    encodeCbor(control.executionSteps),
    optionalTraversalCbor(control.traversal),
  ]);
};

export const hashMidgardRedeemerItemProofControlV1 = (
  control: MidgardRedeemerItemProofControlV1,
): Hash32 =>
  ensureHash32(
    blake2b(
      Buffer.concat([
        CONTROL_DOMAIN,
        encodeMidgardRedeemerItemProofControlV1(control),
      ]),
      { dkLen: 32 },
    ),
    "redeemer_item_proof_control_hash",
  );

export const midgardRedeemerItemDescriptorV1 = (
  control: MidgardRedeemerItemProofControlV1,
): MidgardRedeemerItemDescriptorV1 | null =>
  isWellFormedMidgardRedeemerItemProofControlV1(control) &&
  control.stage >= MidgardRedeemerItemProofStagesV1.Data
    ? {
        itemIndex: control.itemIndex,
        itemCount: control.itemCount,
        totalLength: control.totalLength,
        itemCommitment: control.itemCommitment,
        purposeTag: control.purposeTag,
        pointerIndex: control.pointerIndex,
        dataOffset: control.dataOffset,
        dataLength: control.dataLength,
        executionMemory: control.executionMemory,
        executionSteps: control.executionSteps,
      }
    : null;

export const finalizeMidgardRedeemerItemProofV1 = (
  control: MidgardRedeemerItemProofControlV1,
): MidgardCekDataSummaryV1 | null =>
  isWellFormedMidgardRedeemerItemProofControlV1(control) &&
  control.mode === MidgardRedeemerItemProofModesV1.Data &&
  control.stage === MidgardRedeemerItemProofStagesV1.Terminal &&
  control.traversal !== null
    ? finalizeMidgardCekDataTraverseV1(control.traversal)
    : null;

export const nextMidgardRedeemerItemProofSpanV1 = (
  control: MidgardRedeemerItemProofControlV1,
): { readonly absoluteStart: number; readonly length: number } | null => {
  if (!isWellFormedMidgardRedeemerItemProofControlV1(control)) return null;
  if (control.stage === MidgardRedeemerItemProofStagesV1.Header) {
    return {
      absoluteStart: 0,
      length: Math.min(
        control.totalLength,
        MIDGARD_REDEEMER_ITEM_MAX_HEADER_SPAN_V1,
      ),
    };
  }
  if (control.stage === MidgardRedeemerItemProofStagesV1.Tail) {
    return {
      absoluteStart: control.dataOffset + control.dataLength,
      length:
        control.totalLength - control.dataOffset - control.dataLength,
    };
  }
  if (
    control.stage === MidgardRedeemerItemProofStagesV1.Data &&
    control.traversal !== null
  ) {
    return nextMidgardCekDataTraverseSpanV1(control.traversal);
  }
  return null;
};

const authenticatedSpan = ({
  control,
  absoluteStart,
  length,
  chunkProof,
  nextChunkProof,
}: {
  readonly control: MidgardRedeemerItemProofControlV1;
  readonly absoluteStart: number;
  readonly length: number;
  readonly chunkProof: MidgardBoundedItemChunkProofV1;
  readonly nextChunkProof: MidgardBoundedItemChunkProofV1 | null;
}): Buffer | null => {
  if (
    length <= 0 ||
    length > MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1 ||
    absoluteStart < 0 ||
    absoluteStart + length > control.totalLength
  ) {
    return null;
  }
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) /
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  const matches = (
    proof: MidgardBoundedItemChunkProofV1,
    chunkIndex: number,
  ): boolean =>
    proof.fieldIndex === MIDGARD_REDEEMER_ITEM_FIELD_INDEX_V1 &&
    proof.itemIndex === control.itemIndex &&
    proof.totalLength === control.totalLength &&
    proof.chunkIndex === chunkIndex &&
    verifyMidgardBoundedItemChunkProofV1({
      expectedCommitment: control.itemCommitment,
      proof,
    });
  if (
    lastChunkIndex > firstChunkIndex + 1 ||
    !matches(chunkProof, firstChunkIndex)
  ) {
    return null;
  }
  const localStart =
    absoluteStart -
    firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1;
  if (lastChunkIndex === firstChunkIndex) {
    return nextChunkProof === null
      ? chunkProof.chunk.subarray(localStart, localStart + length)
      : null;
  }
  return nextChunkProof !== null &&
    matches(nextChunkProof, lastChunkIndex)
    ? Buffer.concat([chunkProof.chunk, nextChunkProof.chunk]).subarray(
        localStart,
        localStart + length,
      )
    : null;
};

export const advanceMidgardRedeemerItemProofV1 = ({
  control,
  witness,
}: {
  readonly control: MidgardRedeemerItemProofControlV1;
  readonly witness: MidgardRedeemerItemProofWitnessV1;
}): MidgardRedeemerItemProofControlV1 | null => {
  if (!isWellFormedMidgardRedeemerItemProofControlV1(control)) return null;
  const span = nextMidgardRedeemerItemProofSpanV1(control);
  let sourceBytes: Buffer | null = null;
  if (span === null) {
    if (
      witness.chunkProof !== null ||
      witness.nextChunkProof !== null
    ) {
      return null;
    }
  } else {
    if (witness.chunkProof === null) return null;
    sourceBytes = authenticatedSpan({
      control,
      ...span,
      chunkProof: witness.chunkProof,
      nextChunkProof: witness.nextChunkProof,
    });
    if (sourceBytes === null) return null;
  }
  try {
    if (
      control.stage === MidgardRedeemerItemProofStagesV1.Header &&
      witness.action.kind === "openHeader" &&
      sourceBytes !== null
    ) {
      const outer = readCanonicalHead(sourceBytes, 0, 4);
      const purpose =
        outer?.value === 4
          ? readCanonicalHead(sourceBytes, outer.nextOffset, 0)
          : null;
      const pointer =
        purpose === null
          ? null
          : readCanonicalHead(sourceBytes, purpose.nextOffset, 0);
      const data =
        pointer === null
          ? null
          : readCanonicalHead(sourceBytes, pointer.nextOffset, 2);
      if (
        outer === null ||
        purpose === null ||
        pointer === null ||
        data === null
      ) {
        return null;
      }
      const next = {
        ...control,
        stage: MidgardRedeemerItemProofStagesV1.Tail,
        purposeTag: purpose.value,
        pointerIndex: pointer.value,
        dataOffset: data.nextOffset,
        dataLength: data.value,
      } satisfies MidgardRedeemerItemProofControlV1;
      return isWellFormedMidgardRedeemerItemProofControlV1(next)
        ? next
        : null;
    }
    if (
      control.stage === MidgardRedeemerItemProofStagesV1.Tail &&
      witness.action.kind === "openTail" &&
      sourceBytes !== null
    ) {
      const outer = readCanonicalHead(sourceBytes, 0, 4);
      const memory =
        outer?.value === 2
          ? readCanonicalHead(sourceBytes, outer.nextOffset, 0)
          : null;
      const steps =
        memory === null
          ? null
          : readCanonicalHead(sourceBytes, memory.nextOffset, 0);
      if (
        outer === null ||
        memory === null ||
        steps === null ||
        steps.nextOffset !== sourceBytes.length
      ) {
        return null;
      }
      const next = {
        ...control,
        stage:
          control.mode === MidgardRedeemerItemProofModesV1.Data
            ? MidgardRedeemerItemProofStagesV1.Data
            : MidgardRedeemerItemProofStagesV1.Terminal,
        executionMemory: BigInt(memory.value),
        executionSteps: BigInt(steps.value),
        traversal:
          control.mode === MidgardRedeemerItemProofModesV1.Data
            ? initialMidgardCekDataTraverseControlV1({
                sourceStart: control.dataOffset,
                sourceLength: control.dataLength,
              })
            : null,
      } satisfies MidgardRedeemerItemProofControlV1;
      return isWellFormedMidgardRedeemerItemProofControlV1(next)
        ? next
        : null;
    }
    if (
      control.stage === MidgardRedeemerItemProofStagesV1.Data &&
      control.traversal !== null
    ) {
      if (
        control.traversal.stage ===
          MidgardCekDataTraverseStagesV1.Terminal &&
        witness.action.kind === "finishData" &&
        sourceBytes === null
      ) {
        const next = {
          ...control,
          stage: MidgardRedeemerItemProofStagesV1.Terminal,
        } satisfies MidgardRedeemerItemProofControlV1;
        return isWellFormedMidgardRedeemerItemProofControlV1(next)
          ? next
          : null;
      }
      if (witness.action.kind === "traverseData") {
        const nextTraversal = advanceMidgardCekDataTraverseV1({
          control: control.traversal,
          sourceBytes,
          action: witness.action.action,
        });
        if (nextTraversal === null) return null;
        const next = {
          ...control,
          traversal: nextTraversal,
        } satisfies MidgardRedeemerItemProofControlV1;
        return isWellFormedMidgardRedeemerItemProofControlV1(next)
          ? next
          : null;
      }
    }
    return null;
  } catch {
    return null;
  }
};

const spanProofs = ({
  item,
  absoluteStart,
  length,
}: {
  readonly item: MidgardBoundedItemV1;
  readonly absoluteStart: number;
  readonly length: number;
}): Pick<
  MidgardRedeemerItemProofWitnessV1,
  "chunkProof" | "nextChunkProof"
> => {
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) /
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  );
  return {
    chunkProof: buildMidgardBoundedItemChunkProofV1(
      item,
      firstChunkIndex,
    ),
    nextChunkProof:
      lastChunkIndex === firstChunkIndex
        ? null
        : buildMidgardBoundedItemChunkProofV1(item, lastChunkIndex),
  };
};

export const buildMidgardRedeemerItemProofTraceV1 = ({
  itemIndex,
  itemCount,
  itemBytes,
  mode,
  expectedPurposeTag = -1,
  expectedPointerIndex = -1,
}: {
  readonly itemIndex: number;
  readonly itemCount: number;
  readonly itemBytes: Uint8Array;
  readonly mode: MidgardRedeemerItemProofModeV1;
  readonly expectedPurposeTag?: number;
  readonly expectedPointerIndex?: number;
}): MidgardRedeemerItemProofTraceV1 => {
  const item = buildMidgardBoundedItemV1({
    fieldIndex: MIDGARD_REDEEMER_ITEM_FIELD_INDEX_V1,
    itemIndex,
    bytes: itemBytes,
  });
  const initial = initialMidgardRedeemerItemProofControlV1({
    mode,
    itemIndex,
    itemCount,
    totalLength: item.bytes.length,
    itemCommitment: item.commitment,
    expectedPurposeTag,
    expectedPointerIndex,
  });
  const steps: MidgardRedeemerItemProofTraceStepV1[] = [];
  let control = initial;
  const emit = (
    witness: MidgardRedeemerItemProofWitnessV1,
  ): void => {
    const next = advanceMidgardRedeemerItemProofV1({
      control,
      witness,
    });
    if (next === null) {
      throw new Error("V1 redeemer-item proof trace failed closed");
    }
    steps.push({ control, witness, next });
    control = next;
  };
  for (const action of [
    { kind: "openHeader" } as const,
    { kind: "openTail" } as const,
  ]) {
    const span = nextMidgardRedeemerItemProofSpanV1(control);
    if (span === null) throw new Error("Missing redeemer item span");
    emit({ action, ...spanProofs({ item, ...span }) });
  }
  if (mode === MidgardRedeemerItemProofModesV1.Data) {
    const descriptor = midgardRedeemerItemDescriptorV1(control);
    if (descriptor === null || control.traversal === null) {
      throw new Error("Missing redeemer Data descriptor");
    }
    const traversal = buildMidgardCekDataTraverseTraceV1({
      sourceStart: descriptor.dataOffset,
      source: item.bytes.subarray(
        descriptor.dataOffset,
        descriptor.dataOffset + descriptor.dataLength,
      ),
    });
    if (
      !hashMidgardCekDataTraverseControlV1(traversal.initial).equals(
        hashMidgardCekDataTraverseControlV1(control.traversal),
      )
    ) {
      throw new Error("Redeemer Data traversal did not bind its source");
    }
    for (const traversalStep of traversal.steps) {
      const span = nextMidgardRedeemerItemProofSpanV1(control);
      emit({
        action: {
          kind: "traverseData",
          action: traversalStep.action,
        },
        ...(span === null
          ? { chunkProof: null, nextChunkProof: null }
          : spanProofs({ item, ...span })),
      });
    }
    emit({
      action: { kind: "finishData" },
      chunkProof: null,
      nextChunkProof: null,
    });
  }
  if (
    control.stage !== MidgardRedeemerItemProofStagesV1.Terminal
  ) {
    throw new Error("Redeemer item proof did not reach terminal");
  }
  return { item, initial, steps, terminal: control };
};
