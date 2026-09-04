import { blake2b } from "@noble/hashes/blake2.js";

import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBoundedItem,
  type MidgardBoundedItemChunkProof,
  verifyMidgardBoundedItemChunkProof,
} from "./bounded-item.js";
import {
  advanceMidgardCekDataTraverse,
  buildMidgardCekDataTraverseTrace,
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataTraverse,
  hashMidgardCekDataTraverseControl,
  initialMidgardCekDataTraverseControl,
  isWellFormedMidgardCekDataTraverseControl,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  type MidgardCekDataTraverseAction,
  type MidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
  nextMidgardCekDataTraverseSpan,
} from "./cek-data-traverse.js";
import type { MidgardCekDataSummary } from "./cek-semantic.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
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

const optionalTraversalCbor = (
  traversal: MidgardCekDataTraverseControl | null,
): Buffer =>
  traversal === null
    ? Buffer.from("d87a80", "hex")
    : Buffer.concat([
        Buffer.from("d8799f", "hex"),
        encodeMidgardCekDataTraverseControl(traversal),
        Buffer.from([0xff]),
      ]);

export const encodeMidgardRedeemerItemProofControl = (
  control: MidgardRedeemerItemProofControl,
): Buffer => {
  if (!isWellFormedMidgardRedeemerItemProofControl(control)) {
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

export const hashMidgardRedeemerItemProofControl = (
  control: MidgardRedeemerItemProofControl,
): Hash32 =>
  ensureHash32(
    blake2b(
      Buffer.concat([
        CONTROL_DOMAIN,
        encodeMidgardRedeemerItemProofControl(control),
      ]),
      { dkLen: 32 },
    ),
    "redeemer_item_proof_control_hash",
  );

export const midgardRedeemerItemDescriptor = (
  control: MidgardRedeemerItemProofControl,
): MidgardRedeemerItemDescriptor | null =>
  isWellFormedMidgardRedeemerItemProofControl(control) &&
  control.stage >= MidgardRedeemerItemProofStages.Data
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

export const finalizeMidgardRedeemerItemProof = (
  control: MidgardRedeemerItemProofControl,
): MidgardCekDataSummary | null =>
  isWellFormedMidgardRedeemerItemProofControl(control) &&
  control.mode === MidgardRedeemerItemProofModes.Data &&
  control.stage === MidgardRedeemerItemProofStages.Terminal &&
  control.traversal !== null
    ? finalizeMidgardCekDataTraverse(control.traversal)
    : null;

export const nextMidgardRedeemerItemProofSpan = (
  control: MidgardRedeemerItemProofControl,
): { readonly absoluteStart: number; readonly length: number } | null => {
  if (!isWellFormedMidgardRedeemerItemProofControl(control)) return null;
  if (control.stage === MidgardRedeemerItemProofStages.Header) {
    return {
      absoluteStart: 0,
      length: Math.min(
        control.totalLength,
        MIDGARD_REDEEMER_ITEM_MAX_HEADER_SPAN,
      ),
    };
  }
  if (control.stage === MidgardRedeemerItemProofStages.Tail) {
    return {
      absoluteStart: control.dataOffset + control.dataLength,
      length: control.totalLength - control.dataOffset - control.dataLength,
    };
  }
  if (
    control.stage === MidgardRedeemerItemProofStages.Data &&
    control.traversal !== null
  ) {
    return nextMidgardCekDataTraverseSpan(control.traversal);
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
  readonly control: MidgardRedeemerItemProofControl;
  readonly absoluteStart: number;
  readonly length: number;
  readonly chunkProof: MidgardBoundedItemChunkProof;
  readonly nextChunkProof: MidgardBoundedItemChunkProof | null;
}): Buffer | null => {
  if (
    length <= 0 ||
    length > MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN ||
    absoluteStart < 0 ||
    absoluteStart + length > control.totalLength
  ) {
    return null;
  }
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const matches = (
    proof: MidgardBoundedItemChunkProof,
    chunkIndex: number,
  ): boolean =>
    proof.fieldIndex === MIDGARD_REDEEMER_ITEM_FIELD_INDEX &&
    proof.itemIndex === control.itemIndex &&
    proof.totalLength === control.totalLength &&
    proof.chunkIndex === chunkIndex &&
    verifyMidgardBoundedItemChunkProof({
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
    absoluteStart - firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
  if (lastChunkIndex === firstChunkIndex) {
    return nextChunkProof === null
      ? chunkProof.chunk.subarray(localStart, localStart + length)
      : null;
  }
  return nextChunkProof !== null && matches(nextChunkProof, lastChunkIndex)
    ? Buffer.concat([chunkProof.chunk, nextChunkProof.chunk]).subarray(
        localStart,
        localStart + length,
      )
    : null;
};

export const advanceMidgardRedeemerItemProof = ({
  control,
  witness,
}: {
  readonly control: MidgardRedeemerItemProofControl;
  readonly witness: MidgardRedeemerItemProofWitness;
}): MidgardRedeemerItemProofControl | null => {
  if (!isWellFormedMidgardRedeemerItemProofControl(control)) return null;
  const span = nextMidgardRedeemerItemProofSpan(control);
  let sourceBytes: Buffer | null = null;
  if (span === null) {
    if (witness.chunkProof !== null || witness.nextChunkProof !== null) {
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
      control.stage === MidgardRedeemerItemProofStages.Header &&
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
        stage: MidgardRedeemerItemProofStages.Tail,
        purposeTag: purpose.value,
        pointerIndex: pointer.value,
        dataOffset: data.nextOffset,
        dataLength: data.value,
      } satisfies MidgardRedeemerItemProofControl;
      return isWellFormedMidgardRedeemerItemProofControl(next) ? next : null;
    }
    if (
      control.stage === MidgardRedeemerItemProofStages.Tail &&
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
          control.mode === MidgardRedeemerItemProofModes.Data
            ? MidgardRedeemerItemProofStages.Data
            : MidgardRedeemerItemProofStages.Terminal,
        executionMemory: BigInt(memory.value),
        executionSteps: BigInt(steps.value),
        traversal:
          control.mode === MidgardRedeemerItemProofModes.Data
            ? initialMidgardCekDataTraverseControl({
                sourceStart: control.dataOffset,
                sourceLength: control.dataLength,
              })
            : null,
      } satisfies MidgardRedeemerItemProofControl;
      return isWellFormedMidgardRedeemerItemProofControl(next) ? next : null;
    }
    if (
      control.stage === MidgardRedeemerItemProofStages.Data &&
      control.traversal !== null
    ) {
      if (
        control.traversal.stage === MidgardCekDataTraverseStages.Terminal &&
        witness.action.kind === "finishData" &&
        sourceBytes === null
      ) {
        const next = {
          ...control,
          stage: MidgardRedeemerItemProofStages.Terminal,
        } satisfies MidgardRedeemerItemProofControl;
        return isWellFormedMidgardRedeemerItemProofControl(next) ? next : null;
      }
      if (witness.action.kind === "traverseData") {
        const nextTraversal = advanceMidgardCekDataTraverse({
          control: control.traversal,
          sourceBytes,
          action: witness.action.action,
        });
        if (nextTraversal === null) return null;
        const next = {
          ...control,
          traversal: nextTraversal,
        } satisfies MidgardRedeemerItemProofControl;
        return isWellFormedMidgardRedeemerItemProofControl(next) ? next : null;
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
  readonly item: MidgardBoundedItem;
  readonly absoluteStart: number;
  readonly length: number;
}): Pick<MidgardRedeemerItemProofWitness, "chunkProof" | "nextChunkProof"> => {
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  return {
    chunkProof: buildMidgardBoundedItemChunkProof(item, firstChunkIndex),
    nextChunkProof:
      lastChunkIndex === firstChunkIndex
        ? null
        : buildMidgardBoundedItemChunkProof(item, lastChunkIndex),
  };
};

export const buildMidgardRedeemerItemProofTrace = ({
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
  readonly mode: MidgardRedeemerItemProofMode;
  readonly expectedPurposeTag?: number;
  readonly expectedPointerIndex?: number;
}): MidgardRedeemerItemProofTrace => {
  const item = buildMidgardBoundedItem({
    fieldIndex: MIDGARD_REDEEMER_ITEM_FIELD_INDEX,
    itemIndex,
    bytes: itemBytes,
  });
  const initial = initialMidgardRedeemerItemProofControl({
    mode,
    itemIndex,
    itemCount,
    totalLength: item.bytes.length,
    itemCommitment: item.commitment,
    expectedPurposeTag,
    expectedPointerIndex,
  });
  const steps: MidgardRedeemerItemProofTraceStep[] = [];
  let control = initial;
  const emit = (witness: MidgardRedeemerItemProofWitness): void => {
    const next = advanceMidgardRedeemerItemProof({
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
    const span = nextMidgardRedeemerItemProofSpan(control);
    if (span === null) throw new Error("Missing redeemer item span");
    emit({ action, ...spanProofs({ item, ...span }) });
  }
  if (mode === MidgardRedeemerItemProofModes.Data) {
    const descriptor = midgardRedeemerItemDescriptor(control);
    if (descriptor === null || control.traversal === null) {
      throw new Error("Missing redeemer Data descriptor");
    }
    const traversal = buildMidgardCekDataTraverseTrace({
      sourceStart: descriptor.dataOffset,
      source: item.bytes.subarray(
        descriptor.dataOffset,
        descriptor.dataOffset + descriptor.dataLength,
      ),
    });
    if (
      !hashMidgardCekDataTraverseControl(traversal.initial).equals(
        hashMidgardCekDataTraverseControl(control.traversal),
      )
    ) {
      throw new Error("Redeemer Data traversal did not bind its source");
    }
    for (const traversalStep of traversal.steps) {
      const span = nextMidgardRedeemerItemProofSpan(control);
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
  if (control.stage !== MidgardRedeemerItemProofStages.Terminal) {
    throw new Error("Redeemer item proof did not reach terminal");
  }
  return { item, initial, steps, terminal: control };
};
