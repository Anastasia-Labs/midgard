import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBoundedItem,
} from "./bounded-item.js";
import {
  advanceMidgardCekDataTraverse,
  buildMidgardCekDataTraverseTrace,
  hashMidgardCekDataTraverseControl,
  initialMidgardCekDataTraverseControl,
  MidgardCekDataTraverseStages,
} from "./cek-data-traverse.js";
import {
  midgardRedeemerItemDescriptor,
  nextMidgardRedeemerItemProofSpan,
  readMidgardRedeemerItemProofSource,
} from "./redeemer-item-proof.authenticated-span.js";
import {
  initialMidgardRedeemerItemProofControl,
  isWellFormedMidgardRedeemerItemProofControl,
  MIDGARD_REDEEMER_ITEM_FIELD_INDEX,
  type MidgardRedeemerItemProofControl,
  type MidgardRedeemerItemProofMode,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  type MidgardRedeemerItemProofTrace,
  type MidgardRedeemerItemProofTraceStep,
  type MidgardRedeemerItemProofWitness,
  readCanonicalHead,
} from "./redeemer-item-proof.is-well-formed-midgard-redeemer-item-proof-control.js";

export const advanceMidgardRedeemerItemProof = ({
  control,
  witness,
}: {
  readonly control: MidgardRedeemerItemProofControl;
  readonly witness: MidgardRedeemerItemProofWitness;
}): MidgardRedeemerItemProofControl | null => {
  const source = readMidgardRedeemerItemProofSource({ control, witness });
  if (source === null) return null;
  const { sourceBytes } = source;
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
