import { blake2b } from "@noble/hashes/blake2.js";

import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBoundedItemChunkProof,
  verifyMidgardBoundedItemChunkProof,
} from "./bounded-item.js";
import {
  finalizeMidgardCekDataTraverse,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  nextMidgardCekDataTraverseSpan,
} from "./cek-data-traverse.js";
import type { MidgardCekDataSummary } from "./cek-semantic.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  CONTROL_DOMAIN,
  isWellFormedMidgardRedeemerItemProofControl,
  MIDGARD_REDEEMER_ITEM_FIELD_INDEX,
  MIDGARD_REDEEMER_ITEM_MAX_HEADER_SPAN,
  type MidgardRedeemerItemDescriptor,
  type MidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  type MidgardRedeemerItemProofWitness,
  optionalTraversalCbor,
} from "./redeemer-item-proof.is-well-formed-midgard-redeemer-item-proof-control.js";

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

/** Authenticate the canonical next source window, distinguishing no window from invalid evidence. */
export const readMidgardRedeemerItemProofSource = ({
  control,
  witness,
}: {
  readonly control: MidgardRedeemerItemProofControl;
  readonly witness: MidgardRedeemerItemProofWitness;
}): { readonly sourceBytes: Buffer | null } | null => {
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
  return { sourceBytes };
};
