import { blake2b } from "@noble/hashes/blake2.js";

import { MIDGARD_BOUNDED_ITEM_CHUNK_BYTES } from "./bounded-item.js";
import { nextMidgardCekDataTraverseSpan } from "./cek-data-traverse.js";
import {
  isWellFormedMidgardLedgerOutputProofControl,
  proofMatchesOutputChunk,
} from "./ledger-output-proof.is-well-formed-midgard-ledger-output-proof-control.js";
import {
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofResultKinds,
  type MidgardLedgerOutputProofStepResult,
  type MidgardLedgerOutputProofWitness,
  type MidgardLedgerOutputSpanWindow,
} from "./ledger-output-proof.midgard-ledger-output-proof-witness.js";
import {
  advanceMidgardNativeScriptStructureToken,
  MidgardNativeScriptStructureResultKinds,
} from "./native-script-scan.js";

export const authenticatedOutputSpan = ({
  control,
  absoluteStart,
  length,
  witness,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly absoluteStart: number;
  readonly length: number;
  readonly witness: MidgardLedgerOutputProofWitness;
}): Buffer | null => {
  if (
    length <= 0 ||
    length > MIDGARD_BOUNDED_ITEM_CHUNK_BYTES ||
    absoluteStart < 0 ||
    absoluteStart + length > control.totalLength ||
    witness === null ||
    witness.kind !== "chunks"
  ) {
    return null;
  }
  const firstChunkIndex = Math.floor(
    absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const lastChunkIndex = Math.floor(
    (absoluteStart + length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  if (
    lastChunkIndex > firstChunkIndex + 1 ||
    !proofMatchesOutputChunk({
      control,
      proof: witness.chunkProof,
      chunkIndex: firstChunkIndex,
    })
  ) {
    return null;
  }
  if (lastChunkIndex === firstChunkIndex) {
    if (witness.nextChunkProof !== null) return null;
    const localStart =
      absoluteStart - firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
    return Buffer.from(
      witness.chunkProof.chunk.subarray(localStart, localStart + length),
    );
  }
  if (
    witness.nextChunkProof === null ||
    !proofMatchesOutputChunk({
      control,
      proof: witness.nextChunkProof,
      chunkIndex: lastChunkIndex,
    })
  ) {
    return null;
  }
  const localStart =
    absoluteStart - firstChunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES;
  return Buffer.from(
    Buffer.concat([
      witness.chunkProof.chunk,
      witness.nextChunkProof.chunk,
    ]).subarray(localStart, localStart + length),
  );
};

/**
 * Bind redeemer-supplied window bytes to the recorded span-window commitment
 * (one hash plus equality) and slice the demanded span out of them. Mirrors
 * `ledger_output_proof_v1.bound_window_bytes_v1`.
 */
export const boundMidgardLedgerOutputWindowBytes = ({
  spanWindow,
  absoluteStart,
  length,
  bytes,
}: {
  readonly spanWindow: MidgardLedgerOutputSpanWindow | null;
  readonly absoluteStart: number;
  readonly length: number;
  readonly bytes: Buffer;
}): Buffer | null => {
  if (spanWindow === null) return null;
  if (
    length <= 0 ||
    absoluteStart < spanWindow.start ||
    absoluteStart + length > spanWindow.start + spanWindow.length ||
    bytes.length !== spanWindow.length ||
    !Buffer.from(blake2b(bytes, { dkLen: 32 })).equals(spanWindow.digest)
  ) {
    return null;
  }
  const localStart = absoluteStart - spanWindow.start;
  return Buffer.from(bytes.subarray(localStart, localStart + length));
};

export const authenticatedDatumSource = ({
  control,
  witness,
}: {
  readonly control: MidgardLedgerOutputProofControl;
  readonly witness: Extract<
    MidgardLedgerOutputProofWitness,
    { readonly kind: "datum" }
  >;
}): { readonly sourceBytes: Buffer | null } | null => {
  const span = nextMidgardCekDataTraverseSpan(control.datum!);
  if (span === null) {
    return witness.window === null ? { sourceBytes: null } : null;
  }
  if (witness.window === null) return null;
  const sourceBytes = boundMidgardLedgerOutputWindowBytes({
    spanWindow: control.spanWindow,
    absoluteStart: span.absoluteStart,
    length: span.length,
    bytes: witness.window,
  });
  return sourceBytes === null ? null : { sourceBytes };
};

export const advancedOutputProof = (
  control: MidgardLedgerOutputProofControl,
): MidgardLedgerOutputProofStepResult | null =>
  isWellFormedMidgardLedgerOutputProofControl(control)
    ? {
        kind: MidgardLedgerOutputProofResultKinds.Advanced,
        control,
      }
    : null;

export const mapNativeStructureResult = (
  result: ReturnType<typeof advanceMidgardNativeScriptStructureToken>,
  control: MidgardLedgerOutputProofControl,
): MidgardLedgerOutputProofStepResult | null => {
  if (result === null) return null;
  if (result.kind === MidgardNativeScriptStructureResultKinds.Advanced) {
    return advancedOutputProof({
      ...control,
      nativeScript: result.control,
    });
  }
  if (result.kind === MidgardNativeScriptStructureResultKinds.NodeLimit) {
    return {
      kind: MidgardLedgerOutputProofResultKinds.NativeScriptNodeLimit,
    };
  }
  if (result.kind === MidgardNativeScriptStructureResultKinds.DepthLimit) {
    return {
      kind: MidgardLedgerOutputProofResultKinds.NativeScriptDepthLimit,
    };
  }
  return {
    kind: MidgardLedgerOutputProofResultKinds.InvalidReferenceScript,
  };
};
