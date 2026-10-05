import {
  MIDGARD_BLAKE2B_BLOCK_BYTES,
  MidgardBlake2b224TraceStages,
} from "./blake2b-224-trace.js";
import {
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  midgardBoundedItemChunkCount,
  midgardBoundedItemExpectedChunkLength,
} from "./bounded-item.js";
import { nextMidgardCekDataTraverseSpan } from "./cek-data-traverse.js";
import { midgardBlake2b } from "./codec/blake2b.js";
import {
  isWellFormedMidgardLedgerOutputProofControl,
  proofMatchesOutputChunk,
} from "./ledger-output-proof.is-well-formed-midgard-ledger-output-proof-control.js";
import {
  type MidgardLedgerOutputProofControl,
  MidgardLedgerOutputProofResultKinds,
  MidgardLedgerOutputProofStages,
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

type OutputSpan = {
  readonly absoluteStart: number;
  readonly length: number;
};

/**
 * Whether the recorded span window covers `[absoluteStart, absoluteStart +
 * length)`. Mirrors `ledger_output_proof_v1.window_covers_v1`.
 */
export const midgardLedgerOutputWindowCovers = ({
  spanWindow,
  absoluteStart,
  length,
}: {
  readonly spanWindow: MidgardLedgerOutputSpanWindow | null;
  readonly absoluteStart: number;
  readonly length: number;
}): boolean =>
  spanWindow !== null &&
  length > 0 &&
  absoluteStart >= spanWindow.start &&
  absoluteStart + length <= spanWindow.start + spanWindow.length;

/**
 * The output span of reference-script chunk `chunkIndex`. Mirrors
 * `ledger_output_proof_v1.reference_script_chunk_span_v1`.
 */
export const midgardLedgerOutputReferenceScriptChunkSpan = ({
  itemOffset,
  itemLength,
  chunkIndex,
}: {
  readonly itemOffset: number;
  readonly itemLength: number;
  readonly chunkIndex: number;
}): OutputSpan => ({
  absoluteStart: itemOffset + chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  length: midgardBoundedItemExpectedChunkLength({
    totalLength: itemLength,
    chunkIndex,
  }),
});

/**
 * The script bytes the next ready block of the reference-script hash trace
 * reads; the first block's leading byte is the language tag, which is not
 * read from the output. Mirrors
 * `ledger_output_proof_v1.script_hash_content_span_v1`.
 */
export const midgardLedgerOutputScriptHashContentSpan = ({
  referenceScriptOffset,
  cursor,
  totalLength,
}: {
  readonly referenceScriptOffset: number;
  readonly cursor: number;
  readonly totalLength: number;
}): OutputSpan => {
  const expectedLength = Math.min(
    MIDGARD_BLAKE2B_BLOCK_BYTES,
    totalLength - cursor,
  );
  return cursor === 0
    ? { absoluteStart: referenceScriptOffset, length: expectedLength - 1 }
    : {
        absoluteStart: referenceScriptOffset + cursor - 1,
        length: expectedLength,
      };
};

/**
 * The output span the control's consuming stage step reads next, or `null`
 * when that step reads no output bytes. Mirrors
 * `ledger_output_proof_v1.demanded_span_v1`.
 */
export const demandedMidgardLedgerOutputProofSpan = (
  control: MidgardLedgerOutputProofControl,
): OutputSpan | null => {
  if (control.stage === MidgardLedgerOutputProofStages.DatumTraversal) {
    return control.datum === null
      ? null
      : nextMidgardCekDataTraverseSpan(control.datum);
  }
  if (
    control.stage === MidgardLedgerOutputProofStages.ReferenceScriptCommitment
  ) {
    const itemOffset = control.outputScan.referenceScriptItemOffset;
    const itemLength = control.totalLength - itemOffset;
    const chunkIndex = control.referenceScriptFrontier.count;
    return chunkIndex === midgardBoundedItemChunkCount(itemLength)
      ? null
      : midgardLedgerOutputReferenceScriptChunkSpan({
          itemOffset,
          itemLength,
          chunkIndex,
        });
  }
  if (control.stage === MidgardLedgerOutputProofStages.ScriptHash) {
    const scriptHash = control.scriptHash;
    if (
      scriptHash === null ||
      scriptHash.stage !== MidgardBlake2b224TraceStages.Ready
    ) {
      return null;
    }
    const span = midgardLedgerOutputScriptHashContentSpan({
      referenceScriptOffset: control.outputScan.referenceScriptOffset,
      cursor: scriptHash.cursor,
      totalLength: scriptHash.totalLength,
    });
    return span.length > 0 ? span : null;
  }
  return null;
};

/**
 * The length of the window a span attach records at `start`: the maximal
 * window, `min(chunk bytes, totalLength - start)`. Mirrors
 * `ledger_output_proof_v1.attach_window_length_v1`.
 */
export const midgardLedgerOutputAttachWindowLength = (
  totalLength: number,
  start: number,
): number => Math.min(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES, totalLength - start);

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
  if (
    spanWindow === null ||
    !midgardLedgerOutputWindowCovers({ spanWindow, absoluteStart, length }) ||
    bytes.length !== spanWindow.length ||
    !Buffer.from(midgardBlake2b(bytes, { dkLen: 32 })).equals(spanWindow.digest)
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
