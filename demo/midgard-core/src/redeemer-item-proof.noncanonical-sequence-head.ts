import {
  buildMidgardBoundedItemChunkProof,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
} from "./bounded-item.js";
import { buildMidgardCekDataTraversePrefix } from "./cek-data-traverse.build-midgard-cek-data-traverse-trace.js";
import { MidgardCekDataTraverseStages } from "./cek-data-traverse.js";
import {
  hasNonCanonicalDataHead,
  hasNonCanonicalDefiniteSequenceHead,
} from "./cek-data-traverse.noncanonical-sequence-head.js";
import { parseMidgardCekDataNodesPrefix } from "./cek-data-traverse.parse-midgard-cek-data-nodes.js";
import {
  advanceMidgardRedeemerItemProof,
  buildMidgardRedeemerItemProofTrace,
} from "./redeemer-item-proof.advance-midgard-redeemer-item-proof.js";
import {
  nextMidgardRedeemerItemProofSpan,
  readMidgardRedeemerItemProofSource,
} from "./redeemer-item-proof.authenticated-span.js";
import {
  initialMidgardRedeemerItemProofControl,
  type MidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofModes,
  MidgardRedeemerItemProofStages,
  type MidgardRedeemerItemProofWitness,
} from "./redeemer-item-proof.is-well-formed-midgard-redeemer-item-proof-control.js";

export { hasNonCanonicalDefiniteSequenceHead } from "./cek-data-traverse.noncanonical-sequence-head.js";

/** Classifies the first supported authenticated Data refusal; other syntax stays unsupported. */
export const inspectMidgardRedeemerSequenceHeads = (
  source: Uint8Array,
):
  | { readonly kind: "canonical" }
  | { readonly kind: "refusal"; readonly offset: number }
  | { readonly kind: "unsupported"; readonly cause: unknown } => {
  try {
    const prefix = parseMidgardCekDataNodesPrefix(Buffer.from(source));
    return prefix.refusalOffset === null
      ? { kind: "canonical" }
      : { kind: "refusal", offset: prefix.refusalOffset };
  } catch (cause) {
    return { kind: "unsupported", cause };
  }
};

/** Refusal is semantic only after the exact maximal head window is authenticated. */
export const isMidgardRedeemerDataHeadRejection = (
  control: MidgardRedeemerItemProofControl,
  witness: MidgardRedeemerItemProofWitness,
): boolean => {
  if (
    control.stage !== MidgardRedeemerItemProofStages.Data ||
    (control.traversal?.stage !== MidgardCekDataTraverseStages.Head &&
      control.traversal?.stage !== MidgardCekDataTraverseStages.Close &&
      control.traversal?.stage !== MidgardCekDataTraverseStages.LargeFields) ||
    witness.action.kind !== "traverseData" ||
    witness.action.action !== null
  )
    return false;
  const source = readMidgardRedeemerItemProofSource({ control, witness });
  return (
    source?.sourceBytes !== null &&
    source?.sourceBytes !== undefined &&
    (control.traversal?.stage === MidgardCekDataTraverseStages.LargeFields
      ? hasNonCanonicalDefiniteSequenceHead(source.sourceBytes)
      : hasNonCanonicalDataHead(source.sourceBytes))
  );
};

/** Replays the original prefix to its bounded first refusal without normalizing Data. */
export const buildMidgardRedeemerDataHeadRejectionTrace = (input: {
  readonly itemIndex: number;
  readonly itemCount: number;
  readonly itemBytes: Uint8Array;
}) => {
  const descriptor = buildMidgardRedeemerItemProofTrace({
    ...input,
    mode: MidgardRedeemerItemProofModes.Descriptor,
  });
  const initial = initialMidgardRedeemerItemProofControl({
    mode: MidgardRedeemerItemProofModes.Data,
    itemIndex: input.itemIndex,
    itemCount: input.itemCount,
    totalLength: descriptor.item.bytes.length,
    itemCommitment: descriptor.item.commitment,
  });
  let control = initial;
  const steps = descriptor.steps.map(({ witness }) => {
    const next = advanceMidgardRedeemerItemProof({ control, witness });
    if (next === null)
      throw new Error(
        "redeemer canonicity refusal lost its authenticated item prefix",
      );
    const step = { control, witness, next };
    control = next;
    return step;
  });
  const traversal = buildMidgardCekDataTraversePrefix({
    sourceStart: control.dataOffset,
    source: descriptor.item.bytes.subarray(
      control.dataOffset,
      control.dataOffset + control.dataLength,
    ),
  });
  if (traversal.refusalOffset === null)
    throw new Error("no authenticated sequence refusal in redeemer");
  for (const traversalStep of traversal.steps) {
    const span = nextMidgardRedeemerItemProofSpan(control);
    const first =
      span === null
        ? null
        : Math.floor(span.absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    const last =
      span === null
        ? null
        : Math.floor(
            (span.absoluteStart + span.length - 1) /
              MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
          );
    const witness: MidgardRedeemerItemProofWitness = {
      action: { kind: "traverseData", action: traversalStep.action },
      chunkProof:
        first === null
          ? null
          : buildMidgardBoundedItemChunkProof(descriptor.item, first),
      nextChunkProof:
        first === null || last === null || first === last
          ? null
          : buildMidgardBoundedItemChunkProof(descriptor.item, last),
    };
    const next = advanceMidgardRedeemerItemProof({ control, witness });
    if (next === null)
      throw new Error(
        "redeemer refusal prefix did not replay its original bytes",
      );
    steps.push({ control, witness, next });
    control = next;
  }
  const span = nextMidgardRedeemerItemProofSpan(control);
  if (span === null)
    throw new Error("redeemer canonicity refusal has no head source span");
  const first = Math.floor(
    span.absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const last = Math.floor(
    (span.absoluteStart + span.length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const witness: MidgardRedeemerItemProofWitness = {
    action: { kind: "traverseData", action: null },
    chunkProof: buildMidgardBoundedItemChunkProof(descriptor.item, first),
    nextChunkProof:
      first === last
        ? null
        : buildMidgardBoundedItemChunkProof(descriptor.item, last),
  };
  if (!isMidgardRedeemerDataHeadRejection(control, witness))
    throw new Error(
      "redeemer bytes do not prove a bounded sequence-head refusal",
    );
  return { initial, steps, control, witness };
};
