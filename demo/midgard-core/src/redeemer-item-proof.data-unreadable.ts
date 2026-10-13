import {
  buildMidgardBoundedItemChunkProof,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardBoundedItem,
} from "./bounded-item.js";
import { driveMidgardCekDataTraverse } from "./cek-data-traverse.drive.js";
import { type MidgardCekDataTraverseAction } from "./cek-data-traverse.js";
import { isUnreadableMidgardCekDataTraverse } from "./cek-data-traverse.unreadable.js";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  assertMidgardPlutusDataWellFormed,
} from "./plutus-data-cbor.js";
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

/**
 * Classifies redeemer Data bytes by the streamed traverse's verdict:
 * canonical, or `Unreadable` at the source offset of the step that reads the
 * offending byte. `unsupported` is only a span the traverse cannot carry at
 * all (empty or beyond its source bound).
 *
 * Canonical bytes are recognized by the single-pass admission gate
 * (`assertMidgardPlutusDataWellFormed` plus the exact `serialiseData`
 * re-encoding), whose verdict the canonical Data corpus test locks to the
 * traverse's; only a refused span pays for the hashed traverse, to name its
 * offset.
 */
export const inspectMidgardRedeemerData = (
  source: Uint8Array,
):
  | { readonly kind: "canonical" }
  | { readonly kind: "refusal"; readonly offset: number }
  | { readonly kind: "unsupported"; readonly cause: unknown } => {
  try {
    assertMidgardPlutusDataWellFormed(source);
    const hex = Buffer.from(source).toString("hex");
    if (aikenSerialisedPlutusDataCborPreservingMapOrder(hex) === hex)
      return { kind: "canonical" };
  } catch {
    // Not canonical; the traverse names where.
  }
  try {
    const outcome = driveMidgardCekDataTraverse({ source });
    return outcome.verdict === "accepted"
      ? { kind: "canonical" }
      : { kind: "refusal", offset: outcome.refusal.control.offset };
  } catch (cause) {
    return { kind: "unsupported", cause };
  }
};

/**
 * The redeemer item's Data is `Unreadable` at this step: the item proof is in
 * its Data stage and the traverse's `Unreadable` holds for the authenticated
 * window and the witness's traverse action (mirrors `apply_step`'s
 * `RedeemerItemProofInvalid` in redeemer-item-proof-v1.ak). The window is
 * authenticated against the item commitment first, so wrong evidence is never
 * invalid Data.
 */
export const isMidgardRedeemerDataUnreadable = (
  control: MidgardRedeemerItemProofControl,
  witness: MidgardRedeemerItemProofWitness,
): boolean => {
  if (
    control.stage !== MidgardRedeemerItemProofStages.Data ||
    control.traversal === null ||
    witness.action.kind !== "traverseData"
  )
    return false;
  const source = readMidgardRedeemerItemProofSource({ control, witness });
  return (
    source !== null &&
    isUnreadableMidgardCekDataTraverse({
      control: control.traversal,
      sourceBytes: source.sourceBytes,
      action: witness.action.action,
    })
  );
};

const traverseWitness = (
  item: MidgardBoundedItem,
  control: MidgardRedeemerItemProofControl,
  action: MidgardCekDataTraverseAction,
): MidgardRedeemerItemProofWitness => {
  const span = nextMidgardRedeemerItemProofSpan(control);
  if (span === null)
    return {
      action: { kind: "traverseData", action },
      chunkProof: null,
      nextChunkProof: null,
    };
  const first = Math.floor(
    span.absoluteStart / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  const last = Math.floor(
    (span.absoluteStart + span.length - 1) / MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  );
  return {
    action: { kind: "traverseData", action },
    chunkProof: buildMidgardBoundedItemChunkProof(item, first),
    nextChunkProof:
      first === last ? null : buildMidgardBoundedItemChunkProof(item, last),
  };
};

/**
 * Replays the original item to the Data step the traverse refuses, without
 * normalizing Data: the descriptor prefix, every advancing traverse step, and
 * the final `Unreadable` step's control and witness.
 */
export const buildMidgardRedeemerDataUnreadableTrace = (input: {
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
        "redeemer Data refusal lost its authenticated item prefix",
      );
    const step = { control, witness, next };
    control = next;
    return step;
  });
  const outcome = driveMidgardCekDataTraverse({
    sourceStart: control.dataOffset,
    source: descriptor.item.bytes.subarray(
      control.dataOffset,
      control.dataOffset + control.dataLength,
    ),
  });
  if (outcome.verdict !== "unreadable")
    throw new Error("redeemer Data is canonical; no step refuses it");
  // The script-sources executors claim Unreadable at a byte-reading step
  // (no action) only. A redeemer's Data span is the item decoder's exact CBOR
  // extent, so no attach or finalize step can be the one that refuses it.
  if (outcome.refusal.action !== null)
    throw new Error("redeemer Data refusal is not at a byte-reading step");
  for (const traversalStep of outcome.steps) {
    const witness = traverseWitness(
      descriptor.item,
      control,
      traversalStep.action,
    );
    const next = advanceMidgardRedeemerItemProof({ control, witness });
    if (next === null)
      throw new Error(
        "redeemer Data refusal prefix did not replay its original bytes",
      );
    steps.push({ control, witness, next });
    control = next;
  }
  const witness = traverseWitness(
    descriptor.item,
    control,
    outcome.refusal.action,
  );
  if (!isMidgardRedeemerDataUnreadable(control, witness))
    throw new Error("redeemer bytes do not prove an Unreadable Data step");
  return { initial, steps, control, witness };
};
