/**
 * Opening the coordinates a committed verdict claims.
 *
 * A committed verdict (a forced leaf's `ForcedTxInvalid{reason}`) names
 * coordinates the block producer chose: a field index, an item index, an
 * input source kind, a script item kind. A claimed coordinate the transaction
 * does not have is a classification outcome, never a classifier error. Every
 * detector that opens such a coordinate goes through these openers, which
 * return `undefined` for a coordinate outside the transaction; the detector
 * then reports no finding of its own family for that claim, and the header
 * classifier decides what the claim means for the block
 * (`absentForcedClaimDetections`).
 *
 * Only the claim is forgiven. Authenticated material that disagrees with its
 * commitment, or a decoder that disagrees with itself, still throws, so a
 * genuine classifier fault keeps failing closed.
 */
import {
  decodeMidgardFieldPreimage,
  MidgardTxCodecError,
} from "@al-ft/midgard-core";
import {
  type NetworkOrderContext,
  type RejectionReason,
  rejectionReasonPhase,
} from "@al-ft/midgard-sdk";

/** A claimed index as an array position, or `undefined` outside any array. */
export const claimedIndex = (claimed: bigint | number): number | undefined => {
  const value = typeof claimed === "bigint" ? Number(claimed) : claimed;
  return Number.isSafeInteger(value) && value >= 0 ? value : undefined;
};

/** The item a claimed index names, or `undefined` when the list lacks it. */
export const claimedItem = <T>(
  items: readonly T[],
  claimed: bigint | number,
): T | undefined => {
  const position = claimedIndex(claimed);
  return position === undefined ? undefined : items[position];
};

/**
 * The items of a field preimage, or `undefined` when its bytes are not a
 * well-formed §5.1 field: such a field's item count is itself what a claim
 * may dispute, so it cannot show a claimed item absent.
 */
export const claimedFieldItems = (
  preimage: Uint8Array,
): readonly Buffer[] | undefined => {
  try {
    return decodeMidgardFieldPreimage(preimage);
  } catch (cause) {
    if (cause instanceof MidgardTxCodecError) return undefined;
    throw cause;
  }
};

/** `PlutusScriptUndecodable.item_kind` values a transaction can carry. */
const SCRIPT_ITEM_KINDS: ReadonlySet<bigint> = new Set([0n, 1n]);

/**
 * Whether a claimed reason ranks at or after `phase` in first-fault order. A
 * reason naming a script item kind no transaction carries has no phase, so it
 * reaches none.
 */
export const claimedReasonReachesPhase = (
  reason: RejectionReason,
  context: NetworkOrderContext,
  phase: number,
): boolean =>
  !(
    typeof reason === "object" &&
    "PlutusScriptUndecodable" in reason &&
    !SCRIPT_ITEM_KINDS.has(reason.PlutusScriptUndecodable.item_kind)
  ) && rejectionReasonPhase(reason, context) >= phase;

/** The highest decode class `PlutusScriptUndecodable.class` can name. */
const MAX_PLUTUS_DECODE_CLASS = Number.MAX_SAFE_INTEGER - 100;

/**
 * A claimed `PlutusScriptUndecodable` payload as positions, or `undefined`
 * when it names an item kind no transaction carries, an index outside any
 * array, or a decode class outside `1..MAX_SAFE_INTEGER - 100`.
 */
export const claimedPlutusUndecodable = (
  reason: Readonly<{ item_kind: bigint; item_index: bigint; class: bigint }>,
):
  | Readonly<{ itemKind: 0 | 1; itemIndex: number; decodeClass: number }>
  | undefined => {
  const itemIndex = claimedIndex(reason.item_index);
  const decodeClass = claimedIndex(reason.class);
  if (
    !SCRIPT_ITEM_KINDS.has(reason.item_kind) ||
    itemIndex === undefined ||
    decodeClass === undefined ||
    decodeClass < 1 ||
    decodeClass > MAX_PLUTUS_DECODE_CLASS
  )
    return undefined;
  return { itemKind: reason.item_kind === 0n ? 0 : 1, itemIndex, decodeClass };
};
