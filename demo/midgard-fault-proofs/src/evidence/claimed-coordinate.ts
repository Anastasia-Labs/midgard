/**
 * Opening the coordinates a committed verdict claims.
 *
 * A committed verdict (a forced leaf's `ForcedTxInvalid{reason}`) names
 * coordinates the block producer chose: a field index, an item index, an
 * input source kind, a script item kind. Each is any integer the rejection
 * codec admits, -2^64 through 2^64 - 1. A claimed coordinate the transaction
 * does not have is a classification outcome, never a classifier error: the
 * family that owns the reason contradicts it, as its on-chain step does, from
 * the authenticated item count alone. Every detector that opens such a
 * coordinate goes through these openers, which compare the claim as an
 * integer and narrow it to an array position only once it names an item.
 *
 * Only the claim is forgiven. Authenticated material that disagrees with its
 * commitment, or a decoder that disagrees with itself, still throws, so a
 * genuine classifier fault keeps failing closed.
 */
import {
  decodeMidgardFieldPreimage,
  MidgardTxCodecError,
} from "@al-ft/midgard-core";
import { type RejectionReason } from "@al-ft/midgard-sdk/core";
import {
  type NetworkOrderContext,
  rejectionReasonPhase,
} from "@al-ft/midgard-sdk/fraud-proof";

/** The smallest and largest coordinates the rejection codec admits. */
const CLAIMED_COORDINATE_LIMIT = 1n << 64n;

/**
 * The position `claimed` names among `count` items, or `undefined` when it
 * names none of them: negative, at or past `count`, or past 2^53.
 */
export const claimedIndex = (
  claimed: bigint,
  count: number,
): number | undefined =>
  claimed >= 0n && claimed < BigInt(count) ? Number(claimed) : undefined;

/** The item a claimed index names, or `undefined` when the list lacks it. */
export const claimedItem = <T>(
  items: readonly T[],
  claimed: bigint,
): T | undefined => {
  const position = claimedIndex(claimed, items.length);
  return position === undefined ? undefined : items[position];
};

/** A claimed coordinate as an artifact carries it: its decimal string. */
export const claimedCoordinateDecimal = (claimed: bigint): string =>
  claimed.toString();

/**
 * Reads back {@link claimedCoordinateDecimal}: refuses anything but the
 * canonical decimal of an integer the rejection codec admits.
 */
export const admitClaimedCoordinateDecimal = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "string" || !/^(?:0|-?[1-9][0-9]*)$/u.test(value))
    throw new Error(`${label} is not a canonical integer decimal`);
  const claimed = BigInt(value);
  if (
    claimed >= CLAIMED_COORDINATE_LIMIT ||
    claimed < -CLAIMED_COORDINATE_LIMIT
  )
    throw new Error(`${label} is outside the rejection codec`);
  return claimed;
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
