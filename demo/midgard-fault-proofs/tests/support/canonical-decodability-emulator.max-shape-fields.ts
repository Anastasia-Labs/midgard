/**
 * The 32,768-byte committed fields the canonicalDecodability max-shape
 * lifecycle walks: the largest legal item count, and the costliest single
 * batch one step can be handed.
 */
import { MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES } from "@al-ft/midgard-core";
import {
  MIDGARD_ENVELOPE_BATCH_ITEMS,
  miscountedMidgardFieldPreimage,
} from "@al-ft/midgard-sdk";

const B = MIDGARD_ENVELOPE_BATCH_ITEMS;

/** The most items a 32,768-byte field holds: empty ones under `99 NNNN`. */
export const MAX_ITEMS = MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES - 3;

/** `declared` under a minimal head, then the maximum count of `40` items. */
export const maxField = (declared: number): Buffer =>
  miscountedMidgardFieldPreimage(
    declared,
    Array.from({ length: MAX_ITEMS }, () => Buffer.alloc(0)),
  );

/**
 * B + 1 items declared over B present, every one at the two-byte head or
 * wider and as many three-byte heads as the byte bound pays for: the batch
 * with the most per-item work one step can be handed.
 */
export const worstBatchField = (): Buffer => {
  const twoByte = Buffer.alloc(24);
  const threeByte = Buffer.alloc(256);
  const threeByteCount = Math.floor(
    (MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES - 3 - 26 * B) / 233,
  );
  return miscountedMidgardFieldPreimage(B + 1, [
    ...Array.from({ length: threeByteCount }, () => threeByte),
    ...Array.from({ length: B - threeByteCount }, () => twoByte),
  ]);
};
