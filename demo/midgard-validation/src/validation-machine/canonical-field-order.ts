import {
  decodeMidgardFieldPreimage,
  midgardFieldItemOrderIsValid,
} from "@al-ft/midgard-core/codec";
import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core/consensus-validation";

import { canonicalFieldItemEncodedLength } from "./canonical-field-item.js";
import { oversizedCanonicalOutputIndex } from "./canonical-output-bound.js";

export type CanonicalFieldOrderFault = Readonly<{
  fieldIndex: number;
  itemIndex: number;
}>;

/** §5.3 fixed item payload widths; the field door opens nothing else. */
const FIXED_ITEM_BYTES: Readonly<Record<number, number>> = {
  0: 38,
  1: 38,
  3: 28,
  4: 28,
  7: 101,
};

/**
 * The first `FieldItemWidthIllegal` order refusal CanonicalDecode reaches.
 *
 * The walk visits fields and items in the machine's order. An item the
 * machine refuses or cannot open for its width comes first, so the walk
 * stops there with `undefined` and leaves that verdict to its existing path.
 */
export const canonicalFieldOrderFault = (
  bytes: Buffer,
  sourceKind: "normal" | "forced",
): CanonicalFieldOrderFault | undefined => {
  try {
    for (const field of deriveMidgardTxFieldPreimages(bytes, sourceKind)) {
      const items = decodeMidgardFieldPreimage(field.preimageCbor);
      for (let itemIndex = 0; itemIndex < items.length; itemIndex += 1) {
        const item = items[itemIndex]!;
        const fixed = FIXED_ITEM_BYTES[field.fieldIndex];
        if (
          (fixed !== undefined && item.length !== fixed) ||
          (field.fieldIndex === 5 && item.length === 0) ||
          canonicalFieldItemEncodedLength(field.fieldIndex, item.length) ===
            null
        )
          return undefined;
        if (
          itemIndex > 0 &&
          !midgardFieldItemOrderIsValid(
            field.fieldIndex,
            items[itemIndex - 1]!,
            item,
          )
        )
          return { fieldIndex: field.fieldIndex, itemIndex };
      }
    }
  } catch {
    return undefined;
  }
  return undefined;
};

/** CanonicalDecode's encoded length of item `itemIndex`; null is FIWI. */
export const canonicalOrderedItemEncodedLength = (
  fieldIndex: number,
  items: readonly { readonly bytes: Buffer }[],
  itemIndex: number,
): number | null => {
  const current = items[itemIndex]!.bytes;
  return itemIndex > 0 &&
    !midgardFieldItemOrderIsValid(
      fieldIndex,
      items[itemIndex - 1]!.bytes,
      current,
    )
    ? null
    : canonicalFieldItemEncodedLength(fieldIndex, current.length);
};

/** Whether CanonicalDecode refuses an item of this transaction (width or order). */
export const canonicalDecodeItemRefusal = (
  bytes: Buffer,
  sourceKind: "normal" | "forced",
): boolean =>
  oversizedCanonicalOutputIndex(bytes, sourceKind) !== undefined ||
  canonicalFieldOrderFault(bytes, sourceKind) !== undefined;
