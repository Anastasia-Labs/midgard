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
 * The first `FieldItemWidthIllegal` order refusal CanonicalDecode reaches,
 * over the nine field preimages in field order (the order of
 * `deriveMidgardTxFieldPreimages`).
 *
 * The walk visits fields and items in the machine's order. An item the
 * machine refuses or cannot open for its width comes first, so the walk
 * stops there with `undefined` and leaves that verdict to its existing path.
 */
export const canonicalFieldOrderFaultInPreimages = (
  fieldPreimages: readonly Uint8Array[],
): CanonicalFieldOrderFault | undefined => {
  try {
    for (
      let fieldIndex = 0;
      fieldIndex < fieldPreimages.length;
      fieldIndex += 1
    ) {
      const items = decodeMidgardFieldPreimage(fieldPreimages[fieldIndex]!);
      for (let itemIndex = 0; itemIndex < items.length; itemIndex += 1) {
        const item = items[itemIndex]!;
        const fixed = FIXED_ITEM_BYTES[fieldIndex];
        if (
          (fixed !== undefined && item.length !== fixed) ||
          (fieldIndex === 5 && item.length === 0) ||
          canonicalFieldItemEncodedLength(fieldIndex, item.length) === null
        )
          return undefined;
        if (
          itemIndex > 0 &&
          !midgardFieldItemOrderIsValid(fieldIndex, items[itemIndex - 1]!, item)
        )
          return { fieldIndex, itemIndex };
      }
    }
  } catch {
    return undefined;
  }
  return undefined;
};

/** `canonicalFieldOrderFaultInPreimages` over the bytes' derived fields. */
export const canonicalFieldOrderFault = (
  bytes: Buffer,
  sourceKind: "normal" | "forced",
): CanonicalFieldOrderFault | undefined => {
  let fieldPreimages: readonly Uint8Array[];
  try {
    fieldPreimages = deriveMidgardTxFieldPreimages(bytes, sourceKind).map(
      (field) => field.preimageCbor,
    );
  } catch {
    return undefined;
  }
  return canonicalFieldOrderFaultInPreimages(fieldPreimages);
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
