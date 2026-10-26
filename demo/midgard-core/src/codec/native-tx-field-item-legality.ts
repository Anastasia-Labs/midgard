import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";
import type { MidgardNativeTxCanonical } from "./native.validate-midgard-native-tx-canonical.js";
import {
  decodeMidgardFieldArrayHeader,
  decodeMidgardFieldPreimage,
  midgardFieldStride,
} from "./native-tx-field-access.decode-midgard-field-array-header-at.js";
import { midgardFieldItemOrderIsValid } from "./native-tx-field-order.js";

export type MidgardFieldItemFault = Readonly<{
  fieldIndex: number;
  itemIndex: number;
}>;

export class MidgardFieldItemError extends MidgardTxCodecError {
  readonly fault: MidgardFieldItemFault;

  constructor(fault: MidgardFieldItemFault) {
    super(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `field[${fault.fieldIndex}].item[${fault.itemIndex}] is not canonical`,
    );
    this.name = "MidgardFieldItemError";
    this.fault = fault;
  }
}

/** Twin of fixed-item-legality-v1.ak; width is the payload length. */
export const midgardFieldItemWidthIsLegal = (
  fieldIndex: number,
  itemWidth: number,
): boolean => {
  if (!Number.isSafeInteger(itemWidth) || itemWidth < 0) return false;
  if (fieldIndex === 0 || fieldIndex === 1) return itemWidth === 38;
  if (fieldIndex === 3 || fieldIndex === 4) return itemWidth === 28;
  if (fieldIndex === 7) return itemWidth === 101;
  if (fieldIndex === 2) return itemWidth <= 16384;
  if (fieldIndex === 5) return itemWidth > 0;
  return fieldIndex === 6 || fieldIndex === 8;
};

const fixedShapeIsLegal = (fieldIndex: number, item: Buffer): boolean => {
  if (fieldIndex === 0 || fieldIndex === 1)
    return (
      item.length === 40 &&
      item.subarray(0, 5).equals(Buffer.from("5826825820", "hex")) &&
      item[37] === 0x19
    );
  if (fieldIndex === 3 || fieldIndex === 4)
    return item.length === 30 && item[0] === 0x58 && item[1] === 0x1c;
  if (fieldIndex === 7)
    return (
      item.length === 103 &&
      item.subarray(0, 5).equals(Buffer.from("5865825820", "hex")) &&
      item[37] === 0x58 &&
      item[38] === 0x40
    );
  return false;
};

/** Exact wrapper plus payload, with total refusal for committed malformed bytes. */
export const midgardFixedFieldItemIsLegal = (
  fieldIndex: number,
  previous: Buffer | null,
  item: Buffer,
): boolean => {
  if (!fixedShapeIsLegal(fieldIndex, item)) return false;
  if (previous === null) return true;
  if (!fixedShapeIsLegal(fieldIndex, previous)) return false;
  // CanonicalDecode's order rule over the payloads after the two-byte wrapper.
  return midgardFieldItemOrderIsValid(
    fieldIndex,
    previous.subarray(2),
    item.subarray(2),
  );
};

/** Inspect one exact committed field; its grammar failures remain typed. */
export const locateMidgardFieldItemFault = (
  fieldIndex: number,
  field: Buffer,
): MidgardFieldItemFault | null => {
  const stride = midgardFieldStride(fieldIndex);
  if (stride === 0) {
    const items = decodeMidgardFieldPreimage(field);
    for (const [itemIndex, item] of items.entries()) {
      if (!midgardFieldItemWidthIsLegal(fieldIndex, item.length))
        return { fieldIndex, itemIndex };
    }
    return null;
  }
  const { count, nextOffset } = decodeMidgardFieldArrayHeader(field);
  if (field.length !== nextOffset + count * stride)
    throw new MidgardTxCodecError(
      MidgardTxCodecErrorCodes.CborDecode,
      `field[${fieldIndex}] count and fixed stride do not match its length`,
    );
  let previous: Buffer | null = null;
  for (let itemIndex = 0; itemIndex < count; itemIndex += 1) {
    const offset = nextOffset + itemIndex * stride;
    const item = field.subarray(offset, offset + stride);
    if (!midgardFixedFieldItemIsLegal(fieldIndex, previous, item))
      return { fieldIndex, itemIndex };
    previous = item;
  }
  return null;
};

/**
 * Walk CanonicalDecode's actual field/item order before ledger item decoders.
 * Fixed strides preserve bad wrappers as verdict bytes. A bad count/total-length
 * envelope is a whole-field grammar failure; it never invents an item extent.
 */
export const locateMidgardTransactionFieldItemFault = (
  tx: Pick<MidgardNativeTxCanonical, "body" | "witnessSet">,
): MidgardFieldItemFault | null => {
  const fields = [
    tx.body.spendInputsPreimageCbor,
    tx.body.referenceInputsPreimageCbor,
    tx.body.outputsPreimageCbor,
    tx.body.requiredObserversPreimageCbor,
    tx.body.requiredSignersPreimageCbor,
    tx.body.mintPreimageCbor,
    tx.witnessSet.scriptTxWitsPreimageCbor,
    tx.witnessSet.addrTxWitsPreimageCbor,
    tx.witnessSet.redeemerTxWitsPreimageCbor,
  ];
  for (const [fieldIndex, field] of fields.entries()) {
    const fault = locateMidgardFieldItemFault(fieldIndex, field);
    if (fault !== null) return fault;
  }
  return null;
};
