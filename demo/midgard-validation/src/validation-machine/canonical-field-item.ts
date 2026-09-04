/**
 * Canonical CBOR argument header sizes and field-item encoded lengths.
 */

export const canonicalCborArgumentHeaderSize = (value: number): number => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("canonical CBOR argument must be a non-negative integer");
  }
  if (value < 24) return 1;
  if (value < 0x100) return 2;
  if (value < 0x1_0000) return 3;
  if (value < 0x1_0000_0000) return 5;
  return 9;
};

export const MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX = 6;

export const MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX = 7;

export const canonicalFieldItemEncodedLength = (
  fieldIndex: number,
  itemLength: number,
): number => {
  if (
    [0, 1, 2, 3, 4, MIDGARD_ADDRESS_WITNESSES_FIELD_INDEX].includes(fieldIndex)
  ) {
    return canonicalCborArgumentHeaderSize(itemLength) + itemLength;
  }
  if (fieldIndex === MIDGARD_SCRIPT_WITNESSES_FIELD_INDEX || fieldIndex === 8) {
    return itemLength;
  }
  if (fieldIndex !== 5 || itemLength === 0) {
    throw new Error(
      `invalid canonical field item length at field ${fieldIndex.toString()}`,
    );
  }
  return itemLength - 1;
};
