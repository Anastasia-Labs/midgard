import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";

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
): number | null => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex > 8 ||
    (fieldIndex === 5 && itemLength === 0)
  ) {
    throw new Error(
      `invalid canonical field item length at field ${fieldIndex.toString()}`,
    );
  }
  if (
    fieldIndex === 2 &&
    itemLength > MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes
  )
    return null;
  // All nine §5.1 fields wrap each item in a definite byte string.
  return canonicalCborArgumentHeaderSize(itemLength) + itemLength;
};
