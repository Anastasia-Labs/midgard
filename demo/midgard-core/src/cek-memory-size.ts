/**
 * Plutus ExMemory sizes, counted in 64-bit words the way Cardano counts them.
 *
 * - Integer: 1 word for zero, otherwise the words needed for the magnitude.
 * - Byte string: 1 word when empty, otherwise ceil(length / 8).
 * - String: the UTF-8 length divided by 4, rounded down.
 * - BLS12-381 values: G1 18, G2 36, Miller-loop result 72.
 */
export const midgardCekIntegerMemoryWords = (value: bigint): bigint => {
  const magnitude = value < 0n ? -value : value;
  if (magnitude === 0n) return 1n;
  return BigInt(Math.floor((magnitude.toString(2).length - 1) / 64) + 1);
};

export const midgardCekBytesMemoryWords = (length: bigint | number): bigint => {
  const size = BigInt(length);
  if (size < 0n) throw new Error("byte length is negative");
  return size === 0n ? 1n : (size - 1n) / 8n + 1n;
};

export const midgardCekStringMemoryWords = (
  utf8Length: bigint | number,
): bigint => {
  const size = BigInt(utf8Length);
  if (size < 0n) throw new Error("string length is negative");
  return size / 4n;
};

export const MIDGARD_CEK_BLS_G1_MEMORY = 18n;
export const MIDGARD_CEK_BLS_G2_MEMORY = 36n;
export const MIDGARD_CEK_BLS_ML_RESULT_MEMORY = 72n;
