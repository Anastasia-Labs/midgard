import { hexToBytes } from "@al-ft/midgard-core/hex";

export const parseFixedHexParam = (
  value: unknown,
  byteLength: number,
): Buffer | null => {
  if (typeof value !== "string") {
    return null;
  }
  try {
    return hexToBytes(value, { byteLength, trim: false });
  } catch {
    return null;
  }
};
