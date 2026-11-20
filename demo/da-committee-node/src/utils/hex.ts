export const isHex = (value: string): boolean =>
  value.length % 2 === 0 && /^[0-9a-fA-F]*$/.test(value);

export const normalizeHex = (
  value: string,
  {
    byteLength,
    fieldName,
  }: {
    readonly byteLength?: number;
    readonly fieldName: string;
  },
): string => {
  const normalized = value.trim().toLowerCase();
  if (!isHex(normalized)) {
    throw new Error(`${fieldName} must be even-length hex`);
  }
  if (byteLength !== undefined && normalized.length !== byteLength * 2) {
    throw new Error(`${fieldName} must be ${byteLength.toString()} bytes`);
  }
  return normalized;
};

export const bytesToHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const hexToBytes = (
  value: string,
  fieldName: string,
  byteLength?: number,
): Buffer => Buffer.from(normalizeHex(value, { fieldName, byteLength }), "hex");

export const timingSafeHexEqual = (left: string, right: string): boolean => {
  const leftBytes = Buffer.from(left, "hex");
  const rightBytes = Buffer.from(right, "hex");
  return (
    leftBytes.length === rightBytes.length &&
    cryptoTimingSafeEqual(leftBytes, rightBytes)
  );
};

const cryptoTimingSafeEqual = (left: Buffer, right: Buffer): boolean => {
  if (left.length !== right.length) {
    return false;
  }
  let diff = 0;
  for (let index = 0; index < left.length; index += 1) {
    diff |= left[index]! ^ right[index]!;
  }
  return diff === 0;
};
