export type NormalizeHexOptions = {
  readonly fieldName?: string;
  readonly byteLength?: number;
  readonly allowEmpty?: boolean;
  readonly trim?: boolean;
};

const HEX_PATTERN = /^[0-9a-f]*$/;

const hexLengthLabel = (byteLength: number | undefined): string =>
  byteLength === undefined
    ? "an even-length hex string"
    : `a ${byteLength.toString()}-byte hex string`;

export const normalizeHex = (
  value: string,
  options: NormalizeHexOptions = {},
): string => {
  const fieldName = options.fieldName ?? "value";
  const normalized =
    options.trim === false ? value.toLowerCase() : value.trim().toLowerCase();
  const expectedLength =
    options.byteLength === undefined ? undefined : options.byteLength * 2;
  if (
    (normalized.length === 0 && options.allowEmpty !== true) ||
    normalized.length % 2 !== 0 ||
    !HEX_PATTERN.test(normalized) ||
    (expectedLength !== undefined && normalized.length !== expectedLength)
  ) {
    throw new Error(
      `${fieldName} must be ${hexLengthLabel(options.byteLength)}`,
    );
  }
  return normalized;
};

export const hexToBytes = (
  value: string,
  options: NormalizeHexOptions = {},
): Buffer => Buffer.from(normalizeHex(value, options), "hex");

export const compareHex = (
  left: string,
  right: string,
  options: NormalizeHexOptions = {},
): number => hexToBytes(left, options).compare(hexToBytes(right, options));
