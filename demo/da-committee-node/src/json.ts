export const jsonBigIntStringReplacer = (
  _key: string,
  value: unknown,
): unknown => (typeof value === "bigint" ? value.toString() : value);
