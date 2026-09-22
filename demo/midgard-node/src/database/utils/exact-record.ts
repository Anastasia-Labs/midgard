/**
 * Fail-closed record shape check shared by the table adapters that decode
 * durable JSON/JSONB evidence.
 *
 * Rejects anything that is not a null-or-`Object.prototype`-prototyped plain
 * object, anything carrying symbol keys or non-enumerable own properties, and
 * any key set that is not exactly `fields`.
 */
export const exactRecord = (
  value: unknown,
  fields: readonly string[],
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a plain record`);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    throw new Error(`${label} must be a plain record`);
  }
  const keys = Reflect.ownKeys(value);
  if (
    keys.length !== Object.keys(value).length ||
    keys.length !== fields.length ||
    keys.some((key) => typeof key !== "string" || !fields.includes(key))
  ) {
    throw new Error(`${label} must contain exactly ${fields.join(", ")}`);
  }
  return value as Record<string, unknown>;
};
