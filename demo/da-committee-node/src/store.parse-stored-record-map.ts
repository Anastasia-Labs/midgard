export const parseStoredRecordMap = <T>(
  value: unknown,
  parseRecord: (entry: unknown) => T,
  expectedKey: (entry: T) => string,
  label: string,
): Record<string, T> => {
  if (value === undefined) {
    return {};
  }
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return Object.fromEntries(
    Object.entries(value).map(([key, rawEntry]) => {
      const entry = parseRecord(rawEntry);
      if (key !== expectedKey(entry)) {
        throw new Error(`${label} key ${key} does not match record identity`);
      }
      return [key, entry];
    }),
  );
};

export const jsonReplacer = (_key: string, value: unknown): unknown =>
  typeof value === "bigint"
    ? { __midgardWatcherType: "bigint", value: value.toString() }
    : value;

export const jsonReviver = (_key: string, value: unknown): unknown => {
  if (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value) &&
    (value as { __midgardWatcherType?: unknown }).__midgardWatcherType ===
      "bigint"
  ) {
    const record = value as Record<string, unknown>;
    if (
      Object.keys(record).length !== 2 ||
      !Object.hasOwn(record, "__midgardWatcherType") ||
      !Object.hasOwn(record, "value") ||
      typeof record.value !== "string" ||
      !/^(?:0|-?[1-9][0-9]*)$/.test(record.value)
    ) {
      throw new Error("invalid canonical committee node bigint encoding");
    }
    return BigInt(record.value);
  }
  return value;
};

const reviveStoredJson = (value: unknown): unknown => {
  if (typeof value === "object" && value !== null) {
    if (Array.isArray(value)) {
      for (let index = 0; index < value.length; index += 1) {
        const revived = reviveStoredJson(value[index]);
        if (revived !== value[index]) value[index] = revived;
      }
    } else {
      const record = value as Record<string, unknown>;
      for (const key of Object.keys(record)) {
        const revived = reviveStoredJson(record[key]);
        if (revived !== record[key])
          Object.defineProperty(record, key, {
            value: revived,
            writable: true,
            enumerable: true,
            configurable: true,
          });
      }
    }
  }
  return jsonReviver("", value);
};

/**
 * `JSON.parse(text, jsonReviver)` without the reviver parse path. Passing a
 * reviver makes V8 keep every value's source text for the reviver context,
 * several times the plain parse cost, and stores re-read whole files and rows
 * on every operation. The walk applies the same reviver bottom-up in the same
 * key order and redefines only the values it changes, as JSON.parse's own
 * internalization does, so results and refusals are identical.
 */
export const parseStoredJson = (text: string): unknown =>
  reviveStoredJson(JSON.parse(text));
