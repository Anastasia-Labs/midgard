import { access, rename } from "node:fs/promises";
import { join } from "node:path";

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

/**
 * The committee node's file store was named `watcher.json` before the DA
 * committee role was split out of the watcher.  Rename a legacy file in place
 * so an existing directory store keeps its data.
 */
export const committeeStoreFilePath = async (
  directory: string,
): Promise<string> => {
  const filePath = join(directory, "committee.json");
  const legacyPath = join(directory, "watcher.json");
  try {
    await access(filePath);
    return filePath;
  } catch {
    // fall through: no current-name store yet
  }
  try {
    await access(legacyPath);
  } catch {
    return filePath;
  }
  await rename(legacyPath, filePath);
  return filePath;
};

export const isNodeError = (error: unknown): error is NodeJS.ErrnoException =>
  error instanceof Error && "code" in error;

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
