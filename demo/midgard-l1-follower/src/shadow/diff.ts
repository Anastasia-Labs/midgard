/**
 * The shadow diff's value model: both sides are normalised to plain JSON
 * (bigints and Buffers to strings, Maps to objects, Sets to sorted arrays,
 * object keys sorted), then compared structurally.
 */
export type Json =
  | null
  | boolean
  | number
  | string
  | readonly Json[]
  | { readonly [key: string]: Json };

const sortedEntries = (entries: [string, Json][]): { [key: string]: Json } =>
  Object.fromEntries(
    entries.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)),
  ) as { [key: string]: Json };

const keyText = (key: unknown): string => {
  const value = normalise(key);
  return typeof value === "string" ? value : JSON.stringify(value);
};

const compareJson = (a: Json, b: Json): number => {
  const left = JSON.stringify(a);
  const right = JSON.stringify(b);
  return left < right ? -1 : left > right ? 1 : 0;
};

export const normalise = (value: unknown): Json => {
  if (value === null || value === undefined) return null;
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "number")
    return Number.isFinite(value) ? value : String(value);
  if (typeof value === "string" || typeof value === "boolean") return value;
  if (Buffer.isBuffer(value) || value instanceof Uint8Array)
    return Buffer.from(value).toString("hex");
  if (value instanceof Map)
    return sortedEntries(
      [...(value as Map<unknown, unknown>)].map(([key, item]) => [
        keyText(key),
        normalise(item),
      ]),
    );
  if (value instanceof Set)
    return [...(value as Set<unknown>)].map(normalise).sort(compareJson);
  if (Array.isArray(value)) return (value as unknown[]).map(normalise);
  if (typeof value === "object")
    return sortedEntries(
      Object.entries(value as Record<string, unknown>)
        .filter(([, item]) => item !== undefined)
        .map(([key, item]) => [key, normalise(item)]),
    );
  return typeof value === "symbol" ? value.toString() : "function";
};

/** One difference: the JSON path and both sides (absent: `undefined`). */
export type DiffEntry = Readonly<{
  path: string;
  projected?: Json;
  current?: Json;
}>;

const isObject = (value: Json): value is { readonly [key: string]: Json } =>
  value !== null && typeof value === "object" && !Array.isArray(value);

const walk = (
  path: string,
  projected: Json | undefined,
  current: Json | undefined,
  out: DiffEntry[],
  limit: number,
): void => {
  if (out.length >= limit) return;
  if (projected === undefined || current === undefined) {
    out.push({
      path,
      ...(projected === undefined ? {} : { projected }),
      ...(current === undefined ? {} : { current }),
    });
    return;
  }
  if (Array.isArray(projected) && Array.isArray(current)) {
    const left = projected as readonly Json[];
    const right = current as readonly Json[];
    for (let i = 0; i < Math.max(left.length, right.length); i += 1)
      walk(`${path}[${i}]`, left[i], right[i], out, limit);
    return;
  }
  if (isObject(projected) && isObject(current)) {
    const keys = new Set([...Object.keys(projected), ...Object.keys(current)]);
    for (const key of [...keys].sort())
      walk(`${path}.${key}`, projected[key], current[key], out, limit);
    return;
  }
  if (JSON.stringify(projected) !== JSON.stringify(current))
    out.push({ path, projected, current });
};

/** Paths where the two normalised views differ, at most `limit` of them. */
export const diffValues = (
  projected: unknown,
  current: unknown,
  limit = 20,
): DiffEntry[] => {
  const out: DiffEntry[] = [];
  walk("$", normalise(projected), normalise(current), out, limit);
  return out;
};
