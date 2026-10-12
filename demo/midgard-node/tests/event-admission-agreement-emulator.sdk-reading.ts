/** Lets the SDK build a transaction carrying a byte string it never writes. */
import { Constr, Data } from "@lucid-evolution/lucid";
import { vi } from "vitest";

/** A byte string of a length the node's decoder refuses, and the well-formed
 * one the SDK's own codec reads it as while it builds the row's transaction. */
export type Swap = Readonly<{ short: string; padded: string }>;

const swapStrings = (value: unknown, from: string, to: string): unknown => {
  if (value === from) return to;
  if (Array.isArray(value))
    return value.map((item) => swapStrings(item, from, to));
  if (value instanceof Constr)
    return new Constr(
      value.index,
      value.fields.map((item) => swapStrings(item, from, to)),
    );
  if (value instanceof Map)
    return new Map(
      [...value].map(([k, v]) => [
        swapStrings(k, from, to),
        swapStrings(v, from, to),
      ]),
    );
  if (value !== null && typeof value === "object")
    return Object.fromEntries(
      Object.entries(value).map(([k, v]) => [k, swapStrings(v, from, to)]),
    );
  return value;
};

/** Runs `build` with the SDK's own codec reading `swap.short` as
 * `swap.padded`. The SDK checks its decode of every order it builds; this lets
 * it build a transaction whose bytes carry a value it never writes, so only
 * the chain judges that value. */
export const withSdkReading = async <T>(
  swap: Swap | undefined,
  build: () => Promise<T>,
): Promise<T> => {
  if (swap === undefined) return build();
  const { from, to } = Data;
  const shortCbor = to(swap.short);
  const paddedCbor = to(swap.padded);
  const fromSpy = vi.spyOn(Data, "from").mockImplementation(((
    raw: string,
    schema?: never,
  ) => {
    try {
      return from(raw, schema);
    } catch (error) {
      if (!raw.includes(shortCbor)) throw error;
      return from(raw.replaceAll(shortCbor, paddedCbor), schema);
    }
  }) as typeof Data.from);
  const toSpy = vi.spyOn(Data, "to").mockImplementation(((
    value: never,
    schema?: never,
    options?: never,
  ) => {
    try {
      return to(value, schema, options);
    } catch {
      const padded = swapStrings(value, swap.short, swap.padded) as never;
      return to(padded, schema, options).replaceAll(paddedCbor, shortCbor);
    }
  }) as typeof Data.to);
  try {
    return await build();
  } finally {
    fromSpy.mockRestore();
    toSpy.mockRestore();
  }
};
