/**
 * Datum decoding shared by the operator-lifecycle readers.
 *
 * Node data reaches these modules either as `Constr` instances (straight from
 * `Data.from`) or as structurally equivalent plain objects (after a
 * structured-clone boundary, as in cached wallet views). `castDatum`
 * normalizes before casting so every reader tolerates both.
 */
import { Constr, Data } from "@lucid-evolution/lucid";

const isRawConstr = (
  value: unknown,
): value is { readonly index: number | bigint; readonly fields: unknown[] } =>
  typeof value === "object" &&
  value !== null &&
  "index" in value &&
  "fields" in value &&
  Array.isArray((value as { fields: unknown }).fields);

const normalizeDataValue = (value: unknown): unknown => {
  if (value instanceof Constr) {
    return value;
  }
  if (isRawConstr(value)) {
    return new Constr(
      Number(value.index),
      value.fields.map(normalizeDataValue),
    );
  }
  if (Array.isArray(value)) {
    return value.map(normalizeDataValue);
  }
  if (value instanceof Map) {
    return new Map(
      [...value.entries()].map(([key, inner]) => [
        normalizeDataValue(key),
        normalizeDataValue(inner),
      ]),
    );
  }
  return value;
};

/** Casts `value` to `schema`, throwing when it does not fit. */
export const castDatum = <TDatum>(value: unknown, schema: unknown): TDatum =>
  Data.castFrom(normalizeDataValue(value) as never, schema as never) as TDatum;
