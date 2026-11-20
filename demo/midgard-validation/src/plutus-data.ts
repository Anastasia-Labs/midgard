import { Constr, Data } from "@lucid-evolution/lucid";

type LocalPlutusData =
  | bigint
  | string
  | LocalPlutusData[]
  | Map<LocalPlutusData, LocalPlutusData>
  | Constr<LocalPlutusData>;

type ConstrLike = {
  readonly index: number;
  readonly fields: readonly unknown[];
};

const isConstrLike = (value: unknown): value is ConstrLike =>
  typeof value === "object" &&
  value !== null &&
  Number.isInteger((value as { readonly index?: unknown }).index) &&
  Array.isArray((value as { readonly fields?: unknown }).fields);

const toLocalPlutusData = (value: unknown): LocalPlutusData => {
  if (typeof value === "bigint" || typeof value === "string") {
    return value;
  }
  if (value instanceof Constr || isConstrLike(value)) {
    return new Constr(
      value.index,
      value.fields.map((field) => toLocalPlutusData(field)),
    );
  }
  if (Array.isArray(value)) {
    return value.map((field) => toLocalPlutusData(field));
  }
  if (value instanceof Map) {
    return new Map(
      [...value.entries()].map(([key, mapValue]) => [
        toLocalPlutusData(key),
        toLocalPlutusData(mapValue),
      ]),
    );
  }
  throw new TypeError("Unsupported Plutus data value");
};

export const plutusDataToCborHex = (
  value: unknown,
  options?: { readonly canonical?: boolean },
): string => Data.to(toLocalPlutusData(value) as never, undefined, options);
