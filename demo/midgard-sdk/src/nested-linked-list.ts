import { Data, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { DataCoercionError, MissingDatumError } from "@/common.js";

export const NestedLinkedListElementDataSchema = Data.Enum([
  Data.Object({
    Root: Data.Object({
      data: Data.Any(),
    }),
  }),
  Data.Object({
    InnerRoot: Data.Object({
      data: Data.Any(),
      child_link: Data.Nullable(Data.Bytes()),
    }),
  }),
  Data.Object({
    Node: Data.Object({
      data: Data.Any(),
    }),
  }),
]);
export type NestedLinkedListElementData = Data.Static<
  typeof NestedLinkedListElementDataSchema
>;
export const NestedLinkedListElementData =
  NestedLinkedListElementDataSchema as unknown as NestedLinkedListElementData;

export const NestedLinkedListDatumSchema = Data.Object({
  data: NestedLinkedListElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type NestedLinkedListDatum = Data.Static<
  typeof NestedLinkedListDatumSchema
>;
export const NestedLinkedListDatum =
  NestedLinkedListDatumSchema as unknown as NestedLinkedListDatum;

export const nestedLinkedListRootDatum = (
  data: Data,
  link: string | null,
): NestedLinkedListDatum => ({
  data: {
    Root: {
      data,
    },
  },
  link,
});

export const nestedLinkedListInnerRootDatum = (
  data: Data,
  childLink: string | null,
  link: string | null,
): NestedLinkedListDatum => ({
  data: {
    InnerRoot: {
      data,
      child_link: childLink,
    },
  },
  link,
});

export const nestedLinkedListNodeDatum = (
  data: Data,
  link: string | null,
): NestedLinkedListDatum => ({
  data: {
    Node: {
      data,
    },
  },
  link,
});

export const encodeNestedLinkedListDatum = (
  datum: NestedLinkedListDatum,
): string => Data.to<NestedLinkedListDatum>(datum, NestedLinkedListDatum);

export const decodeNestedLinkedListDatum = (
  datum: string,
): NestedLinkedListDatum =>
  Data.from(datum, NestedLinkedListDatum) as NestedLinkedListDatum;

export const getNestedLinkedListDatumFromUTxO = (
  utxo: UTxO,
): Effect.Effect<
  NestedLinkedListDatum,
  DataCoercionError | MissingDatumError
> => {
  if (!utxo.datum) {
    return Effect.fail(
      new MissingDatumError({
        message: "Provided UTxO was expected to carry an inline datum",
        cause: `No datum found in ${utxo.txHash}.${utxo.outputIndex}`,
      }),
    );
  }

  try {
    return Effect.succeed(decodeNestedLinkedListDatum(utxo.datum));
  } catch (e) {
    return Effect.fail(
      new DataCoercionError({
        message:
          "Could not derive nested linked-list datum from provided UTxO datum",
        cause: e,
      }),
    );
  }
};
