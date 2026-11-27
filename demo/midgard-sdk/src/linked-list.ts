import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Assets, Data, fromText, UTxO } from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import {
  DataCoercionError,
  GenericErrorFields,
  MissingDatumError,
} from "./common.js";

export const NODE_ASSET_NAME = fromText("Node");
export const STATE_QUEUE_NODE_ASSET_NAME_PREFIX = fromText("MBLC");
export const REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX = fromText("MREG");
export const ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX = fromText("MACT");
export const RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX = fromText("MRET");

const CANONICAL_NODE_ASSET_NAME_PREFIXES = [
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  NODE_ASSET_NAME,
] as const;

export const NodeKeySchema = Data.Enum([
  Data.Object({ Key: Data.Object({ key: Data.Bytes() }) }),
  Data.Literal("Empty"),
]);
export type NodeKey = Data.Static<typeof NodeKeySchema>;
export const NodeKey = asDataType<NodeKey>(NodeKeySchema);

export const LinkedListElementDataSchema = Data.Enum([
  Data.Object({
    Root: Data.Object({
      data: Data.Any(),
    }),
  }),
  Data.Object({
    Node: Data.Object({
      data: Data.Any(),
    }),
  }),
]);
export type LinkedListElementData = Data.Static<
  typeof LinkedListElementDataSchema
>;
export const LinkedListElementData = asDataType<LinkedListElementData>(
  LinkedListElementDataSchema,
);

export const LinkedListDatumSchema = Data.Object({
  data: LinkedListElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type LinkedListDatum = Data.Static<typeof LinkedListDatumSchema>;
export const LinkedListDatum = asDataType<LinkedListDatum>(
  LinkedListDatumSchema,
);

export const LinkedListNodeViewSchema = Data.Object({
  key: NodeKeySchema,
  next: NodeKeySchema,
  data: Data.Any(),
});
export type LinkedListNodeView = Data.Static<typeof LinkedListNodeViewSchema>;
export const LinkedListNodeView = asDataType<LinkedListNodeView>(
  LinkedListNodeViewSchema,
);

const extractSingletonAssetName = (assets: Assets): string => {
  const nonAdaUnits = Object.keys(assets).filter((unit) => unit !== "lovelace");
  if (nonAdaUnits.length !== 1) {
    throw new Error(
      `Expected exactly one non-ADA linked-list authentication asset, found ${nonAdaUnits.length}`,
    );
  }
  return nonAdaUnits[0].slice(56);
};

const assetNameToNodeKey = (assetName: string): NodeKey => {
  const prefix = CANONICAL_NODE_ASSET_NAME_PREFIXES.find((candidate) =>
    assetName.startsWith(candidate),
  );
  return {
    Key: {
      key: prefix === undefined ? assetName : assetName.slice(prefix.length),
    },
  };
};

const linkToNodeKey = (link: string | null): NodeKey =>
  link === null ? "Empty" : { Key: { key: link } };

const nodeKeyToLink = (key: NodeKey): string | null =>
  key === "Empty" ? null : key.Key.key;

export const linkedListDatumToNodeView = (
  linkedListDatum: LinkedListDatum,
  assetName: string,
): LinkedListNodeView => {
  if ("Root" in linkedListDatum.data) {
    return {
      key: "Empty",
      next: linkToNodeKey(linkedListDatum.link),
      data: linkedListDatum.data.Root.data,
    };
  }
  return {
    key: assetNameToNodeKey(assetName),
    next: linkToNodeKey(linkedListDatum.link),
    data: linkedListDatum.data.Node.data,
  };
};

export const nodeViewToLinkedListDatum = (
  nodeView: LinkedListNodeView,
): LinkedListDatum => {
  if (nodeView.key === "Empty") {
    return {
      data: {
        Root: {
          data: nodeView.data,
        },
      },
      link: nodeKeyToLink(nodeView.next),
    };
  }
  return {
    data: {
      Node: {
        data: nodeView.data,
      },
    },
    link: nodeKeyToLink(nodeView.next),
  };
};

export const encodeLinkedListNodeView = (
  nodeView: LinkedListNodeView,
): string =>
  Data.to<LinkedListDatum>(
    nodeViewToLinkedListDatum(nodeView),
    LinkedListDatum,
  );

export const getLinkedListNodeViewFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<LinkedListNodeView, DataCoercionError | MissingDatumError> => {
  if (!nodeUTxO.datum) {
    return Effect.fail(
      new MissingDatumError({
        message: "Provided UTxO was expected to carry an inline datum",
        cause: `No datum found in ${nodeUTxO.txHash}.${nodeUTxO.outputIndex}`,
      }),
    );
  }

  try {
    return Effect.succeed(
      linkedListDatumToNodeView(
        Data.from(nodeUTxO.datum, LinkedListDatum),
        extractSingletonAssetName(nodeUTxO.assets),
      ),
    );
  } catch (e) {
    return Effect.fail(
      new DataCoercionError({
        message:
          "Could not derive linked-list node view from provided UTxO datum",
        cause: e,
      }),
    );
  }
};

export class LinkedListError extends EffectData.TaggedError(
  "LinkedListError",
)<GenericErrorFields> {}
