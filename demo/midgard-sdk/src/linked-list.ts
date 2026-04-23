import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  LucidError,
  MissingDatumError,
  ValueSchema,
} from "@/common.js";
import { Data as EffectData, Effect } from "effect";
import {
  Assets,
  Data,
  fromText,
  LucidEvolution,
  makeReturn,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";

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
export const NodeKey = NodeKeySchema as unknown as NodeKey;

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
export const LinkedListElementData =
  LinkedListElementDataSchema as unknown as LinkedListElementData;

export const LinkedListDatumSchema = Data.Object({
  data: LinkedListElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type LinkedListDatum = Data.Static<typeof LinkedListDatumSchema>;
export const LinkedListDatum =
  LinkedListDatumSchema as unknown as LinkedListDatum;

export const NodeDatumSchema = LinkedListDatumSchema;
export type NodeDatum = LinkedListDatum;
export const NodeDatum = LinkedListDatum;

export const LinkedListNodeViewSchema = Data.Object({
  key: NodeKeySchema,
  next: NodeKeySchema,
  data: Data.Any(),
});
export type LinkedListNodeView = Data.Static<
  typeof LinkedListNodeViewSchema
>;
export const LinkedListNodeView =
  LinkedListNodeViewSchema as unknown as LinkedListNodeView;

const extractSingletonAssetName = (assets: Assets): string => {
  const nonAdaUnits = Object.keys(assets).filter((unit) => unit !== "lovelace");
  if (nonAdaUnits.length !== 1) {
    throw new Error(
      `Expected exactly one non-ADA linked-list authentication asset, found ${nonAdaUnits.length}`,
    );
  }
  return nonAdaUnits[0]!.slice(56);
};

const assetNameToNodeKey = (assetName: string): NodeKey => {
  const prefix = CANONICAL_NODE_ASSET_NAME_PREFIXES.find((candidate) =>
    assetName.startsWith(candidate),
  );
  return {
    Key: {
      key:
        prefix === undefined
          ? assetName
          : assetName.slice(prefix.length),
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
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const linkedListDatum = Data.from(datumCBOR, LinkedListDatum);
      const assetName = extractSingletonAssetName(nodeUTxO.assets);
      return Effect.succeed(
        linkedListDatumToNodeView(linkedListDatum, assetName),
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
  } else {
    return Effect.fail(
      new MissingDatumError({
        message: "Provided UTxO was expected to carry an inline datum",
        cause: `No datum found in ${nodeUTxO.txHash}.${nodeUTxO.outputIndex}`,
      }),
    );
  }
};

export const getNodeDatumFromUTxO = getLinkedListNodeViewFromUTxO;

export class LinkedListError extends EffectData.TaggedError(
  "LinkedListError",
)<GenericErrorFields> {}

export type LinkedListInitParams = {
  validator: AuthenticatedValidator;
  rootAssetName: string;
  data?: Data;
  redeemer: string;
  lovelace?: bigint;
};

export const incompleteInitLinkedListTxProgram = (
  lucid: LucidEvolution,
  params: LinkedListInitParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(params.validator.policyId, params.rootAssetName)]: 1n,
    };

    const rootData = params.data ?? Data.to([]);

    const nodeDatum: LinkedListDatum = {
      data: {
        Root: {
          data: rootData,
        },
      },
      link: null,
    };

    const encodedDatum = Data.to<LinkedListDatum>(nodeDatum, LinkedListDatum);
    const tx = lucid
      .newTx()
      .mintAssets(assets, params.redeemer)
      .pay.ToAddressWithData(
        params.validator.spendingScriptAddress,
        { kind: "inline", value: encodedDatum },
        {
          ...(params.lovelace === undefined ? {} : { lovelace: params.lovelace }),
          ...assets,
        },
      )
      .attach.Script(params.validator.mintingScript);

    return tx;
  });

export const unsignedLinkedListTxProgram = (
  lucid: LucidEvolution,
  initParams: LinkedListInitParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteInitLinkedListTxProgram(
      lucid,
      initParams,
    );
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: true }),
      catch: (e) =>
        new LucidError({
          message: `Failed to build the linked list initialization transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for initializing all Midgard contracts.
 * This includes: Hub Oracle, State Queue, Settlement Queue,
 * Registered/Active/Retired Operators, Scheduler, Escape Hatch,
 * and Fraud Proof Catalogue.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param initParams - Parameters for initializing all Midgard contracts.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedLinkedListTx = (
  lucid: LucidEvolution,
  initParams: LinkedListInitParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedLinkedListTxProgram(lucid, initParams)).unsafeRun();
