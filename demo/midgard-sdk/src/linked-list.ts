import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  LucidError,
  UnauthenticUtxoError,
} from "@/common.js";
import { Array, Order, Data as EffectData, Effect } from "effect";
import {
  Assets,
  Data,
  fromText,
  LucidEvolution,
  makeReturn,
  PolicyId,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { authenticateUTxO, AuthenticUTxO } from "./internals.js";

export const ElementDataSchema = Data.Enum([
  Data.Object({ Root: Data.Any() }),
  Data.Object({ Node: Data.Any() }),
]);
export type ElementData = Data.Static<typeof ElementDataSchema>;
export const ElementData = ElementDataSchema as unknown as ElementData;

export const LinkSchema = Data.Nullable(Data.Bytes());
export type Link = Data.Static<typeof LinkSchema>;
export const Link = LinkSchema as unknown as Link;

export const ElementSchema = Data.Object({
  data: ElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type Element = Data.Static<typeof ElementSchema>;
export const Element = ElementSchema as unknown as Element;

export type RootElementUTxO<TRootData> = { coercedData: TRootData };
export type NodeElementUTxO<TNodeData> = {
  key: string;
  coercedData: TNodeData;
};

export type ElementUTxO<TRootData, TNodeData> =
  | AuthenticUTxO<Element, RootElementUTxO<TRootData>>
  | AuthenticUTxO<Element, NodeElementUTxO<TNodeData>>;

export type LinkedListParameters<TRootData, TNodeData> = {
  nftPolicy: PolicyId;
  rootKey: string;
  nodeKeyPrefix: string;
  rootType: TRootData;
  nodeType: TNodeData;
};

export const utxoToElementUTxO = <R, N>(
  utxo: UTxO,
  params: LinkedListParameters<R, N>,
): Effect.Effect<ElementUTxO<R, N>, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const authUTxO = yield* authenticateUTxO<Element>(
      utxo,
      params.nftPolicy,
      Element,
    );
    if ("Root" in authUTxO.datum.data) {
      if (authUTxO.assetName === params.rootKey) {
        const rootData = authUTxO.datum.data.Root;
        const root = Data.castFrom(rootData, params.rootType);
        return { ...authUTxO, coercedData: root };
      } else {
        return yield* new UnauthenticUtxoError({
          message: "Failed to authenticate the given UTxO as an ElementUTxO",
          cause: "Found element UTxO datum mismatches its asset name",
        });
      }
    } else {
      if (authUTxO.assetName.startsWith(params.nodeKeyPrefix)) {
        const nodeData = authUTxO.datum.data.Node;
        const node = Data.castFrom(nodeData, params.nodeType);
        return {
          ...authUTxO,
          coercedData: node,
          key: authUTxO.assetName.slice(0, params.nodeKeyPrefix.length),
        };
      } else {
        return yield* new UnauthenticUtxoError({
          message: "Failed to authenticate the given UTxO as an ElementUTxO",
          cause: "Unexpected key prefix in asset name",
        });
      }
    }
  });

export const utxosToElementUTxOs = <R, N>(
  utxos: UTxO[],
  params: LinkedListParameters<R, N>,
): Effect.Effect<ElementUTxO<R, N>[]> =>
  Effect.allSuccesses(utxos.map((u) => utxoToElementUTxO<R, N>(u, params)));

export const findElementUTxOByKey = <R, N, T extends ElementUTxO<R, N>>(
  key: string | null,
  elements: T[],
  params: LinkedListParameters<R, N>,
): Effect.Effect<T, LinkedListError> =>
  Effect.gen(function* () {
    if (elements.length <= 0) {
      return yield* new LinkedListError({
        message: "Failed to find ElementUTxO with the given key",
        cause: "Empty list of ElementUTxOs provided",
      });
    } else if (key === params.rootKey) {
      const foundUTxO = elements.find(
        (e) => e.assetName === params.rootKey && "Root" in e.datum.data,
      );
      if (foundUTxO === undefined) {
        return yield* new LinkedListError({
          message: "Root element not found",
          cause:
            "None of the provided ElementUTxOs matched the given root key along with a Root element data",
        });
      } else {
        return foundUTxO;
      }
    } else {
      const foundUTxO = elements.find(
        (e) =>
          e.assetName.startsWith(params.nodeKeyPrefix) &&
          "Node" in e.datum.data,
      );
      if (foundUTxO === undefined) {
        return yield* new LinkedListError({
          message: "Node element not found",
          cause:
            "None of the provided ElementUTxOs had an NFT with an asset name starting with the given prefix, along with a Node element data",
        });
      } else {
        return foundUTxO;
      }
    }
  });

/**
 * Given a list of `ElementUTxO` values, this function sorts them by their keys
 * (with the given comparison function, ascending by default), and keeps as many
 * of the elements that correctly point to their links. In other words, any
 * dangling element is silently dropped from the given list of elements.
 *
 * This function does NOT mutate the given array of elements.
 *
 * This function also does not work if any of the **node elements** have empty
 * keys. Root key is not considered at all.
 *
 * @param elements: The array of `ElementUTxO` values that you want to sort.
 */
export const takeSortedElements = <R, N, T extends ElementUTxO<R, N>>(
  elements: T[],
  compFn: (k0: string, k1: string) => -1 | 0 | 1 = (k0, k1) =>
    Buffer.from(k0, "hex").compare(Buffer.from(k1, "hex")),
): Effect.Effect<T[], LinkedListError> =>
  Effect.gen(function* () {
    if (elements.length <= 0) return elements;

    // This function only works if none of the node elements are identified with
    // empty keys.
    let emptyKeyEncountered = false;

    // We initially sort by keys:
    const sortedByKeys = Array.sortWith(
      elements,
      (e) => {
        if ("key" in e) {
          if (e.key === "") {
            emptyKeyEncountered = true;
          }
          return e.key;
        } else {
          return "";
        }
      },
      Order.make(compFn),
    );

    if (emptyKeyEncountered) {
      return yield* new LinkedListError({
        message: "Sort failed",
        cause: "An element with empty key was encountered",
      });
    }

    // In a second pass we make sure each element is followed by its link:
    const firstElement = sortedByKeys[0];
    const [remainingElements, _lastElement] = yield* Effect.reduce(
      sortedByKeys,
      [[], firstElement],
      ([acc, prev]: [T[], T | undefined], curr: T) =>
        Effect.gen(function* () {
          if (prev === undefined) {
            return [acc, undefined];
          } else if ("key" in curr) {
            if (prev.datum.link === curr.key) {
              return [[...acc, prev], curr];
            } else {
              return [[...acc, prev], undefined];
            }
          } else {
            return yield* new LinkedListError({
              message: "Sort failed",
              cause: "More than 1 root element encountered",
            });
          }
        }),
    );

    return remainingElements;
  });

export type LinkedListInitParams = {
  validator: AuthenticatedValidator;
  rootData: Data;
  redeemer: string;
  rootKey: string;
};

export const incompleteInitLinkedListTxProgram = (
  lucid: LucidEvolution,
  params: LinkedListInitParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(params.validator.policyId, fromText(params.rootKey))]: 1n,
    };

    const elementDatum: Element = {
      data: { Root: params.rootData },
      link: null,
    };

    const encodedDatum = Data.to<Element>(elementDatum, Element);
    const tx = lucid
      .newTx()
      .mintAssets(assets, params.redeemer)
      .pay.ToAddressWithData(
        params.validator.spendingScriptAddress,
        { kind: "inline", value: encodedDatum },
        assets,
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

export class LinkedListError extends EffectData.TaggedError(
  "LinkedListError",
)<GenericErrorFields> {}
