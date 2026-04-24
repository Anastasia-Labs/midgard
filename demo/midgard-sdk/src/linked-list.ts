import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  LucidError,
  UnauthenticUtxoError,
} from "@/common.js";
import { Data as EffectData, Effect } from "effect";
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
  key: string,
  elements: T[],
): Effect.Effect<T, LinkedListError> =>
  Effect.gen(function* () {
    if (elements.length <= 0) {
      return yield* new LinkedListError({
        message: "Failed to find ElementUTxO with the given key",
        cause: "Empty list of ElementUTxOs provided",
      });
    } else {
      const foundUTxO = elements.find(
        (e) => "Node" in e.datum.data && e.assetName.endsWith(key),
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
 * This function assumes provided `ElementUTxO`s have already undergone
 * authentication. Therefore no validation is performed on the assets.
 */
export const findRootElement = <R, N, T extends ElementUTxO<R, N>>(
  elements: T[],
): Effect.Effect<T, LinkedListError> => {
  const rootElements = elements.filter((e) => "key" in e);
  if (rootElements.length === 1) {
    return Effect.succeed(rootElements[0]);
  } else {
    return new LinkedListError({
      message: "Failed to find the root element",
      cause: "Expected exactly one Root element",
    });
  }
};

export const getRootData = <R, N, T extends ElementUTxO<R, N>>(
  element: T,
): Effect.Effect<R, LinkedListError> => {
  if ("key" in element) {
    return new LinkedListError({
      message: "Failed to map given element UTxO to a root",
      cause: "Given ElementUTxO is a node element",
    });
  } else {
    return Effect.succeed(element.coercedData);
  }
};

export const getNodeDataAndKey = <R, N, T extends ElementUTxO<R, N>>(
  element: T,
): Effect.Effect<{ nodeData: N; key: string }, LinkedListError> => {
  if ("key" in element) {
    return Effect.succeed({ nodeData: element.coercedData, key: element.key });
  } else {
    return new LinkedListError({
      message: "Failed to map given element UTxO to a node",
      cause: "Given ElementUTxO has no key",
    });
  }
};

/**
 * Expects the root element and the last element (the one without link) to be
 * present in the given list of element UTxOs.  Starts with root's link, and
 * keeps collecting linked elements up until either the given list is exhausted,
 * or a linked element is not found.
 */
export const sortElementUTxOs = <R, N, T extends ElementUTxO<R, N>>(
  elements: T[],
): Effect.Effect<{ root: T; nodes: T[] }, LinkedListError> =>
  Effect.gen(function* () {
    const root = yield* findRootElement(elements);
    const nodes: T[] = [];
    let latestLink = root.datum.link;
    while (latestLink) {
      const linkedElementUTxO = yield* findElementUTxOByKey(
        latestLink,
        elements,
      );
      nodes.push(linkedElementUTxO);
      latestLink = linkedElementUTxO.datum.link;
    }
    if (latestLink === null) {
      return { root, nodes };
    } else {
      return yield* new LinkedListError({
        message: "Sorting of the given linked list element UTxOs failed",
        cause: "The last element pointed to a missing element",
      });
    }
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
