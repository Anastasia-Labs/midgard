import {
  AuthenticatedValidator,
  GenericErrorFields,
  LucidError,
} from "@/common.js";
import { Array, Order, Data as EffectData, Effect } from "effect";
import {
  Assets,
  Data,
  fromText,
  LucidEvolution,
  makeReturn,
  toUnit,
  TxBuilder,
  TxSignBuilder,
} from "@lucid-evolution/lucid";
import { AuthenticUTxO } from "./internals.js";

export const ElementDataSchema = Data.Enum([
  Data.Object({ Root: Data.Any() }),
  Data.Object({ Node: Data.Any() }),
]);
export type ElementData = Data.Static<typeof ElementDataSchema>;
export const ElementData = ElementDataSchema as unknown as ElementData;

export const ElementSchema = Data.Object({
  data: ElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type Element = Data.Static<typeof ElementSchema>;
export const Element = ElementSchema as unknown as Element;

export type RootElementUTxO<TRootData> = { data: TRootData };
export type NodeElementUTxO<TNodeData> = { key: string; data: TNodeData };

export type ElementUTxO<TRootData, TNodeData> =
  | AuthenticUTxO<Element, RootElementUTxO<TRootData>>
  | AuthenticUTxO<Element, NodeElementUTxO<TNodeData>>;

export interface LinkedElement {
  key?: string;
  datum: {
    link: string | null;
  };
}

/**
 * Function to find a node in a linked list by its key.
 */
export const findLinkInLinkedList = <T extends { key?: string }>(
  link: string | null,
  elements: T[],
): Effect.Effect<T, LinkedListError> => {
  const errorMessage = `Failed to find link in elements`;

  if (link === null) {
    return Effect.fail(
      new LinkedListError({
        message: errorMessage,
        cause: `Given link is null`,
      }),
    );
  }
  const foundLink = elements.find((u) => u.key === link);
  if (foundLink) {
    return Effect.succeed(foundLink);
  } else {
    return Effect.fail(
      new LinkedListError({
        message: errorMessage,
        cause: `Link "${link}" not found among given elements`,
      }),
    );
  }
};

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
export const takeSortedElements = <
  R extends Data,
  N extends Data,
  T extends ElementUTxO<R, N>
>(elements: T[], compFn: (k0: string, k1: string) => -1 | 0 | 1 = (k0, k1) => {
  if (k0 === k1) {
    return 0;
  } else if (k0 < k1) {
    return -1;
  } else {
    return 1;
  }
}): Effect.Effect<T[], LinkedListError> =>
  Effect.gen(function* () {
    if (elements.length <= 0) return elements;

    // This function only works if none of the node elements are identified with
    // empty keys.
    let emptyKeyEncountered = false;

    // We initially sort by keys:
    const sortedByKeys = Array.sortWith(elements, (e) => {
      if ("key" in e) {
        if (e.key === "") {
          emptyKeyEncountered = true;
        }
        return e.key;
      } else {
        return "";
      }
    }, Order.make(compFn));

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
      ([acc, prev]: [T[], T | undefined], curr: T) => Effect.gen(function* () {
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
  data?: Data;
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

    const rootData = params.data ?? Data.to([]);

    const elementDatum: Element = {
      data: { Root: rootData },
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
