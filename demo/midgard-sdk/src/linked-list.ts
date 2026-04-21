import {
  AuthenticatedValidator,
  GenericErrorFields,
  LucidError,
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
 * Function to sort a linked list.
 */
export const sortLinkedList = <T extends LinkedElement>(
  elements: T[],
  root: T,
  firstLink: string | null,
): Effect.Effect<T[], LinkedListError> =>
  Effect.gen(function* () {
    const elementMap = new Map(
      elements.filter((e) => e.key !== undefined).map((e) => [e.key!, e]),
    );
    const sorted: T[] = [root];
    let link = firstLink;

    while (link !== null) {
      const nextElement = elementMap.get(link);

      if (nextElement) {
        sorted.push(nextElement);
        link = nextElement.datum.link;
      } else {
        return yield* Effect.fail(
          new LinkedListError({
            message: `Failed to sort elements in linked list`,
            cause: `Root node not found among given elements`,
          }),
        );
      }
    }
    return sorted;
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
