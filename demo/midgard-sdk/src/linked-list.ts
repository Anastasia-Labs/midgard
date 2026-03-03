import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  LucidError,
  MissingDatumError,
  POSIXTimeSchema,
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

export const ElementDataSchema= Data.Enum([
    Data.Object({ Root: Data.Object({ data: Data.Any() }) }),
    Data.Object({ Node: Data.Object({ data: Data.Any() }) }),
]);
export type ElementData = Data.Static<typeof ElementDataSchema>;
export const ElementData = ElementDataSchema as unknown as ElementData;

export const ElementSchema = Data.Object({
  data: ElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type Element = Data.Static<typeof ElementSchema>;
export const Element = ElementSchema as unknown as Element;

export const ActiveandRetiredNodeDataSchema = Data.Object({
   bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type ActiveandRetiredNodeData = Data.Static<typeof ActiveandRetiredNodeDataSchema>;
export const ActiveandRetiredNodeData = ActiveandRetiredNodeDataSchema as unknown as ActiveandRetiredNodeData;

export const ActiveandRetiredElementDataSchema= Data.Enum([
    Data.Object({ Root: Data.Bytes() }),
    Data.Object({ Node: ActiveandRetiredNodeDataSchema }),
]);
export type ActiveandRetiredElementData = Data.Static<typeof ActiveandRetiredElementDataSchema>;
export const ActiveandRetiredElementData = ActiveandRetiredElementDataSchema as unknown as ActiveandRetiredElementData;

export const ActiveandRetiredElementSchema = Data.Object({
  data: ActiveandRetiredElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type ActiveandRetiredElement = Data.Static<typeof ActiveandRetiredElementSchema>;
export const ActiveandRetiredElement = ActiveandRetiredElementSchema as unknown as ActiveandRetiredElement;

export const RegisteredNodeDataSchema = Data.Object({
   activation_time: Data.Nullable(POSIXTimeSchema),
});
export type RegisteredNodeData = Data.Static<typeof RegisteredNodeDataSchema>;
export const RegisteredNodeData = RegisteredNodeDataSchema as unknown as RegisteredNodeData;

export const RegisteredElementDataSchema= Data.Enum([
    Data.Object({ Root: Data.Bytes() }),
    Data.Object({ Node: RegisteredNodeDataSchema }),
]);
export type RegisteredElementData = Data.Static<typeof RegisteredElementDataSchema>;
export const RegisteredElementData = RegisteredElementDataSchema as unknown as RegisteredElementData;

export const RegisteredElementSchema = Data.Object({
  data: RegisteredElementDataSchema,
  link: Data.Nullable(Data.Bytes()),
});
export type RegisteredElement = Data.Static<typeof RegisteredElementSchema>;
export const RegisteredElement = RegisteredElementSchema as unknown as RegisteredElement;

export const getElementDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<Element, DataCoercionError | MissingDatumError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const elementDatum = Data.from(datumCBOR, Element);
      return Effect.succeed(elementDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: "Could not coerce provided UTxO's datum to a `Element`",
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

export class LinkedListError extends EffectData.TaggedError(
  "LinkedListError",
)<GenericErrorFields> {}

export type LinkedListInitParams = {
  validator: AuthenticatedValidator;
  data?: Data;
  redeemer: string;
};

export const incompleteInitLinkedListTxProgram = (
  lucid: LucidEvolution,
  params: LinkedListInitParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(params.validator.policyId, NODE_ASSET_NAME)]: 1n,
    };

    const rootData = params.data ?? Data.to([]);

    const elementDatum: Element = {
      data: { Root: { data: rootData } },
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
