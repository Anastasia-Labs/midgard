import {
  AuthenticatedValidator,
  DataCoercionError,
  GenericErrorFields,
  LucidError,
  MissingDatumError,
} from "@/common.js";
import { Data as EffectData, Effect } from "effect";
import {
  Assets,
  Data,
  LucidEvolution,
  makeReturn,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { NODE_ASSET_NAME } from "@/constants.js";

export const NodeKeySchema = Data.Enum([
  Data.Object({ Key: Data.Object({ key: Data.Bytes() }) }),
  Data.Literal("Empty"),
]);
export type NodeKey = Data.Static<typeof NodeKeySchema>;
export const NodeKey = NodeKeySchema as unknown as NodeKey;

export const NodeDatumSchema = Data.Object({
  key: NodeKeySchema,
  next: NodeKeySchema,
  data: Data.Any(),
});
export type NodeDatum = Data.Static<typeof NodeDatumSchema>;
export const NodeDatum = NodeDatumSchema as unknown as NodeDatum;

export const getNodeDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<NodeDatum, DataCoercionError | MissingDatumError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const nodeDatum = Data.from(datumCBOR, NodeDatum);
      return Effect.succeed(nodeDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: "Could not coerce provided UTxO's datum to a `NodeDatum`",
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

    const nodeDatum: NodeDatum = {
      key: "Empty",
      next: "Empty",
      data: rootData,
    };

    const encodedDatum = Data.to<NodeDatum>(nodeDatum, NodeDatum);
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
