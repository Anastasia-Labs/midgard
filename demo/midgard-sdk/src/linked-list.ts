import {
  DataCoercionError,
  GenericErrorFields,
  LucidError,
  MissingDatumError,
  ValueSchema,
} from "./common.js";
import { Data as EffectData, Effect } from "effect";
import {
  Assets,
  Data,
  fromText,
  LucidEvolution,
  makeReturn,
  Script,
  toUnit,
  TxBuilder,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";

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

export const CommonSchema = Data.Object({
  ownCS: Data.Bytes(),
  mint: ValueSchema,
  nodeInputs: Data.Array(NodeKeySchema),
  nodeOutputs: Data.Array(NodeKeySchema),
});
export type Common = Data.Static<typeof CommonSchema>;
export const Common = CommonSchema as unknown as Common;

// Add this near the top with other schemas
export const KeyUnorderedLinkedListRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Literal("PrependUnsafe"),
  Data.Literal("AppendUnsafe"),
  Data.Literal("Remove"),
]);

export type KeyUnorderedLinkedListRedeemer = Data.Static<
  typeof KeyUnorderedLinkedListRedeemerSchema
>;
export const KeyUnorderedLinkedListRedeemer =
  KeyUnorderedLinkedListRedeemerSchema as unknown as KeyUnorderedLinkedListRedeemer;

export const KeyOrderedLinkedListRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Literal("PrependSafe"),
  Data.Literal("AppendSafe"),
  Data.Literal("Insert"),
  Data.Literal("Remove"),
]);

export type KeyOrderedLinkedListRedeemer = Data.Static<
  typeof KeyOrderedLinkedListRedeemerSchema
>;
export const KeyOrderedLinkedListRedeemer =
  KeyOrderedLinkedListRedeemerSchema as unknown as KeyOrderedLinkedListRedeemer;

// Empty root data for lists that don't need data in root
export const EmptyRootDataSchema = Data.Tuple([]);

export type EmptyRootData = Data.Static<typeof EmptyRootDataSchema>;
export const EmptyRootData = EmptyRootDataSchema as unknown as EmptyRootData;

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
          message: "Could not coerce provided UTxO's datum to a node datum",
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new MissingDatumError({
        message: "Provided UTxO was expected to carry an inlined `NodeDatum`",
        cause: `No datum found in ${nodeUTxO.txHash}.${nodeUTxO.outputIndex}`,
      }),
    );
  }
};

export class LinkedListError extends EffectData.TaggedError(
  "LinkedListError",
)<GenericErrorFields> {}

export type LinkedListInitParams = {
  policyId: string;
  address: string;
  mintScript: Script;
  dataSchema: any;
  data: any;
};

export const initLinkedListProgram = (
  lucid: LucidEvolution,
  params: LinkedListInitParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    const assets: Assets = {
      [toUnit(params.policyId, fromText("Node"))]: 1n,
    };

    const nodeDatum: NodeDatum = {
      key: "Empty",
      next: "Empty",
      data: Data.castTo(params.data, params.dataSchema),
    };

    const encodedDatum = Data.to<NodeDatum>(nodeDatum, NodeDatum);

    const redeemer: KeyUnorderedLinkedListRedeemer = "Init";
    const encodedRedeemer = Data.to(redeemer, KeyUnorderedLinkedListRedeemer);

    const tx = lucid
      .newTx()
      .mintAssets(assets, encodedRedeemer)
      .pay.ToAddressWithData(
        params.address,
        { kind: "inline", value: encodedDatum },
        assets,
      )
      .attach.Script(params.mintScript);

    return tx;
  });

export const unsignedLinkedListTxProgram = (
  lucid: LucidEvolution,
  initParams: LinkedListInitParams,
): Effect.Effect<TxSignBuilder, LucidError> =>
  Effect.gen(function* () {
    const commitTx = yield* initLinkedListProgram(lucid, initParams);
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: true }),
      catch: (e) =>
        new LucidError({
          message: `Failed to build the init state queue transaction: ${e}`,
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
