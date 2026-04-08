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

/**
 * Shared linked-list primitives used by several Midgard on-chain state
 * machines.
 *
 * Registered, active, and retired operators all reuse the same anchor-node
 * pattern. Centralizing the datum and initialization logic keeps those lists
 * structurally consistent at genesis.
 */
export const NodeKeySchema = Data.Enum([
  Data.Object({ Key: Data.Object({ key: Data.Bytes() }) }),
  Data.Literal("Empty"),
]);
export type NodeKey = Data.Static<typeof NodeKeySchema>;
export const NodeKey = NodeKeySchema as unknown as NodeKey;

/**
 * Datum stored at each node in a Midgard linked list.
 */
export const NodeDatumSchema = Data.Object({
  key: NodeKeySchema,
  next: NodeKeySchema,
  data: Data.Any(),
});
export type NodeDatum = Data.Static<typeof NodeDatumSchema>;
export const NodeDatum = NodeDatumSchema as unknown as NodeDatum;

/**
 * Extracts and decodes a linked-list node datum from a UTxO.
 */
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

/**
 * Parameters for minting the initial linked-list anchor node.
 */
export type LinkedListInitParams = {
  validator: AuthenticatedValidator;
  data?: Data;
  redeemer: string;
  lovelace?: bigint;
};

/**
 * Builds a transaction fragment that mints the anchor NFT and creates the first
 * linked-list node for a state machine.
 */
export const incompleteInitLinkedListTxProgram = (
  lucid: LucidEvolution,
  params: LinkedListInitParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    const mintedAssets: Assets = {
      [toUnit(params.validator.policyId, NODE_ASSET_NAME)]: 1n,
    };
    const outputAssets: Assets =
      params.lovelace === undefined
        ? mintedAssets
        : {
            ...mintedAssets,
            lovelace: params.lovelace,
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
      .mintAssets(mintedAssets, params.redeemer)
      .pay.ToAddressWithData(
        params.validator.spendingScriptAddress,
        { kind: "inline", value: encodedDatum },
        outputAssets,
      )
      .attach.Script(params.validator.mintingScript);

    return tx;
  });

/**
 * Completes the linked-list initialization fragment with local UPLC evaluation
 * enabled.
 */
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
 * Promise-style wrapper for linked-list initialization transactions.
 */
export const unsignedLinkedListTx = (
  lucid: LucidEvolution,
  initParams: LinkedListInitParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedLinkedListTxProgram(lucid, initParams)).unsafeRun();
