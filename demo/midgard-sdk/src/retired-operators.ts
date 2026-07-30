import {
  Data,
  fromText,
  LucidEvolution,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AuthenticatedValidator,
  LucidError,
  OutputReferenceSchema,
  POSIXTimeSchema,
} from "@/common.js";
import { authenticateUTxOs, AuthenticUTxO } from "@/internals.js";

import { SlashingArgumentsSchema } from "./active-operators.js";
import {
  incompleteInitLinkedListTxProgram,
  LinkedListDatum,
  linkedListDatumToNodeView,
} from "./linked-list.js";

export const RETIRED_OPERATORS_ROOT_ASSET_NAME = fromText(
  "MIDGARD_RETIRED_OPERATORS",
);

export const RetiredOperatorDatumSchema = Data.Object({
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type RetiredOperatorDatum = Data.Static<
  typeof RetiredOperatorDatumSchema
>;
export const RetiredOperatorDatum =
  RetiredOperatorDatumSchema as unknown as RetiredOperatorDatum;
export const castRetiredOperatorDatumToData = (
  datum: RetiredOperatorDatum,
): unknown => Data.castTo(datum, RetiredOperatorDatum);

export const RetiredOperatorMintRedeemerSchema = Data.Enum([
  Data.Object({
    Init: Data.Object({
      output_index: Data.Integer(),
    }),
  }),
  Data.Literal("Deinit"),
  Data.Object({
    RetireOperator: Data.Object({
      new_retired_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      bond_unlock_time: Data.Nullable(POSIXTimeSchema),
      hub_oracle_ref_input_index: Data.Integer(),
      retired_operator_anchor_element_output_index: Data.Integer(),
      retired_operator_inserted_node_output_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RecoverOperatorBond: Data.Object({
      retired_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      retired_operator_anchor_element_input_outref: OutputReferenceSchema,
      retired_operator_anchor_element_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashOperator: Data.Object({
      slashing_arguments: SlashingArgumentsSchema,
    }),
  }),
]);
export type RetiredOperatorMintRedeemer = Data.Static<
  typeof RetiredOperatorMintRedeemerSchema
>;
export const RetiredOperatorMintRedeemer =
  RetiredOperatorMintRedeemerSchema as unknown as RetiredOperatorMintRedeemer;

export type RetiredOperatorInitParams = {
  validator: AuthenticatedValidator;
  lovelace?: bigint;
};

export type RetiredOperatorUTxO = AuthenticUTxO<RetiredOperatorDatum>;

export type FetchRetiredOperatorParams = {
  retiredOperatorAddress: string;
  operator: string;
  retiredOperatorPolicyId: string;
};

export const fetchRetiredOperatorUTxOs = (
  params: FetchRetiredOperatorParams,
  lucid: LucidEvolution,
): Effect.Effect<RetiredOperatorUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUtxos = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.retiredOperatorAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Retired Operators UTxOs",
          cause: err,
        }),
    });
    if (allUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Retired Operators transaction",
        cause: "No UTxOs found in Retired Operators Contract address",
      });
    }
    const linkedListUTxOs = yield* authenticateUTxOs<LinkedListDatum>(
      allUtxos,
      params.retiredOperatorPolicyId,
      LinkedListDatum,
    );
    return yield* Effect.allSuccesses(
      linkedListUTxOs.map(({ assetName, utxo, datum }) =>
        Effect.try(() => ({
          assetName,
          utxo,
          datum: Data.castFrom(
            linkedListDatumToNodeView(datum, assetName).data as never,
            RetiredOperatorDatum as never,
          ) as RetiredOperatorDatum,
        })),
      ),
    );
  });

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRetiredOperatorInitTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorInitParams,
): Effect.Effect<TxBuilder> =>
  incompleteInitLinkedListTxProgram(lucid, {
    validator: params.validator,
    rootAssetName: RETIRED_OPERATORS_ROOT_ASSET_NAME,
    data: "",
    redeemer: (outputIndex) =>
      Data.to(
        { Init: { output_index: outputIndex } },
        RetiredOperatorMintRedeemer,
      ),
    lovelace: params.lovelace,
  });
