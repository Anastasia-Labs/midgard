import {
  AuthenticatedValidator,
  POSIXTimeSchema,
  LucidError,
} from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
import { Data, UTxO, fromText } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "../linked-list.js";
import { SlashingArgumentsSchema } from "./common.js";

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
  Data.Object({
    Deinit: Data.Object({
      input_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RetireOperator: Data.Object({
      new_retired_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      bond_unlock_time: Data.Nullable(POSIXTimeSchema),
      hub_oracle_ref_input_index: Data.Integer(),
      retired_operator_anchor_element_input_index: Data.Integer(),
      retired_operator_anchor_element_output_index: Data.Integer(),
      retired_operator_inserted_node_output_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    ReregisterOperator: Data.Object({
      retired_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      retired_operator_bond_unlock_time: Data.Nullable(POSIXTimeSchema),
      hub_oracle_ref_input_index: Data.Integer(),
      retired_operator_anchor_element_input_index: Data.Integer(),
      retired_operator_removed_node_input_index: Data.Integer(),
      retired_operator_anchor_element_output_index: Data.Integer(),
      registered_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    RecoverOperatorBond: Data.Object({
      retired_operator_key: Data.Bytes({ minLength: 28, maxLength: 28 }),
      retired_operator_anchor_element_input_index: Data.Integer(),
      retired_operator_removed_node_input_index: Data.Integer(),
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
  outputIndex?: bigint;
  lovelace?: bigint;
};

export type RetiredOperatorDeinitParams = {};
export type RetiredOperatorRetireParams = {};
export type RetiredOperatorRemoveOperatorParams = {};
export type RetiredOperatorRecoverSlashBondParams = {};

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
    const allUtxos: UTxO[] = yield* Effect.tryPromise({
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
    const retiredOperatorUTxOs: RetiredOperatorUTxO[] =
      yield* authenticateUTxOs<RetiredOperatorDatum>(
        allUtxos,
        params.retiredOperatorPolicyId,
        RetiredOperatorDatum,
      );
    return retiredOperatorUTxOs;
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
  Effect.gen(function* () {
    const rootData = "";

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      rootAssetName: RETIRED_OPERATORS_ROOT_ASSET_NAME,
      data: rootData,
      redeemer: Data.to(
        { Init: { output_index: params.outputIndex ?? 0n } },
        RetiredOperatorMintRedeemer,
      ),
      lovelace: params.lovelace,
    });
  });

/**
 * Deinit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRetiredOperatorDeinitTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorDeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Retire
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRetiredOperatorRetireTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorRetireParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * RemoveOperator
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRetiredOperatorRemoveOperatorTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorRemoveOperatorParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Recover
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRetiredOperatorRecoverSlashBondTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorRecoverSlashBondParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
