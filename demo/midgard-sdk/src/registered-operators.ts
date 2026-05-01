import { AuthenticatedValidator, POSIXTimeSchema } from "@/common.js";
import {
  Data,
  LucidEvolution,
  TxBuilder,
  fromText,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

export const REGISTERED_OPERATORS_ROOT_ASSET_NAME = fromText(
  "MIDGARD_REGISTERED_OPERATORS",
);

export const RegisteredOperatorDatumSchema = Data.Object({
  operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type RegisteredOperatorDatum = Data.Static<
  typeof RegisteredOperatorDatumSchema
>;
export const RegisteredOperatorDatum =
  RegisteredOperatorDatumSchema as unknown as RegisteredOperatorDatum;
export const castRegisteredOperatorDatumToData = (
  datum: RegisteredOperatorDatum,
): unknown => Data.castTo(datum, RegisteredOperatorDatum);

export const DuplicateOperatorStatusSchema = Data.Enum([
  Data.Literal("DuplicateIsRegistered"),
  Data.Object({
    DuplicateIsActive: Data.Object({
      hub_oracle_ref_input_index: Data.Integer(),
    }),
  }),
  Data.Literal("DuplicateIsRetired"),
]);
export type DuplicateOperatorStatus = Data.Static<
  typeof DuplicateOperatorStatusSchema
>;
export const DuplicateOperatorStatus =
  DuplicateOperatorStatusSchema as unknown as DuplicateOperatorStatus;

export const OperatorOriginSchema = Data.Enum([
  Data.Object({
    ReturningOperator: Data.Object({
      retired_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    NewOperator: Data.Object({
      retired_operators_element_ref_input_index: Data.Integer(),
    }),
  }),
]);
export type OperatorOrigin = Data.Static<typeof OperatorOriginSchema>;
export const OperatorOrigin = OperatorOriginSchema as unknown as OperatorOrigin;

export const RegisteredOperatorMintRedeemerSchema = Data.Enum([
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
    RegisterOperator: Data.Object({
      registering_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      root_input_index: Data.Integer(),
      root_output_index: Data.Integer(),
      registered_node_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      active_operators_element_ref_input_index: Data.Integer(),
      operator_origin: OperatorOriginSchema,
    }),
  }),
  Data.Object({
    ActivateOperator: Data.Object({
      activating_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      anchor_element_input_index: Data.Integer(),
      removed_node_input_index: Data.Integer(),
      anchor_element_output_index: Data.Integer(),
      hub_oracle_ref_input_index: Data.Integer(),
      retired_operators_element_ref_input_index: Data.Integer(),
      active_operators_redeemer_index: Data.Integer(),
    }),
  }),
  Data.Object({
    DeregisterOperator: Data.Object({
      deregistering_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      anchor_element_input_index: Data.Integer(),
      removed_node_input_index: Data.Integer(),
      anchor_element_output_index: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashDuplicateOperator: Data.Object({
      duplicate_operator: Data.Bytes({ minLength: 28, maxLength: 28 }),
      anchor_element_input_index: Data.Integer(),
      removed_node_input_index: Data.Integer(),
      anchor_element_output_index: Data.Integer(),
      duplicate_node_ref_input_index: Data.Integer(),
      duplicate_operator_status: DuplicateOperatorStatusSchema,
    }),
  }),
  Data.Object({
    SlashFraudulentOperator: Data.Object({
      slashing_arguments: Data.Any(),
    }),
  }),
]);
export type RegisteredOperatorMintRedeemer = Data.Static<
  typeof RegisteredOperatorMintRedeemerSchema
>;
export const RegisteredOperatorMintRedeemer =
  RegisteredOperatorMintRedeemerSchema as unknown as RegisteredOperatorMintRedeemer;

export type RegisteredOperatorInitParams = {
  validator: AuthenticatedValidator;
  outputIndex?: bigint;
  lovelace?: bigint;
};

export type RegisteredOperatorDeinitParams = {};
export type RegisteredOperatorRegisterParams = {};
export type RegisteredOperatorDeregisterParams = {};
export type RegisteredOperatorActivateParams = {};
export type RegisteredOperatorRemoveDuplicateSlashBondParams = {};

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRegisteredOperatorInitTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const rootData = "";

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      rootAssetName: REGISTERED_OPERATORS_ROOT_ASSET_NAME,
      data: rootData,
      redeemer: Data.to(
        { Init: { output_index: params.outputIndex ?? 0n } },
        RegisteredOperatorMintRedeemer,
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
export const incompleteRegisteredOperatorDeinitTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorDeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Register
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRegisteredOperatorRegisterTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorRegisterParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Deregister
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRegisteredOperatorDeregisterTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorDeregisterParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Activate
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRegisteredOperatorActivateTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorActivateParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * RemoveDuplicate
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteRegisteredOperatorRemoveDuplicateSlashBondTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorRemoveDuplicateSlashBondParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
