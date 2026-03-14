import { AuthenticatedValidator, POSIXTimeSchema } from "@/common.js";
import { Data, LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  incompleteInitLinkedListTxProgram,
  RegisteredElementSchema,
} from "./linked-list.js";
import { Int } from "effect/Schema";

export const REGISTERED_ROOT_KEY: string = "MIDGARD_REGISTERED_OPERATORS";
export const REGISTERED_NODE_ASSET_NAME_PREFIX: string = "MREG";

export type RegisteredOperatorDatum = Data.Static<
  typeof RegisteredElementSchema
>;
export const RegisteredOperatorDatum =
  RegisteredElementSchema as unknown as RegisteredOperatorDatum;

export const DuplicateOperatorStatusSchema = Data.Enum([
  Data.Literal("DuplicateIsRegistered"),
  Data.Object({
    DuplicateIsActive: Data.Object({ hubOracleRefInputIndex: Data.Integer() }),
  }),
  Data.Literal("DuplicateIsRetired"),
]);
export type DuplicateOperatorStatus = Data.Static<
  typeof DuplicateOperatorStatusSchema
>;
export const DuplicateOperatorStatus =
  DuplicateOperatorStatusSchema as unknown as DuplicateOperatorStatus;

export const RegisteredOperatorMintRedeemerSchema = Data.Enum([
  Data.Object({ Init: Data.Object({ outputIndex: Data.Integer() }) }),
  Data.Object({ Deinit: Data.Object({ inputIndex: Data.Integer() }) }),
  Data.Object({
    RegisterOperator: Data.Object({
      registeringOperator: Data.Bytes(),
      rootInputIndex: Data.Integer(),
      rootOutputIndex: Data.Integer(),
      registeredNodeOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorsElementRefInputIndex: Data.Integer(),
      retiredOperatorsElementRefInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    ActivateOperator: Data.Object({
      activatingOperator: Data.Bytes(),
      anchorElementInputIndex: Data.Integer(),
      removedNodeInputIndex: Data.Integer(),
      anchorElementOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorsElementRefInputIndex: Data.Integer(),
      activeOperatorsRedeemerIndex: Data.Bytes(),
    }),
  }),
  Data.Object({
    DeregisterOperator: Data.Object({
      deregisteringOperator: Data.Bytes(),
      anchorElementInputIndex: Data.Integer(),
      removedNodeInputIndex: Data.Integer(),
      anchorElementOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveDuplicateSlashBond: Data.Object({
      duplicateOperator: Data.Bytes(),
      anchorElementInputIndex: Data.Integer(),
      removedNodeInputIndex: Data.Integer(),
      anchorElementOutputIndex: Data.Integer(),
      duplicateNodeRefInputIndex: Data.Integer(),
      duplicateOperatorStatus: DuplicateOperatorStatusSchema,
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
    const rootData = "00";

    const mintRedeemer = Data.to(
      { Init: { outputIndex: 0n } },
      RegisteredOperatorMintRedeemer,
    );

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: rootData,
      redeemer: mintRedeemer,
      rootKey: REGISTERED_ROOT_KEY,
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
