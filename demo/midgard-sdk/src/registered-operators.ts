import { AuthenticatedValidator, POSIXTimeSchema } from "@/common.js";
import { Data, LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

export const RegisteredOperatorDatumSchema = Data.Object({
  registrationTime: POSIXTimeSchema,
});
export type RegisteredOperatorDatum = Data.Static<
  typeof RegisteredOperatorDatumSchema
>;
export const RegisteredOperatorDatum =
  RegisteredOperatorDatumSchema as unknown as RegisteredOperatorDatum;

export const RegisteredOperatorWitnessStatusSchema = Data.Enum([
  Data.Literal("Registered"),
  Data.Literal("Active"),
  Data.Literal("Retired"),
]);
export type RegisteredOperatorWitnessStatus = Data.Static<
  typeof RegisteredOperatorWitnessStatusSchema
>;
export const RegisteredOperatorWitnessStatus =
  RegisteredOperatorWitnessStatusSchema as unknown as RegisteredOperatorWitnessStatus;

export const RegisteredOperatorMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    Register: Data.Object({
      keyToPrepend: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorRefInputIndex: Data.Integer(),
      activeOperatorAssetName: Data.Bytes(),
      retiredOperatorRefInputIndex: Data.Integer(),
      retiredOperatorAssetName: Data.Bytes(),
      prependedNodeOutputIndex: Data.Integer(),
      anchorNodeOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    Activate: Data.Object({
      nodeToActivateKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorRefInputIndex: Data.Integer(),
      retiredOperatorAsset_name: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
      activeOperatorsInsertedNodeOutputIndex: Data.Integer(),
      activeOperatorsAnchorNodeOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    Deregister: Data.Object({
      nodeToDeregisterKey: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveDuplicateSlashBond: Data.Object({
      duplicateNodeKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      duplicateNodeRefInputIndex: Data.Integer(),
      duplicateNodeRefInputAssetName: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
      witnessStatus: RegisteredOperatorWitnessStatusSchema,
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

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: rootData,
      redeemer: Data.to("Init", RegisteredOperatorMintRedeemer),
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
