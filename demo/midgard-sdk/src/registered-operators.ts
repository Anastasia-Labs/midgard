import { AuthenticatedValidator, POSIXTimeSchema } from "@/common.js";
import { Data, LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

/**
 * SDK data and transaction builders for the registered-operators state machine.
 *
 * Registered operators are stored in a linked-list structure on chain, so the
 * redeemer shapes here carry the positional data needed to keep that structure
 * consistent during inserts, removals, and status transitions.
 */
export const RegisteredOperatorDatumSchema = Data.Object({
  registrationTime: POSIXTimeSchema,
});
export type RegisteredOperatorDatum = Data.Static<
  typeof RegisteredOperatorDatumSchema
>;
export const RegisteredOperatorDatum =
  RegisteredOperatorDatumSchema as unknown as RegisteredOperatorDatum;

/**
 * Witness status used when resolving duplicate slash-bond scenarios.
 */
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

/**
 * Mint redeemers for the registered-operators validator/policy pair.
 */
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
 * Builds the linked-list initialization fragment for registered operators.
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
 * Stub entry point for tearing down the registered-operators state machine.
 */
export const incompleteRegisteredOperatorDeinitTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorDeinitParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for appending a newly registered operator.
 */
export const incompleteRegisteredOperatorRegisterTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorRegisterParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for removing a registered operator.
 */
export const incompleteRegisteredOperatorDeregisterTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorDeregisterParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for promoting a registered operator to active status.
 */
export const incompleteRegisteredOperatorActivateTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorActivateParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for removing an operator that duplicated a slash bond.
 */
export const incompleteRegisteredOperatorRemoveDuplicateSlashBondTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorRemoveDuplicateSlashBondParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
