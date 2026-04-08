import { AuthenticatedValidator, POSIXTimeSchema } from "@/common.js";
import { Data } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

/**
 * SDK data shapes and transaction builders for the retired-operators state
 * machine.
 *
 * Retired operators stay in protocol state until their bond is either recovered
 * or slashed, so the redeemers here model both normal retirement and punitive
 * cleanup paths.
 */
export const RetiredOperatorDatumSchema = Data.Object({
  commitmentTime: Data.Nullable(POSIXTimeSchema),
});
export type RetiredOperatorDatum = Data.Static<
  typeof RetiredOperatorDatumSchema
>;
export const RetiredOperatorDatum =
  RetiredOperatorDatumSchema as unknown as RetiredOperatorDatum;

/**
 * Mint redeemers accepted by the retired-operators policy.
 */
export const RetiredOperatorMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    RetireOperator: Data.Object({
      newRetireOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorAppendedNodeOutputIndex: Data.Integer(),
      retiredOperatorAnchorNodeOutputIndex: Data.Integer(),
      activeOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RecoverOperatorBond: Data.Object({
      retiredOperatorKey: Data.Bytes(),
      removedNodeInputIndex: Data.Integer(),
      anchorNodeInputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorSlashBond: Data.Object({
      slashedRetiredOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorSlashedNodeInputIndex: Data.Integer(),
      retiredOperatorAnchorNodeInputIndex: Data.Integer(),
      state_queueRedeemerIndex: Data.Integer(),
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
};

export type RetiredOperatorDeinitParams = {};
export type RetiredOperatorRetireParams = {};
export type RetiredOperatorRemoveOperatorParams = {};
export type RetiredOperatorRecoverSlashBondParams = {};

/**
 * Builds the linked-list initialization fragment for retired operators.
 */
export const incompleteRetiredOperatorInitTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorInitParams,
): Effect.Effect<TxBuilder> =>
  Effect.gen(function* () {
    const rootData = "00";

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: rootData,
      redeemer: Data.to("Init", RetiredOperatorMintRedeemer),
    });
  });

/**
 * Stub entry point for tearing down the retired-operators state machine.
 */
export const incompleteRetiredOperatorDeinitTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorDeinitParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for appending an operator to the retired list.
 */
export const incompleteRetiredOperatorRetireTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorRetireParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for removing a retired operator from the list.
 */
export const incompleteRetiredOperatorRemoveOperatorTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorRemoveOperatorParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for recovering the bond of a retired operator.
 */
export const incompleteRetiredOperatorRecoverSlashBondTxProgram = (
  lucid: LucidEvolution,
  params: RetiredOperatorRecoverSlashBondParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
