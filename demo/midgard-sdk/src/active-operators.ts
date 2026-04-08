import { AuthenticatedValidator, POSIXTimeSchema } from "@/common.js";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

/**
 * SDK data shapes and transaction builders for the active-operators state
 * machine.
 *
 * Active operators participate in block commitment and retirement flows, so the
 * redeemers here carry the cross-state-machine indices needed to keep the
 * ledger view aligned with on-chain script-context ordering.
 */
export const ActiveOperatorDatumSchema = Data.Object({
  commitmentTime: Data.Nullable(POSIXTimeSchema),
});
export type ActiveOperatorDatum = Data.Static<typeof ActiveOperatorDatumSchema>;
export const ActiveOperatorDatum = 
  ActiveOperatorDatumSchema as unknown as ActiveOperatorDatum;

/**
 * Aiken-compatible view of the active-operator datum used during
 * initialization.
 */
const ActiveOperatorDatumAikenOptionSchema = Data.Enum([
  Data.Object({
    Some: Data.Tuple([Data.Integer()]),
  }),
  Data.Literal("None"),
]);
const ActiveOperatorDatumAikenSchema = Data.Object({
  bond_unlock_time: ActiveOperatorDatumAikenOptionSchema,
});

/**
 * Spend redeemers accepted by the active-operators validator.
 */
export const ActiveOperatorSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateCommitmentTime: Data.Object({
      activeNodeInputIndex: Data.Integer(),
      prevActiveNodeRefInputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type ActiveOperatorSpendRedeemer = Data.Static<
  typeof ActiveOperatorSpendRedeemerSchema
>;
export const ActiveOperatorSpendRedeemer =
  ActiveOperatorSpendRedeemerSchema as unknown as ActiveOperatorSpendRedeemer;

/**
 * Mint redeemers accepted by the active-operators policy.
 */
export const ActiveOperatorMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Object({
    ActivateOperator: Data.Object({
      newActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorAppendedNodeOutputIndex: Data.Integer(),
      activeOperatorAnchorNodeOutputIndex: Data.Integer(),
      registeredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorSlashBond: Data.Object({
      slashedActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorSlashedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RetireOperator: Data.Object({
      activeOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorRemovedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      retiredOperatorInsertedNodeOutputIndex: Data.Integer(),
      retiredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type ActiveOperatorMintRedeemer = Data.Static<
  typeof ActiveOperatorMintRedeemerSchema
>;
export const ActiveOperatorMintRedeemer =
  ActiveOperatorMintRedeemerSchema as unknown as ActiveOperatorMintRedeemer;

export type ActiveOperatorInitParams = {
  validator: AuthenticatedValidator;
};
export type ActiveOperatorDeinitParams = {};
export type ActiveOperatorActivateParams = {};
export type ActiveOperatorRetireParams = {};
export type ActiveOperatorListStateTransitionParams = {};
export type ActiveOperatorRemoveSlashBondParams = {};
export type ActiveOperatorUpdateCommitmentTimeParams = {};

/**
 * Builds the linked-list initialization fragment for active operators.
 *
 * The root datum is encoded in the Aiken-facing shape expected by the on-chain
 * validator so the initial list anchor is born in a protocol-valid state.
 */
export const incompleteActiveOperatorInitTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const rootData = Data.castTo(
      { bond_unlock_time: null } as never,
      ActiveOperatorDatumAikenSchema as never,
    );

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: rootData,
      redeemer: Data.to("Init", ActiveOperatorMintRedeemer),
    });
  });

/**
 * Stub entry point for tearing down the active-operators state machine.
 */
export const incompleteActiveOperatorDeinitTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorDeinitParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
/**
 * Stub entry point for activating a registered operator.
 */
export const incompleteActiveOperatorActivateTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorActivateParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for retiring an active operator into the retired list.
 */
export const incompleteActiveOperatorRetireTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorRetireParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for generic list-state transitions on active operators.
 */
export const incompleteActiveOperatorListStateTransitionTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorListStateTransitionParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for removing the slash bond of an active operator.
 */
export const incompleteActiveOperatorRemoveSlashBondTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorRemoveSlashBondParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};

/**
 * Stub entry point for updating an active operator's commitment timestamp.
 */
export const incompleteActiveOperatorUpdateCommitmentTimeTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorUpdateCommitmentTimeParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
