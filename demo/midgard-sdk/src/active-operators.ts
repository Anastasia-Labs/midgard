import {
  AuthenticatedValidator,
  AuthenticUTxO,
  LucidError,
  POSIXTimeSchema,
  utxosToAuthenticUTxOs,
} from "@/common.js";
import { LucidEvolution, TxBuilder, UTxO, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

export const ActiveOperatorSpendRedeemerSchema = Data.Enum([
  Data.Literal("ListStateTransition"),
  Data.Object({
    UpdateBondHoldNewState: Data.Object({
      activeNodeOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    UpdateBondHoldNewSettlement: Data.Object({
      activeNodeOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      settlementQueueInputIndex: Data.Integer(),
      settlementQueueRedeemerIndex: Data.Integer(),
      newBondUnlockTime: POSIXTimeSchema,
    }),
  }),
]);
export type ActiveOperatorSpendRedeemer = Data.Static<
  typeof ActiveOperatorSpendRedeemerSchema
>;
export const ActiveOperatorSpendRedeemer =
  ActiveOperatorSpendRedeemerSchema as unknown as ActiveOperatorSpendRedeemer;

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
    RemoveOperatorBadState: Data.Object({
      slashedActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorSlashedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorBadSettlement: Data.Object({
      slashedActiveOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorSlashedNodeInputIndex: Data.Integer(),
      activeOperatorAnchorNodeInputIndex: Data.Integer(),
      settlementInputIndex: Data.Integer(),
      settlementRedeemerIndex: Data.Integer(),
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

export const ActiveOperatorDatumSchema = Data.Object({
  key: Data.Nullable(Data.Bytes()),
  link: Data.Nullable(Data.Bytes()),
  bondUnlockTime: Data.Nullable(POSIXTimeSchema),
});
export type ActiveOperatorDatum = Data.Static<typeof ActiveOperatorDatumSchema>;
export const ActiveOperatorDatum =
  ActiveOperatorDatumSchema as unknown as ActiveOperatorDatum;

export type ActiveOperatorInitParams = {
  validator: AuthenticatedValidator;
};
export type ActiveOperatorDeinitParams = {};
export type ActiveOperatorActivateParams = {};
export type ActiveOperatorRetireParams = {};
export type ActiveOperatorListStateTransitionParams = {};
export type ActiveOperatorRemoveSlashBondParams = {};
export type ActiveOperatorUpdateCommitmentTimeParams = {};

export type ActiveOperatorUTxO = AuthenticUTxO<
  ActiveOperatorDatum,
  { status: "ActiveOperator" }
>;

export type FetchActiveOperatorParams = {
  activeOperatorsAddress: string;
  operator: string;
  activeOperatorsPolicyId: string;
};

export const getActiveOperatorUTxOs = (
  params: FetchActiveOperatorParams,
  lucid: LucidEvolution,
): Effect.Effect<ActiveOperatorUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(params.activeOperatorsAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Active Operators UTxOs",
          cause: err,
        }),
    });
    if (allUtxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the Active Operators transaction",
        cause: "No UTxOs found in Active Operators Contract address",
      });
    }
    const activeOperatorsUTxOs: ActiveOperatorUTxO[] =
      yield* utxosToAuthenticUTxOs<ActiveOperatorDatum>(
        allUtxos,
        params.activeOperatorsPolicyId,
        ActiveOperatorDatum,
      );
    return activeOperatorsUTxOs;
  });

export const utxosToActiveOperatorUTxOs = (
  lucid: LucidEvolution,
  activeOperatorsAddress: string,
  activeOperatorsPolicyId: string,
): Effect.Effect<ActiveOperatorUTxO[], LucidError> =>
  Effect.gen(function* () {
    const activeOperatorAllUtxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(activeOperatorsAddress),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch Active Operators UTxOs",
          cause: err,
        }),
    });
    if (activeOperatorAllUtxos.length === 0) {
      return yield* Effect.fail(
        new LucidError({
          message: "Failed to build the Active Operators transaction",
          cause: "No UTxOs found in Active Operators Contract address",
        }),
      );
    }
    const activeOperatorUTxOs: ActiveOperatorUTxO[] =
      yield* utxosToAuthenticUTxOs<
        ActiveOperatorDatum,
        { status: "ActiveOperator" }
      >(activeOperatorAllUtxos, activeOperatorsPolicyId, ActiveOperatorDatum);
    return activeOperatorUTxOs;
  });

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorInitTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorInitParams,
): Effect.Effect<TxBuilder, never> =>
  Effect.gen(function* () {
    const rootData = "00";

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: rootData,
      redeemer: Data.to("Init", ActiveOperatorMintRedeemer),
    });
  });

/**
 * The program that performs the deinit of an operator.
 *
 * @param lucid - The LucidEvolution instance to use for the deinit.
 * @param params - The parameters required for deinit.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorDeinitTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorDeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
/**
 * The program that performs the activation of an operator.
 *
 * @param lucid - The LucidEvolution instance to use for the activation.
 * @param params - The parameters required for activation.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorActivateTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorActivateParams,
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
export const incompleteActiveOperatorRetireTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorRetireParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * ListStateTransition
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorListStateTransitionTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorListStateTransitionParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * Remove slash bond
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorRemoveSlashBondTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorRemoveSlashBondParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

/**
 * UpdateCommitmentTime
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteActiveOperatorUpdateCommitmentTimeTxProgram = (
  lucid: LucidEvolution,
  params: ActiveOperatorUpdateCommitmentTimeParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
