import {
  AuthenticatedValidator,
  POSIXTimeSchema,
  LucidError,
} from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
import { Data, UTxO } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "./linked-list.js";

export const RetiredOperatorDatumSchema = Data.Object({
  key: Data.Nullable(Data.Bytes()),
  link: Data.Nullable(Data.Bytes()),
  bondUnlockTime: Data.Nullable(POSIXTimeSchema),
});
export type RetiredOperatorDatum = Data.Static<
  typeof RetiredOperatorDatumSchema
>;
export const RetiredOperatorDatum =
  RetiredOperatorDatumSchema as unknown as RetiredOperatorDatum;

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
    RemoveOperatorBadState: Data.Object({
      slashedRetiredOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorSlashedNodeInputIndex: Data.Integer(),
      retiredOperatorAnchorNodeInputIndex: Data.Integer(),
      stateQueueRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RemoveOperatorBadSettlement: Data.Object({
      slashedRetiredOperatorKey: Data.Bytes(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorSlashedNodeInputIndex: Data.Integer(),
      retiredOperatorAnchorNodeInputIndex: Data.Integer(),
      settlementInputIndex: Data.Integer(),
      settlementRedeemerIndex: Data.Integer(),
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
    const rootData = "00";

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: rootData,
      redeemer: Data.to("Init", RetiredOperatorMintRedeemer),
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
