import {
  DataCoercionError,
  getStateToken,
  POSIXTimeSchema,
  UnauthenticUtxoError,
} from "@/common.js";
import { LucidEvolution, TxBuilder, UTxO } from "@lucid-evolution/lucid";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

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

export type ActiveOperatorInitParams = {};
export type ActiveOperatorDeinitParams = {};
export type ActiveOperatorActivateParams = {};
export type ActiveOperatorRetireParams = {};
export type ActiveOperatorListStateTransitionParams = {};
export type ActiveOperatorRemoveSlashBondParams = {};
export type ActiveOperatorUpdateCommitmentTimeParams = {};

export type ActiveOperatorUTxO = {
  utxo: UTxO;
  datum: ActiveOperatorDatum;
  assetName: string;
};

export const getActiveOperatorDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<ActiveOperatorDatum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const activeOperatorDatum = Data.from(datumCBOR, ActiveOperatorDatum);
      return Effect.succeed(activeOperatorDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to an active operator datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Active operator datum coercion failed`,
        cause: `No datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToActiveOperatorUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<
  ActiveOperatorUTxO,
  DataCoercionError | UnauthenticUtxoError
> =>
  Effect.gen(function* () {
    const datum = yield* getActiveOperatorDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `ActiveOperatorNodeUTxO`",
          cause:
            "UTxO's NFT policy ID is not the same as the active operator's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToActiveOperatorUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<ActiveOperatorUTxO[]> => {
  const effects = utxos.map((u) => utxoToActiveOperatorUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

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
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};

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
