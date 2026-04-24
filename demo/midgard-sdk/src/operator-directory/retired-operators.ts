import {
  AuthenticatedValidator,
  POSIXTimeSchema,
  LucidError,
  VerificationKeyHashSchema,
} from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
import { Data, fromText, UTxO } from "@lucid-evolution/lucid";
import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "@/linked-list.js";
import { Element } from "@/linked-list.js";
import { SlashingArgumentsSchema } from "./internal.js";

export const RETIRED_ROOT_KEY: string = fromText("MIDGARD_RETIRED_OPERATORS");
export const RETIRED_NODE_ASSET_NAME_PREFIX: string = fromText("MRET");

export const RetiredOperatorNodeSchema = Data.Object({
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type RetiredOperatorNodeData = Data.Static<
  typeof RetiredOperatorNodeSchema
>;
export const RetiredOperatorNodeData =
  RetiredOperatorNodeSchema as unknown as RetiredOperatorNodeData;

export type RetiredOperatorDatum = Element;
export const RetiredOperatorDatum = Element;

export const RetiredOperatorMintRedeemerSchema = Data.Enum([
  Data.Object({ Init: Data.Object({ outputIndex: Data.Integer() }) }),
  Data.Object({ Deinit: Data.Object({ inputIndex: Data.Integer() }) }),
  Data.Object({
    RetireOperator: Data.Object({
      newRetiredOperatorKey: Data.Bytes(),
      bondUnlockTime: Data.Nullable(POSIXTimeSchema),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorAnchorElementInputIndex: Data.Integer(),
      retiredOperatorAnchorElementOutputIndex: Data.Integer(),
      retiredOperatorInsertedNodeOutputIndex: Data.Integer(),
      activeOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    ReregisterOperator: Data.Object({
      retiredOperatorKey: VerificationKeyHashSchema,
      retiredOperatorBondUnlockTime: Data.Nullable(POSIXTimeSchema),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorAnchorElementInputIndex: Data.Integer(),
      retiredOperatorRemovedNodeInputIndex: Data.Integer(),
      retiredOperatorAnchorElementOutputIndex: Data.Integer(),
      registeredOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    RecoverOperatorBond: Data.Object({
      retiredOperatorKey: Data.Bytes(),
      retiredOperatorAnchorElementInputIndex: Data.Integer(),
      retiredOperatorRemovedNodeInputIndex: Data.Integer(),
      retiredOperatorAnchorElementOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashOperator: Data.Object({
      slashingArguments: SlashingArgumentsSchema,
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
    const rootData = "";

    const mintRedeemer = Data.to(
      { Init: { outputIndex: 0n } },
      RetiredOperatorMintRedeemer,
    );

    return yield* incompleteInitLinkedListTxProgram(lucid, {
      validator: params.validator,
      data: rootData,
      redeemer: mintRedeemer,
      rootKey: RETIRED_ROOT_KEY,
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
