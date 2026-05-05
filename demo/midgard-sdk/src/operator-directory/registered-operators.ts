import {
  AuthenticatedValidator,
  POSIXTimeSchema,
  VerificationKeyHashSchema,
} from "@/common.js";
import {
  Data,
  fromText,
  LucidEvolution,
  TxBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { incompleteInitLinkedListTxProgram } from "@/linked-list.js";
import { Element } from "@/linked-list.js";
import { SlashingArgumentsSchema } from "./internal.js";

export const REGISTERED_ROOT_KEY: string = fromText(
  "MIDGARD_REGISTERED_OPERATORS",
);
export const REGISTERED_NODE_ASSET_NAME_PREFIX: string = fromText("MREG");

export const RegisteredNodeDataSchema = Data.Object({
  operator: VerificationKeyHashSchema,
  bond_unlock_time: Data.Nullable(POSIXTimeSchema),
});
export type RegisteredNodeData = Data.Static<typeof RegisteredNodeDataSchema>;
export const RegisteredNodeData =
  RegisteredNodeDataSchema as unknown as RegisteredNodeData;

export type RegisteredOperatorDatum = Element;
export const RegisteredOperatorDatum = Element;

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
  Data.Object({ Init: Data.Object({ outputIndex: Data.Integer() }) }),
  Data.Object({ Deinit: Data.Object({ inputIndex: Data.Integer() }) }),
  Data.Object({
    RegisterOperator: Data.Object({
      registeringOperator: VerificationKeyHashSchema,
      rootInputIndex: Data.Integer(),
      rootOutputIndex: Data.Integer(),
      registeredNodeOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      activeOperatorsElementRefInputIndex: Data.Integer(),
      operatorOrigin: OperatorOriginSchema,
    }),
  }),
  Data.Object({
    ActivateOperator: Data.Object({
      activatingOperator: VerificationKeyHashSchema,
      anchorElementInputIndex: Data.Integer(),
      removedNodeInputIndex: Data.Integer(),
      anchorElementOutputIndex: Data.Integer(),
      hubOracleRefInputIndex: Data.Integer(),
      retiredOperatorsElementRefInputIndex: Data.Integer(),
      activeOperatorsRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    DeregisterOperator: Data.Object({
      deregisteringOperator: VerificationKeyHashSchema,
      anchorElementInputIndex: Data.Integer(),
      removedNodeInputIndex: Data.Integer(),
      anchorElementOutputIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    SlashDuplicateOperator: Data.Object({
      duplicateOperator: VerificationKeyHashSchema,
      anchorElementInputIndex: Data.Integer(),
      removedNodeInputIndex: Data.Integer(),
      anchorElementOutputIndex: Data.Integer(),
      duplicateNodeRefInputIndex: Data.Integer(),
      duplicateOperatorStatus: DuplicateOperatorStatusSchema,
    }),
  }),
  Data.Object({
    SlashFraudulentOperator: SlashingArgumentsSchema,
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
export type RegisteredOperatorSlashDuplicateOperatorParams = {};

/**
 * NOTE: Silently clamps negative values to 0. Registered operators on-chain
 *       script prevents negative values.
 */
export const intToBigEndianUint8Array = (n: bigint): Uint8Array => {
  if (n <= 0n) {
    return new Uint8Array([0]);
  }

  const bytes: number[] = [];
  while (n > 0n) {
    bytes.push(Number(n & 0xffn));
    n >>= 8n;
  }

  bytes.reverse();
  return new Uint8Array(bytes);
};

export const bigEndianUint8ArrayToInt = (bytes: Uint8Array): bigint => {
  let value = 0n;

  for (const byte of bytes) {
    value = (value << 8n) | BigInt(byte);
  }

  return value;
};

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
export const incompleteRegisteredOperatorSlashDuplicateOperatorTxProgram = (
  lucid: LucidEvolution,
  params: RegisteredOperatorSlashDuplicateOperatorParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
