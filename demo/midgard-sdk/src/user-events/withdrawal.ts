import {
  AddressData,
  AddressSchema,
  HashingError,
  LucidError,
  POSIXTime,
  POSIXTimeSchema,
  hashHexWithBlake2b256,
} from "@/common.js";
import { buildMintTransaction, UserEventMintRedeemer } from "@/index.js";
import {
  WithdrawalBody,
  WithdrawalEventSchema,
  WithdrawalInfo,
  WithdrawalSignature,
} from "@/ledger-state.js";
import {
  CML,
  Data,
  LucidEvolution,
  Script,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

export type WithdrawalOrderParams = {
  withdrawalScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  inclusionTime: POSIXTime;
  withdrawalBody: WithdrawalBody;
  withdrawalSignature: WithdrawalSignature;
  refundAddress: AddressData;
  refundDatum: Data;
};

export const WithdrawalOrderDatumSchema = Data.Object({
  event: WithdrawalEventSchema,
  inclusionTime: POSIXTimeSchema,
  refundAddress: AddressSchema,
  refundDatum: Data.Any(),
});
export type WithdrawalOrderDatum = Data.Static<
  typeof WithdrawalOrderDatumSchema
>;
export const WithdrawalOrderDatum =
  WithdrawalOrderDatumSchema as unknown as WithdrawalOrderDatum;

/**
 * WithdrawalOrder
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const withdrawalOrderTxBuilder = (
  lucid: LucidEvolution,
  params: WithdrawalOrderParams,
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
  Effect.gen(function* () {
    const redeemer: UserEventMintRedeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const authenticateEvent = Data.to(redeemer, UserEventMintRedeemer);
    const utxos: UTxO[] = yield* Effect.promise(() =>
      lucid.wallet().getUtxos(),
    );
    if (utxos.length === 0) {
      yield* Effect.fail(
        new LucidError({
          message: "Failed to build the withdrawal transaction",
          cause: "No UTxOs found in wallet",
        }),
      );
    }
    const inputUtxo = utxos[0];
    const transactionInput = CML.TransactionInput.new(
      CML.TransactionHash.from_hex(inputUtxo.txHash),
      BigInt(inputUtxo.outputIndex),
    );

    const assetName = yield* hashHexWithBlake2b256(
      transactionInput.to_cbor_hex(),
    );
    const withdrawalNFT = toUnit(params.policyId, assetName);
    const withdrawalOrderDatum: WithdrawalOrderDatum = {
      event: {
        id: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        info: {
          body: params.withdrawalBody,
          signature: params.withdrawalSignature,
          validity: "WithdrawalIsValid",
        },
      },
      inclusionTime: BigInt(params.inclusionTime),
      refundAddress: params.refundAddress,
      refundDatum: params.refundDatum,
    };
    const withdrawalOrderDatumCBOR = Data.to(
      withdrawalOrderDatum,
      WithdrawalOrderDatum,
    );
    const tx = buildMintTransaction({
      lucid,
      inputUtxo,
      nft: withdrawalNFT,
      mintRedeemer: authenticateEvent,
      scriptAddress: params.withdrawalScriptAddress,
      datum: withdrawalOrderDatumCBOR,
      assets: { [withdrawalNFT]: 1n },
      validTo: Number(params.inclusionTime),
      mintingPolicy: params.mintingPolicy,
    });
    return tx;
  });
