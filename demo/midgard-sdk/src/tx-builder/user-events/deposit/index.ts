import {
  CML,
  Data,
  LucidEvolution,
  toUnit,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Datum, DepositParams, MintRedeemer } from "./types.js";
import {
  hashHexWithBlake2b256,
  HashingError,
  LucidError,
} from "@/utils/common.js";
import { Effect } from "effect";

/**
 * Deposit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */

export const depositTxBuilder = (
  lucid: LucidEvolution,
  params: DepositParams,
): Effect.Effect<TxBuilder, HashingError | LucidError> =>
  Effect.gen(function* () {
    const utxos: UTxO[] = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (err) =>
        new LucidError({
          message: "Failed to fetch wallet UTxOs",
          cause: err,
        }),
    });

    if (utxos.length === 0) {
      yield* new LucidError({
        message: "Failed to build the deposit transaction",
        cause: "No UTxOs found in wallet",
      });
    }

    const inputUtxo = utxos[0];
    const transactionInput = CML.TransactionInput.new(
      CML.TransactionHash.from_hex(inputUtxo.txHash),
      BigInt(inputUtxo.outputIndex),
    );
    const assetName = yield* hashHexWithBlake2b256(
      transactionInput.to_cbor_hex(),
    );
    const depositNFT = toUnit(params.policyId, assetName);
    const depositDatum: Datum = {
      event: {
        id: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        info: params.depositInfo,
      },
      inclusionTime: params.inclusionTime,
    };
    const depositDatumCBOR = Data.to(depositDatum, Datum);

    const mintRedeemer: MintRedeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, MintRedeemer);

    const tx = lucid
      .newTx()
      .collectFrom([inputUtxo])
      .mintAssets(
        {
          [depositNFT]: 1n,
        },
        mintRedeemerCBOR,
      )
      .pay.ToAddressWithData(
        params.depositScriptAddress,
        {
          kind: "inline",
          value: depositDatumCBOR,
        },
        { [depositNFT]: 1n },
      )
      .validTo(Number(params.inclusionTime))
      .attach.MintingPolicy(params.mintingPolicy);
    return tx;
  });

export * from "./types.js";
