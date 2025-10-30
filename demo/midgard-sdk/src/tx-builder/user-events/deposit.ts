import {
  Address,
  CML,
  LucidEvolution,
  toUnit,
  TxBuilder,
  Data,
  PolicyId,
  UTxO,
  Script,
  fromText,
} from "@lucid-evolution/lucid";
import {
  hashHexWithBlake2b256,
  HashingError,
  LucidError,
} from "@/utils/common.js";
import { Effect } from "effect";
import {
  OutputReferenceSchema,
  POSIXTime,
  POSIXTimeSchema,
} from "@/tx-builder/common.js";
import { getProtocolParameters } from "@/protocolParameters.js";

export type DepositParams = {
  depositScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  depositAmount: bigint;
  depositInfo: DepositInfo;
};

export const DepositInfoSchema = Data.Object({
  l2Address: Data.Bytes(),
  l2Datum: Data.Nullable(Data.Bytes()),
});
export type DepositInfo = Data.Static<typeof DepositInfoSchema>;
export const DepositInfo = DepositInfoSchema as unknown as DepositInfo;

export const DepositEventSchema = Data.Object({
  id: OutputReferenceSchema,
  info: DepositInfoSchema,
});
export type DepositEvent = Data.Static<typeof DepositEventSchema>;
export const DepositEvent = DepositEventSchema as unknown as DepositEvent;

export const DatumSchema = Data.Object({
  event: DepositEventSchema,
  inclusionTime: POSIXTimeSchema,
});
export type Datum = Data.Static<typeof DatumSchema>;
export const Datum = DatumSchema as unknown as Datum;

export type DepositUTxO = {
  utxo: UTxO;
  datum: Datum;
  assetName: string;
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
};

export const MintRedeemerSchema = Data.Enum([
  Data.Object({
    AuthenticateEvent: Data.Object({
      nonceInputIndex: Data.Integer(),
      eventOutputIndex: Data.Integer(),
      hubRefInputIndex: Data.Integer(),
      witnessRegistrationRedeemerIndex: Data.Integer(),
    }),
  }),
  Data.Object({
    BurnEventNFT: Data.Object({
      nonceAssetName: Data.Bytes(),
      witnessUnregistrationRedeemerIndex: Data.Integer(),
    }),
  }),
]);
export type MintRedeemer = Data.Static<typeof MintRedeemerSchema>;
export const MintRedeemer = MintRedeemerSchema as unknown as MintRedeemer;

export type FetchConfig = {
  depositAddress: Address;
  depositPolicyId: PolicyId;
  inclusionStartTime: POSIXTime;
  inclusionEndTime: POSIXTime;
};

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

    // Convert non-hex strings to hex string, since the address type doesn't enforce that
    const depositInfo = {
      l2Address: fromText(params.depositInfo.l2Address),
      l2Datum: params.depositInfo.l2Datum,
    };

    const currTime = Date.now();
    const network = lucid.config().network ?? "Mainnet";
    const waitTime = getProtocolParameters(network).event_wait_duration;
    const inclusionTime = currTime + waitTime;

    const depositDatum: Datum = {
      event: {
        id: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        info: depositInfo,
      },
      inclusionTime: BigInt(inclusionTime),
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

    // TODO: Currently there are no considerations for fees and/or min ADA.
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
        { lovelace: params.depositAmount, [depositNFT]: 1n },
      )
      .validTo(inclusionTime)
      .attach.MintingPolicy(params.mintingPolicy);
    return tx;
  }).pipe(
    Effect.catchAllDefect((defect) => {
      return Effect.fail(
        new LucidError({
          message: "Caught defect from depositTxBuilder",
          cause: defect,
        }),
      );
    }),
  );
