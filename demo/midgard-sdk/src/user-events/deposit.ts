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
  TxSignBuilder,
  fromHex,
} from "@lucid-evolution/lucid";
import {
  DataCoercionError,
  GenericErrorFields,
  HashingError,
  LucidError,
  UnauthenticUtxoError,
  makeReturn,
  getStateToken,
  hashHexWithBlake2b256,
  utxosAtByNFTPolicyId,
  isEventUTxOInclusionTimeInBounds,
} from "@/common.js";
import { Data as EffectData, Effect } from "effect";
import { OutputReference, POSIXTime, POSIXTimeSchema } from "@/common.js";
import { getProtocolParameters } from "@/protocol-parameters.js";
import { DepositEventSchema, DepositInfo } from "@/ledger-state.js";
import { buildUserEventMintTransaction, UserEventMintRedeemer } from "./index.js";

export type DepositParams = {
  depositScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  depositAmount: bigint;
  depositInfo: DepositInfo;
};

export const DepositDatumSchema = Data.Object({
  event: DepositEventSchema,
  inclusionTime: POSIXTimeSchema,
});
export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = DepositDatumSchema as unknown as DepositDatum;

export type DepositUTxO = {
  utxo: UTxO;
  datum: DepositDatum;
  assetName: string;
  idCbor: Buffer;
  infoCbor: Buffer;
  inclusionTime: Date;
};

export type DepositFetchConfig = {
  depositAddress: Address;
  depositPolicyId: PolicyId;
  inclusionTimeUpperBound?: POSIXTime;
  inclusionTimeLowerBound?: POSIXTime;
};

export const getDepositDatumFromUTxO = (
  nodeUTxO: UTxO,
): Effect.Effect<DepositDatum, DataCoercionError> => {
  const datumCBOR = nodeUTxO.datum;
  if (datumCBOR) {
    try {
      const depositDatum = Data.from(datumCBOR, DepositDatum);
      return Effect.succeed(depositDatum);
    } catch (e) {
      return Effect.fail(
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to a deposit datum`,
          cause: e,
        }),
      );
    }
  } else {
    return Effect.fail(
      new DataCoercionError({
        message: `Deposit datum coercion failed`,
        cause: `No datum found`,
      }),
    );
  }
};

/**
 * Validates correctness of datum, and having a single NFT.
 */
export const utxoToDepositUTxO = (
  utxo: UTxO,
  nftPolicy: string,
): Effect.Effect<DepositUTxO, DataCoercionError | UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const datum = yield* getDepositDatumFromUTxO(utxo);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: "Failed to convert UTxO to `DepositUTxO`",
          cause: "UTxO's NFT policy ID is not the same as the deposit's",
        }),
      );
    }
    return {
      utxo,
      datum,
      assetName,
      idCbor: Buffer.from(fromHex(Data.to(datum.event.id, OutputReference))),
      infoCbor: Buffer.from(fromHex(Data.to(datum.event.info, DepositInfo))),
      inclusionTime: new Date(Number(datum.inclusionTime)),
    };
  });

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToDepositUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<DepositUTxO[]> => {
  const effects = utxos.map((u) => utxoToDepositUTxO(u, nftPolicy));
  return Effect.allSuccesses(effects);
};

export const fetchDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: DepositFetchConfig,
): Effect.Effect<DepositUTxO[], LucidError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* utxosAtByNFTPolicyId(
      lucid,
      config.depositAddress,
      config.depositPolicyId,
    );
    const depositUTxOs = yield* utxosToDepositUTxOs(
      allUTxOs,
      config.depositPolicyId,
    );

    const validDepositUTxOs = depositUTxOs.filter((utxo) =>
      isEventUTxOInclusionTimeInBounds(
        utxo,
        config.inclusionTimeLowerBound,
        config.inclusionTimeUpperBound,
      ),
    );
    return validDepositUTxOs;
  });

export const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  config: DepositFetchConfig,
) => makeReturn(fetchDepositUTxOsProgram(lucid, config));

/**
 * Deposit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const incompleteDepositTxProgram = (
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

    const depositDatum: DepositDatum = {
      event: {
        id: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        info: depositInfo,
      },
      inclusionTime: BigInt(inclusionTime),
    };
    const depositDatumCBOR = Data.to(depositDatum, DepositDatum);

    const mintRedeemer: UserEventMintRedeemer = {
      AuthenticateEvent: {
        nonceInputIndex: 0n,
        eventOutputIndex: 0n,
        hubRefInputIndex: 0n,
        witnessRegistrationRedeemerIndex: 0n,
      },
    };
    const mintRedeemerCBOR = Data.to(mintRedeemer, UserEventMintRedeemer);
    const assets = {
      lovelace: params.depositAmount,
    };

    // TODO: Currently there are no considerations for fees and/or min ADA.
    const tx = buildUserEventMintTransaction({
          lucid,
          inputUtxo,
          nft: depositNFT,
          mintRedeemer: mintRedeemerCBOR,
          scriptAddress: params.depositScriptAddress,
          datum: depositDatumCBOR,
          extraAssets: assets,
          validTo: inclusionTime,
          mintingPolicy: params.mintingPolicy,
        });
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

export const unsignedDepositTxProgram = (
  lucid: LucidEvolution,
  depositParams: DepositParams,
): Effect.Effect<TxSignBuilder, HashingError | LucidError | DepositError> =>
  Effect.gen(function* () {
    const commitTx = yield* incompleteDepositTxProgram(lucid, depositParams);
    const completedTx: TxSignBuilder = yield* Effect.tryPromise({
      try: () => commitTx.complete({ localUPLCEval: false }),
      catch: (e) =>
        new DepositError({
          message: `Failed to build the transaction: ${e}`,
          cause: e,
        }),
    });
    return completedTx;
  });

/**
 * Builds completed tx for submitting deposits using the provided
 * `LucidEvolution` instance and a deposit config.
 *
 * @param lucid - The `LucidEvolution` API object.
 * @param depositParams - Parameters required for commiting deposits.
 * @returns A promise that resolves to a `TxSignBuilder` instance.
 */
export const unsignedDepositTx = (
  lucid: LucidEvolution,
  depositParams: DepositParams,
): Promise<TxSignBuilder> =>
  makeReturn(unsignedDepositTxProgram(lucid, depositParams)).unsafeRun();

export class DepositError extends EffectData.TaggedError(
  "DepositError",
)<GenericErrorFields> {}
