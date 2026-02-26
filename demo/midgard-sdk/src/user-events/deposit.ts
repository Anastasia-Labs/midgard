import {
  LucidEvolution,
  toUnit,
  TxBuilder,
  Data,
  UTxO,
  Script,
  TxSignBuilder,
  fromHex,
  PolicyId,
} from "@lucid-evolution/lucid";
import {
  GenericErrorFields,
  HashingError,
  LucidError,
  makeReturn,
  UnspecifiedNetworkError,
} from "@/common.js";
import { AuthenticUTxO, authenticateUTxOs } from "@/internals.js";
import { Data as EffectData, Effect } from "effect";
import { OutputReference, POSIXTimeSchema } from "@/common.js";
import { DepositEventSchema, DepositInfo } from "@/ledger-state.js";
import {
  buildUserEventMintTransaction,
  fetchUserEventUTxOsProgram,
  findInclusionTimeForUserEvent,
  getNonceInputAndAssetName,
  UserEventExtraFields,
  UserEventFetchConfig,
  UserEventMintRedeemer,
} from "./internals.js";

export const DepositDatumSchema = Data.Object({
  event: DepositEventSchema,
  inclusionTime: POSIXTimeSchema,
});
export type DepositDatum = Data.Static<typeof DepositDatumSchema>;
export const DepositDatum = DepositDatumSchema as unknown as DepositDatum;

export type DepositUTxO = AuthenticUTxO<DepositDatum, UserEventExtraFields>;

export type DepositFetchConfig = UserEventFetchConfig;

/**
 * Silently drops invalid UTxOs.
 */
export const utxosToDepositUTxOs = (
  utxos: UTxO[],
  nftPolicy: PolicyId,
): Effect.Effect<DepositUTxO[]> => {
  const calculateExtraFields = (datum: DepositDatum): UserEventExtraFields => ({
    idCbor: Buffer.from(fromHex(Data.to(datum.event.id, OutputReference))),
    infoCbor: Buffer.from(fromHex(Data.to(datum.event.info, DepositInfo))),
    inclusionTime: new Date(Number(datum.inclusionTime)),
  });

  return authenticateUTxOs<DepositDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    DepositDatum,
    calculateExtraFields,
  );
};

export const fetchDepositUTxOsProgram = (
  lucid: LucidEvolution,
  config: DepositFetchConfig,
): Effect.Effect<DepositUTxO[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToDepositUTxOs(utxos, config.eventPolicyId),
  );

export const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  config: DepositFetchConfig,
) => makeReturn(fetchDepositUTxOsProgram(lucid, config));

export type DepositParams = {
  depositScriptAddress: string;
  mintingPolicy: Script;
  policyId: string;
  depositAmount: bigint;
  depositInfo: DepositInfo;
};

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
): Effect.Effect<
  TxBuilder,
  HashingError | LucidError | UnspecifiedNetworkError
> =>
  Effect.gen(function* () {
    const { inputUtxo, assetName } = yield* getNonceInputAndAssetName(
      lucid,
      "deposit",
    );

    const depositNFT = toUnit(params.policyId, assetName);

    const inclusionTime = yield* findInclusionTimeForUserEvent(lucid);

    const depositDatum: DepositDatum = {
      event: {
        id: {
          txHash: { hash: inputUtxo.txHash },
          outputIndex: BigInt(inputUtxo.outputIndex),
        },
        info: params.depositInfo,
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
): Effect.Effect<
  TxSignBuilder,
  HashingError | LucidError | UnspecifiedNetworkError | DepositError
> =>
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
