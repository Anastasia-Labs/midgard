import { Database } from "@/services/database.js";
import { Effect, Data as EffectData } from "effect";
import { DatabaseError } from "@/database/utils/common.js";
import * as UserEvents from "@/database/utils/user-events.js";
import { CML, Data, PolicyId } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { NodeConfig } from "@/services/config.js";

export const tableName = "deposits_utxos";

export const insertEntry = (
  entry: UserEvents.Entry,
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.insertEntry(tableName, entry);

export const insertEntries = (
  entries: UserEvents.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.insertEntries(tableName, entries);

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly UserEvents.Entry[], DatabaseError, Database> =>
  UserEvents.retrieveTimeBoundEntries(tableName, startTime, endTime);

export const retrieveAllEntries = (): Effect.Effect<
  readonly UserEvents.Entry[],
  DatabaseError,
  Database
> => UserEvents.retrieveAllEntries(tableName);

export const delEntries = (
  ids: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.delEntries(tableName, ids);

export const depositEventToCmlTransactionUnspentOutput = (
  entry: UserEvents.Entry,
  policyId: PolicyId,
): Effect.Effect<
  CML.TransactionUnspentOutput,
  DepositConversionError,
  NodeConfig
> =>
  Effect.gen(function* () {
    const l1Utxo = CML.TransactionUnspentOutput.from_cbor_bytes(
      entry[UserEvents.Columns.L1_UTXO_CBOR],
    );
    const policyIdScriptHash: CML.ScriptHash = yield* Effect.try({
      try: () => CML.ScriptHash.from_hex(policyId),
      catch: (e) =>
        new DepositConversionError({
          message: `Failed to convert policyId from hex to CML.ScriptHash`,
          cause: e,
        }),
    });
    const assets = CML.MapAssetNameToCoin.new();

    const insertedAssetsCode = assets.insert(
      CML.AssetName.from_hex(entry[UserEvents.Columns.ASSET_NAME]),
      1n,
    );

    // We assume that it returns a number if insertions were unsuccessful
    if (insertedAssetsCode !== undefined) {
      yield* Effect.fail(
        new DepositConversionError({
          message: `Failed to insert ASSET_NAME to MapAssetNameToCoin with code: ${insertedAssetsCode}`,
          cause: `ASSET_NAME: ${entry[UserEvents.Columns.ASSET_NAME]}`,
        }),
      );
    }

    const verificationNftMultiasset = CML.MultiAsset.new();
    const insertedMultiassetsCode = verificationNftMultiasset.insert_assets(
      policyIdScriptHash,
      assets,
    ); // Same return as above
    if (insertedMultiassetsCode !== undefined) {
      yield* Effect.fail(
        new DepositConversionError({
          message: `Failed to insert assets to MultiAsset with code: ${insertedMultiassetsCode}`,
          cause: `policyIdScriptHash: ${policyIdScriptHash}, assets: ${assets},`,
        }),
      );
    }
    const verificationNft = CML.Value.new(0n, verificationNftMultiasset);

    // We need to subtract the L2 midgard nft before inserting the values to L2 UTxO
    const l2Amount: CML.Value = l1Utxo
      .output()
      .amount()
      .checked_sub(verificationNft);

    const depositDatum = Data.from(
      SDK.bufferToHex(entry[UserEvents.Columns.INFO]),
      SDK.DepositInfo,
    );

    const config = yield* NodeConfig;

    const l2AddressBech32 = SDK.midgardAddressToBech32(
      config.NETWORK,
      depositDatum.l2Address,
    );

    let l2Datum = undefined;
    if (depositDatum.l2Datum !== null) {
      l2Datum = CML.DatumOption.from_cbor_hex(depositDatum.l2Datum);
    }

    const transactionOutput = CML.TransactionOutput.new(
      CML.Address.from_bech32(l2AddressBech32),
      l2Amount,
      l2Datum,
    );

    const transactionId = CML.TransactionHash.from_hex(
      entry[UserEvents.Columns.ASSET_NAME],
    );
    const transactionInput = CML.TransactionInput.new(transactionId, 0n);

    const utxo = CML.TransactionUnspentOutput.new(
      transactionInput,
      transactionOutput,
    );
    return utxo;
  });

export class DepositConversionError extends EffectData.TaggedError(
  "DepositConversionError",
)<SDK.GenericErrorFields> {}
