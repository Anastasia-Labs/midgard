import { Database } from "@/services/database.js";
import { Effect } from "effect";
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
  SDK.CmlDeserializationError,
  NodeConfig
> =>
  Effect.gen(function* () {
    const l1Utxo = CML.TransactionUnspentOutput.from_cbor_bytes(
      entry[UserEvents.Columns.L1_UTXO_CBOR],
    );

    const policyIdScriptHash: CML.ScriptHash = yield* Effect.try({
      try: () => CML.ScriptHash.from_hex(policyId),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to convert policyId from hex to CML.ScriptHash (deposit event to utxo)`,
          cause: e,
        }),
    });

    const assetName: CML.AssetName = yield* Effect.try({
      try: () => CML.AssetName.from_hex(entry[UserEvents.Columns.ASSET_NAME]),
      catch: (e) =>
        new SDK.CmlDeserializationError({
          message: `Failed to convert entry[ASSET_NAME] from hex to CML.AssetName (deposit event to utxo)`,
          cause: e,
        }),
    });

    const assets = CML.MapAssetNameToCoin.new();
    assets.insert(assetName, 1n);

    const authNftMultiasset = CML.MultiAsset.new();
    authNftMultiasset.insert_assets(policyIdScriptHash, assets);

    const authNft = CML.Value.new(0n, authNftMultiasset);

    // We need to subtract the L1 deposit NFT before inserting the values to L2
    // UTxO.
    const l2Amount: CML.Value = l1Utxo.output().amount().checked_sub(authNft);

    const depositInfo = Data.from(
      SDK.bufferToHex(entry[UserEvents.Columns.INFO]),
      SDK.DepositInfo,
    );

    const config = yield* NodeConfig;

    const l2AddressBech32 = SDK.midgardAddressToBech32(
      config.NETWORK,
      depositInfo.l2Address,
    );

    let l2Datum = undefined;

    if (depositInfo.l2Datum !== null) {
      const l2DatumCBOR = depositInfo.l2Datum;
      l2Datum = yield* Effect.try({
        try: () => CML.DatumOption.from_cbor_hex(l2DatumCBOR),
        catch: (e) =>
          new SDK.CmlDeserializationError({
            message: "Specified L2 datum was malformed.",
            cause: e,
          }),
      });
    }

    const transactionOutput = CML.TransactionOutput.new(
      CML.Address.from_bech32(l2AddressBech32),
      l2Amount,
      l2Datum,
    );

    // Output reference of the equivalent UTxO will be the L1 NFT's asset name
    // (which itself is the hash of the nonce used for creating the event), with
    // output index 0.
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
