import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import {
  MidgardContracts,
  Globals,
  Lucid,
  Database,
  NodeConfig,
} from "@/services/index.js";
import {
  assetsToValue,
  CML,
  credentialToAddress,
  Data as LucidData,
  LucidEvolution,
  type Credential,
  type Network,
} from "@lucid-evolution/lucid";
import { DepositsDB, MempoolLedgerDB, UserEventsUtils } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";
import { computeHash32 } from "@/midgard-tx-codec/hash.js";

const credentialFromAddressData = (
  credential: SDK.CredentialD,
): Credential =>
  "PublicKeyCredential" in credential
    ? {
        type: "Key",
        hash: credential.PublicKeyCredential[0],
      }
    : {
        type: "Script",
        hash: credential.ScriptCredential[0],
      };

const addressFromDepositInfo = (
  network: Network,
  addressData: SDK.AddressData,
): string => {
  const stakeCredential =
    addressData.stakeCredential === null
      ? undefined
      : "Inline" in addressData.stakeCredential
        ? credentialFromAddressData(addressData.stakeCredential.Inline[0])
        : undefined;

  return credentialToAddress(
    network,
    credentialFromAddressData(addressData.paymentCredential),
    stakeCredential,
  );
};

const depositUTxOToEntry = (
  depositUTxO: SDK.DepositUTxO,
  network: Network,
): Effect.Effect<DepositsDB.Entry, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const l2Address = addressFromDepositInfo(
        network,
        depositUTxO.datum.event.info.l2Address,
      );
      const output = CML.ConwayFormatTxOut.new(
        CML.Address.from_bech32(l2Address),
        assetsToValue(depositUTxO.utxo.assets),
      );
      const l2Datum = depositUTxO.datum.event.info.l2Datum;
      if (l2Datum !== null) {
        output.set_datum_option(
          CML.DatumOption.new_datum(
            CML.PlutusData.from_cbor_hex(LucidData.to(l2Datum)),
          ),
        );
      }

      return {
        [UserEventsUtils.Columns.ID]: depositUTxO.idCbor,
        [UserEventsUtils.Columns.INFO]: depositUTxO.infoCbor,
        [UserEventsUtils.Columns.INCLUSION_TIME]: depositUTxO.inclusionTime,
        [DepositsDB.Columns.LEDGER_TX_ID]: computeHash32(depositUTxO.idCbor),
        [DepositsDB.Columns.LEDGER_OUTPUT]: Buffer.from(
          CML.TransactionOutput.new_conway_format_tx_out(output).to_cbor_bytes(),
        ),
        [DepositsDB.Columns.LEDGER_ADDRESS]: l2Address,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to project deposit UTxO into an offchain ledger entry",
        cause,
      }),
  });

const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<SDK.DepositUTxO[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const { deposit: depositAuthValidator } = yield* MidgardContracts;
    const fetchConfig: SDK.DepositFetchConfig = {
      depositAddress: depositAuthValidator.spendingScriptAddress,
      depositPolicyId: depositAuthValidator.policyId,
      inclusionTimeLowerBound: BigInt(inclusionStartTime),
      inclusionTimeUpperBound: BigInt(inclusionEndTime),
    };
    return yield* SDK.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

export const fetchAndInsertDepositUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const nodeConfig = yield* NodeConfig;
  const globals = yield* Globals;
  yield* Ref.set(globals.HEARTBEAT_DEPOSIT_FETCH, Date.now());
  const startTime: number = yield* Ref.get(globals.LATEST_DEPOSIT_FETCH_TIME);
  const endTime: number = Date.now();

  yield* Effect.logDebug("🏦 fetching DepositUTxOs...");

  const depositUTxOs: SDK.DepositUTxO[] = yield* fetchDepositUTxOs(
    lucid,
    startTime,
    endTime,
  );

  if (depositUTxOs.length <= 0) {
    yield* Effect.logDebug("🏦 No deposit UTxOs found.");
    return;
  }

  yield* Effect.logInfo(`🏦 ${depositUTxOs.length} deposit UTxOs found.`);

  const entries = yield* Effect.forEach(depositUTxOs, (utxo) =>
    depositUTxOToEntry(utxo, nodeConfig.NETWORK),
  );

  yield* DepositsDB.insertEntries(entries);

  const currentBlockStartTimeMs = yield* Ref.get(
    globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
  );
  const projectedEntries = entries.filter(
    (entry) =>
      entry[UserEventsUtils.Columns.INCLUSION_TIME].getTime() >
      currentBlockStartTimeMs,
  );
  if (projectedEntries.length > 0) {
    const projectedLedgerEntries = yield* Effect.forEach(
      projectedEntries,
      DepositsDB.toLedgerEntry,
    );
    yield* MempoolLedgerDB.insert(projectedLedgerEntries);
    yield* Ref.update(globals.MEMPOOL_LEDGER_VERSION, (version) => version + 1);
    yield* Effect.logInfo(
      `🏦 Projected ${projectedEntries.length} deposit UTxO(s) into mempool_ledger for the current block pre-state.`,
    );
  }

  yield* Ref.set(globals.LATEST_DEPOSIT_FETCH_TIME, endTime);
});

export const fetchAndInsertDepositUTxOsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🏦 Fetch and insert DepositUTxOs to DepositsDB.");
    const action = fetchAndInsertDepositUTxOs.pipe(
      Effect.withSpan("fetch-and-inser-deposi-utxos-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
