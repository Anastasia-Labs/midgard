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
import {
  DepositsDB,
  TxOrdersDB,
  UserEvents,
  WithdrawalsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";
import { blake2b } from "@noble/hashes/blake2.js";

const fetchUserEventUTxOs = (
  lucid: LucidEvolution,
  inclusionStartTime: number,
  inclusionEndTime: number,
): Effect.Effect<
  {
    deposits: SDK.DepositUTxO[];
    txOrders: SDK.TxOrderUTxO[];
    withdrawals: SDK.WithdrawalUTxO[];
  },
  SDK.LucidError,
  MidgardContracts
> =>
  Effect.gen(function* () {
    const { deposit, txOrder, withdrawal } = yield* MidgardContracts;
    const inclusionTimeLowerBound = BigInt(inclusionStartTime);
    const inclusionTimeUpperBound = BigInt(inclusionEndTime);
    const depositFetchConfig: SDK.DepositFetchConfig = {
      depositAddress: deposit.spendingScriptAddress,
      depositPolicyId: deposit.policyId,
      inclusionTimeLowerBound,
      inclusionTimeUpperBound,
    };
    const txOrderFetchConfig: SDK.TxOrderFetchConfig = {
      eventAddress: txOrder.spendingScriptAddress,
      eventPolicyId: txOrder.policyId,
      inclusionTimeLowerBound,
      inclusionTimeUpperBound,
    };
    const withdrawalFetchConfig: SDK.WithdrawalFetchConfig = {
      eventAddress: withdrawal.spendingScriptAddress,
      eventPolicyId: withdrawal.policyId,
      inclusionTimeLowerBound,
      inclusionTimeUpperBound,
    };
    return {
      deposits: yield* SDK.fetchDepositUTxOsProgram(lucid, depositFetchConfig),
      txOrders: yield* SDK.fetchTxOrderUTxOsProgram(lucid, txOrderFetchConfig),
      withdrawals: yield* SDK.fetchWithdrawalUTxOsProgram(
        lucid,
        withdrawalFetchConfig,
      ),
    };
  });

const credentialFromAddressData = (credential: SDK.CredentialD): Credential =>
  "PublicKeyCredential" in credential
    ? { type: "Key", hash: credential.PublicKeyCredential[0] }
    : { type: "Script", hash: credential.ScriptCredential[0] };

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
        [UserEvents.Columns.ID]: depositUTxO.idCbor,
        [UserEvents.Columns.INFO]: depositUTxO.infoCbor,
        [UserEvents.Columns.INCLUSION_TIME]: depositUTxO.inclusionTime,
        [DepositsDB.Columns.LEDGER_TX_ID]: Buffer.from(
          blake2b(depositUTxO.idCbor, { dkLen: 32 }),
        ),
        [DepositsDB.Columns.LEDGER_OUTPUT]: Buffer.from(
          CML.TransactionOutput.new_conway_format_tx_out(
            output,
          ).to_cbor_bytes(),
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

const userEventUTxOsToEntry = (
  eventUTxOs: (SDK.TxOrderUTxO | SDK.WithdrawalUTxO)[],
): UserEvents.Entry[] => {
  return eventUTxOs.map((utxo) => ({
    [UserEvents.Columns.ID]: utxo.idCbor,
    [UserEvents.Columns.INFO]: utxo.infoCbor,
    [UserEvents.Columns.INCLUSION_TIME]: utxo.inclusionTime,
  }));
};

export const syncUserEvents: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const globals = yield* Globals;
  const startTime: number = yield* Ref.get(
    globals.LATEST_USER_EVENTS_FETCH_TIME,
  );
  const endTime: number = Date.now();

  yield* Effect.logDebug("🏦 Fetching user event UTxOs...");

  const { deposits, txOrders, withdrawals } = yield* fetchUserEventUTxOs(
    lucid,
    startTime,
    endTime,
  );

  if (deposits.length <= 0 && txOrders.length <= 0 && withdrawals.length <= 0) {
    yield* Effect.logInfo(
      `🏦 No user events found within [${startTime}, ${endTime})`,
    );
    return;
  }

  yield* Effect.logInfo(`🏦 ${deposits.length} deposit event(s) found.`);
  yield* Effect.logInfo(`🏦 ${txOrders.length} tx order(s) found.`);
  yield* Effect.logInfo(`🏦 ${withdrawals.length} withdrawal order(s) found.`);

  const { NETWORK: network } = yield* NodeConfig;
  const depositEntries: DepositsDB.Entry[] = yield* Effect.forEach(
    deposits,
    (utxo) => depositUTxOToEntry(utxo, network),
  );
  const txOrderEntries: UserEvents.Entry[] = userEventUTxOsToEntry(txOrders);
  const withdrawalEntries: UserEvents.Entry[] =
    userEventUTxOsToEntry(withdrawals);

  yield* Effect.all([
    DepositsDB.insertEntries(depositEntries),
    TxOrdersDB.insertEntries(txOrderEntries),
    WithdrawalsDB.insertEntries(withdrawalEntries),
  ]);

  yield* Ref.set(globals.LATEST_USER_EVENTS_FETCH_TIME, endTime);
});

export const syncUserEventsFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🏦 Sync user events to db");
    const action = syncUserEvents.pipe(
      Effect.withSpan("sync-user-events-fiber"),
      Effect.catchAllCause(Effect.logWarning),
    );
    yield* Effect.repeat(action, schedule);
  });
