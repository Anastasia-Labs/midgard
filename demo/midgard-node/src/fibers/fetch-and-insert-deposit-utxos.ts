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
  toUnit,
  type Assets,
  type Credential,
  type Network,
} from "@lucid-evolution/lucid";
import { DepositsDB, UserEventsUtils } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Schedule } from "effect";
import { computeHash32 } from "@/midgard-tx-codec/hash.js";

/**
 * Background ingestion for deposit UTxOs into the authoritative off-chain
 * deposit log.
 *
 * Projection into the mempool ledger is intentionally handled by a separate
 * step so ingestion remains idempotent and projection can enforce its own
 * timing and exactly-once rules.
 */

/**
 * Converts the SDK's credential representation into Lucid's credential shape.
 */
const credentialFromAddressData = (credential: SDK.CredentialD): Credential =>
  "PublicKeyCredential" in credential
    ? {
        type: "Key",
        hash: credential.PublicKeyCredential[0],
      }
    : {
        type: "Script",
        hash: credential.ScriptCredential[0],
      };

/**
 * Reconstructs a bech32 L2 address from deposit-event address data.
 */
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

/**
 * Removes the L1-only deposit auth NFT while preserving user-deposited value.
 */
const projectedDepositAssets = (
  depositUTxO: SDK.DepositUTxO,
  depositPolicyId: string,
): Assets => {
  const depositAuthUnit = toUnit(depositPolicyId, depositUTxO.assetName);
  const depositAuthQuantity = depositUTxO.utxo.assets[depositAuthUnit] ?? 0n;
  if (depositAuthQuantity !== 1n) {
    throw new Error(
      `Deposit auth NFT invariant violated for ${depositUTxO.utxo.txHash}#${depositUTxO.utxo.outputIndex.toString()}: expected exactly 1 ${depositAuthUnit}, found ${depositAuthQuantity.toString()}`,
    );
  }

  const assets: Assets = { ...depositUTxO.utxo.assets };
  delete assets[depositAuthUnit];
  return assets;
};

/**
 * Projects one deposit UTxO into the database row shape used by the off-chain
 * deposits ledger.
 */
const depositUTxOToEntry = (
  depositUTxO: SDK.DepositUTxO,
  network: Network,
  depositPolicyId: string,
): Effect.Effect<DepositsDB.Entry, SDK.LucidError> =>
  Effect.try({
    try: () => {
      const l2Address = addressFromDepositInfo(
        network,
        depositUTxO.datum.event.info.l2_address,
      );
      const output = CML.ConwayFormatTxOut.new(
        CML.Address.from_bech32(l2Address),
        assetsToValue(projectedDepositAssets(depositUTxO, depositPolicyId)),
      );
      const l2Datum = depositUTxO.datum.event.info.l2_datum;
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
        [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: Buffer.from(
          depositUTxO.utxo.txHash,
          "hex",
        ),
        [DepositsDB.Columns.LEDGER_TX_ID]: computeHash32(depositUTxO.idCbor),
        [DepositsDB.Columns.LEDGER_OUTPUT]: Buffer.from(
          CML.TransactionOutput.new_conway_format_tx_out(
            output,
          ).to_cbor_bytes(),
        ),
        [DepositsDB.Columns.LEDGER_ADDRESS]: l2Address,
        [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
        [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
      };
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to project deposit UTxO into an offchain ledger entry",
        cause,
      }),
  });

/**
 * Fetches the currently visible deposit UTxO set.
 *
 * Production correctness matters more than incremental scan efficiency here:
 * repeatedly reconciling the full visible deposit set avoids missing deposits
 * when provider/indexer visibility lags behind the node's previous scan time.
 */
const fetchDepositUTxOs = (
  lucid: LucidEvolution,
  config?: Pick<
    SDK.DepositFetchConfig,
    "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
  >,
): Effect.Effect<SDK.DepositUTxO[], SDK.LucidError, MidgardContracts> =>
  Effect.gen(function* () {
    const { deposit: depositAuthValidator } = yield* MidgardContracts;
    const fetchConfig: SDK.DepositFetchConfig = {
      eventAddress: depositAuthValidator.spendingScriptAddress,
      eventPolicyId: depositAuthValidator.policyId,
      ...config,
    };
    return yield* SDK.fetchDepositUTxOsProgram(lucid, fetchConfig);
  });

const reconcileVisibleDepositUTxOs = (
  config?: Pick<
    SDK.DepositFetchConfig,
    "inclusionTimeLowerBound" | "inclusionTimeUpperBound"
  >,
): Effect.Effect<
  { readonly reconciledCount: number; readonly completedAt: Date },
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    const { api: lucid } = yield* Lucid;
    const { deposit: depositAuthValidator } = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;

    const depositUTxOs: SDK.DepositUTxO[] = yield* fetchDepositUTxOs(
      lucid,
      config,
    );

    if (depositUTxOs.length <= 0) {
      yield* Effect.logDebug("🏦 No deposit UTxOs found.");
      return {
        reconciledCount: 0,
        completedAt: new Date(),
      } as const;
    }

    yield* Effect.logInfo(`🏦 ${depositUTxOs.length} deposit UTxOs found.`);

    const entries = yield* Effect.forEach(depositUTxOs, (utxo) =>
      depositUTxOToEntry(utxo, nodeConfig.NETWORK, depositAuthValidator.policyId),
    );

    yield* DepositsDB.insertEntries(entries);
    return {
      reconciledCount: entries.length,
      completedAt: new Date(),
    } as const;
  });

/**
 * Runs one deposit-discovery pass and persists newly visible deposits into the
 * authoritative deposit log.
 */
export const fetchAndInsertDepositUTxOs: Effect.Effect<
  void,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | Globals | NodeConfig
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.HEARTBEAT_DEPOSIT_FETCH, Date.now());

  yield* Effect.logDebug("🏦 fetching DepositUTxOs...");
  const { reconciledCount, completedAt } =
    yield* reconcileVisibleDepositUTxOs();
  yield* Ref.set(globals.LATEST_DEPOSIT_FETCH_TIME, completedAt.getTime());
  if (reconciledCount <= 0) {
    return;
  }
  yield* Effect.logInfo(
    `🏦 Reconciled ${reconciledCount} visible deposit UTxO(s) into deposits_utxos.`,
  );
});

export const fetchAndInsertDepositUTxOsForCommitBarrier = (
  inclusionTimeUpperBound: Date,
): Effect.Effect<
  Date,
  SDK.LucidError | DatabaseError,
  MidgardContracts | Lucid | Database | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `🏦 Running commit-time deposit ingestion barrier up to ${inclusionTimeUpperBound.toISOString()}.`,
    );
    const { reconciledCount, completedAt } =
      yield* reconcileVisibleDepositUTxOs({
        inclusionTimeUpperBound: BigInt(inclusionTimeUpperBound.getTime()),
      });
    yield* Effect.logInfo(
      `🏦 Commit-time deposit barrier reconciled ${reconciledCount} deposit UTxO(s); fetch completed at ${completedAt.toISOString()} and locked the visibility barrier at ${inclusionTimeUpperBound.toISOString()}.`,
    );
    return inclusionTimeUpperBound;
  });

/**
 * Fiber wrapper that repeats deposit fetching on the provided schedule.
 */
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
