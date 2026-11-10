import { outRefToCbor } from "@al-ft/lucid-midgard";
import * as SDK from "@al-ft/midgard-sdk";
import { TxSubmitError, UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { Data, Effect } from "effect";

import * as MempoolLedgerDB from "./database/mempoolLedger.js";
import { DatabaseError } from "./database/utils/common.js";
import { Columns as LedgerColumns } from "./database/utils/ledger.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
  publishMempoolLedgerDelta,
} from "./services/index.js";
import {
  type IntentJournal,
  journaledIntent,
  openPlan,
} from "./services/intent-journal.js";
import { observedLedgerTip } from "./transactions/ledger-tip-lower-bound.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
} from "./transactions/utils.js";
import { readSelectedWalletViewInputs } from "./transactions/utils.wallet-view.js";
import { ledgerTipBackdatedValidFromMs } from "./workers/utils/commit-end-time.js";

/** The deposit's lower bound looks back a minute from the observed tip. */
const GENESIS_DEPOSIT_BACKDATE_MS = 60_000;

export class GenesisDepositError extends Data.TaggedError(
  "GenesisDepositError",
)<{
  message: string;
  cause: unknown;
}> {}

/**
 * Seeds the local mempool ledger with configured genesis UTxOs on non-mainnet
 * networks.
 */
const insertGenesisUtxos: Effect.Effect<
  void,
  DatabaseError,
  NodeConfig | Database | Globals
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const globals = yield* Globals;

  if (config.NETWORK === "Mainnet") {
    yield* Effect.logInfo(`🟣 On mainnet—No genesis UTxOs will be inserted.`);
    return;
  }

  const ledgerEntries = config.GENESIS_UTXOS.map((utxo: UTxO) => {
    const core = utxoToCore(utxo);
    return {
      [LedgerColumns.TX_ID]: Buffer.from(utxo.txHash, "hex"),
      // §5.3 field-0/1 item encoding — the ledger key on-chain
      // `ledger_outref_key` derives, not CML's minimal-index form.
      [LedgerColumns.OUTREF]: outRefToCbor(utxo),
      [LedgerColumns.OUTPUT]: Buffer.from(core.output().to_cbor_bytes()),
      [LedgerColumns.ADDRESS]: utxo.address,
    };
  });

  yield* Effect.logInfo(
    `🟣 On testnet — Inserting ${ledgerEntries.length} genesis UTxOs...`,
  );

  yield* MempoolLedgerDB.insert(ledgerEntries);
  yield* publishMempoolLedgerDelta(
    globals,
    { full: true, upserts: [], deletes: [] },
    config.VALIDATION_LEDGER_DELTA_LOG_MAX,
  );

  yield* Effect.logInfo(
    `🟣 Successfully inserted ${ledgerEntries.length} genesis UTxOs. Funded addresses are:
${Array.from(new Set(config.GENESIS_UTXOS.map((u) => u.address))).join("\n")}`,
  );
}).pipe(
  Effect.catchTag("DatabaseError", (_e) =>
    Effect.logInfo(`🟣 Genesis UTxOs already exist. Skipping insertion.`),
  ),
  Effect.andThen(Effect.succeed(Effect.void)),
);

/**
 * Submits an initial deposit transaction for the configured genesis wallet
 * funds when genesis UTxOs are present. It inserts into the deposit list, so
 * it is journaled as a list insert keyed by its event key
 * (`list_insert:deposit:<event key>`): S6 resends it while that key is not
 * in the follower's key set. Where no follower runs, it goes out
 * unjournaled (`no_follower`).
 */
export const submitGenesisDeposits: Effect.Effect<
  void,
  | SDK.LucidError
  | SDK.HashingError
  | SDK.HubOracleError
  | SDK.Bech32DeserializationError
  | GenesisDepositError
  | TxSubmitError
  | TxConfirmError
  | TxSignError,
  MidgardContracts | Lucid | NodeConfig | IntentJournal
> = Effect.gen(function* () {
  yield* Effect.logInfo(`🟣 Building genesis deposit tx...`);

  const contracts = yield* MidgardContracts;
  const config = yield* NodeConfig;
  const lucid = yield* Lucid;

  if (config.GENESIS_UTXOS.length <= 0) {
    return;
  }

  // S5: the plan opens before the build's first L1 read.
  const plan = yield* openPlan;
  yield* lucid.switchToOperatorsMainWallet;

  // The deposit's nonce and funding come from the operator wallet's view
  // (§8.5), never from the provider.
  const walletInputs = yield* readSelectedWalletViewInputs(
    lucid.api,
    "the genesis deposit",
  ).pipe(
    Effect.mapError(
      (cause) =>
        new GenesisDepositError({
          message: `Failed to read the wallet view to fund the genesis deposit: ${cause.message}`,
          cause,
        }),
    ),
  );
  // The lower bound backs off from the observed L1 ledger tip, not the wall
  // clock, so the deposit is open there however long ago the last block
  // landed; it never reaches back past the longest event range.
  const tip = yield* Effect.mapError(
    observedLedgerTip(lucid.api),
    (cause) =>
      new GenesisDepositError({
        message: "The genesis deposit needs the observed L1 ledger tip",
        cause,
      }),
  );
  const validTo = SDK.resolveUserEventValidTo(lucid.api);
  const validity = {
    validFrom: ledgerTipBackdatedValidFromMs({
      lucid: lucid.api,
      submitSlotSnapshot: tip,
      backdateMs: GENESIS_DEPOSIT_BACKDATE_MS,
      rangeFloorMs: validTo - Number(SDK.MAX_EVENT_VALIDITY_RANGE_LENGTH_MS),
    }),
    validTo,
  };
  // Hard-coded 10 ADA deposit.
  const { tx, metadata } = yield* SDK.buildUnsignedDepositTxWithMetadataProgram(
    lucid.api,
    contracts,
    {
      l2Address: config.GENESIS_UTXOS[0].address,
      l2Datum: null,
      lovelace: 10_000_000n,
      additionalAssets: {},
      walletInputs,
      validity,
    },
  ).pipe(
    Effect.catchTags({
      UserEventBuildError: (error) =>
        Effect.fail(
          new GenesisDepositError({
            message: error.message,
            cause: error.cause,
          }),
        ),
      InvalidUtxoError: (error) =>
        Effect.fail(
          new GenesisDepositError({
            message: error.message,
            cause: error,
          }),
        ),
    }),
  );
  yield* handleSignSubmit(
    lucid.api,
    tx,
    journaledIntent(
      "list_insert",
      `list_insert:deposit:${metadata.depositAssetName}`,
      plan,
    ),
  );
}).pipe(Effect.tapError(Effect.logInfo));

/**
 * Full genesis workflow: seed local ledger state and submit initial deposits in
 * parallel.
 */
export const program: Effect.Effect<
  void,
  never,
  MidgardContracts | Database | Lucid | NodeConfig | Globals | IntentJournal
> = Effect.all([insertGenesisUtxos, submitGenesisDeposits], {
  concurrency: "unbounded",
}).pipe(Effect.catchAllCause(Effect.logInfo));
