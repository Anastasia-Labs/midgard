import * as SDK from "@al-ft/midgard-sdk";
import {
  Assets,
  Data,
  LucidEvolution,
  TransactionError,
  TxBuilder,
  TxSignBuilder,
  UTxO,
  toUnit,
} from "@lucid-evolution/lucid";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { Effect, Option, Ref, Schedule } from "effect";
import {
  TxConfirmError,
  handleSignSubmit,
  TxSubmitError,
  TxSignError,
} from "@/transactions/utils.js";
import {
  AddressHistoryDB,
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  ProcessedMempoolDB,
} from "@/database/index.js";
import { deleteLedgerMpt, deleteMempoolMpt } from "@/workers/utils/mpt.js";
import { DatabaseError } from "@/database/utils/common.js";
import { FileSystemError } from "@/utils.js";

const BATCH_SIZE = 80;

const spendAndBurnBeaconUTxOs = (
  tx: TxBuilder,
  authVal: SDK.AuthenticatedValidator,
  utxos: SDK.BeaconUTxO[],
): TxBuilder => {
  const assetsToBurn: Assets = {};
  utxos.map((u) => {
    const assetUnit = toUnit(u.policyId, u.assetName);
    if (assetsToBurn[assetUnit] !== undefined) {
      assetsToBurn[assetUnit] -= 1n;
    } else {
      assetsToBurn[assetUnit] = -1n;
    }
    tx.collectFrom([u.utxo]);
  });
  tx.mintAssets(assetsToBurn, Data.void())
    .attach.Script(authVal.spendingScript)
    .attach.Script(authVal.mintingScript);
  return tx;
};

type InternalAccumulator =
  | { kind: "count_so_far"; count: number }
  | {
      kind: "residual";
      authVal: SDK.AuthenticatedValidator;
      utxos: SDK.BeaconUTxO[];
    };

const spendAndBurntAllUTxOs: Effect.Effect<
  void,
  SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError,
  Lucid | AlwaysSucceedsContract
> = Effect.gen(function* () {
  const lucid = yield* Lucid;
  const midgardValidators = yield* AlwaysSucceedsContract;
  const allAuthVals = [
    midgardValidators.deposit,
    ...SDK.getInitializedValidatorsFromMidgardValidators(midgardValidators),
  ];
  const tx = lucid.api.newTx();
  const [finalAcc, txContainingLastAuthVal] = yield* Effect.reduce(
    allAuthVals,
    [{ kind: "count_so_far", count: 0 }, tx],
    ([initAcc, txSoFar]: [InternalAccumulator, TxBuilder], authVal) =>
      Effect.gen(function* () {
        const acc: InternalAccumulator = {
          kind: "count_so_far",
          count: 0,
        };
        let tx: TxBuilder;
        if (initAcc.kind == "residual") {
          acc.count = initAcc.utxos.length;
          tx = spendAndBurnBeaconUTxOs(txSoFar, initAcc.authVal, initAcc.utxos);
        } else {
          acc.count = initAcc.count;
          tx = txSoFar;
        }
        const authValUTxOs = yield* SDK.utxosAtByNFTPolicyId(
          lucid.api,
          authVal.spendingScriptAddress,
          authVal.policyId,
        );
        const utxosToSpend = authValUTxOs.slice(0, BATCH_SIZE - acc.count);
        const residual = authValUTxOs.slice(BATCH_SIZE - acc.count);
        const newTx = spendAndBurnBeaconUTxOs(tx, authVal, utxosToSpend);
        if (residual.length > 0) {
          const completedTx = yield* newTx.completeProgram();
          yield* lucid.switchToOperatorsMainWallet;
          yield* handleSignSubmit(lucid.api, completedTx);
          const newAcc: InternalAccumulator = {
            kind: "residual",
            authVal,
            utxos: residual,
          };
          return [newAcc, lucid.api.newTx()];
        } else {
          const newAcc: InternalAccumulator = {
            kind: "count_so_far",
            count: acc.count + utxosToSpend.length,
          };
          return [newAcc, newTx];
        }
      }),
  );
  const lastTx =
    finalAcc.kind === "residual"
      ? spendAndBurnBeaconUTxOs(
          txContainingLastAuthVal,
          finalAcc.authVal,
          finalAcc.utxos,
        )
      : txContainingLastAuthVal;
  const completedLastTx = yield* lastTx.completeProgram();
  yield* handleSignSubmit(lucid.api, completedLastTx);
}).pipe(
  Effect.mapError((e) =>
    e._tag === "TxBuilderError" || e._tag === "RunTimeError"
      ? new SDK.LucidError({ message: "", cause: e })
      : e,
  ),
);

export const resetDatabases: Effect.Effect<
  void,
  DatabaseError | FileSystemError,
  NodeConfig | Database
> = Effect.all(
  [
    MempoolDB.clear,
    MempoolLedgerDB.clear,
    ProcessedMempoolDB.clear,
    BlocksDB.clear,
    ImmutableDB.clear,
    LatestLedgerDB.clear,
    ConfirmedLedgerDB.clear,
    AddressHistoryDB.clear,
    deleteMempoolMpt,
    deleteLedgerMpt,
  ],
  { discard: true },
);

export const program: Effect.Effect<
  void,
  | SDK.LucidError
  | TxSubmitError
  | TxSignError
  | TxConfirmError
  | DatabaseError
  | FileSystemError,
  Lucid | NodeConfig | AlwaysSucceedsContract | Globals | Database
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.RESET_IN_PROGRESS, true);

  yield* Effect.all([spendAndBurntAllUTxOs, resetDatabases]);

  yield* Effect.logInfo(`🚧 Resetting global variables...`);
  yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
  yield* Ref.set(globals.AVAILABLE_CONFIRMED_BLOCK, "");

  yield* Ref.set(globals.RESET_IN_PROGRESS, false);
  yield* Effect.logInfo(`🚧 Reset completed.`);
});
