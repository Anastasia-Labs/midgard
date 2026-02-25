import * as SDK from "@al-ft/midgard-sdk";
import { Assets, Data, TxBuilder, toUnit } from "@lucid-evolution/lucid";
import {
  AlwaysSucceedsContract,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { Effect, Ref } from "effect";
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

const BATCH_SIZE = 200;

/**
 * This function can only be used once per `AuthenticatedValidator` per tx.
 */
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
    tx.collectFrom([u.utxo], Data.void());
  });
  tx.mintAssets(assetsToBurn, Data.void())
    .attach.Script(authVal.spendingScript)
    .attach.Script(authVal.mintingScript);
  return tx;
};

type InternalAccumulator = {
  tx: TxBuilder;
  count: number;
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
  const submitIfFull = (acc: InternalAccumulator) =>
    Effect.gen(function* () {
      // Reject transactions that are not filled.
      if (acc.count < BATCH_SIZE) {
        return acc;
      }
      const completedTx = yield* acc.tx.completeProgram();
      yield* lucid.switchToOperatorsMainWallet;
      yield* handleSignSubmit(lucid.api, completedTx);
      const resetAcc: InternalAccumulator = {
        tx: lucid.api.newTx(),
        count: 0,
      };
      return resetAcc;
    });

  const initialAccumulator: InternalAccumulator = {
    tx: lucid.api.newTx(),
    count: 0,
  };
  const finalAcc = yield* Effect.reduce(
    allAuthVals,
    initialAccumulator,
    (initialAcc, authVal) =>
      Effect.gen(function* () {
        const authValUTxOs = yield* SDK.utxosAtByNFTPolicyId(
          lucid.api,
          authVal.spendingScriptAddress,
          authVal.policyId,
        );
        const { acc } = yield* Effect.iterate(
          { acc: initialAcc, offset: 0 },
          {
            while: ({ offset }) => offset < authValUTxOs.length,
            body: ({ acc, offset }) =>
              Effect.gen(function* () {
                const txAcc = yield* submitIfFull(acc);
                const capacity = BATCH_SIZE - txAcc.count;
                const utxosToSpend = authValUTxOs.slice(
                  offset,
                  offset + capacity,
                );
                // For this tx segment, apply this validator exactly once.
                const nextAcc: InternalAccumulator = {
                  tx: spendAndBurnBeaconUTxOs(txAcc.tx, authVal, utxosToSpend),
                  count: txAcc.count + utxosToSpend.length,
                };
                return {
                  acc: nextAcc,
                  offset: offset + utxosToSpend.length,
                };
              }),
          },
        );
        return acc;
      }),
  );

  if (finalAcc.count > 0) {
    const completedLastTx = yield* finalAcc.tx.completeProgram();
    yield* lucid.switchToOperatorsMainWallet;
    yield* handleSignSubmit(lucid.api, completedLastTx);
  }
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
