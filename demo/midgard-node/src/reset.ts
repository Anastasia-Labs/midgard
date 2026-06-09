import * as SDK from "@al-ft/midgard-sdk";
import {
  MidgardContracts,
  Database,
  Globals,
  Lucid,
  NodeConfig,
} from "@/services/index.js";
import { Effect, Ref } from "effect";
import {
  TxConfirmError,
  TxSubmitError,
  TxSignError,
} from "@/transactions/utils.js";
import {
  AddressHistoryDB,
  BlocksTxsDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  BlocksDB,
} from "@/database/index.js";
import { deleteLedgerMpt, deleteMempoolMpt } from "@/workers/utils/mpt.js";
import { DatabaseError } from "@/database/utils/common.js";
import { FileSystemError } from "@/utils.js";

const spendAndBurntAllUTxOs: Effect.Effect<
  void,
  SDK.LucidError | TxSignError | TxSubmitError | TxConfirmError,
  Lucid | MidgardContracts
> = Effect.fail(
  new SDK.LucidError({
    message:
      "Reset endpoint is disabled: deinit path is not implemented for deployed contracts",
    cause: "reset-disabled",
  }),
);

export const resetDatabases: Effect.Effect<
  void,
  DatabaseError | FileSystemError,
  NodeConfig | Database
> = Effect.all(
  [
    MempoolDB.clear,
    MempoolLedgerDB.clear,
    BlocksDB.clear,
    BlocksTxsDB.clear,
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
  Lucid | NodeConfig | MidgardContracts | Globals | Database
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.RESET_IN_PROGRESS, true);

  yield* Effect.all([spendAndBurntAllUTxOs, resetDatabases]);

  yield* Effect.logInfo(`🚧 Resetting global variables...`);
  yield* Ref.set(globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH, Date.now());
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);

  yield* Ref.set(globals.RESET_IN_PROGRESS, false);
  yield* Effect.logInfo(`🚧 Reset completed.`);
});
