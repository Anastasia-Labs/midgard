import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Effect, Option, pipe } from "effect";
import {
  applyTxRequestsToLedger,
  applyDepositsToLedger,
  WorkerInput,
  WorkerOutput,
  applyWithdrawalsToLedger,
  applyTxOrdersToLedger,
  buildNewBlockEntry,
} from "./utils/block-commitment.js";
import {
  Database,
  Lucid,
  AlwaysSucceedsContract,
  NodeConfig,
} from "@/services/index.js";
import { MempoolLedgerDB, BlocksDB } from "@/database/index.js";
import { TxSignError } from "@/transactions/utils.js";
import { MidgardMpt, MptError } from "@/workers/utils/mpt.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { SqlClient } from "@effect/sql";

const mainProgram: Effect.Effect<
  string | BlocksDB.Stats,
  | SDK.CborDeserializationError
  | SDK.CborSerializationError
  | SDK.CmlDeserializationError
  | SDK.CmlUnexpectedError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.LucidError
  | SDK.StateQueueError
  | DatabaseError
  | MptError
  | TxSignError,
  AlwaysSucceedsContract | Database | Lucid | NodeConfig
> = Effect.gen(function* () {
  const optLatestBlock = yield* BlocksDB.retrieveLatestEntry;
  return yield* Option.match(optLatestBlock, {
    onNone: () =>
      Effect.succeed(
        "No blocks available in database to use as block commitment anchor",
      ),
    onSome: (latestBlock) =>
      Effect.gen(function* () {
        const nodeConfig = yield* NodeConfig;
        const currentDate = new Date();
        const { withdrawals, txOrders, txRequests, deposits } =
          yield* BlocksDB.retrieveEvents(
            latestBlock[BlocksDB.Columns.EVENT_END_TIME],
            currentDate,
          );
        const ledgerTrie = yield* MidgardMpt.create(
          "ledger",
          nodeConfig.LEDGER_MPT_DB_PATH,
        );
        yield* ledgerTrie.checkpoint();

        return yield* Effect.gen(function* () {
          const { withdrawnOutRefs, withdrawalsRoot, sizeOfWithdrawals } =
            yield* applyWithdrawalsToLedger(ledgerTrie, withdrawals);
          const {
            txOrdersHashes,
            spentByTxOrders,
            producedByTxOrders,
            txsTrie,
            sizeOfTxOrders,
          } = yield* applyTxOrdersToLedger(ledgerTrie, txOrders);
          const { txRequestsHashes, txsRoot, sizeOfTxRequests } =
            yield* applyTxRequestsToLedger(ledgerTrie, txsTrie, txRequests);
          const { depositLedgerEntries, depositsRoot, sizeOfDeposits } =
            yield* applyDepositsToLedger(ledgerTrie, deposits);

          const ledgerRoot = yield* ledgerTrie.getRootHex();

          const stats: BlocksDB.Stats = {
            [BlocksDB.Columns.DEPOSITS_COUNT]: depositLedgerEntries.length,
            [BlocksDB.Columns.TX_REQUESTS_COUNT]: txRequestsHashes.length,
            [BlocksDB.Columns.TX_ORDERS_COUNT]: txOrdersHashes.length,
            [BlocksDB.Columns.WITHDRAWALS_COUNT]: withdrawnOutRefs.length,
            [BlocksDB.Columns.TOTAL_EVENTS_SIZE]:
              sizeOfWithdrawals +
              sizeOfTxOrders +
              sizeOfTxRequests +
              sizeOfDeposits,
          };

          const newBlockEntry = yield* buildNewBlockEntry(
            latestBlock,
            ledgerRoot,
            txsRoot,
            depositsRoot,
            withdrawalsRoot,
            currentDate,
            stats,
          );

          const sql = yield* SqlClient.SqlClient;

          // TODO: We are not adding any entries to `AddressHistoryDB` here, but
          //       we probably should.
          yield* sql
            .withTransaction(
              Effect.gen(function* () {
                yield* BlocksDB.upsert(newBlockEntry);
                yield* MempoolLedgerDB.insert([
                  ...depositLedgerEntries,
                  ...producedByTxOrders,
                ]);
                yield* MempoolLedgerDB.clearUTxOs([
                  ...withdrawnOutRefs,
                  ...spentByTxOrders,
                ]);
              }),
            )
            .pipe(
              sqlErrorToDatabaseError(
                `(${BlocksDB.tableName} & ${MempoolLedgerDB.tableName})`,
                "Updating MempoolLedgerDB and BlocksDB after updating ledger MPT for block commitment failed",
              ),
            );
          return stats;
        }).pipe(Effect.tapError((_) => ledgerTrie.revert()));
      }),
  });
});

const wrapper = (_workerInput: WorkerInput) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Starting block commitment process...");
    const result = yield* mainProgram;
    if (typeof result === "string") {
      const output: WorkerOutput = {
        type: "FailureOutput",
        error: result,
      };
      return output;
    } else {
      const output: WorkerOutput = {
        type: "SuccessfulCommitmentOutput",
        stats: result,
      };
      return output;
    }
  });

const inputData = workerData as WorkerInput;

const program = pipe(
  wrapper(inputData),
  Effect.provide(AlwaysSucceedsContract.Default),
  Effect.provide(Database.layer),
  Effect.provide(Lucid.Default),
  Effect.provide(NodeConfig.layer),
);

Effect.runPromise(
  program.pipe(
    Effect.catchAllCause((cause) =>
      Effect.succeed({
        type: "FailureOutput",
        error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(
      `👷 Block commitment work completed (${JSON.stringify(output)}).`,
    ),
  );
  parentPort?.postMessage(output);
});
