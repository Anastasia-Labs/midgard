import { parentPort, workerData } from "worker_threads";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, pipe } from "effect";
import {
  WorkerInput,
  WorkerOutput,
  findAllSpentAndProducedUTxOs,
} from "@/utils.js";
import { NodeConfig } from "@/config.js";
import pg from "pg";
import { LatestLedgerDB, MempoolDB } from "@/database/index.js";

const wrapper = (
  _input: WorkerInput,
): Effect.Effect<WorkerOutput, Error, NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;

    const pool = new pg.Pool({
      host: nodeConfig.POSTGRES_HOST,
      user: nodeConfig.POSTGRES_USER,
      password: nodeConfig.POSTGRES_PASSWORD,
      database: nodeConfig.POSTGRES_DB,
      max: 20,
      idleTimeoutMillis: 30000,
      connectionTimeoutMillis: 2000,
    });

    const mempoolTxs = yield* Effect.tryPromise(() =>
      MempoolDB.retrieve(pool),
    ).pipe(Effect.withSpan("retrieve mempool transaction"));
    const mempoolTxHashes: Uint8Array[] = [];
    const mempoolTxCbors: Uint8Array[] = [];

    mempoolTxs.map(({ txHash, txCbor }) => {
      mempoolTxHashes.push(txHash);
      mempoolTxCbors.push(txCbor);
    });

    const { spent: spentList, produced: producedList } =
      yield* findAllSpentAndProducedUTxOs(mempoolTxCbors).pipe(
        Effect.withSpan("findAllSpentAndProducedUTxOs"),
      );

    const latestLedgerUTxOs = yield* Effect.tryPromise(() =>
      LatestLedgerDB.retrieve(pool),
    ).pipe(Effect.withSpan("retrieve latest ledger utxo list"));

    // Remove spent UTxOs from latestLedgerUTxOs
    const filteredUTxOList = latestLedgerUTxOs.filter(
      (utxo) => !spentList.some((spent) => utxo.outputReference == spent),
    );

    // Merge filtered latestLedgerUTxOs with producedList
    const newLatestLedger = [...filteredUTxOList, ...producedList];

    const mempoolTxsTrieProgram = SDK.Utils.mptFromTxs(mempoolTxs);
    const newLatestLedgerTrieProgram = SDK.Utils.mptFromUTxOs(
      spentList,
      producedList,
      newLatestLedger,
    );

    const [mempoolTxsTrie, newLatestLedgerTrie] = yield* Effect.all(
      [mempoolTxsTrieProgram, newLatestLedgerTrieProgram],
      { concurrency: 2 },
    );

    return {
      txRoot: mempoolTxsTrie.hash.toString("hex"),
      utxoRoot: newLatestLedgerTrie.hash.toString("hex"),
    };
  });

if (parentPort === null) {
  throw new Error("MPT computation must be run as a worker");
}

const inputData = workerData as WorkerInput;

const program = pipe(wrapper(inputData), Effect.provide(NodeConfig.layer));

Effect.runPromise(
  program.pipe(
    Effect.catchAll((e) =>
      Effect.succeed({
        error: e instanceof Error ? e.message : "Unknown error from MPT worker",
      }),
    ),
  ),
).then((output) => {
  Effect.runSync(
    Effect.logInfo(`👷 Work completed (${JSON.stringify(output)}).`),
  );
  parentPort?.postMessage(output);
});
