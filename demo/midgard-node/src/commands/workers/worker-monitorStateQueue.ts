import { Duration, Effect, Schedule } from "effect";
import { LucidEvolution, OutRef, Script } from "@lucid-evolution/lucid";
import { parentPort } from "worker_threads";
import sqlite3 from "sqlite3";

import * as SDK from "@al-ft/midgard-sdk";
import { makeConfig, makeUserFn } from "@/config.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { UtilsDB } from "@/database/index.js";
import { StateQueueTx, UtilsTx } from "@/transactions/index.js";
import { logInfo } from "effect/Effect";

let latestBlockOutRef: OutRef = { txHash: "", outputIndex: 0 };

const monitorStateQueue = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  db: sqlite3.Database,
) =>
  Effect.gen(function* () {
    const latestBlock = yield* SDK.Endpoints.fetchLatestCommitedBlockProgram(
      lucid,
      fetchConfig,
    );
    const fetchedBlocksOutRef = UtilsTx.utxoToOutRef(latestBlock);
    if (!UtilsTx.outRefsAreEqual(latestBlockOutRef, fetchedBlocksOutRef)) {
      latestBlockOutRef = fetchedBlocksOutRef;
      logInfo("monitorStateQueue: Committing a new block...");
      yield* StateQueueTx.buildAndSubmitCommitmentBlock(
        lucid,
        db,
        fetchConfig,
        Date.now(),
      );
    }
  });

parentPort?.on("message", (_) => {
  console.log("Starting Worker monitorStateQueue");

  const workerProgram = Effect.gen(function* () {
    const nodeConfig = yield* makeConfig;
    const { user } = yield* makeUserFn(nodeConfig);
    const { spendScriptAddress, policyId } =
      yield* makeAlwaysSucceedsServiceFn(nodeConfig);
    const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
      stateQueueAddress: spendScriptAddress,
      stateQueuePolicyId: policyId,
    };

    const db = yield* Effect.tryPromise({
      try: () => UtilsDB.initializeDb(nodeConfig.DATABASE_PATH),
      catch: (e) => new Error(`${e}`),
    });

    const policy = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(nodeConfig.POLLING_INTERVAL),
    );
    const action = monitorStateQueue(user, fetchConfig, db).pipe(
      Effect.tap((metrics) => {
        parentPort?.postMessage({ type: "metrics", data: metrics });
      }),
      Effect.catchAll((error) => {
        Effect.log("monitorStateQueue: error occured", error);
        return Effect.void;
      }),
    );
    yield* Effect.repeat(action, policy);
  });
  Effect.runPromiseExit(workerProgram);
});
