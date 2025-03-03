import { Duration, Effect, Schedule } from "effect";
import { parentPort } from "worker_threads";

import * as SDK from "@al-ft/midgard-sdk";
import { makeConfig, makeUserFn } from "@/config.js";
import { makeAlwaysSucceedsServiceFn } from "@/services/always-succeeds.js";
import { UtilsDB } from "@/database/index.js";
import { StateQueueTx } from "@/transactions/index.js";

parentPort?.on("message", (_) => {
  console.log("Starting Worker monitorConfirmedState");

  const workerProgram = Effect.gen(function* () {
    const nodeConfig = yield* makeConfig;
    const { user: lucid } = yield* makeUserFn(nodeConfig);
    const { spendScriptAddress, policyId, spendScript, mintScript } =
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
    const action = StateQueueTx.buildAndSubmitMergeTx(
      lucid,
      db,
      fetchConfig,
      spendScript,
      mintScript,
    ).pipe(
      Effect.tap((metrics) => {
        parentPort?.postMessage({ type: "metrics", data: metrics });
      }),
      Effect.catchAll((error) => {
        Effect.log("monitorConfirmedState: error occured", error);
        return Effect.void;
      }),
    );
    yield* Effect.repeat(action, policy);
  });
  Effect.runPromiseExit(workerProgram);
});
