import { NodeConfig, User } from "@/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { AlwaysSucceeds } from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { NodeSdk } from "@effect/opentelemetry";
import { fromHex, getAddressDetails, toHex } from "@lucid-evolution/lucid";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import {
  Cause,
  Chunk,
  Duration,
  Effect,
  Layer,
  Metric,
  pipe,
  Queue,
  Schedule,
} from "effect";
import {
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  InitDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  ProcessedMempoolDB,
} from "../database/index.js";
import {
  ProcessedTx,
  breakDownTx,
  bufferToHex,
  isHexString,
} from "../utils.js";
import { Database } from "@/services/database.js";
import {
  HttpRouter,
  HttpServer,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { createServer } from "node:http";
import { NodeHttpServer } from "@effect/platform-node";
import { HttpBodyError } from "@effect/platform/HttpBody";
import { insertGenesisUtxos } from "@/database/genesis.js";
import { deleteLedgerMpt, deleteMempoolMpt } from "@/workers/utils/mpt.js";
import { Worker } from "worker_threads";
import { WorkerOutput as BlockConfirmationWorkerOutput } from "@/workers/utils/confirm-block-commitments.js";
import { WorkerError } from "@/workers/utils/common.js";

const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

const txQueueSizeGauge = Metric.gauge("tx_queue_size", {
  description: "A tracker for the size of the tx queue before processing",
  bigint: true,
});

const mempoolTxGauge = Metric.gauge("mempool_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the mempool",
  bigint: true,
});

const failWith500Helper = (
  logLabel: string,
  logMsg: string,
  error: any,
  msgOverride?: string,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${logLabel} - ${logMsg}: ${error}`);
    return yield* HttpServerResponse.json(
      { error: msgOverride ?? "Something went wrong" },
      { status: 500 },
    );
  });

const failWith500 = (
  method: "GET" | "POST",
  endpoint: string,
  error?: Error | HttpBodyError | string | unknown,
  msgOverride?: string,
) => failWith500Helper(`${method} /${endpoint}`, "failure", error, msgOverride);

const getTxHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  if (
    typeof txHashParam !== "string" ||
    !isHexString(txHashParam) ||
    txHashParam.length !== 32
  ) {
    // yield* Effect.logInfo(`Invalid transaction hash: ${txHashParam}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashParam}` },
      { status: 404 },
    );
  }
  const txHashBytes = Buffer.from(fromHex(txHashParam));
  const foundCbor: Uint8Array = yield* MempoolDB.retrieveTxCborByHash(
    txHashBytes,
  ).pipe(
    Effect.catchAll((_e) =>
      Effect.gen(function* () {
        const fromImmutable =
          yield* ImmutableDB.retrieveTxCborByHash(txHashBytes);
        yield* Effect.logInfo(
          `GET /tx - Transaction found in ImmutableDB: ${txHashParam}`,
        );
        return fromImmutable;
      }),
    ),
  );
  yield* Effect.logInfo(
    `GET /tx - Transaction found in mempool: ${txHashParam}`,
  );
  return yield* HttpServerResponse.json({ tx: toHex(foundCbor) });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "tx", e)),
  Effect.catchTag("DBSelectError", (e) =>
    failWith500("GET", "tx", e.cause ?? e, "tx not found in database"),
  ),
);

const getUtxosHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["addr"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(`GET /utxos - Invalid address type: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${addr}` },
      { status: 400 },
    );
  }
  try {
    const addrDetails = getAddressDetails(addr);
    if (!addrDetails.paymentCredential) {
      yield* Effect.logInfo(`Invalid address format: ${addr}`);
      return yield* HttpServerResponse.json(
        { error: `Invalid address format: ${addr}` },
        { status: 400 },
      );
    }

    const utxosWithAddress = yield* MempoolLedgerDB.retrieveByAddress(
      addrDetails.address.bech32,
    );

    const response = utxosWithAddress.map((entry) => ({
      outref: bufferToHex(entry.outref),
      value: bufferToHex(entry.output),
    }));

    yield* Effect.logInfo(`Found ${response.length} UTxOs for ${addr}`);
    return yield* HttpServerResponse.json({
      utxos: response,
    });
  } catch (error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "utxos", e)),
  Effect.catchTag("DBSelectError", (e) =>
    failWith500("GET", "utxos", e.cause ?? e, "database error"),
  ),
);

const getBlockHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const hdrHash = params["header_hash"];
  yield* Effect.logInfo(
    `GET /block - Request received for header_hash: ${hdrHash}`,
  );

  if (
    typeof hdrHash !== "string" ||
    !isHexString(hdrHash) ||
    hdrHash.length !== 32
  ) {
    yield* Effect.logInfo(`GET /block - Invalid block hash: ${hdrHash}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid block hash: ${hdrHash}` },
      { status: 400 },
    );
  }
  const hashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(
    Buffer.from(fromHex(hdrHash)),
  );
  yield* Effect.logInfo(
    `GET /block - Found ${hashes.length} txs for block: ${hdrHash}`,
  );
  return yield* HttpServerResponse.json({ hashes });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "block", e)),
  Effect.catchTag("DBSelectError", (e) =>
    failWith500("GET", "block", e.cause ?? e, "database error"),
  ),
);

const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const result = yield* StateQueueTx.stateQueueInit;
  yield* insertGenesisUtxos;
  yield* Effect.logInfo(`GET /init - Initialization successful: ${result}`);
  return yield* HttpServerResponse.json({
    message: `Initiation successful: ${result}`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "init", e)),
  Effect.catchAll((e) => failWith500("GET", "init", e, "sdk error")),
);

const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /commit - Manual block commitment order received`);
  const result = yield* blockCommitmentAction;
  yield* Effect.logInfo(`GET /commit - Block commitment successful: ${result}`);
  return yield* HttpServerResponse.json({
    message: `Block commitment successful: ${result}`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "commit", e)),
  Effect.catchTag("WorkerError", (e) =>
    failWith500("GET", "commit", e.cause ?? e, "failed worker"),
  ),
);

const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /merge - Manual merge order received`);
  const result = yield* mergeAction;
  yield* Effect.logInfo(
    `GET /merge - Merging confirmed state successful: ${result}`,
  );
  return yield* HttpServerResponse.json({
    message: `Merging confirmed state successful: ${result}`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "merge", e)),
  Effect.catchAll((e) => failWith500("GET", "merge", e, "sdk error")),
);

const getResetHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`🚧 Reset request received`);
  yield* StateQueueTx.resetStateQueue;
  yield* Effect.all(
    [
      MempoolDB.clear,
      MempoolLedgerDB.clear,
      ProcessedMempoolDB.clear,
      BlocksDB.clear,
      ImmutableDB.clear,
      LatestLedgerDB.clear,
      ConfirmedLedgerDB.clear,
      deleteMempoolMpt,
      deleteLedgerMpt,
    ],
    { discard: true },
  );
  return yield* HttpServerResponse.json({
    message: `Collected all UTxOs successfully!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "reset", e)),
  Effect.catchAll((e) => failWith500("GET", "reset", e, "sdk error")),
);

const getLogStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const { user: lucid } = yield* User;
  const alwaysSucceeds = yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueuePolicyId: alwaysSucceeds.stateQueueAuthValidator.policyId,
    stateQueueAddress:
      alwaysSucceeds.stateQueueAuthValidator.spendScriptAddress,
  };
  const sortedUTxOs = yield* SDK.Endpoints.fetchSortedStateQueueUTxOsProgram(
    lucid,
    fetchConfig,
  );
  let drawn = `
---------------------------- STATE QUEUE ----------------------------`;
  yield* Effect.allSuccesses(
    sortedUTxOs.map((u) =>
      Effect.gen(function* () {
        let info = "";
        const isHead = u.datum.key === "Empty";
        const isEnd = u.datum.next === "Empty";
        const emoji = isHead ? "🚢" : isEnd ? "⚓" : "⛓ ";
        if (u.datum.key !== "Empty") {
          // if (isHead) {
          const icon = isEnd ? "  " : emoji;
          info = `
${icon} ╰─ header: ${u.datum.key.Key.key}`;
        }
        drawn = `${drawn}
${emoji} ${u.utxo.txHash}#${u.utxo.outputIndex}${info}`;
      }),
    ),
  );
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    message: `State queue drawn in server logs!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", "logStateQueue", e),
  ),
  Effect.catchAll((e) => failWith500("GET", "logStateQueue", e, "sdk error")),
);

const getLogBlocksDBHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Querying BlocksDB...`);
  const allBlocksData = yield* BlocksDB.retrieve;
  const keyValues: Record<string, number> = allBlocksData.reduce(
    (acc: Record<string, number>, entry) => {
      const bHex = toHex(entry.header_hash);
      if (!acc[bHex]) {
        acc[bHex] = 1;
      } else {
        acc[bHex] += 1;
      }
      return acc;
    },
    {} as Record<string, number>,
  );
  let drawn = `
------------------------------ BLOCKS DB ----------------------------`;
  for (const bHex in keyValues) {
    drawn = `${drawn}
${bHex} -──▶ ${keyValues[bHex]} tx(s)`;
  }
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    message: `BlocksDB drawn in server logs!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logBlocksDB", e)),
  Effect.catchTag("DBSelectError", (e) =>
    failWith500("GET", "logBlocksDB", e.cause ?? e, "database error"),
  ),
);

const getLogGlobalsHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Logging global variables...`);
  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${global.BLOCKS_IN_QUEUE}
  LATEST_SYNC ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(global.LATEST_SYNC_OF_STATE_QUEUE_LENGTH).toLocaleString()}
  RESET_IN_PROGRESS ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${global.RESET_IN_PROGRESS}
  AVAILABLE_CONFIRMED_BLOCK ⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${global.AVAILABLE_CONFIRMED_BLOCK}
  PROCESSED_UNSUBMITTED_TXS_COUNT ⋅⋅⋅ ${global.PROCESSED_UNSUBMITTED_TXS_COUNT}
  PROCESSED_UNSUBMITTED_TXS_SIZE ⋅⋅⋅⋅ ${global.PROCESSED_UNSUBMITTED_TXS_SIZE}
  UNCONFIRMED_SUBMITTED_BLOCK ⋅⋅⋅⋅⋅⋅⋅ ${global.UNCONFIRMED_SUBMITTED_BLOCK}
`);
  return yield* HttpServerResponse.json({
    message: `Global variables logged!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logGlobals", e)),
);

const postSubmitHandler = (txQueue: Queue.Enqueue<string>) =>
  Effect.gen(function* () {
    // yield* Effect.logInfo(`◻️  Submit request received for transaction`);
    const params = yield* ParsedSearchParams;
    const txStringParam = params["tx_cbor"];
    if (typeof txStringParam !== "string" || !isHexString(txStringParam)) {
      yield* Effect.logInfo(`▫️ Invalid CBOR provided`);
      return yield* HttpServerResponse.json(
        { error: `Invalid CBOR provided` },
        { status: 400 },
      );
    } else {
      const txString = txStringParam;
      yield* txQueue.offer(txString);
      Effect.runSync(Metric.increment(txCounter));
      return yield* HttpServerResponse.json({
        message: `Successfully added the transaction to the queue`,
      });
    }
  }).pipe(
    Effect.catchTag("HttpBodyError", (e) =>
      failWith500("POST", "submit", e, "▫️ L2 transaction failed"),
    ),
  );

const router = (
  txQueue: Queue.Queue<string>,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  HttpBodyError,
  | Database
  | User
  | NodeConfig
  | AlwaysSucceedsContract
  | HttpServerRequest.HttpServerRequest
> =>
  HttpRouter.empty
    .pipe(
      HttpRouter.get("/tx", getTxHandler),
      HttpRouter.get("/utxos", getUtxosHandler),
      HttpRouter.get("/block", getBlockHandler),
      HttpRouter.get("/init", getInitHandler),
      HttpRouter.get("/commit", getCommitEndpoint),
      HttpRouter.get("/merge", getMergeHandler),
      HttpRouter.get("/reset", getResetHandler),
      HttpRouter.get("/logStateQueue", getLogStateQueueHandler),
      HttpRouter.get("/logBlocksDB", getLogBlocksDBHandler),
      HttpRouter.get("/logGlobals", getLogGlobalsHandler),
      HttpRouter.post("/submit", postSubmitHandler(txQueue)),
    )
    .pipe(
      Effect.catchAllCause((cause) =>
        failWith500Helper(
          "Router unexpected failure",
          "unknown endpoint",
          Cause.pretty(cause),
        ),
      ),
    );

const blockCommitmentAction = Effect.gen(function* () {
  if (!global.RESET_IN_PROGRESS) {
    yield* Effect.logInfo("🔹 New block commitment process started.");
    yield* StateQueueTx.buildAndSubmitCommitmentBlock().pipe(
      Effect.withSpan("buildAndSubmitCommitmentBlock"),
    );
  }
});

const blockConfirmationAction = Effect.gen(function* () {
  if (!global.RESET_IN_PROGRESS) {
    yield* Effect.logInfo("🔍 New block confirmation process started.");
    const worker = Effect.async<
      BlockConfirmationWorkerOutput,
      WorkerError,
      never
    >((resume) => {
      Effect.runSync(
        Effect.logInfo(`🔍 Starting block confirmation worker...`),
      );
      const worker = new Worker(
        new URL("./confirm-block-commitments.js", import.meta.url),
        {
          workerData: {
            data: {
              firstRun:
                global.UNCONFIRMED_SUBMITTED_BLOCK === "" &&
                global.AVAILABLE_CONFIRMED_BLOCK === "",
              unconfirmedSubmittedBlock: global.UNCONFIRMED_SUBMITTED_BLOCK,
            },
          },
        },
      );
      worker.on("message", (output: BlockConfirmationWorkerOutput) => {
        if (output.type === "FailedConfirmationOutput") {
          resume(
            Effect.fail(
              new WorkerError({
                worker: "confirm-block-commitments",
                message: `Error in confirmation worker: ${output.error}`,
              }),
            ),
          );
        } else {
          resume(Effect.succeed(output));
        }
        worker.terminate();
      });
      worker.on("error", (e: Error) => {
        resume(
          Effect.fail(
            new WorkerError({
              worker: "confirm-block-commitments",
              message: `Error in confirmation worker: ${e}`,
              cause: e,
            }),
          ),
        );
        worker.terminate();
      });
      worker.on("exit", (code: number) => {
        if (code !== 0) {
          resume(
            Effect.fail(
              new WorkerError({
                worker: "confirm-block-commitments",
                message: `Confirmation worker exited with code: ${code}`,
              }),
            ),
          );
        }
      });
      return Effect.sync(() => {
        worker.terminate();
      });
    });
    const workerOutput: BlockConfirmationWorkerOutput = yield* worker;
    switch (workerOutput.type) {
      case "SuccessfulConfirmationOutput": {
        global.UNCONFIRMED_SUBMITTED_BLOCK = "";
        global.AVAILABLE_CONFIRMED_BLOCK = workerOutput.blocksUTxO;
        yield* Effect.logInfo("🔍 ☑️  Submitted block confirmed.");
        break;
      }
      case "NoTxForConfirmationOutput": {
        break;
      }
      case "FailedConfirmationOutput": {
        break;
      }
    }
  }
});

const mergeAction = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const { user: lucid } = yield* User;
  const { stateQueueAuthValidator, depositAuthValidator } =
    yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueueAddress: stateQueueAuthValidator.spendScriptAddress,
    stateQueuePolicyId: stateQueueAuthValidator.policyId,
  };
  lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX);
  yield* StateQueueTx.buildAndSubmitMergeTx(
    lucid,
    fetchConfig,
    stateQueueAuthValidator.spendScript,
    stateQueueAuthValidator.mintScript,
  );
});

const monitorMempoolAction = Effect.gen(function* () {
  const numTx = yield* MempoolDB.retrieveTxCount;
  yield* mempoolTxGauge(Effect.succeed(BigInt(numTx)));
});

const txQueueProcessorAction = (txQueue: Queue.Dequeue<string>) =>
  Effect.gen(function* () {
    const queueSize = yield* txQueue.size;
    yield* txQueueSizeGauge(Effect.succeed(BigInt(queueSize)));

    const txStringsChunk: Chunk.Chunk<string> = yield* Queue.takeAll(txQueue);
    const txStrings = Chunk.toReadonlyArray(txStringsChunk);
    const brokeDownTxs: ProcessedTx[] = yield* Effect.forEach(txStrings, (tx) =>
      Effect.gen(function* () {
        return yield* breakDownTx(fromHex(tx));
      }),
    );
    yield* MempoolDB.insertMultiple(brokeDownTxs);
  });

const blockCommitmentFork = (rerunDelay: number) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔵 Block commitment fork started.");
    const action = blockCommitmentAction.pipe(
      Effect.withSpan("block-commitment-fork"),
      Effect.catchAllCause(Effect.logWarning),
    );
    const schedule = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(rerunDelay),
    );
    yield* Effect.repeat(action, schedule);
  });

const blockConfirmationFork = (rerunDelay: number) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🟫 Block confirmation fork started.");
    const action = blockConfirmationAction.pipe(
      Effect.withSpan("block-confirmation-fork"),
      Effect.catchAllCause(Effect.logWarning),
    );
    const schedule = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(rerunDelay),
    );
    yield* Effect.repeat(action, schedule);
  });

// possible issues:
// 1. tx-generator: large batch size & high concurrency
// 2. after initing node, can't commit the block
const mergeFork = (rerunDelay: number) =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟠 Merge fork started.");
      const schedule = Schedule.addDelay(Schedule.forever, () =>
        Duration.millis(rerunDelay),
      );
      const action = mergeAction.pipe(
        Effect.withSpan("merge-confirmed-state-fork"),
        Effect.catchAllCause(Effect.logWarning),
      );
      yield* Effect.repeat(action, schedule);
    }),
    // Effect.fork, // Forking ensures the effect keeps running
  );

const monitorMempoolFork = pipe(
  Effect.gen(function* () {
    yield* Effect.logInfo("🟢 Mempool monitor fork started.");
    const schedule = Schedule.fixed("1000 millis");
    yield* Effect.repeat(monitorMempoolAction, schedule);
  }),
  Effect.catchAllCause(Effect.logWarning),
);

const txQueueProcessorFork = (txQueue: Queue.Dequeue<string>) =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🔶 Tx queue processor fork started.");
      const schedule = Schedule.fixed("500 millis");
      yield* Effect.repeat(txQueueProcessorAction(txQueue), schedule);
    }),
    Effect.catchAllCause(Effect.logWarning),
  );

export const runNode = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;

  const prometheusExporter = new PrometheusExporter(
    {
      port: nodeConfig.PROM_METRICS_PORT,
    },
    () => {
      `Prometheus metrics available at http://localhost:${nodeConfig.PROM_METRICS_PORT}/metrics`;
    },
  );

  const originalStop = prometheusExporter.stopServer;
  prometheusExporter.stopServer = async function () {
    Effect.runSync(Effect.logInfo("Prometheus exporter is stopping!"));
    return originalStop();
  };

  const MetricsLive = NodeSdk.layer(() => ({
    resource: { serviceName: "midgard-node" },
    metricReader: prometheusExporter,
    spanProcessor: new BatchSpanProcessor(
      new OTLPTraceExporter({ url: nodeConfig.OLTP_EXPORTER_URL }),
    ),
  }));

  const txQueue = yield* Queue.unbounded<string>();

  yield* InitDB.initializeDb().pipe(Effect.provide(Database.layer));

  const appThread = Layer.launch(
    Layer.provide(
      HttpServer.serve(router(txQueue)),
      NodeHttpServer.layer(createServer, { port: nodeConfig.PORT }),
    ),
  );

  const blockCommitmentThread = blockCommitmentFork(
    nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT,
  );

  const blockConfirmationThread = blockConfirmationFork(
    nodeConfig.WAIT_BETWEEN_BLOCK_CONFIRMATION,
  );

  const mergeThread = mergeFork(nodeConfig.WAIT_BETWEEN_MERGE_TXS);

  const monitorMempoolThread = monitorMempoolFork;

  const txQueueProcessorThread = txQueueProcessorFork(txQueue);

  const program = Effect.all(
    [
      appThread,
      blockCommitmentThread,
      blockConfirmationThread,
      mergeThread,
      monitorMempoolThread,
      txQueueProcessorThread,
    ],
    {
      concurrency: "unbounded",
    },
  ).pipe(
    Effect.provide(Database.layer),
    Effect.provide(AlwaysSucceedsContract.layer),
    Effect.provide(User.layer),
    Effect.provide(NodeConfig.layer),
  );

  pipe(
    program,
    Effect.withSpan("midgard"),
    Effect.provide(MetricsLive),
    Effect.catchAllCause(Effect.logError),
    Effect.runPromise,
  );
});
