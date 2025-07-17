import { NodeConfig, User } from "@/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { AlwaysSucceeds } from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { NodeSdk } from "@effect/opentelemetry";
import { CML, fromHex, getAddressDetails, toHex } from "@lucid-evolution/lucid";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import {
  Duration,
  Effect,
  Layer,
  Metric,
  Option,
  pipe,
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
} from "../database/index.js";
import { findSpentAndProducedUTxOs, isHexString } from "../utils.js";
import { Database } from "@/services/database.js";
import { HttpRouter, HttpServer, HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { createServer } from "node:http";
import { NodeHttpServer } from "@effect/platform-node";
import { HttpBodyError } from "@effect/platform/HttpBody";
import { SqlClient } from "@effect/sql";

const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

const mempoolTxGauge = Metric.gauge("mempool_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the mempool",
  bigint: true,
});

const handle500 = (location: string, error: Error | HttpBodyError) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `Something went wrong at ${location} handler: ${error}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Something went wrong` },
      { status: 500 },
    );
  });

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
  const txHashBytes = fromHex(txHashParam);
  const retMempool = yield* MempoolDB.retrieveTxCborByHash(txHashBytes);
  if (Option.isSome(retMempool)) {
    yield* Effect.logInfo(
      `GET /tx - Transaction found in mempool: ${txHashParam}`,
    );
    return yield* HttpServerResponse.json({ tx: retMempool.value });
  }
  const retImmutable = yield* ImmutableDB.retrieveTxCborByHash(txHashBytes);
  if (Option.isSome(retImmutable)) {
    yield* Effect.logInfo(
      `GET /tx - Transaction found in immutable: ${txHashParam}`,
    );
    return yield* HttpServerResponse.json({ tx: retImmutable.value });
  }
  yield* Effect.logInfo(`Transaction not found: ${txHashParam}`);
  return yield* HttpServerResponse.json(
    { error: `Transaction not found: ${txHashParam}` },
    { status: 404 },
  );
}).pipe(Effect.catchAll((e) => handle500("getTx", e)));

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
    const allUTxOs = yield* MempoolLedgerDB.retrieve();
    const filtered = allUTxOs.filter(({ value }) => {
      const cmlOutput = CML.TransactionOutput.from_cbor_bytes(value);
      return cmlOutput.address().to_bech32() === addrDetails.address.bech32;
    });
    yield* Effect.logInfo(`Found ${filtered.length} UTXOs for ${addr}`);
    return yield* HttpServerResponse.json({ utxos: filtered });
  } catch (error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(Effect.catchAll((e) => handle500("getUtxos", e)));

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
  const hashes = yield* BlocksDB.retrieveTxHashesByBlockHash(fromHex(hdrHash));
  yield* Effect.logInfo(
    `GET /block - Found ${hashes.length} txs for block: ${hdrHash}`,
  );
  return yield* HttpServerResponse.json({ hashes });
}).pipe(Effect.catchAll((e) => handle500("getBlock", e)));

const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const result = yield* StateQueueTx.stateQueueInit;
  yield* Effect.logInfo(`GET /init - Initialization successful: ${result}`);
  return yield* HttpServerResponse.json({
    message: `Initiation successful: ${result}`,
  });
}).pipe(
  Effect.catchAll((e) =>
    Effect.gen(function* () {
      yield* Effect.logInfo(`GET /init - Initialization failed: ${e}`);
      return yield* HttpServerResponse.json(
        { error: "Initialization failed" },
        { status: 500 },
      );
    }),
  ),
);

const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /commit - Manual block commitment order received`);
  const result = yield* blockCommitmentAction;
  yield* Effect.logInfo(`GET /commit - Block commitment successful: ${result}`);
  return yield* HttpServerResponse.json({
    message: `Block commitment successful: ${result}`,
  });
}).pipe(
  Effect.catchAll((e) =>
    Effect.gen(function* () {
      yield* Effect.logInfo(`GET /commit - Block commitment failed: ${e}`);
      return yield* HttpServerResponse.json(
        { error: "Block commitment failed." },
        { status: 500 },
      );
    }),
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
  Effect.catchAll((e) =>
    Effect.gen(function* () {
      yield* Effect.logInfo(
        `GET /merge - Merging confirmed state failed: ${e}`,
      );
      return yield* HttpServerResponse.json(
        { error: "Merging confirmed state failed." },
        { status: 500 },
      );
    }),
  ),
);

const getResetHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`🚧 Reset request received`);
  yield* StateQueueTx.resetStateQueue;
  yield* Effect.all(
    [
      MempoolDB.clear(),
      MempoolLedgerDB.clear(),
      BlocksDB.clear(),
      ImmutableDB.clear(),
      LatestLedgerDB.clear(),
      ConfirmedLedgerDB.clear(),
    ],
    { discard: true },
  );
  return yield* HttpServerResponse.json({
    message: `Collected all UTxOs successfully!`,
  });
}).pipe(
  Effect.catchAll((e) =>
    Effect.gen(function* () {
      yield* Effect.logInfo(`Failed to collect one or more UTxOs. Error: ${e}`);
      return yield* HttpServerResponse.json(
        {
          error: `Failed to collect one or more UTxOs. Please try again. Error: ${e}`,
        },
        { status: 400 },
      );
    }),
  ),
);

const getLogStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const { user: lucid } = yield* User;
  const alwaysSucceeds = yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueuePolicyId: alwaysSucceeds.policyId,
    stateQueueAddress: alwaysSucceeds.spendScriptAddress,
  };
  const sortedUTxOs = yield* SDK.Endpoints.fetchSortedStateQueueUTxOsProgram(
    lucid,
    fetchConfig,
  );
  let drawn = `
---------------------------- STATE QUEUE ----------------------------`;
  yield* Effect.allSuccesses(sortedUTxOs.map((u) => Effect.gen(function* () {
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
  })));
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    message: `State queue drawn in server logs!`,
  });
});

const getLogBlocksDBHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Querying BlocksDB...`);
  const allPairs = yield* BlocksDB.retrieve();
  const keyValues: Record<string, number> = allPairs.reduce(
    (acc: Record<string, number>, [b, _t]) => {
      const bHex = toHex(b);
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
}).pipe(Effect.catchAll((e) => handle500("getLogBlocksDBHandler", e)));

const getLogSemaphoresHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Logging semaphores...`);
  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅ ${global.BLOCKS_IN_QUEUE}
  LATEST_SYNC ⋅⋅⋅⋅⋅⋅⋅⋅ ${(new Date(global.LATEST_SYNC_OF_STATE_QUEUE_LENGTH)).toLocaleString()}
  RESET_IN_PROGRESS ⋅⋅ ${global.RESET_IN_PROGRESS}
`);
  return yield* HttpServerResponse.json({
    message: `Semaphores logged!`,
  });
});

const postSubmitHandler = Effect.gen(function* () {
  // yield* Effect.logInfo(`◻️ Submit request received for transaction`);
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
    const { user: lucid } = yield* User;
    return yield* Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const txCBOR = fromHex(txString);
      const tx = lucid.fromTx(txString);
      const { spent, produced } = yield* findSpentAndProducedUTxOs(txCBOR);
      yield* sql.withTransaction(
        Effect.gen(function* () {
          yield* MempoolDB.insert(fromHex(tx.toHash()), txCBOR);
          yield* MempoolLedgerDB.clearUTxOs(spent);
          yield* MempoolLedgerDB.insert(produced);
        }),
      );
      Effect.runSync(Metric.increment(txCounter));
      return yield* HttpServerResponse.json({
        message: `Successfully submitted the transaction`,
      });
    });
  }
}).pipe(
  Effect.catchAll((e) =>
    Effect.gen(function* () {
      yield* Effect.logInfo(`▫️ L2 transaction failed: ${e}`);
      return yield* HttpServerResponse.json(
        { error: `Something went wrong: ${e}` },
        { status: 400 },
      );
    }),
  ),
);

const router = HttpRouter.empty.pipe(
  HttpRouter.get("/tx", getTxHandler),
  HttpRouter.get("/utxos", getUtxosHandler),
  HttpRouter.get("/block", getBlockHandler),
  HttpRouter.get("/init", getInitHandler),
  HttpRouter.get("/commit", getCommitEndpoint),
  HttpRouter.get("/merge", getMergeHandler),
  HttpRouter.get("/reset", getResetHandler),
  HttpRouter.get("/logStateQueue", getLogStateQueueHandler),
  HttpRouter.get("/logBlocksDB", getLogBlocksDBHandler),
  HttpRouter.get("/logSemaphores", getLogSemaphoresHandler),
  HttpRouter.post("/submit", postSubmitHandler),
);

const blockCommitmentAction = Effect.gen(function* () {
  yield* Effect.logInfo("🔹 New block commitment process started.");
  yield* StateQueueTx.buildAndSubmitCommitmentBlock().pipe(
    Effect.withSpan("buildAndSubmitCommitmentBlock"),
  );
});

const mergeAction = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const { user: lucid } = yield* User;
  const { spendScriptAddress, policyId, spendScript, mintScript } =
    yield* AlwaysSucceeds.AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueueAddress: spendScriptAddress,
    stateQueuePolicyId: policyId,
  };
  lucid.selectWallet.fromSeed(nodeConfig.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX);
  yield* StateQueueTx.buildAndSubmitMergeTx(
    lucid,
    fetchConfig,
    spendScript,
    mintScript,
  );
});

const mempoolAction = Effect.gen(function* () {
  const txList = yield* MempoolDB.retrieve();
  const numTx = BigInt(txList.length);
  yield* mempoolTxGauge(Effect.succeed(numTx));
});

const blockCommitmentFork = (rerunDelay: number) =>
  pipe(
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
    }),
    // Effect.fork, // Forking ensures the effect keeps running
  );

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

const mempoolFork = () =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟢 Mempool fork started.");
      const schedule = Schedule.addDelay(Schedule.forever, () =>
        Duration.millis(1000),
      );
      yield* Effect.repeat(mempoolAction, schedule);
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

  yield* InitDB.initializeDb().pipe(Effect.provide(Database.layer));

  const ListenLayer = Layer.provide(
    HttpServer.serve(router),
    NodeHttpServer.layer(createServer, { port: nodeConfig.PORT }),
  );

  const appThread = pipe(Layer.launch(ListenLayer));

  const blockCommitmentThread = blockCommitmentFork(
    nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT,
  );

  const mergeThread = pipe(
    mergeFork(nodeConfig.WAIT_BETWEEN_MERGE_TXS),
  );

  const monitorMempoolThread = pipe(mempoolFork());

  const program = Effect.all(
    [appThread, blockCommitmentThread, mergeThread, monitorMempoolThread],
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
