import { NodeConfig, User } from "@/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { AlwaysSucceeds } from "@/services/index.js";
import { StateQueueTx } from "@/transactions/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { NodeSdk } from "@effect/opentelemetry";
import { CML, fromHex, getAddressDetails } from "@lucid-evolution/lucid";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import {
  Duration,
  Effect,
  Either,
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
import { SqlClientLive } from "@/services/database.js";
import * as Reactivity from "@effect/experimental/Reactivity";
import { HttpRouter, HttpServer, HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { createServer } from "node:http";
import { NodeHttpServer } from "@effect/platform-node";
import { HttpBodyError } from "@effect/platform/HttpBody";

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
  const result = yield* Effect.either(StateQueueTx.stateQueueInit);
  if (Either.isRight(result)) {
    yield* Effect.logInfo(
      `GET /init - Initialization successful: ${result.right}`,
    );
    return yield* HttpServerResponse.json({
      message: `Initiation successful: ${result}`,
    });
  } else {
    yield* Effect.logInfo(`GET /init - Initialization failed: ${result.left}`);
    return yield* HttpServerResponse.json(
      { error: "Initialization failed" },
      { status: 500 },
    );
  }
}).pipe(Effect.catchAll((e) => handle500("getInit", e)));

const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /commit - Manual block commitment order received`);
  const result = yield* Effect.either(makeBlockCommitmentAction());
  if (Either.isRight(result)) {
    yield* Effect.logInfo(
      `GET /commit - Block commitment successful: ${result.right}`,
    );
    return yield* HttpServerResponse.json({
      message: `Block commitment successful: ${result.right}`,
    });
  } else {
    yield* Effect.logInfo(
      `GET /commit - Block commitment failed: ${result.left}`,
    );
    return yield* HttpServerResponse.json(
      { error: "Block commitment failed." },
      { status: 500 },
    );
  }
}).pipe(Effect.catchAll((e) => handle500("getCommit", e)));

const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /merge - Manual merge order received`);
  const result = yield* Effect.either(makeMergeAction());
  if (Either.isRight(result)) {
    yield* Effect.logInfo(
      `GET /merge - Merging confirmed state successful: ${result.right}`,
    );
    return yield* HttpServerResponse.json({
      message: `Merging confirmed state successful: ${result.right}`,
    });
  } else {
    yield* Effect.logInfo(
      `GET /merge - Merging confirmed state failed: ${result.left}`,
    );
    return yield* HttpServerResponse.json(
      { error: "Merging confirmed state failed." },
      { status: 500 },
    );
  }
}).pipe(Effect.catchAll((e) => handle500("getMerge", e)));

const getResetHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`🚧 Reset request received`);
  const result = yield* Effect.either(StateQueueTx.resetStateQueue);
  if (Either.isLeft(result)) {
    return yield* HttpServerResponse.json(
      {
        error: `Failed to collect one or more UTxOs. Please try again. Error: ${result.left}`,
      },
      { status: 400 },
    );
  } else {
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
  }
}).pipe(Effect.catchAll((e) => handle500("getReset", e)));

const postSubmitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`◻️ Submit request received for transaction`);
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
    const result = yield* Effect.either(
      Effect.gen(function* () {
        const txCBOR = fromHex(txString);
        const tx = lucid.fromTx(txString);
        const { spent, produced } = yield* findSpentAndProducedUTxOs(txCBOR);
        yield* MempoolDB.insert(fromHex(tx.toHash()), txCBOR);
        yield* MempoolLedgerDB.clearUTxOs(spent);
        yield* MempoolLedgerDB.insert(produced);
        Effect.runSync(Metric.increment(txCounter));
      }),
    );
    if (Either.isRight(result)) {
      return yield* HttpServerResponse.json({
        message: `Successfully submitted the transaction`,
      });
    } else {
      yield* Effect.logInfo(`▫️ L2 transaction failed: ${result.left}`);
      return yield* HttpServerResponse.json(
        { error: `Something went wrong: ${result.left}` },
        { status: 400 },
      );
    }
  }
}).pipe(Effect.catchAll((e) => handle500("postSubmit", e)));

const router = HttpRouter.empty.pipe(
  HttpRouter.get("/tx", getTxHandler),
  HttpRouter.get("/utxos", getUtxosHandler),
  HttpRouter.get("/block", getBlockHandler),
  HttpRouter.get("/init", getInitHandler),
  HttpRouter.get("/commit", getCommitEndpoint),
  HttpRouter.get("/merge", getMergeHandler),
  HttpRouter.get("/reset", getResetHandler),
  HttpRouter.post("/submit", postSubmitHandler),
);

const makeBlockCommitmentAction = () =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 New block commitment process started.");
    yield* StateQueueTx.buildAndSubmitCommitmentBlock().pipe(
      Effect.withSpan("buildAndSubmitCommitmentBlock"),
    );
  });

const makeMergeAction = () =>
  Effect.gen(function* () {
    const { user: lucid } = yield* User;
    const { spendScriptAddress, policyId, spendScript, mintScript } =
      yield* AlwaysSucceeds.AlwaysSucceedsContract;
    const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
      stateQueueAddress: spendScriptAddress,
      stateQueuePolicyId: policyId,
    };

    yield* StateQueueTx.buildAndSubmitMergeTx(
      lucid,
      fetchConfig,
      spendScript,
      mintScript,
    );
  });

const makeMempoolAction = () =>
  Effect.gen(function* () {
    const txList = yield* MempoolDB.retrieve();
    const numTx = BigInt(txList.length);
    yield* mempoolTxGauge(Effect.succeed(numTx));
  });

const blockCommitmentFork = (pollingInterval: number) =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🔵 Block commitment fork started.");
      const action = makeBlockCommitmentAction().pipe(
        Effect.withSpan("block-commitment-fork"),
        Effect.catchAllCause(Effect.logWarning),
      );
      const schedule = Schedule.addDelay(Schedule.forever, () =>
        Duration.millis(pollingInterval),
      );
      yield* Effect.repeat(action, schedule);
    }),
    // Effect.fork, // Forking ensures the effect keeps running
  );

// possible issues:
// 1. tx-generator: large batch size & high concurrency
// 2. after initing node, can't commit the block
const mergeFork = (pollingInterval: number) =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟠 Merge fork started.");
      const schedule = Schedule.addDelay(Schedule.forever, () =>
        Duration.millis(pollingInterval),
      );
      const action = makeMergeAction().pipe(
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
      yield* Effect.repeat(makeMempoolAction(), schedule);
    }),
    Effect.catchAllCause(Effect.logWarning),
  );

export const runNode = Effect.gen(function* () {
  const { user } = yield* User;
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

  yield* Effect.logInfo("📚 Opening connection to db...");
  yield* InitDB.initializeDb().pipe(Effect.provide(SqlClientLive));

  const ListenLayer = Layer.provide(
    HttpServer.serve(router),
    NodeHttpServer.layer(createServer, { port: 3000 }),
  );

  const appThread = pipe(
    Layer.launch(ListenLayer),
    Effect.provide(SqlClientLive),
    Effect.provide(User.layer),
    Effect.provide(AlwaysSucceedsContract.layer),
    Effect.provide(NodeConfig.layer),
  );

  const blockCommitmentThread = blockCommitmentFork(
    nodeConfig.POLLING_INTERVAL,
  );

  const mergeThread = pipe(
    mergeFork(nodeConfig.CONFIRMED_STATE_POLLING_INTERVAL),
    Effect.provide(SqlClientLive),
    Effect.provide(User.layer),
    Effect.provide(AlwaysSucceedsContract.layer),
    Effect.provide(NodeConfig.layer),
  );

  const monitorMempoolThread = pipe(
    mempoolFork(),
    Effect.provide(SqlClientLive),
    Effect.provide(NodeConfig.layer),
    Effect.scoped,
    Effect.provide(Reactivity.layer),
  );

  const program = Effect.all(
    [appThread, blockCommitmentThread, mergeThread, monitorMempoolThread],
    {
      concurrency: "unbounded",
    },
  );

  pipe(
    program,
    Effect.withSpan("midgard"),
    Effect.provide(MetricsLive),
    Effect.catchAllCause(Effect.logError),
    Effect.runPromise,
  );
});
