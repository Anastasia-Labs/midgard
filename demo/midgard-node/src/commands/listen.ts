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
  Runtime,
  Schedule,
} from "effect";
import express, { NextFunction } from "express";
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
import { SqlError } from "@effect/sql";
import { Database } from "@/services/database.js";

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

const ledgerSizeGauge = Metric.gauge("ledger_utxo_count", {
  description: "A gauge for tracking the current size of the ledger",
  bigint: true,
});

type EndpointResponse<R> =
  | { _tag: "Success"; body: R }
  | { _tag: "Failure"; status: number; msg: string };

const getTxEndpoint = (
  txHash: unknown,
): Effect.Effect<
  EndpointResponse<{ tx: Uint8Array }>,
  SqlError.SqlError | Error,
  Database
> =>
  Effect.gen(function* () {
    if (
      typeof txHash !== "string" ||
      !isHexString(txHash) ||
      txHash.length !== 32
    )
      return {
        _tag: "Failure",
        status: 400,
        msg: `Invalid transaction hash: ${txHash}`,
      };
    const txHashBytes = fromHex(txHash);
    const retMempool = yield* MempoolDB.retrieveTxCborByHash(txHashBytes);
    if (Option.isSome(retMempool)) {
      yield* Effect.logInfo(
        `GET /tx - Transaction found in mempool: ${txHash}`,
      );
      return { _tag: "Success", body: { tx: retMempool.value } };
    }

    const retImmutable = yield* ImmutableDB.retrieveTxCborByHash(txHashBytes);
    if (Option.isSome(retImmutable)) {
      yield* Effect.logInfo(
        `GET /tx - Transaction found in immutable: ${txHash}`,
      );
      return { _tag: "Success", body: { tx: retImmutable.value } };
    }

    yield* Effect.logInfo(`GET /tx - No transaction found: ${txHash}`);
    return {
      _tag: "Failure",
      status: 404,
      msg: `Transaction not found: ${txHash}`,
    };
  });

const getUtxosEndpoint = (
  addr: unknown,
): Effect.Effect<
  EndpointResponse<{
    utxos: { outputReference: Uint8Array; output: Uint8Array }[];
  }>,
  SqlError.SqlError | Error,
  Database
> =>
  Effect.gen(function* () {
    if (typeof addr !== "string") {
      yield* Effect.logInfo(`GET /utxos - Invalid address type: ${addr}`);
      return { _tag: "Failure", status: 400, msg: `Invalid address: ${addr}` };
    }
    try {
      const addrDetails = getAddressDetails(addr);
      if (addrDetails.paymentCredential) {
        const allUTxOs = yield* MempoolLedgerDB.retrieve();
        const filtered = allUTxOs.filter(({ output }) => {
          const cmlOutput = CML.TransactionOutput.from_cbor_bytes(output);
          const address = cmlOutput.address().to_bech32();
          address === addrDetails.address.bech32;
        });
        yield* Effect.logInfo(
          `GET /utxos - Found ${filtered.length} UTXOs for address: ${addr}`,
        );
        return { _tag: "Success", body: { utxos: filtered } };
      } else {
        yield* Effect.logInfo(
          `GET /utxos - Invalid address (no payment credential): ${addr}`,
        );
        return {
          _tag: "Failure",
          status: 400,
          msg: `Invalid address: ${addr}`,
        };
      }
    } catch (e) {
      yield* Effect.logInfo(
        `GET /utxos - Invalid address format: ${addr}, error: ${e}`,
      );
      return { _tag: "Failure", status: 400, msg: `Invalid address: ${addr}` };
    }
  });

const getBlockEndpoint = (
  hdrHash: unknown,
): Effect.Effect<
  EndpointResponse<Uint8Array<ArrayBufferLike>[]>,
  SqlError.SqlError | Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `GET /block - Request received for header_hash: ${hdrHash}`,
    );
    if (
      typeof hdrHash !== "string" ||
      !isHexString(hdrHash) ||
      hdrHash.length !== 32
    ) {
      yield* Effect.logInfo(
        `GET /block - Invalid block header hash: ${hdrHash}`,
      );
      return {
        _tag: "Failure",
        status: 400,
        msg: `Invalid block header hash: ${hdrHash}`,
      };
    }
    const hashes = yield* BlocksDB.retrieveTxHashesByBlockHash(
      fromHex(hdrHash),
    );
    yield* Effect.logInfo(
      `GET /block - Found ${hashes.length} transactions for block: ${hdrHash}`,
    );
    return { _tag: "Success", body: hashes };
  });

const getInitEndpoint = (): Effect.Effect<
  EndpointResponse<{ message: string }>,
  SqlError.SqlError | Error,
  Database | User | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`✨ Initialization request received`);
    const result = yield* Effect.either(StateQueueTx.stateQueueInit);
    if (Either.isRight(result)) {
      yield* Effect.logInfo(
        `GET /init - Initialization successful: ${result.right}`,
      );
      return {
        _tag: "Success",
        body: { message: `Initiation successful: ${result.right}` },
      };
    } else {
      yield* Effect.logInfo(
        `GET /init - Initialization failed: ${result.left}`,
      );
      return { _tag: "Failure", status: 500, msg: `Initiation failed.` };
    }
  });

const getCommitEndpoint = (): Effect.Effect<
  EndpointResponse<{ message: string }>,
  SqlError.SqlError | Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(
      `GET /commit - Manual block commitment order received`,
    );
    const result = yield* Effect.either(makeBlockCommitmentAction());
    if (Either.isRight(result)) {
      yield* Effect.logInfo(
        `GET /commit - Block commitment successful: ${result.right}`,
      );
      return {
        _tag: "Success",
        body: { message: `Block commitment successful: ${result.right}` },
      };
    } else {
      yield* Effect.logInfo(
        `GET /commit - Block commitment failed: ${result.left}`,
      );
      return { _tag: "Failure", status: 500, msg: `Block commitment failed.` };
    }
  });

const getMergeEndpoint = (): Effect.Effect<
  EndpointResponse<{ message: string }>,
  SqlError.SqlError | Error,
  Database | User | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`GET /merge - Manual merge order received`);
    const result = yield* Effect.either(makeMergeAction());
    if (Either.isRight(result)) {
      yield* Effect.logInfo(
        `GET /merge - Merging confirmed state successful: ${result.right}`,
      );
      return {
        _tag: "Success",
        body: {
          message: `Merging confirmed state successful: ${result.right}`,
        },
      };
    } else {
      yield* Effect.logInfo(
        `GET /merge - Merging confirmed state failed: ${result.left}`,
      );
      return {
        _tag: "Failure",
        status: 500,
        msg: `Merging confirmed state failed.`,
      };
    }
  });

const getResetEndpoint = (): Effect.Effect<
  EndpointResponse<{ message: string }>,
  SqlError.SqlError | Error,
  Database | User | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`🚧 Reset request received`);
    const result = yield* Effect.either(StateQueueTx.resetStateQueue);
    if (Either.isLeft(result)) {
      return {
        _tag: "Failure",
        status: 400,
        msg: `Failed to collect one or more UTxOs. Please try again. Error: ${result.left}`,
      };
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
      return {
        _tag: "Success",
        body: { message: `Collected all UTxOs successfully!` },
      };
    }
  });

const postSubmitEndpoint = (
  txString: unknown,
): Effect.Effect<
  EndpointResponse<{ message: string }>,
  SqlError.SqlError | Error,
  Database | User
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`◻️ Submit request received for transaction`);
    if (typeof txString !== "string" || !isHexString(txString)) {
      yield* Effect.logInfo(`▫️ Invalid CBOR provided`);
      return { _tag: "Failure", status: 400, msg: `Invalid CBOR provided` };
    }
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
      return {
        _tag: "Success",
        body: { message: `Successfully submitted the transaction` },
      };
    } else {
      yield* Effect.logInfo(`▫️ L2 transaction failed: ${result.left}`);
      return {
        _tag: "Failure",
        status: 400,
        msg: `Something went wrong: ${result.left}`,
      };
    }
  });

export const listen = async () => {
  const AppLayer = Layer.mergeAll(NodeConfig.layer, User.layer, Database.layer);
  const makeAppRuntime = Layer.toRuntime(AppLayer).pipe(
    Effect.provide(NodeConfig.layer),
    Effect.scoped,
  );
  const app = express();

  type RunEndpointEffect = <A, E, R>(
    runtime: Runtime.Runtime<any>,
    endpoint: Effect.Effect<EndpointResponse<A>, E, R>,
    response: any,
  ) => Promise<void>;
  const runEndpointEffect: RunEndpointEffect = async (
    runtime,
    endpoint,
    response,
  ) => {
    try {
      const res: EndpointResponse<any> = await Runtime.runPromise(
        runtime,
        endpoint,
      );
      if (res._tag === "Success") await response.status(200).json(res.body);
      else await response.status(res.status).json({ message: res.msg });
    } catch (error) {
      await Runtime.runPromise(
        runtime,
        Effect.logError("Endpoint effect failed:", error),
      );
      await response.status(500).json({ message: "Something went wrong" });
    }
  };

  const runtime = await Effect.runPromise(makeAppRuntime);
  const port = Runtime.runSync(
    runtime,
    Effect.map(NodeConfig, (c) => c.PORT),
  );

  app.get("/tx", async (req, res) => {
    await runEndpointEffect(runtime, getTxEndpoint(req.query.tx_hash), res);
  });
  app.get("/utxos", async (req, res) => {
    await runEndpointEffect(runtime, getUtxosEndpoint(req.query.addr), res);
  });
  app.get("/block", async (req, res) => {
    await runEndpointEffect(
      runtime,
      getBlockEndpoint(req.query.header_hash),
      res,
    );
  });
  app.get("/init", async (_req, res) => {
    await runEndpointEffect(runtime, getInitEndpoint(), res);
  });
  app.get("/commit", async (_req, res) => {
    await runEndpointEffect(runtime, getCommitEndpoint(), res);
  });
  app.get("/merge", async (_req, res) => {
    await runEndpointEffect(runtime, getMergeEndpoint(), res);
  });
  app.get("/reset", async (_req, res) => {
    res.type("text/plain");
    await runEndpointEffect(runtime, getResetEndpoint(), res);
  });
  app.post("/submit", async (req, res) => {
    await runEndpointEffect(
      runtime,
      postSubmitEndpoint(req.query.tx_cbor),
      res,
    );
  });
  app.listen(port, () =>
    Effect.runSync(
      Effect.logInfo(`Server running at http://localhost:${port}`),
    ),
  );
};

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

const makeLedgerAction = () =>
  Effect.gen(function* () {
    const utxoList = yield* LatestLedgerDB.retrieve();
    const utxoCount = BigInt(utxoList.length);
    yield* ledgerSizeGauge(Effect.succeed(utxoCount));
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

const ledgerFork = () =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("📒 Ledger fork started.");
      const schedule = Schedule.addDelay(Schedule.forever, () =>
        Duration.millis(1000),
      );
      yield* Effect.repeat(makeLedgerAction(), schedule);
    }),
    Effect.catchAllCause(Effect.logWarning),
  );

export const runNode = Effect.gen(function* () {
  const { user } = yield* User;
  const nodeConfig = yield* NodeConfig;

  yield* InitDB.initializeDb();

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

  const appThread = Effect.promise(listen);

  const blockCommitmentThread = blockCommitmentFork(
    nodeConfig.POLLING_INTERVAL,
  );

  const mergeThread = pipe(
    mergeFork(nodeConfig.CONFIRMED_STATE_POLLING_INTERVAL),
    Effect.provide(Database.layer),
    Effect.provide(User.layer),
    Effect.provide(AlwaysSucceedsContract.layer),
    Effect.provide(NodeConfig.layer),
  );

  const monitorMempoolThread = pipe(
    mempoolFork(),
    Effect.provide(Database.layer),
    Effect.provide(NodeConfig.layer),
  );

  const monitorLedgerThread = pipe(
    ledgerFork(),
    Effect.provide(Database.layer),
    Effect.provide(NodeConfig.layer),
  );

  const program = Effect.all(
    [
      appThread,
      blockCommitmentThread,
      mergeThread,
      monitorMempoolThread,
      monitorLedgerThread,
    ],
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
