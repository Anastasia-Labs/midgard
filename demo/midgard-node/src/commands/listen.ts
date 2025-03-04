import { NodeConfig, User } from "@/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { StateQueueTx } from "@/transactions/index.js";
import { NodeSdk } from "@effect/opentelemetry";
import { getAddressDetails, LucidEvolution } from "@lucid-evolution/lucid";
import { diag, DiagConsoleLogger, DiagLogLevel } from "@opentelemetry/api";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { Effect, Metric, Option, pipe } from "effect";
import express from "express";
import { Pool } from "pg";
import { Worker } from "worker_threads";
import {
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  UtilsDB,
} from "../database/index.js";
import {
  findSpentAndProducedUTxOs,
  isHexString,
  logInfo,
  logWarning,
} from "../utils.js";

const mempoolTxGauge = Metric.gauge("mempool_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the mempool",
  bigint: true,
});

const commitBlockNumTxGauge = Metric.gauge("commit_block_num_tx_count", {
  description:
    "A gauge for tracking the current number of transactions in the commit block",
  bigint: true,
});

const totalTxSizeGauge = Metric.gauge("total_tx_size", {
  description:
    "A gauge for tracking the total size of transactions in the commit block",
});

const commitBlockCounter = Metric.counter("commit_block_count", {
  description: "A counter for tracking the number of committed blocks",
  bigint: true,
  incremental: true,
});

const commitBlockTxCounter = Metric.counter("commit_block_tx_count", {
  description:
    "A counter for tracking the number of transactions in the commit block",
  bigint: true,
  incremental: true,
});

const commitBlockTxSizeGauge = Metric.gauge("commit_block_tx_size", {
  description: "A gauge for tracking the size of the commit block transaction",
});

const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

const mergeBlockCounter = Metric.counter("merge_block_count", {
  description: "A counter for tracking merge blocks",
  bigint: true,
  incremental: true,
});

export const listen = (
  lucid: LucidEvolution,
  pool: Pool,
  port: number
): Effect.Effect<void, never, never> =>
  Effect.sync(() => {
    const app = express();
    app.get("/tx", (req, res) => {
      const txHash = req.query.tx_hash;
      logInfo(`GET /tx - Request received for tx_hash: ${txHash}`);

      if (
        typeof txHash === "string" &&
        isHexString(txHash) &&
        txHash.length === 32
      ) {
        MempoolDB.retrieveTxCborByHash(pool, txHash).then((ret) => {
          Option.match(ret, {
            onSome: (retrieved) => {
              logInfo(`GET /tx - Transaction found in mempool: ${txHash}`);
              res.json({ tx: retrieved });
            },
            onNone: () =>
              ImmutableDB.retrieveTxCborByHash(pool, txHash).then((ret) => {
                Option.match(ret, {
                  onSome: (retrieved) => {
                    logInfo(
                      `GET /tx - Transaction found in immutable: ${txHash}`
                    );
                    res.json({ tx: retrieved });
                  },
                  onNone: () => {
                    logWarning(`GET /tx - No transaction found: ${txHash}`);
                    res
                      .status(404)
                      .json({ message: "No matching transactions found" });
                  },
                });
              }),
          });
        });
      } else {
        logWarning(`GET /tx - Invalid transaction hash: ${txHash}`);
        res
          .status(404)
          .json({ message: `Invalid transaction hash: ${txHash}` });
      }
    });

    app.get("/utxos", (req, res) => {
      const addr = req.query.addr;
      logInfo(`GET /utxos - Request received for address: ${addr}`);

      if (typeof addr === "string") {
        try {
          const addrDetails = getAddressDetails(addr);
          if (addrDetails.paymentCredential) {
            MempoolLedgerDB.retrieve(pool).then((allUTxOs) => {
              const filtered = allUTxOs.filter(
                (a) => a.address === addrDetails.address.bech32
              );
              logInfo(
                `GET /utxos - Found ${filtered.length} UTXOs for address: ${addr}`
              );
              res.json({ utxos: filtered });
            });
          } else {
            logWarning(
              `GET /utxos - Invalid address (no payment credential): ${addr}`
            );
            res.status(400).json({ message: `Invalid address: ${addr}` });
          }
        } catch (e) {
          logWarning(
            `GET /utxos - Invalid address format: ${addr}, error: ${e}`
          );
          res.status(400).json({ message: `Invalid address: ${addr}` });
        }
      } else {
        logWarning(`GET /utxos - Invalid address type: ${addr}`);
        res.status(400).json({ message: `Invalid address: ${addr}` });
      }
    });

    app.get("/block", (req, res) => {
      const hdrHash = req.query.header_hash;
      logInfo(`GET /block - Request received for header_hash: ${hdrHash}`);

      if (
        typeof hdrHash === "string" &&
        isHexString(hdrHash) &&
        hdrHash.length === 32
      ) {
        BlocksDB.retrieveTxHashesByBlockHash(pool, hdrHash).then((hashes) => {
          logInfo(
            `GET /block - Found ${hashes.length} transactions for block: ${hdrHash}`
          );
          res.json({ hashes });
        });
      } else {
        logWarning(`GET /block - Invalid block header hash: ${hdrHash}`);
        res
          .status(400)
          .json({ message: `Invalid block header hash: ${hdrHash}` });
      }
    });

    app.get("/init", async (_req, res) => {
      logInfo("GET /init - Initialization request received");
      try {
        const program = pipe(
          StateQueueTx.stateQueueInit,
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer)
        );
        const txHash = await Effect.runPromise(program);
        logInfo(`GET /init - Initialization successful: ${txHash}`);
        res.json({ message: `Initiation successful: ${txHash}` });
      } catch (e) {
        logWarning(`GET /init - Initialization failed: ${e}`);
        res.status(500).json({
          message: "Initiation failed.",
        });
      }
    });

    app.get("/reset", async (_req, res) => {
      logInfo("GET /reset - Reset request received");
      res.type("text/plain");
      try {
        const program = pipe(
          StateQueueTx.resetStateQueue,
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer)
        );
        await Effect.runPromise(program);
        res.json({ message: "Collected all UTxOs successfully!" });
      } catch (e) {
        res.status(400).json({
          message: `Failed to collect one or more UTxOs. Please try again. Error: ${e}`,
        });
      }
      try {
        await Promise.all([
          MempoolDB.clear(pool),
          MempoolLedgerDB.clear(pool),
          BlocksDB.clear(pool),
          ImmutableDB.clear(pool),
          LatestLedgerDB.clear(pool),
          ConfirmedLedgerDB.clear(pool),
        ]);
        // res.json({ message: "Cleared all tables successfully!" });
      } catch (_e) {
        // res.status(400).json({
        //   message: "Failed to clear one or more tables. Please try again.",
        // });
      }
    });

    app.post("/submit", async (req, res) => {
      const txCBOR = req.query.tx_cbor;
      logInfo(`POST /submit - Submit request received for transaction`);

      if (typeof txCBOR === "string" && isHexString(txCBOR)) {
        try {
          const tx = lucid.fromTx(txCBOR);
          const spentAndProducedProgram = findSpentAndProducedUTxOs(txCBOR);
          const { spent, produced } = await Effect.runPromise(
            spentAndProducedProgram
          );
          // TODO: Avoid abstraction, dedicate a SQL command.
          await MempoolDB.insert(pool, tx.toHash(), txCBOR);
          await MempoolLedgerDB.clearUTxOs(pool, spent);
          await MempoolLedgerDB.insert(pool, produced);
          // await UtilsDB.modifyMultipleTables(
          //   db,
          //   [MempoolDB.insert, tx.toHash(), txCBOR],
          //   [MempoolLedgerDB.clearUTxOs, spent],
          //   [MempoolLedgerDB.insert, produced],
          // );
          Effect.runSync(Metric.increment(txCounter));
          logInfo(
            `POST /submit - Transaction submitted successfully: ${tx.toHash()}`
          );
          res.json({ message: "Successfully submitted the transaction" });
        } catch (e) {
          logWarning(`POST /submit - Submission failed: ${e}`);
          res.status(400).json({ message: `Something went wrong: ${e}` });
        }
      } else {
        logWarning("POST /submit - Invalid CBOR provided");
        res.status(400).json({ message: "Invalid CBOR provided" });
      }
    });

    app.listen(port, () =>
      logInfo(`Server running at http://localhost:${port}`)
    );
  });

export const storeTx = async (lucid: LucidEvolution, pool: Pool, tx: string) =>
  Effect.gen(function* () {
    const txHash = lucid.fromTx(tx).toHash();
    yield* Effect.tryPromise(() => MempoolDB.insert(pool, txHash, tx));
  });

export const runNode = Effect.gen(function* () {
  diag.setLogger(new DiagConsoleLogger(), DiagLogLevel.DEBUG);
  const { user } = yield* User;
  const nodeConfig = yield* NodeConfig;
  const pool = new Pool({
    host: "localhost",
    user: nodeConfig.POSTGRES_USER,
    max: 20,
    idleTimeoutMillis: 30000,
    connectionTimeoutMillis: 2000,
  });
  const db = yield* Effect.tryPromise({
    try: () => UtilsDB.initializeDb(pool),
    catch: (e) => new Error(`${e}`),
  });
  const prometheusExporter = new PrometheusExporter(
    {
      port: nodeConfig.PROM_METRICS_PORT,
      // host: "localhost",
    },
    () => {
      `Prometheus metrics available at http://localhost:${nodeConfig.PROM_METRICS_PORT}/metrics`;
    }
  );
  const originalStop = prometheusExporter.stopServer;
  prometheusExporter.stopServer = async function () {
    logWarning("Prometheus exporter is stopping!");
    return originalStop();
  };

  const MetricsLive = NodeSdk.layer(() => ({
    resource: { serviceName: "midgard-node" },
    metricReader: prometheusExporter,
  }));

  const workers = [
    new Worker(new URL("./worker-monitorStateQueue.js", import.meta.url)),
    new Worker(new URL("./worker-monitorConfirmedState.js", import.meta.url)),
  ];
  workers.forEach((w) => {
    w.on("message", (message) => {
      switch (message.type) {
        case "commit-block-metrics":
          const { txSize, numTx, totalTxSize } = message.data;
          Effect.runSync(commitBlockTxSizeGauge(Effect.succeed(txSize)));
          Effect.runSync(commitBlockNumTxGauge(Effect.succeed(numTx)));
          Effect.runSync(Metric.increment(commitBlockCounter));
          Effect.runSync(Metric.incrementBy(commitBlockTxCounter, numTx));
          Effect.runSync(totalTxSizeGauge(Effect.succeed(totalTxSize)));
          break;
        case "mempool-metrics":
          Effect.runSync(mempoolTxGauge(Effect.succeed(message.data.numTx)));
          break;
        case "merge-tx-metric":
          Effect.runSync(Metric.increment(mergeBlockCounter));
          break;
        default:
          logWarning(`Unknown message type: ${message.type}`);
      }
    });
    w.postMessage("start");
  });

  yield* Effect.all([listen(user, pool, nodeConfig.PORT)]).pipe(
    Effect.withSpan("midgard-node"),
    Effect.tap(() => Effect.annotateCurrentSpan("migdard-node", "runner")),
    Effect.provide(MetricsLive),
    Effect.catchAllCause(Effect.logError)
  );
});
