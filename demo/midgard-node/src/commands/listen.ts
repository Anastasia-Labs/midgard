import { NodeConfig, User } from "@/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { StateQueueTx, UtilsTx } from "@/transactions/index.js";
import { NodeSdk } from "@effect/opentelemetry";
import { getAddressDetails, LucidEvolution } from "@lucid-evolution/lucid";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { Duration, Effect, Metric, Option, pipe, Schedule } from "effect";
import express from "express";
import pg from "pg";
import {
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  UtilsDB,
} from "../database/index.js";
import { findSpentAndProducedUTxOs, isHexString } from "../utils.js";
import * as SDK from "@al-ft/midgard-sdk";
import { AlwaysSucceeds } from "@/services/index.js";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";

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

export const listen = (
  lucid: LucidEvolution,
  pool: pg.Pool,
  port: number,
): Effect.Effect<void, never, never> =>
  Effect.sync(() => {
    const log = (msg: string) => Effect.runSync(Effect.logInfo(msg));
    const app = express();
    app.get("/tx", (req, res) => {
      const txHash = req.query.tx_hash;
      // logInfo(`GET /tx - Request received for tx_hash: ${txHash}`);

      if (
        typeof txHash === "string" &&
        isHexString(txHash) &&
        txHash.length === 32
      ) {
        MempoolDB.retrieveTxCborByHash(pool, txHash).then((ret) => {
          Option.match(ret, {
            onSome: (retrieved) => {
              log(`GET /tx - Transaction found in mempool: ${txHash}`);
              res.json({ tx: retrieved });
            },
            onNone: () =>
              ImmutableDB.retrieveTxCborByHash(pool, txHash).then((ret) => {
                Option.match(ret, {
                  onSome: (retrieved) => {
                    log(`GET /tx - Transaction found in immutable: ${txHash}`);
                    res.json({ tx: retrieved });
                  },
                  onNone: () => {
                    log(`GET /tx - No transaction found: ${txHash}`);
                    res
                      .status(404)
                      .json({ message: "No matching transactions found" });
                  },
                });
              }),
          });
        });
      } else {
        // log(`GET /tx - Invalid transaction hash: ${txHash}`);
        res
          .status(404)
          .json({ message: `Invalid transaction hash: ${txHash}` });
      }
    });

    app.get("/utxos", (req, res) => {
      const addr = req.query.addr;
      log(`GET /utxos - Request received for address: ${addr}`);

      if (typeof addr === "string") {
        try {
          const addrDetails = getAddressDetails(addr);
          if (addrDetails.paymentCredential) {
            MempoolLedgerDB.retrieve(pool).then((allUTxOs) => {
              const filtered = allUTxOs.filter(
                (a) => a.address === addrDetails.address.bech32,
              );
              log(
                `GET /utxos - Found ${filtered.length} UTXOs for address: ${addr}`,
              );
              res.json({ utxos: filtered });
            });
          } else {
            log(
              `GET /utxos - Invalid address (no payment credential): ${addr}`,
            );
            res.status(400).json({ message: `Invalid address: ${addr}` });
          }
        } catch (e) {
          log(`GET /utxos - Invalid address format: ${addr}, error: ${e}`);
          res.status(400).json({ message: `Invalid address: ${addr}` });
        }
      } else {
        log(`GET /utxos - Invalid address type: ${addr}`);
        res.status(400).json({ message: `Invalid address: ${addr}` });
      }
    });

    app.get("/block", (req, res) => {
      const hdrHash = req.query.header_hash;
      log(`GET /block - Request received for header_hash: ${hdrHash}`);

      if (
        typeof hdrHash === "string" &&
        isHexString(hdrHash) &&
        hdrHash.length === 32
      ) {
        BlocksDB.retrieveTxHashesByBlockHash(pool, hdrHash).then((hashes) => {
          log(
            `GET /block - Found ${hashes.length} transactions for block: ${hdrHash}`,
          );
          res.json({ hashes });
        });
      } else {
        log(`GET /block - Invalid block header hash: ${hdrHash}`);
        res
          .status(400)
          .json({ message: `Invalid block header hash: ${hdrHash}` });
      }
    });

    app.get("/init", async (_req, res) => {
      log("✨ Initialization request received");
      try {
        const program = pipe(
          StateQueueTx.stateQueueInit,
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer),
        );
        const txHash = await Effect.runPromise(program);
        log(`GET /init - Initialization successful: ${txHash}`);
        res.json({ message: `Initiation successful: ${txHash}` });
      } catch (e) {
        log(`GET /init - Initialization failed: ${e}`);
        res.status(500).json({
          message: "Initiation failed.",
        });
      }
    });

    app.get("/commit", async (_req, res) => {
      log("GET /commit - Manual block commitment order received");
      try {
        const program = pipe(
          makeBlockCommitmentAction(pool),
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer),
        );
        const txHash = await Effect.runPromise(program);
        log(`GET /commit - Block commitment successful: ${txHash}`);
        res.json({ message: `Block commitment successful: ${txHash}` });
      } catch (e) {
        log(`GET /commit - Block commitment failed: ${e}`);
        res.status(500).json({
          message: "Block commitment failed.",
        });
      }
    });

    app.get("/merge", async (_req, res) => {
      log("GET /merge - Manual merge order received");
      try {
        const program = pipe(
          makeMergeAction(pool),
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer),
        );
        const txHash = await Effect.runPromise(program);
        log(`GET /merge - Merging confirmed state successful: ${txHash}`);
        res.json({ message: `Merging confirmed state successful: ${txHash}` });
      } catch (e) {
        log(`GET /merge - Merging confirmed state failed: ${e}`);
        res.status(500).json({
          message: "Merging confirmed state failed.",
        });
      }
    });

    app.get("/reset", async (_req, res) => {
      log("🚧 Reset request received");
      res.type("text/plain");
      try {
        const program = pipe(
          StateQueueTx.resetStateQueue,
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer),
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

      // log("◻️ Submit request received for transaction");

      if (typeof txCBOR === "string" && isHexString(txCBOR)) {
        try {
          const tx = lucid.fromTx(txCBOR);
          const spentAndProducedProgram = findSpentAndProducedUTxOs(txCBOR);
          const { spent, produced } = await Effect.runPromise(
            spentAndProducedProgram,
          );
          await MempoolDB.insert(pool, tx.toHash(), txCBOR);
          await MempoolLedgerDB.clearUTxOs(pool, spent);
          await MempoolLedgerDB.insert(pool, produced);
          Effect.runSync(Metric.increment(txCounter));
          // log(`▫️ L2 Transaction processed successfully: ${tx.toHash()}`);
          res.json({ message: "Successfully submitted the transaction" });
        } catch (e) {
          log(`▫️ L2 transaction failed: ${e}`);
          res.status(400).json({ message: `Something went wrong: ${e}` });
        }
      } else {
        log("▫️ Invalid CBOR provided");
        res.status(400).json({ message: "Invalid CBOR provided" });
      }
    });

    app.listen(port, () => log(`Server running at http://localhost:${port}`));
  });

export const storeTx = async (
  lucid: LucidEvolution,
  pool: pg.Pool,
  tx: string,
) =>
  Effect.gen(function* () {
    const txHash = lucid.fromTx(tx).toHash();
    yield* Effect.tryPromise(() => MempoolDB.insert(pool, txHash, tx));
  });

const makeBlockCommitmentAction = (db: pg.Pool) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 New block commitment process started.");
    const { user: lucid } = yield* User;
    const { spendScriptAddress, policyId } =
      yield* AlwaysSucceeds.AlwaysSucceedsContract;
    const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
      stateQueueAddress: spendScriptAddress,
      stateQueuePolicyId: policyId,
    };
    yield* Effect.logInfo(
      "🔹 Querying mempool to see if there are transactions present for inclusion in the block...",
    );
    const txList = yield* Effect.tryPromise(() => MempoolDB.retrieve(db));
    const numTx = BigInt(txList.length);
    // yield* mempoolTxGauge(Effect.succeed(numTx));
    if (numTx > 0) {
      yield* Effect.logInfo(`🔹 Found ${numTx} transaction(s) in mempool.`);
      yield* Effect.logInfo("🔹 Fetching the latest block from L1...");
      const latestBlock = yield* SDK.Endpoints.fetchLatestCommittedBlockProgram(
        lucid,
        fetchConfig,
      ).pipe(Effect.withSpan("fetchLatestCommittedBlockProgram"));
      const fetchedBlocksOutRef = UtilsTx.utxoToOutRef(latestBlock);

      yield* Effect.logInfo(
        `🔹 Success, its out ref is: ${fetchedBlocksOutRef.txHash}#${fetchedBlocksOutRef.outputIndex}`,
      );

      yield* StateQueueTx.buildAndSubmitCommitmentBlock(
        lucid,
        db,
        fetchConfig,
        Date.now(),
      ).pipe(Effect.withSpan("buildAndSubmitCommitmentBlock"));
    } else {
      yield* Effect.logInfo(
        "🔹 There are no transactions in mempool, block submission aborted.",
      );
    }
  });

const makeMergeAction = (db: pg.Pool) =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔸 Merging of oldest block started.");
    const { user: lucid } = yield* User;
    const { spendScriptAddress, policyId, spendScript, mintScript } =
      yield* AlwaysSucceeds.AlwaysSucceedsContract;
    const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
      stateQueueAddress: spendScriptAddress,
      stateQueuePolicyId: policyId,
    };
    yield* StateQueueTx.buildAndSubmitMergeTx(
      lucid,
      db,
      fetchConfig,
      spendScript,
      mintScript,
    );
  });

const makeMempoolAction = (db: pg.Pool) =>
  Effect.gen(function* () {
    const txList = yield* Effect.tryPromise(() => MempoolDB.retrieve(db));
    const numTx = BigInt(txList.length);
    yield* mempoolTxGauge(Effect.succeed(numTx));
  });

const blockCommitmentFork = (db: pg.Pool, pollingInterval: number) =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🔵 Block commitment fork started.");
      const action = makeBlockCommitmentAction(db).pipe(
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
const mergeFork = (db: pg.Pool, pollingInterval: number) =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟠 Merge fork started.");
      const schedule = Schedule.addDelay(Schedule.forever, () =>
        Duration.millis(pollingInterval),
      );
      const action = makeMergeAction(db).pipe(
        Effect.withSpan("merge-confirmed-state-fork"),
        Effect.catchAllCause(Effect.logWarning),
      );
      yield* Effect.repeat(action, schedule);
    }),
    // Effect.fork, // Forking ensures the effect keeps running
  );

const mempoolFork = (db: pg.Pool) =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🟢 Mempool fork started.");
      const schedule = Schedule.addDelay(Schedule.forever, () =>
        Duration.millis(1000),
      );
      yield* Effect.repeat(makeMempoolAction(db), schedule);
    }),
    Effect.catchAllCause(Effect.logWarning),
  );

export const runNode = Effect.gen(function* () {
  const { user } = yield* User;
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
  yield* Effect.tryPromise({
    try: () => UtilsDB.initializeDb(pool),
    catch: (e) => new Error(`${e}`),
  });

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

  const appThread = listen(user, pool, nodeConfig.PORT);

  const blockCommitmentThread = pipe(
    blockCommitmentFork(pool, nodeConfig.POLLING_INTERVAL),
    Effect.provide(User.layer),
    Effect.provide(AlwaysSucceedsContract.layer),
    Effect.provide(NodeConfig.layer),
  );

  const mergeThread = pipe(
    mergeFork(pool, nodeConfig.CONFIRMED_STATE_POLLING_INTERVAL),
    Effect.provide(User.layer),
    Effect.provide(AlwaysSucceedsContract.layer),
    Effect.provide(NodeConfig.layer),
  );

  const monitorMempoolThread = pipe(mempoolFork(pool));

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
