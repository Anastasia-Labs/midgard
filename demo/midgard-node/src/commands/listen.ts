import { findSpentAndProducedUTxOs, isHexString } from "../utils.js";
import {
  LucidEvolution,
  OutRef,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import express from "express";
import sqlite3 from "sqlite3";
import {
  MempoolDB,
  MempoolLedgerDB,
  LatestLedgerDB,
  ConfirmedLedgerDB,
  BlocksDB,
  ImmutableDB,
  UtilsDB,
} from "../database/index.js";
import {
  Duration,
  Effect,
  Option,
  Schedule,
  Metric,
  pipe,
  Logger,
} from "effect";
import { User, NodeConfig } from "@/config.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { NodeSdk } from "@effect/opentelemetry";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { StateQueueTx, UtilsTx } from "@/transactions/index.js";

export const listen = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  port: number,
): Effect.Effect<void, never, never> =>
  Effect.sync(() => {
    Effect.provide(Logger.pretty);
    const app = express();
    const txCounter = Metric.counter("tx_count", {
      description: "A counter for tracking transactions",
      bigint: true,
      incremental: true,
    }).pipe(Metric.tagged("environment", lucid.config().network!));
    app.get("/tx", (req, res) => {
      res.type("text/plain");
      const txHash = req.query.tx_hash;
      if (
        typeof txHash === "string" &&
        isHexString(txHash) &&
        txHash.length === 32
      ) {
        MempoolDB.retrieveTxCborByHash(db, txHash).then((ret) => {
          Option.match(ret, {
            onSome: (retrieved) => res.json({ tx: retrieved }),
            onNone: () =>
              ImmutableDB.retrieveTxCborByHash(db, txHash).then((ret) => {
                Option.match(ret, {
                  onSome: (retrieved) => res.json({ tx: retrieved }),
                  onNone: () =>
                    res
                      .status(404)
                      .json({ message: "No matching transactions found" }),
                });
              }),
          });
        });
      } else {
        res
          .status(400)
          .json({ message: `Invalid transaction hash: ${txHash}` });
      }
    });

    app.get("/utxos", (req, res) => {
      res.type("text/plain");
      const addr = req.query.addr;
      if (typeof addr === "string") {
        try {
          const addrDetails = getAddressDetails(addr);
          if (addrDetails.paymentCredential) {
            MempoolLedgerDB.retrieve(db).then((allUTxOs) =>
              res.json({
                utxos: allUTxOs.filter(
                  (a) => a.address === addrDetails.address.bech32,
                ),
              }),
            );
          } else {
            res.status(400).json({ message: `Invalid address: ${addr}` });
          }
        } catch {
          res.status(400).json({ message: `Invalid address: ${addr}` });
        }
      } else {
        res.status(400).json({ message: `Invalid address: ${addr}` });
      }
    });

    app.get("/block", (req, res) => {
      res.type("text/plain");
      const hdrHash = req.query.header_hash;
      if (
        typeof hdrHash === "string" &&
        isHexString(hdrHash) &&
        hdrHash.length === 32
      ) {
        BlocksDB.retrieveTxHashesByBlockHash(db, hdrHash).then((hashes) =>
          res.json({ hashes }),
        );
      } else {
        res
          .status(400)
          .json({ message: `Invalid block header hash: ${hdrHash}` });
      }
    });

    app.get("/init", async (_req, res) => {
      try {
        const program = pipe(
          StateQueueTx.stateQueueInit,
          Effect.provide(User.layer),
          Effect.provide(AlwaysSucceedsContract.layer),
          Effect.provide(NodeConfig.layer),
        );
        const txHash = await Effect.runPromise(program);
        res.json({ message: `Initiation successful: ${txHash}` });
      } catch (e) {
        Effect.logWarning(`Initiation failed: ${e}`);
        res.status(500).json({
          message: "Initiation failed.",
        });
      }
    });

    app.get("/reset", async (_req, res) => {
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
      } catch (_e) {
        res.status(400).json({
          message: "Failed to collect one or more UTxOs. Please try again.",
        });
      }
      try {
        await Promise.all([
          MempoolDB.clear(db),
          MempoolLedgerDB.clear(db),
          BlocksDB.clear(db),
          ImmutableDB.clear(db),
          LatestLedgerDB.clear(db),
          ConfirmedLedgerDB.clear(db),
        ]);
        res.json({ message: "Cleared all tables successfully!" });
      } catch (_e) {
        res.status(400).json({
          message: "Failed to clear one or more tables. Please try again.",
        });
      }
    });

    app.post("/submit", async (req, res) => {
      res.type("text/plain");
      const txCBOR = req.query.tx_cbor;
      if (typeof txCBOR === "string" && isHexString(txCBOR)) {
        try {
          const tx = lucid.fromTx(txCBOR);
          const spentAndProducedProgram = findSpentAndProducedUTxOs(txCBOR);
          const { spent, produced } = await Effect.runPromise(
            spentAndProducedProgram,
          );
          // TODO: Avoid abstraction, dedicate a SQL command.
          await UtilsDB.modifyMultipleTables(
            db,
            [MempoolDB.insert, tx.toHash(), txCBOR],
            [MempoolLedgerDB.clearUTxOs, spent],
            [MempoolLedgerDB.insert, produced],
          );
          Effect.runSync(Metric.increment(txCounter));
          res.json({ message: "Successfully submitted the transaction" });
        } catch (e) {
          res.status(400).json({ message: "Something went wrong" });
        }
      } else {
        res.status(400).json({ message: "Invalid CBOR provided" });
      }
    });

    app.listen(port, () =>
      Effect.logInfo(`Server running at http://localhost:${port}`),
    );
  });

const monitorStateQueue = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  db: sqlite3.Database,
  pollingInterval: number,
) =>
  Effect.gen(function* () {
    let latestBlockOutRef: OutRef = { txHash: "", outputIndex: 0 };
    const monitor = Effect.gen(function* () {
      const latestBlock = yield* SDK.Endpoints.fetchLatestCommitedBlockProgram(
        lucid,
        fetchConfig,
      );
      const fetchedBlocksOutRef = UtilsTx.utxoToOutRef(latestBlock);
      if (!UtilsTx.outRefsAreEqual(latestBlockOutRef, fetchedBlocksOutRef)) {
        latestBlockOutRef = fetchedBlocksOutRef;
        Effect.logInfo("Committing a new block...");
        yield* StateQueueTx.buildAndSubmitCommitmentBlock(
          lucid,
          db,
          fetchConfig,
          Date.now(),
        );
      }
    });
    const schedule = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(pollingInterval),
    );
    yield* Effect.repeat(monitor, schedule);
  });

export const storeTx = async (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  tx: string,
) =>
  Effect.gen(function* () {
    const txHash = lucid.fromTx(tx).toHash();
    yield* Effect.tryPromise(() => MempoolDB.insert(db, txHash, tx));
  });

const monitorConfirmedState = (
  lucid: LucidEvolution,
  fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig,
  db: sqlite3.Database,
  pollingInterval: number,
) =>
  Effect.gen(function* () {
    const schedule = Schedule.addDelay(Schedule.forever, () =>
      Duration.millis(pollingInterval),
    );
    yield* Effect.repeat(
      StateQueueTx.buildAndSubmitMergeTx(lucid, db, fetchConfig),
      schedule,
    );
  });

export const runNode = Effect.gen(function* () {
  const { user } = yield* User;
  const nodeConfig = yield* NodeConfig;
  const { spendScriptAddress, policyId } = yield* AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueueAddress: spendScriptAddress,
    stateQueuePolicyId: policyId,
  };
  const db = yield* Effect.tryPromise(() =>
    UtilsDB.initializeDb(nodeConfig.DATABASE_PATH),
  );
  const MetricsLive = NodeSdk.layer(() => ({
    resource: { serviceName: "midgard-node" },
    spanProcessor: new BatchSpanProcessor(
      new OTLPTraceExporter({
        url: `http://localhost:${nodeConfig.OTLP_PORT}/v1/traces`,
      }),
    ),
    metricReader: new PrometheusExporter({
      port: nodeConfig.PROM_METRICS_PORT,
    }),
  }));

  yield* Effect.all([
    listen(user, db, nodeConfig.PORT),
    monitorStateQueue(user, fetchConfig, db, nodeConfig.POLLING_INTERVAL),
    monitorConfirmedState(
      user,
      fetchConfig,
      db,
      nodeConfig.CONFIRMED_STATE_POLLING_INTERVAL,
    ),
  ]).pipe(
    Effect.withSpan("midgard-node"),
    Effect.tap(() => Effect.annotateCurrentSpan("migdard-node", "runner")),
    Effect.provide(MetricsLive),
  );
});
