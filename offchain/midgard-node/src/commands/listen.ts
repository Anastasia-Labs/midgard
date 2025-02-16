import {
  Result,
  errorToString,
  fail,
  isHexString,
  logAbort,
  logInfo,
  logWarning,
  ok,
  setupLucid,
} from "../utils.js";
import * as MPF from "@aiken-lang/merkle-patricia-forestry";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  LucidEvolution,
  OutRef,
  ScriptType,
  UTxO,
  CML,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import { findSpentAndProducedUTxOs } from "../../../midgard-sdk/src/utils/ledger-state.js";
import express from "express";
import sqlite3 from "sqlite3";
import * as mempool from "../database/mempool.js";
import * as mempoolLedger from "../database/mempoolLedger.js";
import * as blocks from "../database/blocks.js";
import * as latestLedger from "../database/latestLedger.js";
import * as immutable from "../database/immutable.js";
import { Effect, Option } from "effect";

// TODO: Placehoder, must be imported from SDK.
const fetchLatestBlock = async (
  _lucid: LucidEvolution,
): Promise<Result<UTxO>> => {
  return ok({
    txHash: "",
    outputIndex: 0,
    address: "",
    assets: {},
  });
};

// TODO: Placehoder, must be imported from SDK.
const fetchConfirmedState = async (
  _lucid: LucidEvolution,
): Promise<Result<UTxO>> => {
  return ok({
    txHash: "",
    outputIndex: 0,
    address: "",
    assets: {},
  });
};

const readEndTimeOfConfirmedState = (utxo: UTxO): Result<number> => {
  if (utxo.datum) {
    // const confirmedState = Data.castFrom(
    //   utxo.datum,
    //   SDK.LedgerState.ConfirmedState
    // );
    // return ok(Number(confirmedState.endTime));
    return ok(0);
  } else {
    return fail("Missing datum of the confirmed state.");
  }
};

const utxoToOutRef = (utxo: UTxO): OutRef => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
});

const outRefsAreEqual = (outRef0: OutRef, outRef1: OutRef): boolean => {
  return (
    outRef0.txHash === outRef1.txHash &&
    outRef0.outputIndex === outRef1.outputIndex
  );
};

export const listen = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  port: number,
  pollingInterval: number,
  confirmedStatePollingInterval: number,
) => {
  const app = express();

  app.get("/tx", (req, res) => {
    res.type("text/plain");
    const txHash = req.query.tx_hash;
    const txIsString = typeof txHash === "string";
    const validLength = txHash?.length === 32;

    if (txIsString && isHexString(txHash) && validLength) {
      mempool.retrieveTxCborByHash(db, txHash).then((ret) => {
        Option.match(ret, {
          onSome: (retreived) => {
            res.json({ tx: retreived });
          },
          onNone: () => {
            immutable.retrieveTxCborByHash(db, txHash).then((ret) => {
              Option.match(ret, {
                onSome: (retreived) => {
                  res.json({ tx: retreived });
                },
                onNone: () => {
                  res.status(404);
                  res.json({ message: "No matching transactions found" });
                },
              });
            });
          },
        });
      });
    } else {
      res.status(400);
      res.json({ message: `Invalid transaction hash: ${txHash}` });
    }
  });

  app.get("/utxos", (req, res) => {
    res.type("text/plain");
    const addr = req.query.addr;
    const addrIsString = typeof addr === "string";
    if (addrIsString) {
      try {
        const addrDetails = getAddressDetails(addr);
        if (addrDetails.paymentCredential != undefined) {
          mempoolLedger.retrieve(db).then((allUTxOs) => {
            res.json({
              uxtos: allUTxOs.filter(
                (a) => a.address == addrDetails.address.bech32,
              ),
            });
          });
        } else {
          res.status(400);
          res.json({ message: `Invalid address: ${addr}` });
        }
      } catch {
        res.status(400);
        res.json({ message: `Invalid address: ${addr}` });
      }
    } else {
      res.status(400);
      res.json({ message: `Invalid address: ${addr}` });
    }
  });

  app.get("/block", (req, res) => {
    res.type("text/plain");
    const hdrHash = req.query.header_hash;
    const txIsString = typeof hdrHash === "string";
    const validLength = hdrHash?.length === 32;
    if (txIsString && isHexString(hdrHash) && validLength) {
      blocks
        .retrieveTxHashesByBlockHash(db, hdrHash)
        .then((hashes) => res.json({ hashes: hashes }));
    } else {
      res.status(400);
      res.json({ message: `Invalid block header hash: ${hdrHash}` });
    }
  });

  app.post("/submit", (req, res) => {
    res.type("text/plain");
    const txCBOR = req.query.tx_cbor;
    const txIsString = typeof txCBOR === "string";

    if (txIsString && isHexString(txCBOR)) {
      const tx = lucid.fromTx(txCBOR);
      const spentAndProduced = findSpentAndProducedUTxOs(txCBOR);
      db.run("BEGIN TRANSACTION;", (err) => {
        if (err) {
          res.status(400);
          res.json({ message: "Unable to begin transaction" });
        }
      });
      try {
        latestLedger.clearUTxOs(db, spentAndProduced.spent).then(
          (v) =>
            latestLedger.insert(db, spentAndProduced.produced).then(
              (v) =>
                mempool.insert(db, tx.toHash(), txCBOR).then(
                  (v) =>
                    db.run("COMMIT;", (e) => {
                      if (e) {
                        res.status(400);
                        res.json({ message: "Unable to commit" });
                      }
                      res.json({
                        message: "Successfully submitted the transaction",
                      });
                    }),
                  (r) => {
                    db.run("ROLLBACK;");
                    res.status(400);
                    res.json({
                      message: "Unable to insert the transaction hash",
                    });
                  },
                ),
              (r) => {
                db.run("ROLLBACK;");
                res.status(400);
                res.json({ message: "Unable to insert produced UTxOs" });
              },
            ),
          (r) => {
            db.run("ROLLBACK;");
            res.status(400);
            res.json({ message: "Unable to clear spent UTxOs" });
          },
        );
      } catch (_e) {
        res.status(400);
        res.json({ message: "Something went wrong decoding the transaction" });
      }
    } else {
      res.status(400);
      res.json({ message: "Invalid CBOR provided" });
    }
  });

  app.listen(port, () => {});
  logInfo(`Server running at http://localhost:${port}`);
};

const monitorStateQueue = (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  pollingInterval: number,
) => {
  let latestBlockOutRef: OutRef = { txHash: "", outputIndex: 0 };
  setInterval(async () => {
    const latestBlockOutRefRes = await fetchLatestBlock(lucid);
    if (latestBlockOutRefRes.type === "ok") {
      const fetchedBlocksOutRef = utxoToOutRef(latestBlockOutRefRes.data);
      if (!outRefsAreEqual(latestBlockOutRef, fetchedBlocksOutRef)) {
        latestBlockOutRef = fetchedBlocksOutRef;
        await submitBlock(lucid, latestBlockOutRefRes.data);
      }
    } else {
      logWarning(`Something went wrong while fetching the latest block:
${errorToString(latestBlockOutRefRes.error)}`);
    }
  }, pollingInterval);
};

export const storeTx = async (
  lucid: LucidEvolution,
  db: sqlite3.Database,
  tx: string,
) => {
  const txHash = lucid.fromTx(tx).toHash();
  await mempool.insert(db, txHash, tx);
};

const submitBlock = async (lucid: LucidEvolution, latestBlock: UTxO) => {
  logWarning("submitBlock: TODO");
};

const monitorConfirmedState = (
  lucid: LucidEvolution,
  pollingInterval: number,
) => {
  logWarning("mergeOldestBlock: TODO");
};
