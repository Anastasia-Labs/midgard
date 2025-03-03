import { fromHex, OutRef, UTxO } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import { logAbort, logInfo } from "../utils.js";
import { utxoToNormalizedAssets, utxoToRow } from "./utils.js";

const runAsync = (
  db: sqlite3.Database,
  query: string,
  params?: any[],
  rejectCallback?: (err: Error) => void,
  resolveCallback?: (res: sqlite3.RunResult) => void
) => {
  return new Promise<void>((resolve, reject) => {
    db.run(query, params, function (err) {
      if (err) {
        if (rejectCallback) rejectCallback(err);
        else logAbort(`db: an error occurred: ${err}`);
        reject(err);
      } else {
        if (resolveCallback) resolveCallback(this);
        resolve();
      }
    });
  });
};

const runBeginTransaction = async (db: sqlite3.Database, funcName: string) => {
  runAsync(db, "BEGIN TRANSACTION", undefined, (err) =>
    logAbort(`db: error beginning transaction from ${funcName}: ${err.message}`)
  );
};

const runCommitTransaction = async (db: sqlite3.Database, funcName: string) => {
  runAsync(db, "COMMIT", undefined, (err) =>
    logAbort(
      `db: error committing transaction from ${funcName}: ${err.message}`
    )
  );
};

const runRollbackTransaction = async (
  db: sqlite3.Database,
  funcName: string,
  err: any
) => {
  logAbort(`db: an error occurred during transaction: ${err}`);
  await runAsync(db, "ROLLBACK", undefined, (err) =>
    logAbort(`db: transaction rollback error from ${funcName}: ${err.message}`)
  );
};

export const submitTx = async (
  db: sqlite3.Database,
  txHash: string,
  txCBOR: string,
  spent: OutRef[],
  produced: UTxO[]
): Promise<void> => {
  try {
    await runBeginTransaction(db, "submitTx");

    await runAsync(
      db,
      `INSERT INTO mempool (tx_hash, tx_cbor) VALUES (?, ?)`,
      [fromHex(txHash), fromHex(txCBOR)],
      (err) => logAbort(`mempool db: error inserting tx: ${err.message}`),
      (res) => logInfo(`mempool db: tx stored with rowid ${res.lastID}`)
    );

    await runAsync(
      db,
      `DELETE FROM mempool_ledger WHERE (tx_hash, output_index) IN (${spent
        .map(() => `(?, ?)`)
        .join(", ")})`,
      spent.flatMap((r) => [fromHex(r.txHash), r.outputIndex]),
      (err) =>
        logAbort(`mempool_ledger db: utxos removing error: ${err.message}`),
      (res) => logInfo(`mempool_ledger db: ${res.changes} UTXOs removed`)
    );

    await runAsync(
      db,
      `INSERT INTO mempool_ledger
        (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
        VALUES ${produced.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")}`,
      produced.flatMap((utxo) => Object.values(utxoToRow(utxo))),
      (err) =>
        logAbort(`mempool_ledger db: error inserting UTXOs: ${err.message}`),
      (_) => logInfo(`mempool_ledger db: ${produced.length} new UTXOs added`)
    );

    const normalizedAssets = produced.flatMap((utxo) =>
      utxoToNormalizedAssets(utxo)
    );
    await runAsync(
      db,
      `INSERT INTO mempool_ledger_assets (tx_hash, output_index, unit, quantity)
        VALUES ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}`,
      normalizedAssets.flatMap((v) => Object.values(v)),
      (err) =>
        logAbort(`mempool_ledger db: error inserting assets: ${err.message}`),
      (res) => logInfo(`mempool_ledger db: ${res.changes} assets added`)
    );

    await runCommitTransaction(db, "submitTx");
  } catch (err) {
    runRollbackTransaction(db, "submitTx", err);
  }
};

export const submitBlock = async (
  db: sqlite3.Database,
  spent: OutRef[],
  produced: UTxO[],
  txs: { txHash: string; txCbor: string }[]
) => {
  try {
    const normalizedAssets = produced.flatMap((utxo) =>
      utxoToNormalizedAssets(utxo)
    );

    await runBeginTransaction(db, "submitBlock");

    await runAsync(
      db,
      `DELETE FROM latest_ledger WHERE (tx_hash, output_index) IN (${spent
        .map(() => `(?, ?)`)
        .join(", ")})`,
      spent.flatMap((r) => [fromHex(r.txHash), r.outputIndex]),
      (err) =>
        logAbort(`latest_ledger db: utxos removing error: ${err.message}`)
    );

    await runAsync(
      db,
      `INSERT INTO latest_ledger
          (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
          VALUES ${produced.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")}`,
      produced.flatMap((utxo) => Object.values(utxoToRow(utxo))),
      (err) =>
        logAbort(`latest_ledger db: error inserting UTXOs: ${err.message}`),
      (_) => logInfo(`latest_ledger db: ${produced.length} new UTXOs added`)
    );

    await runAsync(
      db,
      `INSERT INTO latest_ledger_assets (tx_hash, output_index, unit, quantity)
          VALUES ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}`,
      normalizedAssets.flatMap((v) => Object.values(v)),
      (err) =>
        logAbort(`latest_ledger db: error inserting assets: ${err.message}`),
      (res) => logInfo(`latest_ledger db: ${res.changes} assets added`)
    );

    await runAsync(
      db,
      `DELETE FROM mempool`,
      [],
      (err) => logAbort(`mempool db: clearing error: ${err.message}`),
      (_) => logInfo(`mempool db: cleared`)
    );

    await new Promise<void>((resolve, reject) => {
      const query = `INSERT INTO immutable (tx_hash, tx_cbor) VALUES (?, ?)`;
      const stmt = db.prepare(query);

      for (const { txHash, txCbor } of txs) {
        stmt.run([fromHex(txHash), fromHex(txCbor)], function (err) {
          if (err) {
            logAbort(`immutable db: error inserting tx: ${err.message}`);
            reject(err);
            return;
          }
          logInfo(`immutable db: tx stored with rowid ${this.lastID}`);
        });
      }

      stmt.finalize((err) => {
        if (err) {
          reject(err);
        } else {
          resolve();
        }
      });
    });

    await runCommitTransaction(db, "submitBlock");
  } catch (err) {
    runRollbackTransaction(db, "submitBlock", err);
  }
};

export const buildAndSubmitMergeTx = async (
  db: sqlite3.Database,
  spentOutRefs: OutRef[],
  producedUTxOs: UTxO[],
  headerHash: string
) => {
  try {
    await runBeginTransaction(db, "buildAndSubmitMergeTx");

    await runAsync(
      db,
      `DELETE FROM confirmed_ledger WHERE (tx_hash, output_index) IN (${spentOutRefs
        .map(() => `(?, ?)`)
        .join(", ")})`,
      spentOutRefs.flatMap((r) => [fromHex(r.txHash), r.outputIndex]),
      (err) =>
        logAbort(`confirmed_ledger db: error clearing utxos: ${err.message}`)
    );

    await runAsync(
      db,
      `INSERT INTO confirmed_ledger
          (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
          VALUES ${producedUTxOs.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")}`,
      producedUTxOs.flatMap((utxo) => Object.values(utxoToRow(utxo))),
      (err) =>
        logAbort(`confirmed_ledger db: error inserting utxos: ${err.message}`)
    );

    const normalizedAssets = producedUTxOs.flatMap((utxo) =>
      utxoToNormalizedAssets(utxo)
    );
    await runAsync(
      db,
      `INSERT INTO confirmed_ledger_assets (tx_hash, output_index, unit, quantity)
          VALUES ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}`,
      normalizedAssets.flatMap((v) => Object.values(v)),
      (err) =>
        logAbort(`confirmed_ledger db: error inserting assets: ${err.message}`)
    );

    await runAsync(
      db,
      `DELETE from blocks WHERE header_hash = ?`,
      [fromHex(headerHash)],
      (err) => logAbort(`blocks db: clearing error: ${err.message}`)
    );

    await runCommitTransaction(db, "buildAndSubmitMergeTx");
  } catch (err) {
    await runRollbackTransaction(db, "buildAndSubmitMergeTx", err);
  }
};
