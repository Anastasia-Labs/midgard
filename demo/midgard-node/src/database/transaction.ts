import { fromHex, OutRef, UTxO } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import { logAbort, logInfo } from "../utils.js";
import { utxoToNormalizedAssets, utxoToRow } from "./utils.js";

export const submitTx = async (
  db: sqlite3.Database,
  txHash: string,
  txCBOR: string,
  spent: OutRef[],
  produced: UTxO[]
) => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const normalizedAssets = produced.flatMap((utxo) =>
        utxoToNormalizedAssets(utxo)
      );

      await new Promise<void>((resolve) => {
        db.run("BEGIN TRANSACTION;", function (err) {
          if (err) {
            logAbort(`db: error beggining transaction: ${err.message}`);
            reject(err);
          }
          resolve();
        });
      });

      const mempoolInsertQuery = `
      INSERT INTO mempool (tx_hash, tx_cbor) VALUES (?, ?)`;
      const mempoolInsertValues = [fromHex(txHash), fromHex(txCBOR)];
      await new Promise<void>((resolve) => {
        db.run(mempoolInsertQuery, mempoolInsertValues, function (err) {
          if (err) {
            logAbort(`mempool db: error inserting tx: ${err.message}`);
            reject(err);
          }
          logInfo(`mempool db: tx stored with rowid ${this.lastID}`);
          resolve();
        });
      });

      const mempoolLedgerDeleteQuery = `
      DELETE FROM mempool_ledger WHERE (tx_hash, output_index) IN (${spent
        .map(() => `(?, ?)`)
        .join(", ")})`;
      const mempoolLedgerDeleteValues = spent.flatMap((r) => [
        fromHex(r.txHash),
        r.outputIndex,
      ]);
      await new Promise<void>((resolve) => {
        db.run(
          mempoolLedgerDeleteQuery,
          mempoolLedgerDeleteValues,
          function (err) {
            if (err) {
              logAbort(
                `mempool_ledger db: utxos removing error: ${err.message}`
              );
              reject(err);
            }
            logInfo(`mempool_ledger db: ${this.changes} UTXOs removed`);
            resolve();
          }
        );
      });

      const mempoolLedgerInsertQuery = `
        INSERT INTO mempool_ledger
            (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
          VALUES ${produced.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")};
          `;
      const mempoolLedgerInsertValues = produced.flatMap((utxo) =>
        Object.values(utxoToRow(utxo))
      );
      await new Promise<void>((resolve) => {
        db.run(
          mempoolLedgerInsertQuery,
          mempoolLedgerInsertValues,
          function (err) {
            if (err) {
              logAbort(
                `mempool_ledger db: error inserting UTXOs: ${err.message}`
              );
              reject(err);
            }
            logInfo(`mempool_ledger db: ${produced.length} new UTXOs added`);
            resolve();
          }
        );
      });

      const mempoolLedgerInsertAssetsQuery = `
      INSERT INTO mempool_ledger_assets (tx_hash, output_index, unit, quantity)
        VALUES ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}
        `;
      const mempoolLedgerInsertAssetsValues = normalizedAssets.flatMap((v) =>
        Object.values(v)
      );
      await new Promise<void>((resolve) => {
        db.run(
          mempoolLedgerInsertAssetsQuery,
          mempoolLedgerInsertAssetsValues,
          function (err) {
            if (err) {
              logAbort(
                `mempool_ledger db: error inserting assets: ${err.message}`
              );
              reject(err);
            }
            logInfo(
              `mempool_ledger db: ${normalizedAssets.length} assets added`
            );
            resolve();
          }
        );
      });

      await new Promise<void>((resolve) => {
        db.run("COMMIT;", function (err) {
          if (err) {
            logAbort(`db: error commiting transaction: ${err.message}`);
            reject(err);
          }
          resolve();
        });
      });

      resolve();
    } catch (err) {
      await new Promise<void>((resolve) => {
        db.run("ROLLBACK;", function (err) {
          if (err) {
            logAbort(`db: transaction rollback error: ${err.message}`);
            reject(err);
          }
          resolve();
        });
      });
      throw err;
    }
  });
};

export const submitBlock = async (
  db: sqlite3.Database,
  spent: OutRef[],
  produced: UTxO[],
  txs: { txHash: string; txCbor: string }[]
) => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const normalizedAssets = produced.flatMap((utxo) =>
        utxoToNormalizedAssets(utxo)
      );

      await new Promise<void>((resolve) => {
        db.run("BEGIN TRANSACTION;", function (err) {
          if (err) {
            logAbort(`db: error beggining transaction: ${err.message}`);
            reject(err);
          }
          resolve();
        });
      });

      await new Promise<void>((resolve) => {
        const latestLedgerDeleteQuery = `
          DELETE FROM latest_ledger WHERE (tx_hash, output_index) IN (${spent
            .map(() => `(?, ?)`)
            .join(", ")});`;
        const latestLedgerDeleteValues = spent.flatMap((r) => [
          fromHex(r.txHash),
          r.outputIndex,
        ]);
        db.run(latestLedgerDeleteQuery, latestLedgerDeleteValues, (err) => {
          if (err) {
            logAbort(`latest_ledger db: utxos removing error: ${err.message}`);
            reject(err);
          }
          resolve();
        });
      });

      await new Promise<void>((resolve) => {
        const latestLedgerInsertQuery = `
          INSERT INTO latest_ledger
              (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
            VALUES ${produced.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")};
            `;
        const latestLedgerInsertValues = produced.flatMap((utxo) =>
          Object.values(utxoToRow(utxo))
        );
        db.run(
          latestLedgerInsertQuery,
          latestLedgerInsertValues,
          function (err) {
            if (err) {
              logAbort(
                `latest_ledger db: error inserting UTXOs: ${err.message}`
              );
              reject(err);
            }
            logInfo(`latest_ledger db: ${produced.length} new UTXOs added`);
            resolve();
          }
        );
      });

      const latestLedgerInsertAssetsQuery = `
      INSERT INTO latest_ledger_assets (tx_hash, output_index, unit, quantity)
        VALUES ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}
        `;
      const latestLedgerInsertAssetsValues = normalizedAssets.flatMap((v) =>
        Object.values(v)
      );
      await new Promise<void>((resolve) => {
        db.run(
          latestLedgerInsertAssetsQuery,
          latestLedgerInsertAssetsValues,
          function (err) {
            if (err) {
              logAbort(
                `latest_ledger db: error inserting assets: ${err.message}`
              );
              reject(err);
            }
            logInfo(
              `latest_ledger db: ${normalizedAssets.length} assets added`
            );
            resolve();
          }
        );
      });

      await new Promise<void>((resolve) => {
        db.run(`DELETE FROM mempool;`, function (err) {
          if (err) {
            logAbort(`mempool db: clearing error: ${err.message}`);
            reject(err);
          }
          logInfo(`mempool db: cleared`);
          resolve();
        });
      });

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

      await new Promise<void>((resolve) => {
        db.run("COMMIT;", function (err) {
          if (err) {
            logAbort(`db: error commiting transaction: ${err.message}`);
            reject(err);
          }
          resolve();
        });
      });

      resolve();
    } catch (err) {
      await new Promise<void>((resolve) => {
        db.run("ROLLBACK;", function (err) {
          if (err) {
            logAbort(`db: transaction rollback error: ${err.message}`);
            reject(err);
          }
          resolve();
        });
      });
      throw err;
    }
  });
};
