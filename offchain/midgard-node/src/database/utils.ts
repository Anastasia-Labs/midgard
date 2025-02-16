import {
  fromHex,
  OutRef,
  ScriptType,
  toHex,
  UTxO,
} from "@lucid-evolution/lucid";
import { Option } from "effect";
import sqlite3 from "sqlite3";
import { logAbort, logInfo } from "../utils.js";

export const insertUTxOs = async (
  db: sqlite3.Database,
  tableName: string,
  assetTableName: string,
  utxos: UTxO[],
) => {
  const values = utxos.flatMap((utxo) => Object.values(utxoToRow(utxo)));
  const query = `
    INSERT INTO ${tableName}
      (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
    VALUES
    ${utxos.map(() => `(?, ?, ?, ?, ?, ?, ?)`).join(", ")}
  `;
  const normalizedAssets = utxos.flatMap((utxo) =>
    utxoToNormalizedAssets(utxo),
  );
  const assetQuery = `
    INSERT INTO ${assetTableName}
      (tx_hash, output_index, unit, quantity)
    VALUES
     ${normalizedAssets.map(() => `(?, ?, ?, ?)`).join(", ")}
  `;
  const assetValues = normalizedAssets.flatMap((v) => Object.values(v));
  return new Promise<void>((resolve, reject) => {
    db.run("BEGIN TRANSACTION;", (err) => {
      if (err) {
        logAbort(`${tableName} db: error starting transaction: ${err.message}`);
        return reject(err);
      }
      db.run(query, values, (err) => {
        if (err) {
          logAbort(`${tableName} db: error inserting UTXOs: ${err.message}`);
          db.run("ROLLBACK;", () => reject(err));
        } else {
          logInfo(`${tableName} db: ${utxos.length} new UTXOs added`);
          db.run(assetQuery, assetValues, (err) => {
            if (err) {
              logAbort(
                `${tableName} db: error inserting assets: ${err.message}`,
              );
              db.run("ROLLBACK;", () => reject(err));
            } else {
              logInfo(
                `${tableName}: ${normalizedAssets.length} assets added to ${assetTableName}`,
              );
              db.run("COMMIT;", (err) => {
                if (err) {
                  logAbort(
                    `${tableName}: error committing transaction: ${err.message}`,
                  );
                  return reject(err);
                }
                resolve();
              });
            }
          });
        }
      });
    });
  });
};

export const retrieveUTxOs = async (
  db: sqlite3.Database,
  tableName: string,
  assetTableName: string,
): Promise<UTxO[]> => {
  const query = `
    SELECT
      t.tx_hash,
      t.output_index,
      address,
      json_group_array(json_object('unit', hex(a.unit), 'quantity', a.quantity)) AS assets,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    FROM ${tableName} AS t
      LEFT JOIN ${assetTableName} AS a
        ON t.tx_hash = a.tx_hash AND t.output_index = a.output_index
    GROUP BY
      t.tx_hash,
      t.output_index,
      address,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    ORDER BY
      t.tx_hash,
      t.output_index;
    ;
    `;
  return new Promise((resolve, reject) => {
    db.all(query, (err, rows: UTxOFromRow[]) => {
      if (err) {
        logAbort(`${tableName} db: error retrieving utxos: ${err.message}`);
        return reject(err);
      }
      resolve(rows.map((r) => utxoFromRow(r)));
    });
  });
};

export const clearUTxOs = async (
  db: sqlite3.Database,
  tableName: string,
  refs: OutRef[],
) => {
  const query = `DELETE FROM ${tableName} WHERE (tx_hash, output_index) IN (${refs
    .map(() => `(?, ?)`)
    .join(", ")})`;
  const values = refs.flatMap((r) => [fromHex(r.txHash), r.outputIndex]);
  await new Promise<void>((resolve, reject) => {
    db.run(query, values, function (err) {
      if (err) {
        logAbort(`${tableName} db: utxos removing error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`${tableName} db: ${this.changes} utxos removed`);
        resolve();
      }
    });
  });
};

export const retrieveTxCborByHash = async (
  db: sqlite3.Database,
  tableName: string,
  txHash: string,
): Promise<Option.Option<string>> => {
  const result = await retrieveTxCborsByHashes(db, tableName, [txHash]);
  return Option.fromIterable(result);
};

export const retrieveTxCborsByHashes = async (
  db: sqlite3.Database,
  tableName: string,
  txHashes: string[]
): Promise<string[]> => {
  const query = `SELECT tx_cbor FROM ${tableName} WHERE tx_hash IN (${txHashes
    .map(() => `(?)`)
    .join(", ")});`;
  const values = txHashes.map((th) => fromHex(th));
  const result = await new Promise<string[]>((resolve, reject) => {
    db.all(query, values, (err, rows: { tx_cbor: Buffer }[]) => {
      if (err) {
        logAbort(`${tableName} db: retrieving error: ${err.message}`);
        reject(err);
      }
      resolve(rows.map((r) => toHex(new Uint8Array(r.tx_cbor))));
    });
  });
  return result;
};

export const clearTable = async (db: sqlite3.Database, tableName: string) => {
  const query = `DELETE FROM ${tableName};`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, function (err) {
      if (err) {
        logAbort(`${tableName} db: clearing error: ${err.message}`);
        reject(err);
      } else {
        logInfo(`${tableName} db: cleared`);
        resolve();
      }
    });
  });
};

export interface UTxOFromRow {
  tx_hash: Uint8Array;
  output_index: number;
  address: string;
  assets: string;
  datum_hash?: Uint8Array | null;
  datum?: Uint8Array | null;
  script_ref_type?: string | null;
  script_ref_script?: Uint8Array | null;
}

export function utxoFromRow(row: UTxOFromRow): UTxO {
  const scriptRefType: ScriptType | null =
    row.script_ref_type == "Native"
      ? "Native"
      : row.script_ref_type == "PlutusV1"
        ? "PlutusV1"
        : row.script_ref_type == "PlutusV2"
          ? "PlutusV2"
          : row.script_ref_type == "PlutusV3"
            ? "PlutusV3"
            : null;
  const assets = JSON.parse(row.assets, (_, v) => {
    try {
      return BigInt(v);
    } catch {
      return v;
    }
  });
  return {
    txHash: toHex(row.tx_hash),
    outputIndex: row.output_index,
    address: row.address,
    assets: transformAssetsToObject(row.assets),
    datumHash: row.datum_hash != null ? toHex(row.datum_hash) : null,
    datum: row.datum != null ? toHex(row.datum) : null,
    scriptRef:
      scriptRefType && row.script_ref_script
        ? { type: scriptRefType, script: toHex(row.script_ref_script) }
        : null,
  };
}

// transforms [{"unit":u1,"quantity":q1},...]-like string into assets
const transformAssetsToObject = (
  assetsString: string,
): Record<string, bigint> => {
  if (!assetsString || assetsString === "null") {
    return {};
  }

  try {
    const assetsArray: { unit: string; quantity: any }[] =
      JSON.parse(assetsString);
    const assetsObject: Record<string, bigint> = {};
    assetsArray.forEach((asset) => {
      const unit =
        asset.unit == "6C6F76656C616365"
          ? "lovelace"
          : asset.unit.toLowerCase();
      assetsObject[unit] = BigInt(asset.quantity);
    });
    return assetsObject;
  } catch (error) {
    logAbort("error parsing assets:" + error);
    return {};
  }
};

export interface UTxOToRow {
  tx_hash: Uint8Array;
  output_index: number;
  address: string;
  datum_hash?: Uint8Array | null;
  datum?: Uint8Array | null;
  script_ref_type?: string | null;
  script_ref_script?: Uint8Array | null;
}

export function utxoToRow(utxo: UTxO): UTxOToRow {
  return {
    tx_hash: fromHex(utxo.txHash),
    output_index: utxo.outputIndex,
    address: utxo.address,
    datum_hash: utxo.datumHash != null ? fromHex(utxo.datumHash) : null,
    datum: utxo.datum != null ? fromHex(utxo.datum) : null,
    script_ref_type: utxo.scriptRef?.type || null,
    script_ref_script:
      utxo.scriptRef?.script != null ? fromHex(utxo.scriptRef?.script) : null,
  };
}

export interface NormalizedAsset {
  tx_hash: Uint8Array;
  output_index: number;
  unit: Uint8Array;
  quantity: string; //sqlite threats bigInt as null
}

export function utxoToNormalizedAssets(utxo: UTxO): NormalizedAsset[] {
  return Object.entries(utxo.assets).flatMap(([unit, quantity]) => {
    const asset: NormalizedAsset = {
      tx_hash: fromHex(utxo.txHash),
      output_index: utxo.outputIndex,
      unit: unit == "lovelace" ? fromHex("6c6f76656c616365") : fromHex(unit),
      quantity: quantity.toString(), //sqlite threats bigInt as null
    };
    return asset;
  });
}
