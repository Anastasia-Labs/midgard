import {
  fromHex,
  OutRef,
  ScriptType,
  toHex,
  Unit,
  UTxO,
} from "@lucid-evolution/lucid";
import { Option } from "effect";
import { Pool } from "pg";
import { logAbort, logInfo } from "../utils.js";
import * as blocks from "./blocks.js";
import * as confirmedLedger from "./confirmedLedger.js";
import * as immutable from "./immutable.js";
import * as latestLedger from "./latestLedger.js";
import * as mempool from "./mempool.js";
import * as mempoolLedger from "./mempoolLedger.js";

export async function initializeDb(pool: Pool) {
  try {
    await pool.query(`
      SET default_transaction_isolation TO 'serializable';
    `);

    await pool.query(blocks.createQuery);
    await pool.query(mempool.createQuery);
    await pool.query(mempoolLedger.createQuery);
    await pool.query(immutable.createQuery);
    await pool.query(confirmedLedger.createQuery);
    await pool.query(latestLedger.createQuery);

    logInfo("Connected to the PostgreSQL database");
    return pool;
  } catch (err) {
    logAbort(`Error initializing database: ${err}`);
    throw err;
  }
}

export const insertUTxOs = async (
  pool: Pool,
  tableName: string,
  assetTableName: string,
  utxos: UTxO[],
): Promise<void> => {
  const values = utxos.flatMap((utxo) => Object.values(utxoToRow(utxo)));
  const query = `
    INSERT INTO ${tableName}
      (tx_hash, output_index, address, datum_hash, datum, script_ref_type, script_ref_script)
    VALUES
    ${utxos.map((_, i) => `($${i * 7 + 1}, $${i * 7 + 2}, $${i * 7 + 3}, $${i * 7 + 4}, $${i * 7 + 5}, $${i * 7 + 6}, $${i * 7 + 7})`).join(", ")}
  `;

  const normalizedAssets = utxos.flatMap((utxo) =>
    utxoToNormalizedAssets(utxo),
  );
  const assetQuery = `
    INSERT INTO ${assetTableName}
      (tx_hash, output_index, unit, quantity)
    VALUES
     ${normalizedAssets.map((_, i) => `($${i * 4 + 1}, $${i * 4 + 2}, $${i * 4 + 3}, $${i * 4 + 4})`).join(", ")}
  `;
  const assetValues = normalizedAssets.flatMap((v) => Object.values(v));
  const client = await pool.connect();

  try {
    await client.query("BEGIN");
    await client.query(query, values);
    // logInfo(`${tableName} db: ${utxos.length} new UTXOs added`);
    await client.query(assetQuery, assetValues);
    // logInfo(
    //   `${tableName}: ${normalizedAssets.length} assets added to ${assetTableName}`,
    // );
    await client.query("COMMIT");
    client.release();
  } catch (err) {
    // logAbort(`${tableName} db: error inserting UTXOs or assets: ${err}`);
    if (client) {
      try {
        await client.query("ROLLBACK");
      } catch (rollbackErr) {
        // logAbort(`Error rolling back: ${rollbackErr}`);
      } finally {
        client.release();
      }
    }
    throw err;
  }
};

export const retrieveUTxOs = async (
  pool: Pool,
  tableName: string,
  assetTableName: string,
): Promise<UTxO[]> => {
  const query = `
    SELECT
      t.tx_hash,
      t.output_index,
      address,
      json_agg(json_build_object('unit', encode(a.unit, 'hex'), 'quantity', a.quantity)) AS assets,
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
  `;
  try {
    const result = await pool.query(query);
    return result.rows.map((r) => utxoFromRow(r));
  } catch (err) {
    // logAbort(`${tableName} db: error retrieving utxos: ${err}`);
    throw err;
  }
};

export const retrieveUTxOsByOutRef = async (
  pool: Pool,
  outRef: OutRef,
  tableName: string,
  assetTableName: string,
): Promise<UTxO[]> => {
  const query = `
    SELECT
      t.tx_hash,
      t.output_index,
      address,
      json_agg(json_build_object('unit', encode(a.unit, 'hex'), 'quantity', a.quantity)) AS assets,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    FROM ${tableName} AS t
      LEFT JOIN ${assetTableName} AS a
        ON t.tx_hash = a.tx_hash AND t.output_index = a.output_index
    WHERE t.tx_hash = ${outRef.txHash} AND t.output_index = ${outRef.outputIndex}
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
  `;
  try {
    const result = await pool.query(query);
    return result.rows.map((r) => utxoFromRow(r));
  } catch (err) {
    // logAbort(`${tableName} db: error retrieving utxos: ${err}`);
    throw err;
  }
};

export const clearUTxOs = async (
  pool: Pool,
  tableName: string,
  refs: OutRef[],
): Promise<void> => {
  const query = `DELETE FROM ${tableName} WHERE (tx_hash, output_index) IN (${refs
    .map((_, i) => `($${i * 2 + 1}, $${i * 2 + 2})`)
    .join(", ")})`;
  const values = refs.flatMap((r) => [
    Buffer.from(r.txHash, "hex"),
    r.outputIndex,
  ]);

  try {
    const result = await pool.query(query, values);
    // logInfo(`${tableName} db: ${result.rowCount} utxos removed`);
  } catch (err) {
    // logAbort(`${tableName} db: utxos removing error: ${err}`);
    throw err;
  }
};

export const clearTxs = async (
  pool: Pool,
  tableName: string,
  txHashes: string[],
): Promise<void> => {
  const query = `DELETE FROM ${tableName} WHERE tx_hash IN (${txHashes
    .map((_, i) => `$${i + 1}`)
    .join(", ")})`;
  const values = txHashes.flatMap((h) => [Buffer.from(h, "hex")]);
  try {
    const result = await pool.query(query, values);
    logInfo(`${tableName} db: ${result.rowCount} txs removed`);
  } catch (err) {
    logAbort(`${tableName} db: txs removing error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  pool: Pool,
  tableName: string,
  txHash: string,
): Promise<Option.Option<string>> => {
  const query = `SELECT tx_cbor FROM ${tableName} WHERE tx_hash = $1`;
  try {
    const result = await pool.query(query, [Buffer.from(txHash, "hex")]);
    if (result.rows.length > 0) {
      return Option.some(result.rows[0].tx_cbor.toString("hex"));
    } else {
      return Option.none();
    }
  } catch (err) {
    // logAbort(`db: retrieving error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  tableName: string,
  txHashes: string[],
): Promise<string[]> => {
  const query = `SELECT tx_cbor FROM ${tableName} WHERE tx_hash = ANY($1)`;
  try {
    const result = await pool.query(query, [
      txHashes.map((hash) => Buffer.from(hash, "hex")),
    ]);
    return result.rows.map((row) => row.tx_cbor.toString("hex"));
  } catch (err) {
    // logAbort(`${tableName} db: retrieving error: ${err}`);
    throw err;
  }
};

export const clearTable = async (
  pool: Pool,
  tableName: string,
): Promise<void> => {
  const query = `TRUNCATE TABLE ${tableName} CASCADE;`;

  try {
    await pool.query(query);
    // logInfo(`${tableName} db: cleared`);
  } catch (err) {
    // logAbort(`${tableName} db: clearing error: ${err}`);
    throw err;
  }
};

export interface UTxOFromRow {
  tx_hash: Uint8Array;
  output_index: number;
  address: string;
  assets: { unit: string; quantity: string }[];
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
  const rowAssets = row.assets === undefined ? [] : row.assets;
  const assets = rowAssets.reduce(
    (acc, { unit, quantity }) => {
      const quantityBigInt = BigInt(quantity);
      const key = unit === "6c6f76656c616365" ? "lovelace" : unit;
      acc[key] = quantityBigInt;
      return acc;
    },
    {} as Record<Unit | "lovelace", bigint>,
  );
  return {
    txHash: toHex(row.tx_hash),
    outputIndex: row.output_index,
    address: row.address,
    assets: assets,
    datumHash: row.datum_hash != null ? toHex(row.datum_hash) : null,
    datum: row.datum != null ? toHex(row.datum) : null,
    scriptRef:
      scriptRefType && row.script_ref_script
        ? { type: scriptRefType, script: toHex(row.script_ref_script) }
        : null,
  };
}

// function parseAssetsString(assets: string): Record<string, bigint> {
//   // Remove curly braces and split into individual tuple strings
//   const tuplesString = assets.replace(/^\{|\}$/g, "").split('","');

//   // Parse each tuple
//   const tuples = tuplesString.map((tupleString) => {
//     // Remove parentheses and split into unit and quantity
//     const cleanedTuple = tupleString.replace(/^\(|\)$/g, "").split(",");
//     const unit = cleanedTuple[0]; // First part is the unit
//     const quantity = cleanedTuple[1]; // Second part is the quantity
//     return [unit, BigInt(quantity)] as [string, bigint]; // Convert quantity to BigInt
//   });

//   // Convert to Record<string, bigint>
//   return tuples.reduce(
//     (acc, [unit, quantity]) => {
//       acc[unit] = quantity;
//       return acc;
//     },
//     {} as Record<string, bigint>
//   );
// }

// // transforms [{"unit":u1,"quantity":q1},...]-like string into assets
// const transformAssetsToObject = (
//   assetsString: string
// ): Record<string, bigint> => {
//   if (!assetsString || assetsString === "null") {
//     return {};
//   }

//   try {
//     const assetsArray: { unit: string; quantity: any }[] =
//       JSON.parse(assetsString);
//     const assetsObject: Record<string, bigint> = {};
//     assetsArray.forEach((asset) => {
//       const unit =
//         asset.unit === "6C6F76656C616365"
//           ? "lovelace"
//           : asset.unit.toLowerCase();
//       assetsObject[unit] = BigInt(asset.quantity);
//     });
//     return assetsObject;
//   } catch (error) {
//     logAbort("error parsing assets:" + error);
//     return {};
//   }
// };

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
