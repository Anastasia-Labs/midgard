import {
  asBuffer,
  asNumber,
  type Dialect,
  type SqlTx,
  type SqlValue,
} from "../sql/backend.js";
import type {
  Cursor,
  OutRef,
  Point,
  StoredBlock,
  StoredOutput,
  StoredTx,
} from "../types.js";
import {
  BLOCK_COLUMNS,
  readCursor,
  STORED_OUTPUT_FROM,
  STORED_OUTPUT_SELECT,
  storedBlockFromRow,
  storedOutputFromRow,
  storedTxFromRow,
  TX_COLUMNS,
} from "./rows.js";

/** Why a read at a point cannot be answered. */
export type PointRefusal = Readonly<{
  kind: "point_not_canonical" | "point_beyond_retention" | "not_initialized";
  detail: string;
}>;

export type PointStatus =
  | Readonly<{ kind: "canonical"; height: number; depth: number }>
  | PointRefusal;

export type UtxoRead =
  | Readonly<{ kind: "ok"; utxos: readonly StoredOutput[] }>
  | PointRefusal;

/**
 * Where a point stands against the stored chain. `depth` is 0 at the cursor.
 * A point below `prunedThroughSlot` is `point_beyond_retention` even if its
 * block row survived as a checkpoint: facts there are no longer complete.
 */
export const pointStatusIn = async (
  tx: SqlTx,
  dialect: Dialect,
  point: Point,
): Promise<PointStatus> => {
  const cursor = await readCursor(tx, dialect);
  if (cursor === null)
    return { kind: "not_initialized", detail: "no cursor row" };
  if (point.slot < cursor.prunedThroughSlot)
    return {
      kind: "point_beyond_retention",
      detail: `slot ${point.slot} is below the retained window (slot ${cursor.prunedThroughSlot})`,
    };
  const rows = await tx.query(
    "SELECT height FROM l1_blocks WHERE slot = ? AND hash = ?",
    [point.slot, point.hash],
  );
  const row = rows[0];
  if (row === undefined)
    return {
      kind: "point_not_canonical",
      detail: `${point.hash.toString("hex")} at slot ${point.slot} is not on the stored chain`,
    };
  const height = asNumber(row.height);
  return { kind: "canonical", height, depth: cursor.height - height };
};

const liveClause = (at: number | null): string =>
  at === null
    ? "o.spent_slot IS NULL"
    : "(o.created_slot IS NULL OR o.created_slot <= ?) AND (o.spent_slot IS NULL OR o.spent_slot > ?)";

const liveParams = (at: number | null): SqlValue[] =>
  at === null ? [] : [at, at];

const ORDER = " ORDER BY o.tx_hash, o.output_index";

/** A UTxO filter for `liveUtxosIn`. */
export type UtxoFilter =
  | Readonly<{ by: "address"; address: Buffer }>
  | Readonly<{ by: "payment_credential"; hash: Buffer }>
  | Readonly<{ by: "unit"; policyId: Buffer; assetName?: Buffer }>
  | Readonly<{ by: "outref"; outRefs: readonly OutRef[] }>;

const filterSql = (
  filter: UtxoFilter,
): { join: string; where: string; params: SqlValue[] } => {
  switch (filter.by) {
    case "address":
      return { join: "", where: "o.address = ?", params: [filter.address] };
    case "payment_credential":
      return { join: "", where: "o.payment_cred = ?", params: [filter.hash] };
    case "unit":
      return {
        join: "",
        where: `EXISTS (SELECT 1 FROM l1_output_assets a WHERE a.tx_hash = o.tx_hash
          AND a.output_index = o.output_index AND a.policy_id = ?${filter.assetName === undefined ? "" : " AND a.asset_name = ?"})`,
        params:
          filter.assetName === undefined
            ? [filter.policyId]
            : [filter.policyId, filter.assetName],
      };
    case "outref":
      return {
        join: "",
        where:
          filter.outRefs.length === 0
            ? "1 = 0"
            : `(${filter.outRefs.map(() => "(o.tx_hash = ? AND o.output_index = ?)").join(" OR ")})`,
        params: filter.outRefs.flatMap((outRef) => [
          outRef.txHash,
          outRef.index,
        ]),
      };
  }
};

/**
 * Tracked UTxOs live at the tip, or live at `at` (the §5.2 "live at slot s"
 * predicate), matching `filter`, in ledger order.
 */
export const liveUtxosIn = async (
  tx: SqlTx,
  dialect: Dialect,
  filter: UtxoFilter,
  at?: Point,
): Promise<UtxoRead> => {
  if (at !== undefined) {
    const status = await pointStatusIn(tx, dialect, at);
    if (status.kind !== "canonical") return status;
  }
  const slot = at?.slot ?? null;
  const { where, params } = filterSql(filter);
  const rows = await tx.query(
    `SELECT ${STORED_OUTPUT_SELECT} FROM ${STORED_OUTPUT_FROM} WHERE ${where} AND ${liveClause(slot)}${ORDER}`,
    [...params, ...liveParams(slot)],
  );
  return {
    kind: "ok",
    utxos: rows.map((row) => storedOutputFromRow(dialect, row)),
  };
};

/** The stored row of an outref, live or spent (null if never tracked or pruned). */
export const outputIn = async (
  tx: SqlTx,
  dialect: Dialect,
  outRef: OutRef,
): Promise<StoredOutput | null> => {
  const rows = await tx.query(
    `SELECT ${STORED_OUTPUT_SELECT} FROM ${STORED_OUTPUT_FROM} WHERE o.tx_hash = ? AND o.output_index = ?`,
    [outRef.txHash, outRef.index],
  );
  const row = rows[0];
  return row === undefined ? null : storedOutputFromRow(dialect, row);
};

export type Spender =
  | Readonly<{ kind: "unspent" }>
  | Readonly<{ kind: "spent"; txHash: Buffer; slot: number }>
  | Readonly<{ kind: "unknown" }>;

/** Who spent an outref: `unknown` when it is not a tracked row (or was pruned). */
export const spenderOfIn = async (
  tx: SqlTx,
  outRef: OutRef,
): Promise<Spender> => {
  const rows = await tx.query(
    "SELECT spent_tx, spent_slot FROM l1_outputs WHERE tx_hash = ? AND output_index = ?",
    [outRef.txHash, outRef.index],
  );
  const row = rows[0];
  if (row === undefined) return { kind: "unknown" };
  if (row.spent_tx === null) return { kind: "unspent" };
  return {
    kind: "spent",
    txHash: asBuffer(row.spent_tx),
    slot: asNumber(row.spent_slot),
  };
};

export const txByHashIn = async (
  tx: SqlTx,
  dialect: Dialect,
  hash: Buffer,
): Promise<StoredTx | null> => {
  const rows = await tx.query(
    `SELECT ${TX_COLUMNS} FROM l1_txs WHERE tx_hash = ?`,
    [hash],
  );
  const row = rows[0];
  return row === undefined ? null : storedTxFromRow(dialect, row);
};

/** Whether a block hash is on the stored chain (blocks below the window are not stored). */
export const isCanonicalIn = async (
  tx: SqlTx,
  hash: Buffer,
): Promise<boolean> =>
  (await tx.query("SELECT 1 AS one FROM l1_blocks WHERE hash = ?", [hash]))
    .length === 1;

export const blockByHashIn = async (
  tx: SqlTx,
  hash: Buffer,
): Promise<StoredBlock | null> => {
  const row = (
    await tx.query(`SELECT ${BLOCK_COLUMNS} FROM l1_blocks WHERE hash = ?`, [
      hash,
    ])
  )[0];
  return row === undefined ? null : storedBlockFromRow(row);
};

export const blockAtHeightIn = async (
  tx: SqlTx,
  height: number,
): Promise<StoredBlock | null> => {
  const row = (
    await tx.query(`SELECT ${BLOCK_COLUMNS} FROM l1_blocks WHERE height = ?`, [
      height,
    ])
  )[0];
  return row === undefined ? null : storedBlockFromRow(row);
};

/** The highest stored block at or below `slot` (for slot-to-height mapping). */
export const blockAtOrBeforeSlotIn = async (
  tx: SqlTx,
  slot: number,
): Promise<StoredBlock | null> => {
  const row = (
    await tx.query(
      `SELECT ${BLOCK_COLUMNS} FROM l1_blocks WHERE slot <= ? ORDER BY slot DESC LIMIT 1`,
      [slot],
    )
  )[0];
  return row === undefined ? null : storedBlockFromRow(row);
};

export const tipIn = (tx: SqlTx, dialect: Dialect): Promise<Cursor | null> =>
  readCursor(tx, dialect);
