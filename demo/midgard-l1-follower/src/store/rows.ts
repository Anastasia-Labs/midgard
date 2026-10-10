import {
  assetsFromJson,
  assetsToJson,
  decodeOutRef,
  encodeOutRef,
  redeemersFromJson,
  withdrawalsFromJson,
} from "../codec.js";
import {
  asBuffer,
  asNullableBigInt,
  asNullableBuffer,
  asNullableNumber,
  asNumber,
  asString,
  type Dialect,
  type SqlRow,
  type SqlTx,
  type SqlValue,
} from "../sql/backend.js";
import type {
  Cursor,
  OutputSummary,
  OutRef,
  ScriptType,
  StoredBlock,
  StoredOutput,
  StoredTx,
} from "../types.js";

/** Raised when the stored rows contradict the writer's own bookkeeping. */
export class StoreIntegrityError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "StoreIntegrityError";
  }
}

export const CURSOR_COLUMNS =
  "slot, hash, height, generation, origin_slot, origin_hash, pruned_through_slot";

export const cursorFromRow = (row: SqlRow): Cursor => ({
  point: { slot: asNumber(row.slot), hash: asBuffer(row.hash) },
  height: asNumber(row.height),
  generation: asNumber(row.generation),
  origin: { slot: asNumber(row.origin_slot), hash: asBuffer(row.origin_hash) },
  prunedThroughSlot: asNumber(row.pruned_through_slot),
});

/** The cursor row, optionally row-locked (§7.1 step 1, §8.1). */
export const readCursor = async (
  tx: SqlTx,
  dialect: Dialect,
  lock?: "update" | "share",
): Promise<Cursor | null> => {
  const rows = await tx.query(
    `SELECT ${CURSOR_COLUMNS} FROM l1_follower_cursor${lock === undefined ? "" : dialect.lockClause(lock)}`,
  );
  const row = rows[0];
  return row === undefined ? null : cursorFromRow(row);
};

/** Postgres allows 65,535 parameters per statement and SQLite 32,766. */
const MAX_PARAMETERS = 30_000;

/** Multi-row `INSERT`, chunked under the parameter limit of both backends. */
export const insertRows = async (
  tx: SqlTx,
  table: string,
  columns: readonly string[],
  rows: readonly (readonly SqlValue[])[],
  suffix = "",
): Promise<void> => {
  if (rows.length === 0) return;
  const perStatement = Math.max(1, Math.floor(MAX_PARAMETERS / columns.length));
  const tuple = `(${columns.map(() => "?").join(", ")})`;
  for (let start = 0; start < rows.length; start += perStatement) {
    const chunk = rows.slice(start, start + perStatement);
    await tx.query(
      `INSERT INTO ${table} (${columns.join(", ")}) VALUES ${chunk.map(() => tuple).join(", ")}${suffix}`,
      chunk.flat(),
    );
  }
};

export const OUTPUT_COLUMNS = [
  "tx_hash",
  "output_index",
  "address",
  "payment_cred",
  "payment_cred_is_script",
  "stake_cred",
  "lovelace",
  "assets",
  "datum_hash",
  "datum",
  "script_ref_hash",
  "created_slot",
  "created_tx_index",
  "spent_slot",
  "spent_tx",
  "seed_slot",
] as const;

export type OutputPlacement =
  | Readonly<{ kind: "created"; slot: number; txIndex: number }>
  | Readonly<{ kind: "seed"; seedSlot: number }>;

export const outputRow = (
  dialect: Dialect,
  outRef: OutRef,
  output: OutputSummary,
  placement: OutputPlacement,
): SqlValue[] => [
  outRef.txHash,
  outRef.index,
  output.address,
  output.paymentCredential?.hash ?? null,
  output.paymentCredential === null
    ? null
    : dialect.bool(output.paymentCredential.isScript),
  output.stakeCredential,
  output.lovelace.toString(),
  dialect.json(assetsToJson(output.assets)),
  output.datumHash,
  output.datum,
  output.scriptRef?.hash ?? null,
  placement.kind === "created" ? placement.slot : null,
  placement.kind === "created" ? placement.txIndex : null,
  null,
  null,
  placement.kind === "seed" ? placement.seedSlot : null,
];

export const assetRows = (
  outRef: OutRef,
  output: OutputSummary,
): SqlValue[][] =>
  [...output.assets.entries()].flatMap(([policy, names]) =>
    [...names.entries()].map(([name, quantity]): SqlValue[] => [
      outRef.txHash,
      outRef.index,
      Buffer.from(policy, "hex"),
      Buffer.from(name, "hex"),
      quantity.toString(),
    ]),
  );

/** Inserts outputs with their assets and reference scripts. */
export const insertOutputs = async (
  tx: SqlTx,
  dialect: Dialect,
  outputs: readonly Readonly<{
    outRef: OutRef;
    output: OutputSummary;
    placement: OutputPlacement;
  }>[],
): Promise<void> => {
  const scripts = new Map<string, SqlValue[]>();
  for (const { output } of outputs)
    if (output.scriptRef !== null)
      scripts.set(output.scriptRef.hash.toString("hex"), [
        output.scriptRef.hash,
        output.scriptRef.type,
        output.scriptRef.bytes,
      ]);
  await insertRows(
    tx,
    "l1_scripts",
    ["script_hash", "script_type", "bytes"],
    [...scripts.values()],
    " ON CONFLICT (script_hash) DO NOTHING",
  );
  await insertRows(
    tx,
    "l1_outputs",
    OUTPUT_COLUMNS,
    outputs.map(({ outRef, output, placement }) =>
      outputRow(dialect, outRef, output, placement),
    ),
  );
  await insertRows(
    tx,
    "l1_output_assets",
    ["tx_hash", "output_index", "policy_id", "asset_name", "quantity"],
    outputs.flatMap(({ outRef, output }) => assetRows(outRef, output)),
  );
};

/** Columns for `storedOutputFromRow`, over `l1_outputs o LEFT JOIN l1_scripts s`. */
export const STORED_OUTPUT_SELECT = `o.tx_hash, o.output_index, o.address, o.payment_cred,
  o.payment_cred_is_script, o.stake_cred, o.lovelace, o.assets, o.datum_hash, o.datum,
  o.script_ref_hash, s.script_type, s.bytes AS script_bytes, o.created_slot,
  o.created_tx_index, o.spent_slot, o.spent_tx, o.seed_slot`;

export const STORED_OUTPUT_FROM =
  "l1_outputs o LEFT JOIN l1_scripts s ON s.script_hash = o.script_ref_hash";

const isScriptType = (value: string): value is ScriptType =>
  value === "native" ||
  value === "plutus_v1" ||
  value === "plutus_v2" ||
  value === "plutus_v3";

export const storedOutputFromRow = (
  dialect: Dialect,
  row: SqlRow,
): StoredOutput => {
  const paymentCred = asNullableBuffer(row.payment_cred);
  const scriptHash = asNullableBuffer(row.script_ref_hash);
  let scriptRef: OutputSummary["scriptRef"] = null;
  if (scriptHash !== null) {
    const type = asString(row.script_type);
    if (!isScriptType(type))
      throw new StoreIntegrityError(`unknown script type ${type}`);
    scriptRef = { hash: scriptHash, type, bytes: asBuffer(row.script_bytes) };
  }
  const createdSlot = asNullableNumber(row.created_slot);
  const spentSlot = asNullableNumber(row.spent_slot);
  return {
    outRef: {
      txHash: asBuffer(row.tx_hash),
      index: asNumber(row.output_index),
    },
    output: {
      address: asBuffer(row.address),
      paymentCredential:
        paymentCred === null
          ? null
          : {
              hash: paymentCred,
              isScript: dialect.readBool(row.payment_cred_is_script),
            },
      stakeCredential: asNullableBuffer(row.stake_cred),
      lovelace: BigInt(asString(row.lovelace)),
      assets: assetsFromJson(row.assets),
      datumHash: asNullableBuffer(row.datum_hash),
      datum: asNullableBuffer(row.datum),
      scriptRef,
    },
    created:
      createdSlot === null
        ? null
        : { slot: createdSlot, txIndex: asNumber(row.created_tx_index) },
    seedSlot: asNullableNumber(row.seed_slot),
    spent:
      spentSlot === null
        ? null
        : { slot: spentSlot, txHash: asBuffer(row.spent_tx) },
  };
};

export const TX_COLUMNS = `tx_hash, block_slot, block_tx_index, is_valid, inputs,
  reference_inputs, collaterals, output_count, has_collateral_return, mint,
  withdrawals, redeemers, invalid_before, invalid_after, body_cbor, witness_cbor, aux_cbor`;

export const storedTxFromRow = (dialect: Dialect, row: SqlRow): StoredTx => ({
  hash: asBuffer(row.tx_hash),
  blockSlot: asNumber(row.block_slot),
  blockTxIndex: asNumber(row.block_tx_index),
  isValid: dialect.readBool(row.is_valid),
  inputs: dialect.readOutRefList(row.inputs).map(decodeOutRef),
  referenceInputs: dialect
    .readOutRefList(row.reference_inputs)
    .map(decodeOutRef),
  collaterals: dialect.readOutRefList(row.collaterals).map(decodeOutRef),
  outputCount: asNumber(row.output_count),
  hasCollateralReturn: dialect.readBool(row.has_collateral_return),
  mint: assetsFromJson(row.mint),
  withdrawals: withdrawalsFromJson(row.withdrawals),
  redeemers: redeemersFromJson(row.redeemers),
  invalidBefore: asNullableBigInt(row.invalid_before),
  invalidAfter: asNullableBigInt(row.invalid_after),
  bodyCbor: asBuffer(row.body_cbor),
  witnessCbor: asBuffer(row.witness_cbor),
  auxCbor: asNullableBuffer(row.aux_cbor),
});

export const BLOCK_COLUMNS =
  "slot, hash, height, parent_hash, qualifying_tx_count";

export const storedBlockFromRow = (row: SqlRow): StoredBlock => ({
  slot: asNumber(row.slot),
  hash: asBuffer(row.hash),
  height: asNumber(row.height),
  parentHash: asNullableBuffer(row.parent_hash),
  qualifyingTxCount: asNumber(row.qualifying_tx_count),
});

export const encodeOutRefs = (outRefs: readonly OutRef[]): Buffer[] =>
  outRefs.map(encodeOutRef);
