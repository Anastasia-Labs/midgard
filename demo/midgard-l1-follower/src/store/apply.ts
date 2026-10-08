import {
  assetsToJson,
  mintPolicies,
  redeemersToJson,
  withdrawalsToJson,
} from "../codec.js";
import type { SqlTx, SqlValue } from "../sql/backend.js";
import type { BlockSummary, Cursor, OutRef, TrackedSet } from "../types.js";
import type { StoreContext } from "./context.js";
import { type QualifiedTx, qualifyBlock } from "./qualify.js";
import {
  encodeOutRefs,
  insertOutputs,
  insertRows,
  readCursor,
  StoreIntegrityError,
} from "./rows.js";

export type ApplyRejection = Readonly<{
  kind: "rejected";
  reason: "not_initialized" | "not_on_cursor";
  detail: string;
}>;

export type BlockApplied = Readonly<{
  kind: "applied";
  cursor: Cursor;
  qualified: readonly QualifiedTx[];
  /** Tracked outrefs the block left live (created and not spent in it). */
  created: readonly OutRef[];
  /** Tracked outrefs the block spent. */
  spent: readonly OutRef[];
}>;

const TX_INSERT_COLUMNS = [
  "tx_hash",
  "block_slot",
  "block_tx_index",
  "is_valid",
  "inputs",
  "reference_inputs",
  "collaterals",
  "output_count",
  "has_collateral_return",
  "mint",
  "withdrawals",
  "redeemers",
  "invalid_before",
  "invalid_after",
  "body_cbor",
  "witness_cbor",
  "aux_cbor",
] as const;

const continuityProblem = (
  cursor: Cursor,
  block: BlockSummary,
): string | null => {
  if (block.parentHash === null || !block.parentHash.equals(cursor.point.hash))
    return `parent ${block.parentHash?.toString("hex") ?? "none"} is not the cursor ${cursor.point.hash.toString("hex")}`;
  if (block.height !== cursor.height + 1)
    return `height ${block.height} does not follow the cursor height ${cursor.height}`;
  if (block.point.slot <= cursor.point.slot)
    return `slot ${block.point.slot} is not above the cursor slot ${cursor.point.slot}`;
  return null;
};

/**
 * Applies one block in the caller's write transaction (the sequential
 * writer, §6 item 4 with one transaction per block): qualification against
 * the tracked-outref set, fact rows, the S3 derivations, then the cursor.
 */
export const applyBlockIn = async (
  tx: SqlTx,
  context: StoreContext,
  block: BlockSummary,
  tracked: TrackedSet,
  isLive: (key: string) => boolean,
): Promise<BlockApplied | ApplyRejection> => {
  const { dialect } = context;
  const previous = await readCursor(tx, dialect, "update");
  if (previous === null)
    return {
      kind: "rejected",
      reason: "not_initialized",
      detail: "no cursor row",
    };
  const problem = continuityProblem(previous, block);
  if (problem !== null)
    return { kind: "rejected", reason: "not_on_cursor", detail: problem };
  const slot = block.point.slot;
  const qualified = qualifyBlock(block, tracked, isLive);
  await tx.query(
    "INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count) VALUES (?, ?, ?, ?, ?)",
    [slot, block.point.hash, block.height, block.parentHash, qualified.length],
  );
  await insertRows(
    tx,
    "l1_txs",
    TX_INSERT_COLUMNS,
    qualified.map(({ tx: summary }): SqlValue[] => [
      summary.hash,
      slot,
      summary.index,
      dialect.bool(summary.isValid),
      dialect.outRefList(encodeOutRefs(summary.inputs)),
      dialect.outRefList(encodeOutRefs(summary.referenceInputs)),
      dialect.outRefList(encodeOutRefs(summary.collaterals)),
      summary.outputs.length,
      dialect.bool(summary.collateralReturn !== null),
      dialect.json(assetsToJson(summary.mint)),
      dialect.json(withdrawalsToJson(summary.withdrawals)),
      dialect.json(redeemersToJson(summary.redeemers)),
      summary.invalidBefore?.toString() ?? null,
      summary.invalidAfter?.toString() ?? null,
      summary.bodyCbor,
      summary.witnessCbor,
      summary.auxCbor,
    ]),
  );
  await insertRows(
    tx,
    "l1_tx_mint_policies",
    ["tx_hash", "policy_id"],
    qualified.flatMap(({ tx: summary }) =>
      mintPolicies(summary.mint).map((policy): SqlValue[] => [
        summary.hash,
        Buffer.from(policy, "hex"),
      ]),
    ),
  );
  await insertOutputs(
    tx,
    dialect,
    qualified.flatMap(({ tx: summary, created }) =>
      created.map(({ outRef, output }) => ({
        outRef,
        output,
        placement: { kind: "created" as const, slot, txIndex: summary.index },
      })),
    ),
  );
  const spent: OutRef[] = [];
  for (const { tx: summary, spent: consumed } of qualified)
    for (const outRef of consumed) {
      const rows = await tx.query(
        "UPDATE l1_outputs SET spent_slot = ?, spent_tx = ? WHERE tx_hash = ? AND output_index = ? AND spent_slot IS NULL RETURNING output_index",
        [slot, summary.hash, outRef.txHash, outRef.index],
      );
      if (rows.length !== 1)
        throw new StoreIntegrityError(
          `tracked outref ${outRef.txHash.toString("hex")}#${outRef.index} is not a live row`,
        );
      spent.push(outRef);
    }
  for (const derivation of context.derivations)
    await derivation.apply({ tx, dialect, block, qualified, previous });
  await tx.query(
    "UPDATE l1_follower_cursor SET slot = ?, hash = ?, height = ?",
    [slot, block.point.hash, block.height],
  );
  const spentKeys = new Set(
    spent.map((outRef) => `${outRef.txHash.toString("hex")}#${outRef.index}`),
  );
  return {
    kind: "applied",
    cursor: { ...previous, point: block.point, height: block.height },
    qualified,
    created: qualified
      .flatMap(({ created }) => created.map(({ outRef }) => outRef))
      .filter(
        (outRef) =>
          !spentKeys.has(`${outRef.txHash.toString("hex")}#${outRef.index}`),
      ),
    spent,
  };
};
