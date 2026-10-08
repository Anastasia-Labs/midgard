/**
 * Reads of the forced-order projection at a point: each live order with its
 * row and its order output, and the published CEK program material, in one
 * transaction after checking the point is canonical in the follower store.
 */
import {
  decodeOutRef,
  type FactStore,
  liveUtxosIn,
  OUT_REF_BYTES,
  type OutRef,
  type Point,
  type PointRefusal,
  pointStatusIn,
  type StoredOutput,
} from "@al-ft/midgard-l1-follower";
import type { UTxO } from "@lucid-evolution/lucid";

import type { ForcedOrderConfig } from "./config.js";
import { lucidUtxo, outRefLabel } from "./derive.js";
import { FORCED_ORDERS_TABLE } from "./schema.js";

export type ForcedOrderStatus = "resolved" | "carriage_pending" | "malformed";

/** One live forced order at a point. */
export type ForcedOrderRow = Readonly<{
  outRef: OutRef;
  utxo: UTxO;
  status: ForcedOrderStatus;
  /** The point before the order's block (§12.3 step 2 acquires here). */
  parent: Point;
  inclusionTime: bigint;
  referenceInputs: readonly OutRef[];
  mintRedeemer: Buffer | null;
  /** Datums of carriage outputs the order's block created, by outref label. */
  blockDatums: Readonly<Record<string, string | null>>;
  fieldPreimages: Buffer | null;
  detail: string | null;
}>;

export type ForcedOrdersRead =
  | Readonly<{
      kind: "ok";
      orders: readonly ForcedOrderRow[];
      /** Live outputs under the CEK program-material credential. */
      programMaterial: readonly UTxO[];
    }>
  | PointRefusal
  | Readonly<{ kind: "unhealthy"; detail: string }>;

const buf = (value: unknown): Buffer => Buffer.from(value as Uint8Array);
const num = (value: unknown): number => Number(value as number | string);

const decodeReferenceInputs = (bytes: Buffer): OutRef[] => {
  if (bytes.length % OUT_REF_BYTES !== 0)
    throw new Error("stored reference inputs are not whole outrefs");
  const out: OutRef[] = [];
  for (let at = 0; at < bytes.length; at += OUT_REF_BYTES)
    out.push(decodeOutRef(bytes.subarray(at, at + OUT_REF_BYTES)));
  return out;
};

const STATUSES = new Set<string>(["resolved", "carriage_pending", "malformed"]);

/** Every live forced order at `at`, with the CEK material there. */
export const forcedOrdersAt = (
  store: FactStore,
  config: ForcedOrderConfig,
  at: Point,
): Promise<ForcedOrdersRead> =>
  store.transaction("read", async (tx): Promise<ForcedOrdersRead> => {
    const status = await pointStatusIn(tx, store.dialect, at);
    if (status.kind !== "canonical") return status;
    const rows = await tx.query(
      `SELECT order_tx_hash, order_output_index, parent_slot, parent_hash, inclusion_time, status,
         reference_inputs, mint_redeemer, block_datums, field_preimages, detail
       FROM ${FORCED_ORDERS_TABLE}
       WHERE order_slot <= ? AND (spent_slot IS NULL OR spent_slot > ?)
       ORDER BY order_slot, order_tx_hash, order_output_index`,
      [at.slot, at.slot],
    );
    const outRefs = rows.map((row) => ({
      txHash: buf(row.order_tx_hash),
      index: num(row.order_output_index),
    }));
    const live = new Map<string, StoredOutput>();
    if (outRefs.length > 0) {
      const read = await liveUtxosIn(
        tx,
        store.dialect,
        { by: "outref", outRefs },
        at,
      );
      if (read.kind !== "ok") return read;
      for (const utxo of read.utxos) live.set(outRefLabel(utxo.outRef), utxo);
    }
    const material = await liveUtxosIn(
      tx,
      store.dialect,
      {
        by: "payment_credential",
        hash: Buffer.from(config.cekMaterialCredential, "hex"),
      },
      at,
    );
    if (material.kind !== "ok") return material;
    const orders: ForcedOrderRow[] = [];
    for (const [index, row] of rows.entries()) {
      const outRef = outRefs[index]!;
      const output = live.get(outRefLabel(outRef));
      const rowStatus = String(row.status);
      if (output === undefined || !STATUSES.has(rowStatus))
        return {
          kind: "unhealthy",
          detail: `forced order ${outRefLabel(outRef)} has ${output === undefined ? "no live output" : `status ${rowStatus}`}`,
        };
      orders.push({
        outRef,
        utxo: lucidUtxo(outRef, output.output),
        status: rowStatus as ForcedOrderStatus,
        parent: { slot: num(row.parent_slot), hash: buf(row.parent_hash) },
        inclusionTime: BigInt(row.inclusion_time as number | string),
        referenceInputs: decodeReferenceInputs(buf(row.reference_inputs)),
        mintRedeemer: row.mint_redeemer == null ? null : buf(row.mint_redeemer),
        blockDatums: JSON.parse(String(row.block_datums)) as Record<
          string,
          string | null
        >,
        fieldPreimages:
          row.field_preimages == null ? null : buf(row.field_preimages),
        detail: row.detail == null ? null : (row.detail as string),
      });
    }
    return {
      kind: "ok",
      orders,
      programMaterial: material.utxos.map((utxo) =>
        lucidUtxo(utxo.outRef, utxo.output),
      ),
    };
  });
