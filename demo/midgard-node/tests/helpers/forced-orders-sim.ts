/**
 * The forced-order projection in the fork simulator (plan §12, N10): its
 * traffic (carriage publications, orders whose carriage the same block or an
 * earlier one created, orders over tampered carriage, phase-2 failed orders,
 * spends of carriage and of orders) and its check, which compares the
 * projection's table at every step with an independent model of the
 * canonical chain: an order's row exists while its block is canonical, its
 * status follows from where its carriage was created, and it closes in the
 * block that spends it. Once the store pruned, the model drops orders spent
 * at or before the pruned slot (`closed_k_deep`). The follower's key set
 * holds a `forced` key for every order whose block is canonical, spent and
 * pruned or not (N10b): a rollback that removes an order removes its key.
 */
import {
  type BlockSummary,
  decodeBlock,
  type FactStore,
  type FollowerProjection,
  type OutRef,
} from "@al-ft/midgard-l1-follower";
import {
  outRefHex,
  type ScenarioTraffic,
  SIM_ORIGIN,
  type SimTx,
  simTxHash,
} from "@al-ft/midgard-l1-follower/testing";

import {
  decodeFieldPreimages,
  FORCED_ORDERS_TABLE,
  forcedOrderProjection,
  forcedOrdersAt,
} from "../../src/forced-orders/index.js";
import {
  FORCED_CONFIG,
  nativeTransactionCbor,
  orderTx,
  publicationTx,
  publishedOrderMaterial,
  WALLET,
} from "./forced-orders-chain.js";

/** What the corpus exercised, so a suite can prove each case happened. */
export type ForcedOrderSimStats = {
  /** Orders whose carriage an earlier tx of the same block created. */
  resolved: number;
  /** Orders whose carriage an earlier block created. */
  pending: number;
  /** Orders over tampered carriage of the same block. */
  malformed: number;
  /** Phase-2 failed order txs (no row). */
  failedOrders: number;
  /** Orders a later block spent. */
  spentOrders: number;
  /** Orders whose block a rollback orphaned. */
  orphanedOrders: number;
  /** Spent orders whose row a prune removed. */
  prunedOrders: number;
  checks: number;
};

export const zeroForcedOrderSimStats = (): ForcedOrderSimStats => ({
  resolved: 0,
  pending: 0,
  malformed: 0,
  failedOrders: 0,
  spentOrders: 0,
  orphanedOrders: 0,
  prunedOrders: 0,
  checks: 0,
});

type Material = ReturnType<typeof publishedOrderMaterial>;

let materials: Material[] | undefined;
/** Two real order materials, built once (the SDK planner is not free). */
const orderMaterials = (): Material[] =>
  (materials ??= [
    [0x11, 0x22],
    [0x33, 0x44],
  ].map((fills) => publishedOrderMaterial(nativeTransactionCbor(fills))));

const tampered = (bytes: Buffer): Buffer => {
  const copy = Buffer.from(bytes);
  copy[copy.length - 1] ^= 0xff;
  return copy;
};

/** An order the traffic built, by its tx hash. */
type Issued = Readonly<{
  carriage: OutRef;
  tampered: boolean;
  preimage: Buffer;
}>;

type ModelOrder = {
  status: "resolved" | "carriage_pending" | "malformed";
  blockHash: string;
  parentHash: string;
  spentSlot: number | null;
  preimage: Buffer;
};

/** The model's rows of a chain, keyed by order outref. */
const modelOf = (
  blocks: readonly BlockSummary[],
  issued: ReadonlyMap<string, Issued>,
): Map<string, ModelOrder> => {
  const orders = new Map<string, ModelOrder>();
  let parentHash = SIM_ORIGIN.point.hash.toString("hex");
  for (const block of blocks) {
    const created = new Set<string>();
    for (const tx of block.txs) {
      if (!tx.isValid) continue;
      for (const input of tx.inputs) {
        const order = orders.get(outRefHex(input));
        if (order !== undefined && order.spentSlot === null)
          order.spentSlot = block.point.slot;
      }
      const meta = issued.get(tx.hash.toString("hex"));
      if (meta !== undefined) {
        const inBlock = created.has(outRefHex(meta.carriage));
        orders.set(outRefHex({ txHash: tx.hash, index: 0 }), {
          status: !inBlock
            ? "carriage_pending"
            : meta.tampered
              ? "malformed"
              : "resolved",
          blockHash: block.point.hash.toString("hex"),
          parentHash,
          spentSlot: null,
          preimage: meta.preimage,
        });
      }
      tx.outputs.forEach((_, index) =>
        created.add(outRefHex({ txHash: tx.hash, index })),
      );
    }
    parentHash = block.point.hash.toString("hex");
  }
  return orders;
};

const projectedRows = async (store: FactStore) =>
  (
    await store.transaction("read", (tx) =>
      tx.query(
        `SELECT order_tx_hash, order_output_index, status, block_hash, parent_hash, spent_slot, field_preimages FROM ${FORCED_ORDERS_TABLE}`,
      ),
    )
  ).map((row) => {
    const preimages =
      row.field_preimages == null
        ? null
        : decodeFieldPreimages(
            Buffer.from(row.field_preimages as Uint8Array),
          ).map((p) => p.toString("hex"));
    return {
      outRef: outRefHex({
        txHash: Buffer.from(row.order_tx_hash as Uint8Array),
        index: Number(row.order_output_index),
      }),
      status: String(row.status),
      blockHash: Buffer.from(row.block_hash as Uint8Array).toString("hex"),
      parentHash: Buffer.from(row.parent_hash as Uint8Array).toString("hex"),
      spentSlot: row.spent_slot == null ? null : Number(row.spent_slot),
      carriesPreimage: preimages,
    };
  });

/** The outrefs of the follower's `forced` keys, each its own origin. */
const forcedKeys = async (store: FactStore): Promise<string[]> =>
  (
    await store.transaction("read", (tx) =>
      tx.query(
        "SELECT key, origin_outref FROM l1_event_keys WHERE kind = 'forced'",
      ),
    )
  )
    .map((row) => {
      const key = Buffer.from(row.key as Uint8Array);
      if (!key.equals(Buffer.from(row.origin_outref as Uint8Array)))
        return `mismatch:${key.toString("hex")}`;
      return outRefHex({
        txHash: key.subarray(0, 32),
        index: key.readUInt16BE(32),
      });
    })
    .sort();

const byKey = <T extends { outRef: string }>(rows: T[]): T[] =>
  [...rows].sort((a, b) => (a.outRef < b.outRef ? -1 : 1));

const difference = (label: string, a: unknown, b: unknown): string | null => {
  const left = JSON.stringify(a);
  const right = JSON.stringify(b);
  return left === right
    ? null
    : `${label}: projection ${left.slice(0, 600)} vs model ${right.slice(0, 600)}`;
};

/**
 * The projection with traffic and the model check. One per scenario run:
 * the traffic remembers the orders it issued, and the stats accumulate.
 */
export const forcedOrderSimProjection = (
  stats: ForcedOrderSimStats,
): FollowerProjection => {
  const issued = new Map<string, Issued>();
  /** Publications of earlier blocks, for orders that reference them later. */
  const published: { outRef: OutRef; tampered: boolean; material: Material }[] =
    [];
  const orderAddress = FORCED_CONFIG.orderAddress;

  const traffic: ScenarioTraffic = ({ chain, rng, claim }) => {
    const txs: SimTx[] = [];
    const inclusionTime = BigInt(chain.tip.point.slot + rng.range(1, 50));
    const order = (
      material: Material,
      carriage: OutRef,
      isTampered: boolean,
    ): SimTx => {
      const tx = orderTx({
        material: material.material,
        nonceInput: chain.outsideInput(),
        carriage: [carriage],
        inclusionTime,
        nonce: chain.nonce(),
      });
      issued.set(simTxHash(tx).toString("hex"), {
        carriage,
        tampered: isTampered,
        preimage: material.preimage,
      });
      return tx;
    };
    const publish = (material: Material, isTampered: boolean) => {
      const tx = publicationTx(
        isTampered ? tampered(material.preimage) : material.preimage,
        chain.outsideInput(),
        chain.nonce(),
      );
      txs.push(tx);
      return { txHash: simTxHash(tx), index: 0 };
    };
    // Spends first: an order or a carriage output created before this block.
    const live = chain.live();
    const orders = live.filter(
      (u) => u.output.address.toString("hex") === orderAddress,
    );
    if (orders.length > 0 && rng.chance(0.3)) {
      const target = rng.pick(orders);
      if (claim(target.outRef))
        txs.push({
          inputs: [target.outRef],
          outputs: [{ address: WALLET, lovelace: 2_000_000n }],
          nonce: chain.nonce(),
        });
    }
    const liveKeys = new Set(live.map((u) => outRefHex(u.outRef)));
    const carriage = published.filter((p) => liveKeys.has(outRefHex(p.outRef)));
    if (carriage.length > 0 && rng.chance(0.2)) {
      const target = rng.pick(carriage);
      if (claim(target.outRef))
        txs.push({
          inputs: [target.outRef],
          outputs: [{ address: WALLET, lovelace: 1_000_000n }],
          nonce: chain.nonce(),
        });
    }
    const roll = rng.int(6);
    const material = rng.pick(orderMaterials());
    if (roll === 0 || roll === 1) {
      // Same block: the publication, then the order (tampered one time in four).
      const isTampered = rng.chance(0.25);
      txs.push(order(material, publish(material, isTampered), isTampered));
    } else if (roll === 2 && carriage.length > 0) {
      // Carriage from an earlier block (still live, or spent since: §12.3 steps 2-4).
      const target = rng.pick(carriage);
      txs.push(order(target.material, target.outRef, target.tampered));
    } else if (roll === 3) {
      const isTampered = rng.chance(0.2);
      published.push({
        outRef: publish(material, isTampered),
        tampered: isTampered,
        material,
      });
    } else if (roll === 4) {
      // A phase-2 failed order: it mints nothing and opens no row.
      const tx = order(material, publish(material, false), false);
      issued.delete(simTxHash(tx).toString("hex"));
      txs.push({ ...tx, isValid: false, collaterals: [chain.outsideInput()] });
      stats.failedOrders += 1;
    }
    return txs;
  };

  // The check runs after each event; it keeps the canonical blocks itself.
  const canonical: BlockSummary[] = [];
  const seen = new Map<string, string>();
  const spentSeen = new Set<string>();
  const prunedSeen = new Set<string>();
  const check: FollowerProjection["check"] = async ({ store, step }) => {
    stats.checks += 1;
    const { event } = step;
    if (event.kind === "roll_forward") canonical.push(decodeBlock(event.block));
    else {
      const target =
        event.point.kind === "point" ? event.point.hash.toLowerCase() : null;
      while (
        canonical.length > 0 &&
        canonical[canonical.length - 1]!.point.hash.toString("hex") !== target
      )
        canonical.pop();
    }
    const full = modelOf(canonical, issued);
    const prunedThrough =
      (await store.cursor())?.prunedThroughSlot ?? SIM_ORIGIN.point.slot;
    const expected = byKey(
      [...full]
        .filter(([, o]) => o.spentSlot === null || o.spentSlot > prunedThrough)
        .map(([outRef, o]) => ({
          outRef,
          status: o.status,
          blockHash: o.blockHash,
          parentHash: o.parentHash,
          spentSlot: o.spentSlot,
          carriesPreimage:
            o.status === "resolved" ? [o.preimage.toString("hex")] : null,
        })),
    );
    // Of the nine stored preimages, the carried field's is the published one.
    const projected = byKey(await projectedRows(store)).map((row) => {
      const published = full.get(row.outRef)?.preimage.toString("hex");
      return {
        ...row,
        carriesPreimage:
          row.carriesPreimage === null
            ? null
            : row.carriesPreimage.filter((p) => p === published),
      };
    });
    const rows = difference("forced-order rows", projected, expected);
    if (rows !== null) return rows;
    // The reader at the tip: exactly the unspent orders, at their parent points.
    const tip = canonical[canonical.length - 1]?.point ?? SIM_ORIGIN.point;
    const read = await forcedOrdersAt(store, FORCED_CONFIG, tip);
    if (read.kind !== "ok") return `forcedOrdersAt: ${read.kind}`;
    const live = difference(
      "live forced orders",
      read.orders
        .map(
          (o) =>
            `${outRefHex(o.outRef)}:${o.status}:${o.parent.hash.toString("hex")}`,
        )
        .sort(),
      [...full]
        .filter(([, o]) => o.spentSlot === null)
        .map(([outRef, o]) => `${outRef}:${o.status}:${o.parentHash}`)
        .sort(),
    );
    if (live !== null) return live;
    // The key set: every order of the canonical chain, spent or pruned too.
    const keys = difference(
      "forced keys",
      await forcedKeys(store),
      [...full.keys()].sort(),
    );
    if (keys !== null) return keys;
    // Case counters (the model agreed, so these describe the projection).
    for (const [outRef, o] of full) {
      if (!seen.has(outRef)) {
        if (o.status === "resolved") stats.resolved += 1;
        else if (o.status === "malformed") stats.malformed += 1;
        else stats.pending += 1;
      }
      seen.set(outRef, o.blockHash);
      if (o.spentSlot !== null && !spentSeen.has(outRef)) {
        spentSeen.add(outRef);
        stats.spentOrders += 1;
      }
      if (
        o.spentSlot !== null &&
        o.spentSlot <= prunedThrough &&
        !prunedSeen.has(outRef)
      ) {
        prunedSeen.add(outRef);
        stats.prunedOrders += 1;
      }
    }
    for (const outRef of [...seen.keys()])
      if (!full.has(outRef)) {
        seen.delete(outRef);
        spentSeen.delete(outRef);
        stats.orphanedOrders += 1;
      }
    return null;
  };

  return {
    ...forcedOrderProjection(FORCED_CONFIG),
    traffic,
    check,
    // Order outputs are script-locked on L1: only this traffic spends them.
    // Carriage at the wallet stays open to the filler (a carriage spent by
    // anyone is the case §12.3 steps 2 to 4 exist for).
    protects: (output) => output.address.toString("hex") === orderAddress,
  };
};
