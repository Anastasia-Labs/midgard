/**
 * Reads of the node event projection at a point (plan §5.5 P1 for the event
 * lists, P2, P4). Each read checks the point is canonical in the follower
 * store and answers from the temporal rows as of that point, in one
 * transaction, so it never mixes two chain states.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { SlotClock } from "../heads.js";
import type { SqlTx } from "../sql/backend.js";
import type { FactStore } from "../store/fact-store.js";
import {
  liveUtxosIn,
  type PointRefusal,
  pointStatusIn,
} from "../store/reads.js";
import type { OutRef, Point, StoredOutput } from "../types.js";
import { eventKeyOfId } from "./by-id.js";
import {
  type EventKind,
  type EventListConfig,
  type SlotTime,
  slotToPosixMs,
} from "./config.js";
import type { RetirementReason } from "./derive.js";
import { EVENTS_TABLE, RETIREMENTS_TABLE } from "./schema.js";

export type Placement = Readonly<{
  blockHash: string;
  slot: number;
  height: number;
  txHash: string;
  txIndex: number;
}>;

/** One event of the projection (P2), as of a point. All bytes are hex. */
export type ProjectedEvent = Readonly<{
  kind: EventKind;
  key: string;
  idCbor: string;
  inclusionTime: bigint;
  factsCbor: string;
  payloadCbor: string;
  originalAssetsCbor: string;
  admission: Placement & Readonly<{ outRef: OutRef }>;
  retirement:
    | (Placement &
        Readonly<{
          outRef: OutRef;
          reason: RetirementReason | null;
          observerRedeemerIndex: number | null;
          witnessCbor: string | null;
        }>)
    | null;
  /** The live Order holding the event's token at the point, or the Order its retirement spent. */
  location: OutRef;
}>;

export type ProjectionRead<T> =
  | Readonly<{ kind: "ok"; value: T }>
  | PointRefusal
  | Readonly<{ kind: "unhealthy"; detail: string }>;

const buf = (value: unknown): Buffer => Buffer.from(value as Uint8Array);
const num = (value: unknown): number => Number(value as number | string);
const hex = (value: unknown): string => buf(value).toString("hex");

/** Runs `read` at `at` if the point is canonical in the store. */
const atPoint = <T>(
  store: FactStore,
  at: Point,
  read: (tx: SqlTx) => Promise<T>,
): Promise<ProjectionRead<T>> =>
  store.transaction("read", async (tx) => {
    const status = await pointStatusIn(tx, store.dialect, at);
    if (status.kind !== "canonical") return status;
    return { kind: "ok", value: await read(tx) };
  });

/** Live list outputs at the point, by asset name (hex; "" is the root). */
const liveListNodes = async (
  tx: SqlTx,
  store: FactStore,
  list: EventListConfig,
  at: Point,
): Promise<Map<string, StoredOutput[]>> => {
  const read = await liveUtxosIn(
    tx,
    store.dialect,
    { by: "unit", policyId: Buffer.from(list.policyId, "hex") },
    at,
  );
  if (read.kind !== "ok") throw new Error(`list read: ${read.detail}`);
  const byName = new Map<string, StoredOutput[]>();
  for (const utxo of read.utxos)
    for (const name of utxo.output.assets.get(list.policyId)?.keys() ?? [])
      byName.set(name, [...(byName.get(name) ?? []), utxo]);
  return byName;
};

const EVENT_COLUMNS = `e.kind, e.event_key, e.event_id, e.inclusion_time, e.facts_cbor, e.payload_cbor,
  e.original_assets_cbor, e.admission_tx_hash, e.admission_output_index, e.admission_tx_index,
  e.admitted_block_hash, e.admitted_height, e.admitted_slot,
  r.retired_slot, r.retirement_tx_hash, r.retirement_tx_index, r.retired_block_hash, r.retired_height,
  r.order_tx_hash, r.order_output_index, r.reason, r.observer_redeemer_index, r.witness_cbor`;

const eventFromRow = (
  row: Record<string, unknown>,
  live: ReadonlyMap<string, readonly StoredOutput[]>,
): ProjectedEvent => {
  const key = hex(row.event_key);
  const retired = row.retired_slot !== null && row.retired_slot !== undefined;
  const retirement = retired
    ? {
        blockHash: hex(row.retired_block_hash),
        slot: num(row.retired_slot),
        height: num(row.retired_height),
        txHash: hex(row.retirement_tx_hash),
        txIndex: num(row.retirement_tx_index),
        outRef: {
          txHash: buf(row.order_tx_hash),
          index: num(row.order_output_index),
        },
        reason: (row.reason ?? null) as RetirementReason | null,
        observerRedeemerIndex:
          row.observer_redeemer_index === null ||
          row.observer_redeemer_index === undefined
            ? null
            : num(row.observer_redeemer_index),
        witnessCbor:
          row.witness_cbor === null || row.witness_cbor === undefined
            ? null
            : hex(row.witness_cbor),
      }
    : null;
  const holders = live.get(key) ?? [];
  if (retirement === null && holders.length !== 1)
    throw new Error(
      `event ${key} has ${holders.length} live token holders at the point`,
    );
  return {
    kind: row.kind as EventKind,
    key,
    idCbor: hex(row.event_id),
    inclusionTime: BigInt(row.inclusion_time as string | number | bigint),
    factsCbor: hex(row.facts_cbor),
    payloadCbor: hex(row.payload_cbor),
    originalAssetsCbor: hex(row.original_assets_cbor),
    admission: {
      blockHash: hex(row.admitted_block_hash),
      slot: num(row.admitted_slot),
      height: num(row.admitted_height),
      txHash: hex(row.admission_tx_hash),
      txIndex: num(row.admission_tx_index),
      outRef: {
        txHash: buf(row.admission_tx_hash),
        index: num(row.admission_output_index),
      },
    },
    retirement,
    location: retirement?.outRef ?? holders[0]!.outRef,
  };
};

/**
 * Every event of `list` admitted at or before the point, live or retired
 * there (P2), oldest admission first. Retired events whose rows pruning
 * removed (retired more than k deep) are absent.
 */
export const eventsAt = (
  store: FactStore,
  list: EventListConfig,
  at: Point,
): Promise<ProjectionRead<ProjectedEvent[]>> =>
  atPoint(store, at, async (tx) => {
    const live = await liveListNodes(tx, store, list, at);
    const rows = await tx.query(
      `SELECT ${EVENT_COLUMNS} FROM ${EVENTS_TABLE} e
        LEFT JOIN ${RETIREMENTS_TABLE} r ON r.kind = e.kind AND r.event_key = e.event_key AND r.retired_slot <= ?
        WHERE e.kind = ? AND e.admitted_slot <= ?
        ORDER BY e.admitted_slot, e.admission_tx_index, e.admission_output_index`,
      [at.slot, list.kind, at.slot],
    );
    return rows.map((row) => eventFromRow(row, live));
  });

/** A cutoff inside a block: the block's point and the last tx index it admits. */
export type EventCutoff = Readonly<{ point: Point; txIndex: number }>;

/** An event's immutable admission content and placement (P2), without its retirement. */
export type AdmittedEventAt = Omit<ProjectedEvent, "retirement" | "location">;

/**
 * The `kind` event whose id CBOR is `idCbor`, if its admission is at or
 * before `cutoff`: in an earlier block, or in the cutoff's block at a tx
 * index no greater than the cutoff's. Null when no such admission is
 * stored; a retired event whose rows pruning removed (retired more than k
 * deep) reads null too, so a caller that needs it past k pins it first. One
 * lookup by key: it never walks the list.
 */
export const eventAdmittedThrough = (
  store: FactStore,
  kind: EventKind,
  idCbor: string,
  cutoff: EventCutoff,
): Promise<ProjectionRead<AdmittedEventAt | null>> =>
  atPoint(store, cutoff.point, async (tx) => {
    const id = Buffer.from(idCbor, "hex");
    const [row] = await tx.query(
      `SELECT kind, event_key, event_id, inclusion_time, facts_cbor, payload_cbor,
         original_assets_cbor, admission_tx_hash, admission_output_index, admission_tx_index,
         admitted_block_hash, admitted_height, admitted_slot
        FROM ${EVENTS_TABLE}
        WHERE kind = ? AND event_key = ?
          AND (admitted_slot < ? OR (admitted_slot = ? AND admission_tx_index <= ?))`,
      [
        kind,
        eventKeyOfId(id),
        cutoff.point.slot,
        cutoff.point.slot,
        cutoff.txIndex,
      ],
    );
    if (row === undefined) return null;
    if (!buf(row.event_id).equals(id))
      throw new Error("event key names another event id");
    return {
      kind: row.kind as EventKind,
      key: hex(row.event_key),
      idCbor: hex(row.event_id),
      inclusionTime: BigInt(row.inclusion_time as string | number | bigint),
      factsCbor: hex(row.facts_cbor),
      payloadCbor: hex(row.payload_cbor),
      originalAssetsCbor: hex(row.original_assets_cbor),
      admission: {
        blockHash: hex(row.admitted_block_hash),
        slot: num(row.admitted_slot),
        height: num(row.admitted_height),
        txHash: hex(row.admission_tx_hash),
        txIndex: num(row.admission_tx_index),
        outRef: {
          txHash: buf(row.admission_tx_hash),
          index: num(row.admission_output_index),
        },
      },
    };
  });

/** One deposit the block builder must include by its end time. */
export type DueDeposit = Readonly<{
  key: string;
  idCbor: string;
  inclusionTime: bigint;
  retired: boolean;
}>;

/**
 * The block builder's eligibility read: the deposits admitted at the point
 * whose inclusion time is due by `cutoffSlot` (`inclusion_time <= endTime`).
 * Being due says which events a block must include; it is not spendability
 * (`spendableAt`). The caller passes the heads module's `slotNow`; a wall
 * clock never reaches here. Retired deposits are included, marked, while
 * their rows are retained.
 */
export const dueByCutoff = (
  store: FactStore,
  list: EventListConfig,
  at: Point,
  cutoff: Readonly<{ slot: number; slotTime: SlotTime }>,
): Promise<ProjectionRead<DueDeposit[]>> => {
  if (list.kind !== "deposit")
    throw new Error("due deposits read the deposit list");
  const cutoffMs = slotToPosixMs(cutoff.slotTime, cutoff.slot);
  return atPoint(store, at, async (tx) =>
    (
      await tx.query(
        `SELECT event_key, event_id, inclusion_time, retired_slot FROM ${EVENTS_TABLE}
          WHERE kind = ? AND inclusion_time <= ? AND admitted_slot <= ?
          ORDER BY inclusion_time, event_id`,
        [list.kind, cutoffMs, at.slot],
      )
    ).map((row) => ({
      key: hex(row.event_key),
      idCbor: hex(row.event_id),
      inclusionTime: BigInt(row.inclusion_time as string | number | bigint),
      retired:
        row.retired_slot !== null &&
        row.retired_slot !== undefined &&
        num(row.retired_slot) <= at.slot,
    })),
  );
};

/**
 * `dueByCutoff` with the heads module's `slotNow` as the cutoff. Before the
 * clock has observed a tip there is no "now", so nothing is due.
 */
export const dueByCutoffNow = async (
  store: FactStore,
  list: EventListConfig,
  at: Point,
  clock: Pick<SlotClock, "slotNow">,
  slotTime: SlotTime,
): Promise<
  ProjectionRead<DueDeposit[]> | Readonly<{ kind: "no_slot_now" }>
> => {
  const slot = clock.slotNow();
  if (slot === null) return { kind: "no_slot_now" };
  return dueByCutoff(store, list, at, { slot, slotTime });
};

/** One deposit spendable on L2 at the point (P4). */
export type SpendableDeposit = Readonly<{ key: string; idCbor: string }>;

/**
 * P4: a deposit is spendable iff it is included(h) for an own block h that
 * landed or is the one live own block (P3; N3 adds foreign blocks), and its
 * admission is canonical at the point. `included` holds the ids (event id
 * CBOR, hex) those blocks include. Time never makes a deposit spendable: a
 * clock ahead of the tip changes nothing here, and a rollback past the
 * admission removes it.
 */
export const spendableAt = (
  store: FactStore,
  list: EventListConfig,
  at: Point,
  included: ReadonlySet<string>,
): Promise<ProjectionRead<SpendableDeposit[]>> => {
  if (list.kind !== "deposit")
    throw new Error("spendable deposits read the deposit list");
  return atPoint(store, at, async (tx) =>
    (
      await tx.query(
        `SELECT event_key, event_id FROM ${EVENTS_TABLE}
          WHERE kind = ? AND admitted_slot <= ? ORDER BY event_id`,
        [list.kind, at.slot],
      )
    )
      .map((row) => ({ key: hex(row.event_key), idCbor: hex(row.event_id) }))
      .filter((deposit) => included.has(deposit.idCbor)),
  );
};

/** The list's linked walk at the point (P1 for the event lists). */
export type EventList = Readonly<{
  /** Keys in list order, root excluded. */
  keys: readonly string[];
  orders: number;
  fillers: number;
}>;

/**
 * Walks the list from its root at the point. Healthy means one root and a
 * walk through strictly increasing keys that visits every live list output
 * exactly once; anything else is `unhealthy` with the reason.
 */
export const eventListAt = (
  store: FactStore,
  list: EventListConfig,
  at: Point,
): Promise<ProjectionRead<EventList>> =>
  store.transaction("read", async (tx) => {
    const status = await pointStatusIn(tx, store.dialect, at);
    if (status.kind !== "canonical") return status;
    const unhealthy = (detail: string) =>
      ({ kind: "unhealthy", detail }) as const;
    const nodes = await liveListNodes(tx, store, list, at);
    for (const [name, holders] of nodes)
      if (holders.length !== 1)
        return unhealthy(`${holders.length} live outputs hold token ${name}`);
    const decoded = new Map<string, SDK.EventHistoryNode>();
    for (const [name, [holder]] of nodes) {
      const datum = holder!.output.datum;
      if (datum === null) return unhealthy(`node ${name} has no inline datum`);
      try {
        decoded.set(
          name,
          Data.from(datum.toString("hex"), SDK.EventHistoryNode),
        );
      } catch (error) {
        return unhealthy(`node ${name} datum: ${String(error)}`);
      }
    }
    if (!decoded.has("")) return unhealthy("no root");
    const keys: string[] = [];
    let orders = 0;
    let fillers = 0;
    let next = decoded.get("")!.next;
    while (next !== null) {
      const node = decoded.get(next);
      if (node === undefined) return unhealthy(`missing node ${next}`);
      if (keys.length > 0 && next <= keys[keys.length - 1]!)
        return unhealthy(`keys out of order at ${next}`);
      keys.push(next);
      if (node.payload !== "RootContent" && "Order" in node.payload)
        orders += 1;
      else fillers += 1;
      next = node.next;
      if (keys.length > decoded.size) return unhealthy("cycle");
    }
    if (keys.length + 1 !== decoded.size)
      return unhealthy(
        `${decoded.size - 1 - keys.length} nodes are off the walk`,
      );
    return { kind: "ok", value: { keys, orders, fillers } };
  });
