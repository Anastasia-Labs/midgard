/**
 * One event's live Order by its id (NC13): settlement opens the deposit or
 * withdrawal it settles from the follower's facts by key, never by scanning
 * the list or its retention address.
 *
 * - The row: `node_l1_events`, keyed by (kind, event_key), event_key being
 *   blake2b-256 of the event id CBOR. The event derivation writes it at
 *   admission, with the retention output an external payload was read from
 *   (`retained_tx_hash`, `retained_output_index`).
 * - The Order: the one live output holding the list token named by the key
 *   (`l1_output_assets` by policy and asset name).
 * - The retained payload: that one outref.
 *
 * All three reads run at the follower's tip in the caller's transaction, so
 * they never mix two chain states.
 */
import {
  currentViewIn,
  type Dialect,
  liveUtxosIn,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import { datumToHash, type UTxO } from "@lucid-evolution/lucid";

import type { EventKind } from "./config.js";
import { EVENTS_TABLE } from "./schema.js";

export type EventOrderList = Readonly<{
  kind: EventKind;
  /** The list's minting policy (56 hex). */
  policyId: string;
}>;

export type EventOrderRead =
  | Readonly<{ kind: "ok"; order: UTxO; retained: readonly UTxO[] }>
  /** No live event with this id at the tip: never admitted, or retired. */
  | Readonly<{ kind: "absent" }>
  /** The projection disagrees with the facts; the detail names how. */
  | Readonly<{ kind: "unavailable"; detail: string }>;

/** The event key of an event id: blake2b-256 of its CBOR. */
export const eventKeyOfId = (eventId: Buffer): Buffer =>
  Buffer.from(datumToHash(eventId.toString("hex")), "hex");

/**
 * The live Order of the `list` event whose id CBOR is `eventId`, and the
 * retention output its external payload lives in, at the follower's tip.
 */
export const eventOrderByIdIn = async (
  tx: SqlTx,
  dialect: Dialect,
  list: EventOrderList,
  eventId: Buffer,
): Promise<EventOrderRead> => {
  if ((await currentViewIn(tx, dialect)) === null)
    return { kind: "unavailable", detail: "the follower holds no view yet" };
  const key = eventKeyOfId(eventId);
  const rows = await tx.query(
    `SELECT event_id, retained_tx_hash, retained_output_index FROM ${EVENTS_TABLE}
      WHERE kind = ? AND event_key = ? AND retired_slot IS NULL`,
    [list.kind, key],
  );
  const row = rows[0];
  if (row === undefined) return { kind: "absent" };
  if (!Buffer.from(row.event_id as Uint8Array).equals(eventId))
    return { kind: "unavailable", detail: "event key names another event id" };
  const holders = await liveUtxosIn(tx, dialect, {
    by: "unit",
    policyId: Buffer.from(list.policyId, "hex"),
    assetName: key,
  });
  if (holders.kind !== "ok")
    return { kind: "unavailable", detail: holders.detail };
  if (holders.utxos.length !== 1)
    return {
      kind: "unavailable",
      detail: `${holders.utxos.length} live outputs hold the event token`,
    };
  const order = holders.utxos[0]!;
  if (row.retained_tx_hash === null || row.retained_tx_hash === undefined)
    return {
      kind: "ok",
      order: toLucidUtxo(order.outRef, order.output),
      retained: [],
    };
  const retained = await liveUtxosIn(tx, dialect, {
    by: "outref",
    outRefs: [
      {
        txHash: Buffer.from(row.retained_tx_hash as Uint8Array),
        index: Number(row.retained_output_index),
      },
    ],
  });
  if (retained.kind !== "ok")
    return { kind: "unavailable", detail: retained.detail };
  if (retained.utxos.length !== 1)
    return {
      kind: "unavailable",
      detail: "the retained payload output is spent",
    };
  const payload = retained.utxos[0]!;
  return {
    kind: "ok",
    order: toLucidUtxo(order.outRef, order.output),
    retained: [toLucidUtxo(payload.outRef, payload.output)],
  };
};
