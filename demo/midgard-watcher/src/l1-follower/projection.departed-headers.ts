import type {
  DerivationContext,
  DialectName,
  SqlRow,
  SqlTx,
  SqlValue,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  WATCHER_STATE_QUEUE_REMOVAL_KINDS,
  type WatcherStateQueueRemovalKind,
} from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  WATCHER_DEPARTED_HEADERS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
} from "./tables.js";

/**
 * Headers that left the state queue (ticket W1). A header leaves when a
 * valid tx spends its node without re-outputting it: a merge into confirmed
 * state, a removal, or a transition the watcher does not prove. Each
 * departure keeps two facts the queue rows lose once they are pruned:
 *
 * - why it left (`kind`), the release proof the decision bridge and the
 *   availability runtime read for a header of an older observation;
 * - its last attested node, the predecessor context the bridge resolves for
 *   the queue head (whose predecessor is the last merged header) and for a
 *   new header committed over an empty queue.
 *
 * A row stays open while a live node's header names it as its predecessor,
 * and the newest merge stays open while no newer merge replaced it (the
 * confirmed state names it). Closed rows go once they are k deep.
 */

export const DEPARTED_HEADERS_TEMPORAL_TABLE: TemporalTableSpec = {
  name: WATCHER_DEPARTED_HEADERS_TABLE,
  shape: "versioned",
  startColumn: "from_slot",
  endColumn: "to_slot",
  retention: { kind: "closed_k_deep" },
  // Held past k while a proof objective over the header is open.
  pinnedBy: [
    {
      column: "header_hash",
      table: WATCHER_PROOF_PINS_TABLE,
      tableColumn: "header_hash",
    },
  ],
};

export const departedHeadersMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: open while a live node names the header as predecessor or it is the newest merge; closed rows once to_slot is k deep
CREATE TABLE ${WATCHER_DEPARTED_HEADERS_TABLE} (
  header_hash ${bytes} NOT NULL,
  kind text NOT NULL,
  departure_tx_hash ${bytes} NOT NULL,
  departure_tx_index integer NOT NULL,
  departure_block_hash ${bytes} NOT NULL,
  departure_height ${int8} NOT NULL,
  attested_tx_hash ${bytes},
  attested_output_index integer,
  attested_block_hash ${bytes},
  attested_height ${int8},
  attested_slot ${int8},
  header_cbor ${bytes},
  state_queue_node_cbor ${bytes},
  datum_cbor ${bytes},
  next_header_hash ${bytes},
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (header_hash, from_slot)
);
CREATE INDEX ${WATCHER_DEPARTED_HEADERS_TABLE}_from ON ${WATCHER_DEPARTED_HEADERS_TABLE} (from_slot);
CREATE INDEX ${WATCHER_DEPARTED_HEADERS_TABLE}_to ON ${WATCHER_DEPARTED_HEADERS_TABLE} (to_slot);
`;
};

/** Why a header left the queue. */
export type WatcherDepartureKind =
  | "merged"
  | WatcherStateQueueRemovalKind
  | "other";

type QualifiedEntry = DerivationContext["qualified"][number];

const hex = (value: unknown): string =>
  Buffer.from(value as Uint8Array).toString("hex");

const outRefLabel = (outRef: Readonly<{ txHash: Buffer; index: number }>) =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

/** The tx's single state-queue mint redeemer, or null. */
const stateQueueMintRedeemer = (
  entry: QualifiedEntry,
  stateQueuePolicyId: string,
): SDK.StateQueueRedeemer | null => {
  const policies = [...entry.tx.mint.keys()].sort();
  const index = policies.indexOf(stateQueuePolicyId);
  if (index < 0) return null;
  const redeemers = entry.tx.redeemers.filter(
    (redeemer) => redeemer.purpose === "mint" && redeemer.index === index,
  );
  if (redeemers.length !== 1) return null;
  try {
    return Data.from(
      redeemers[0]!.data.toString("hex"),
      SDK.StateQueueRedeemer,
    );
  } catch {
    return null;
  }
};

const burnedNodes = (
  entry: QualifiedEntry,
  stateQueuePolicyId: string,
): readonly string[] =>
  [...(entry.tx.mint.get(stateQueuePolicyId) ?? new Map<string, bigint>())]
    .filter(
      ([assetName, quantity]) =>
        quantity === -1n &&
        assetName.startsWith(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX),
    )
    .map(([assetName]) =>
      assetName.slice(SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length),
    );

/**
 * Why `header` left the queue in `entry`, by the rules the old merged-header
 * walk applied: a MergeToConfirmedStateV1 of exactly this header over a
 * spent root that re-outputs the root; a removal redeemer burning exactly
 * this one node; anything else is `other` and proves nothing.
 */
export const departureKind = (
  entry: QualifiedEntry,
  header: string,
  spentRoots: ReadonlySet<string>,
  stateQueuePolicyId: string,
): WatcherDepartureKind => {
  const redeemer = stateQueueMintRedeemer(entry, stateQueuePolicyId);
  if (typeof redeemer !== "object" || redeemer === null) return "other";
  const burned = burnedNodes(entry, stateQueuePolicyId);
  if ("MergeToConfirmedStateV1" in redeemer) {
    const merge = redeemer.MergeToConfirmedStateV1;
    const consumed = `${merge.confirmed_state_input_outref.transactionId}#${merge.confirmed_state_input_outref.outputIndex.toString()}`;
    const output = entry.tx.outputs[Number(merge.confirmed_state_output_index)];
    const rootUnit = output?.assets
      .get(stateQueuePolicyId)
      ?.get(SDK.STATE_QUEUE_ROOT_ASSET_NAME);
    return merge.header_node_key === header &&
      spentRoots.has(consumed) &&
      burned.length === 1 &&
      burned[0] === header &&
      merge.confirmed_state_output_index >= 0n &&
      rootUnit === 1n
      ? "merged"
      : "other";
  }
  const removal = WATCHER_STATE_QUEUE_REMOVAL_KINDS.find(
    (kind) => kind in redeemer,
  );
  return removal !== undefined && burned.length === 1 && burned[0] === header
    ? removal
    : "other";
};

const isAttested = (stateQueueNodeCbor: unknown): boolean => {
  try {
    return (
      Data.from(hex(stateQueueNodeCbor), SDK.StateQueueNode).da_attestation !==
      SDK.NO_DA_ATTESTATION
    );
  } catch {
    return false;
  }
};

/** The header's newest attested node version still in the queue rows, or null. */
const latestAttestedVersion = async (
  tx: SqlTx,
  header: Buffer,
): Promise<SqlRow | null> => {
  const rows = await tx.query(
    `SELECT q.tx_hash, q.output_index, q.header_cbor, q.state_queue_node_cbor, q.datum_cbor, q.next_header_hash, u.block_hash, u.block_height, u.from_slot FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} q JOIN ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} u ON u.header_hash = q.header_hash AND u.tx_hash = q.tx_hash WHERE q.header_hash = ? AND q.kind = 'node' ORDER BY u.block_height DESC, u.from_slot DESC, q.tx_hash DESC, q.output_index DESC`,
    [header],
  );
  return rows.find((row) => isAttested(row.state_queue_node_cbor)) ?? null;
};

/** Opens the departure row of `header`, which `entry` spent without re-outputting. */
export const recordDeparture = async (
  context: DerivationContext,
  entry: QualifiedEntry,
  header: string,
  kind: WatcherDepartureKind,
): Promise<void> => {
  const { tx, block } = context;
  const headerBytes = Buffer.from(header, "hex");
  const attested = await latestAttestedVersion(tx, headerBytes);
  await tx.query(
    `INSERT INTO ${WATCHER_DEPARTED_HEADERS_TABLE} (header_hash, kind, departure_tx_hash, departure_tx_index, departure_block_hash, departure_height, attested_tx_hash, attested_output_index, attested_block_hash, attested_height, attested_slot, header_cbor, state_queue_node_cbor, datum_cbor, next_header_hash, from_slot, to_slot) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL)`,
    [
      headerBytes,
      kind,
      entry.tx.hash,
      entry.tx.index,
      block.point.hash,
      block.height,
      (attested?.tx_hash ?? null) as SqlValue,
      (attested?.output_index ?? null) as SqlValue,
      (attested?.block_hash ?? null) as SqlValue,
      (attested?.block_height ?? null) as SqlValue,
      (attested?.from_slot ?? null) as SqlValue,
      (attested?.header_cbor ?? null) as SqlValue,
      (attested?.state_queue_node_cbor ?? null) as SqlValue,
      (attested?.datum_cbor ?? null) as SqlValue,
      (attested?.next_header_hash ?? null) as SqlValue,
      block.point.slot,
    ],
  );
};

/**
 * Closes every open departure no live node names as its predecessor,
 * except the newest merge (the confirmed state's header).
 */
export const closeUnreferencedDepartures = async (
  context: DerivationContext,
): Promise<void> => {
  const { tx, block } = context;
  const open = await tx.query(
    `SELECT header_hash, kind, from_slot, departure_tx_index FROM ${WATCHER_DEPARTED_HEADERS_TABLE} WHERE to_slot IS NULL`,
    [],
  );
  if (open.length === 0) return;
  const referenced = new Set<string>();
  for (const row of await tx.query(
    `SELECT header_cbor FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND to_slot IS NULL`,
    [],
  ))
    referenced.add(Data.from(hex(row.header_cbor), SDK.Header).prevHeaderHash);
  const newestMerge = open
    .filter((row) => row.kind === "merged")
    .sort(
      (left, right) =>
        Number(right.from_slot as number | string) -
          Number(left.from_slot as number | string) ||
        Number(right.departure_tx_index as number | string) -
          Number(left.departure_tx_index as number | string),
    )[0];
  for (const row of open) {
    const header = hex(row.header_hash);
    if (row === newestMerge || referenced.has(header)) continue;
    await tx.query(
      `UPDATE ${WATCHER_DEPARTED_HEADERS_TABLE} SET to_slot = ? WHERE header_hash = ? AND to_slot IS NULL`,
      [block.point.slot, Buffer.from(header, "hex")],
    );
  }
};

/** A departed header's last attested node, as the retained-header read returns it. */
export type WatcherDepartedAttestedHeader = Readonly<{
  headerHash: string;
  headerCborHex: string;
  stateQueueNodeCborHex: string;
  linkedListDatumCborHex: string;
  queueOutRef: string;
  nextHeaderHash: string | null;
  transactionHash: string;
  blockHash: string;
  slot: number;
  height: number;
}>;

/**
 * The departure of `header` retained in the rows (open, or closed less than
 * k blocks ago), as its last attested node; `unattested` when it left the
 * queue without one; null when no departure is retained.
 */
export const readDepartedAttestedHeader = async (
  tx: SqlTx,
  header: string,
): Promise<WatcherDepartedAttestedHeader | "unattested" | null> => {
  const row = (
    await tx.query(
      `SELECT * FROM ${WATCHER_DEPARTED_HEADERS_TABLE} WHERE header_hash = ? ORDER BY from_slot DESC LIMIT 1`,
      [Buffer.from(header, "hex")],
    )
  )[0];
  if (row === undefined) return null;
  if (row.attested_tx_hash === null || row.attested_tx_hash === undefined)
    return "unattested";
  return {
    headerHash: header,
    headerCborHex: hex(row.header_cbor),
    stateQueueNodeCborHex: hex(row.state_queue_node_cbor),
    linkedListDatumCborHex: hex(row.datum_cbor),
    queueOutRef: outRefLabel({
      txHash: Buffer.from(row.attested_tx_hash as Uint8Array),
      index: Number(row.attested_output_index as number | string),
    }),
    nextHeaderHash:
      row.next_header_hash === null || row.next_header_hash === undefined
        ? null
        : hex(row.next_header_hash),
    transactionHash: hex(row.attested_tx_hash),
    blockHash: hex(row.attested_block_hash),
    slot: Number(row.attested_slot as number | string),
    height: Number(row.attested_height as number | string),
  };
};

/** One header's departure, as the release proof reads it. */
export type WatcherDeparture = Readonly<{
  headerHash: string;
  kind: WatcherDepartureKind;
  transactionHash: string;
  /** The departing tx's index in its block: chain order within one slot. */
  txIndex: number;
  blockHash: string;
  slot: number;
  height: number;
}>;

/** The departures of `headers` with a departure slot in `(afterSlot, throughSlot]`. */
export const readDepartures = async (
  tx: SqlTx,
  headers: readonly string[],
  afterSlot: number,
  throughSlot: number,
): Promise<readonly WatcherDeparture[]> => {
  const result: WatcherDeparture[] = [];
  for (const header of headers) {
    const rows = await tx.query(
      `SELECT kind, departure_tx_hash, departure_tx_index, departure_block_hash, departure_height, from_slot FROM ${WATCHER_DEPARTED_HEADERS_TABLE} WHERE header_hash = ? AND from_slot > ? AND from_slot <= ? ORDER BY from_slot, departure_tx_index`,
      [Buffer.from(header, "hex"), afterSlot, throughSlot],
    );
    for (const row of rows)
      result.push({
        headerHash: header,
        kind: row.kind as WatcherDepartureKind,
        transactionHash: hex(row.departure_tx_hash),
        txIndex: Number(row.departure_tx_index as number | string),
        blockHash: hex(row.departure_block_hash),
        slot: Number(row.from_slot as number | string),
        height: Number(row.departure_height as number | string),
      });
  }
  return result;
};
