import type {
  DerivationContext,
  DialectName,
  SqlTx,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import { WATCHER_PROTOCOL_INIT_FAULTS_TABLE } from "./tables.js";

/**
 * Deployment-shape checks on the state-queue root (ticket W1). The queue's
 * root output must sit at the state-queue address: the watcher reads the
 * queue only there. A canonical tx that creates an output holding the root
 * unit anywhere else opens a row here, and the view reports the queue as
 * unhealthy while the row is live, so the watcher stays unready with a named
 * reason instead of deciding over a queue it does not see.
 *
 * Rows are never closed: they describe the deployment, not one output. A
 * rollback that removes the creating block removes the row with it.
 */

export const PROTOCOL_INIT_FAULTS_TEMPORAL_TABLE: TemporalTableSpec = {
  name: WATCHER_PROTOCOL_INIT_FAULTS_TABLE,
  shape: "versioned",
  startColumn: "from_slot",
  endColumn: "to_slot",
  retention: { kind: "closed_k_deep" },
};

export const protocolInitFaultsMigrationSql = (
  dialect: DialectName,
): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: rows are never closed, so kept forever (one per misplaced root output)
CREATE TABLE ${WATCHER_PROTOCOL_INIT_FAULTS_TABLE} (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX ${WATCHER_PROTOCOL_INIT_FAULTS_TABLE}_from ON ${WATCHER_PROTOCOL_INIT_FAULTS_TABLE} (from_slot);
CREATE INDEX ${WATCHER_PROTOCOL_INIT_FAULTS_TABLE}_to ON ${WATCHER_PROTOCOL_INIT_FAULTS_TABLE} (to_slot);
`;
};

/** Opens a row for every output of a valid tx that holds the queue root away from the queue address. */
export const recordProtocolInitFaults = async (
  context: DerivationContext,
  entry: DerivationContext["qualified"][number],
  stateQueuePolicyId: string,
  stateQueueAddress: Buffer,
): Promise<void> => {
  if (!entry.tx.isValid) return;
  for (const [index, output] of entry.tx.outputs.entries()) {
    const quantity = output.assets
      .get(stateQueuePolicyId)
      ?.get(SDK.STATE_QUEUE_ROOT_ASSET_NAME);
    if (quantity === undefined || quantity === 0n) continue;
    if (output.address.equals(stateQueueAddress)) continue;
    await context.tx.query(
      `INSERT INTO ${WATCHER_PROTOCOL_INIT_FAULTS_TABLE} (tx_hash, output_index, from_slot, to_slot) VALUES (?, ?, ?, NULL)`,
      [entry.tx.hash, index, context.block.point.slot],
    );
  }
};

/** The first misplaced root output live at `slot`, as `txHash#index`, or null. */
export const readProtocolInitFault = async (
  tx: SqlTx,
  slot: number,
): Promise<string | null> => {
  const row = (
    await tx.query(
      `SELECT tx_hash, output_index FROM ${WATCHER_PROTOCOL_INIT_FAULTS_TABLE} WHERE from_slot <= ? AND (to_slot IS NULL OR to_slot > ?) ORDER BY from_slot, tx_hash, output_index LIMIT 1`,
      [slot, slot],
    )
  )[0];
  return row === undefined
    ? null
    : `${Buffer.from(row.tx_hash as Uint8Array).toString("hex")}#${Number(row.output_index as number | string).toString()}`;
};
