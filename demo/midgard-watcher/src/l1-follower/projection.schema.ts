import type {
  DialectName,
  SchemaSet,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

import { availabilityTxPinsSchemaSql } from "./availability-pins.js";
import {
  CHECKPOINT_TEMPORAL_TABLES,
  checkpointSchemaSql,
} from "./checkpoints.js";
import { followerGenerationSchemaSql } from "./follower-generation.js";
import {
  DA_ATTESTATIONS_TEMPORAL_TABLE,
  daAttestationsSchemaSql,
} from "./projection.da-attestations.js";
import {
  DEPARTED_HEADERS_TEMPORAL_TABLE,
  departedHeadersSchemaSql,
} from "./projection.departed-headers.js";
import {
  FOLLOWED_UNITS_TEMPORAL_TABLES,
  followedUnitsSchemaSql,
} from "./projection.followed-units.js";
import {
  PROTOCOL_INIT_FAULTS_TEMPORAL_TABLE,
  protocolInitFaultsSchemaSql,
} from "./projection.protocol-init.js";
import {
  proofEventPinsSchemaSql,
  proofRetentionSchemaSql,
} from "./proof-retention.schema.js";
import { prunedKeysSchemaSql } from "./pruned-keys.js";
import {
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
} from "./tables.js";

/** The watcher projection's D-t tables and schema (see projection.ts). */

export const WATCHER_TEMPORAL_TABLES: readonly TemporalTableSpec[] = [
  {
    name: WATCHER_QUEUE_OUTPUTS_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  {
    name: WATCHER_QUEUE_UNIT_HISTORY_TABLE,
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
    // Held past k while a proof objective over the header is open; its
    // txs and checkpoints are held through it.
    pinnedBy: [
      {
        column: "header_hash",
        table: WATCHER_PROOF_PINS_TABLE,
        tableColumn: "header_hash",
      },
    ],
  },
  DA_ATTESTATIONS_TEMPORAL_TABLE,
  PROTOCOL_INIT_FAULTS_TEMPORAL_TABLE,
  DEPARTED_HEADERS_TEMPORAL_TABLE,
  ...FOLLOWED_UNITS_TEMPORAL_TABLES,
  ...CHECKPOINT_TEMPORAL_TABLES,
];

const queueOutputsSchemaSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: current rows forever; closed rows once to_slot is k deep
CREATE TABLE ${WATCHER_QUEUE_OUTPUTS_TABLE} (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  kind text NOT NULL CHECK (kind IN ('root', 'node', 'lock', 'malformed')),
  header_hash ${bytes},
  next_header_hash ${bytes},
  header_cbor ${bytes},
  state_queue_node_cbor ${bytes},
  datum_cbor ${bytes},
  malformed text,
  anchor_tx_hash ${bytes} NOT NULL,
  anchor_slot ${int8} NOT NULL,
  anchor_block_hash ${bytes} NOT NULL,
  anchor_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_from ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (from_slot);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_to ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (to_slot);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_header ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (header_hash);
CREATE INDEX ${WATCHER_QUEUE_OUTPUTS_TABLE}_anchor ON ${WATCHER_QUEUE_OUTPUTS_TABLE} (anchor_tx_hash);
`;
};

/**
 * Per state-queue node header, every canonical tx that created or spent one
 * of its node outputs: the history of the node unit. A header's rows close
 * when its node leaves the queue.
 */
const unitHistorySchemaSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: rows of a header in the queue forever; closed rows once to_slot (the header's removal) is k deep
CREATE TABLE ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (
  header_hash ${bytes} NOT NULL,
  tx_hash ${bytes} NOT NULL,
  block_hash ${bytes} NOT NULL,
  block_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (header_hash, tx_hash)
);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_from ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (from_slot);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_to ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (to_slot);
CREATE INDEX ${WATCHER_QUEUE_UNIT_HISTORY_TABLE}_tx ON ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} (tx_hash);
`;
};

export const watcherSchema = (dialect: DialectName): SchemaSet => ({
  namespace: "watcher",
  sql: [
    queueOutputsSchemaSql,
    unitHistorySchemaSql,
    checkpointSchemaSql,
    daAttestationsSchemaSql,
    protocolInitFaultsSchemaSql,
    departedHeadersSchemaSql,
    followedUnitsSchemaSql,
    proofRetentionSchemaSql,
    prunedKeysSchemaSql,
    followerGenerationSchemaSql,
    proofEventPinsSchemaSql,
    availabilityTxPinsSchemaSql,
  ]
    .map((part) => part(dialect))
    .join(""),
});
