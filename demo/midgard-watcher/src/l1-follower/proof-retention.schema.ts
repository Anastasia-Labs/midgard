import type { DialectName } from "@al-ft/midgard-l1-follower";

import {
  WATCHER_PROOF_PIN_UNITS_TABLE,
  WATCHER_PROOF_PINS_TABLE,
  WATCHER_TX_INPUTS_TABLE,
} from "./tables.js";

/**
 * The proof-retention tables (E1 ruling, see proof-retention.ts and
 * tx-inputs.ts). The pins are the watcher's own records (class B: a rewind
 * or a reset never touches them); the resolved inputs are immutable output
 * bytes per outref (class C), kept while their tx is retained.
 */
export const proofRetentionMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  return `
-- class: B; retention: the watcher deletes a row once the objective's completion marker is verified past k
CREATE TABLE ${WATCHER_PROOF_PINS_TABLE} (
  header_hash ${bytes} NOT NULL,
  category text NOT NULL,
  PRIMARY KEY (header_hash, category)
);
-- class: B; retention: the watcher deletes a header's rows when it releases the header's last proof pin
CREATE TABLE ${WATCHER_PROOF_PIN_UNITS_TABLE} (
  header_hash ${bytes} NOT NULL,
  unit ${bytes} NOT NULL,
  PRIMARY KEY (header_hash, unit)
);
CREATE INDEX ${WATCHER_PROOF_PIN_UNITS_TABLE}_unit ON ${WATCHER_PROOF_PIN_UNITS_TABLE} (unit);
-- class: C; retention: rows of a tx a unit history records, while the tx is stored; the watcher deletes the rest
CREATE TABLE ${WATCHER_TX_INPUTS_TABLE} (
  tx_hash ${bytes} NOT NULL,
  out_tx_hash ${bytes} NOT NULL,
  out_index integer NOT NULL,
  output_cbor ${bytes} NOT NULL,
  PRIMARY KEY (tx_hash, out_tx_hash, out_index)
);
CREATE INDEX ${WATCHER_TX_INPUTS_TABLE}_out ON ${WATCHER_TX_INPUTS_TABLE} (out_tx_hash, out_index);
`;
};
