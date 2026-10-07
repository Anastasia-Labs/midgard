import type {
  DerivationContext,
  DialectName,
  TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import { WATCHER_DA_ATTESTATIONS_TABLE } from "./tables.js";

export const DA_ATTESTATIONS_TEMPORAL_TABLE: TemporalTableSpec = {
  name: WATCHER_DA_ATTESTATIONS_TABLE,
  shape: "versioned",
  startColumn: "from_slot",
  endColumn: "to_slot",
  retention: { kind: "closed_k_deep" },
};

/**
 * Per state-queue header, every canonical tx that created an output holding
 * the header's DAAT. Availability reads the attested commitment from the
 * DAAT output Apply spent, so its creating tx stays readable while the
 * header can still be challenged: a header's rows close when its node leaves
 * the queue, as its unit history does.
 */
export const daAttestationsMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: rows of a header in the queue forever; closed rows once to_slot (the header's removal) is k deep
CREATE TABLE ${WATCHER_DA_ATTESTATIONS_TABLE} (
  header_hash ${bytes} NOT NULL,
  tx_hash ${bytes} NOT NULL,
  block_height ${int8} NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8},
  PRIMARY KEY (header_hash, tx_hash)
);
CREATE INDEX ${WATCHER_DA_ATTESTATIONS_TABLE}_from ON ${WATCHER_DA_ATTESTATIONS_TABLE} (from_slot);
CREATE INDEX ${WATCHER_DA_ATTESTATIONS_TABLE}_to ON ${WATCHER_DA_ATTESTATIONS_TABLE} (to_slot);
CREATE INDEX ${WATCHER_DA_ATTESTATIONS_TABLE}_tx ON ${WATCHER_DA_ATTESTATIONS_TABLE} (tx_hash);
`;
};

const DAAT_NAME = new RegExp(
  `^${SDK.DA_ATTESTATION_ASSET_NAME_PREFIX}([0-9a-f]{56})$`,
  "u",
);

/** Records the tx that created each output holding a header's DAAT. */
export const recordDaAttestations = async (
  context: DerivationContext,
  entry: DerivationContext["qualified"][number],
  policy: string,
): Promise<void> => {
  const { tx, block } = context;
  const headers = new Set<string>();
  // Every output, not just tracked ones: a DAAT sits outside the tracked set.
  for (const output of entry.tx.isValid ? entry.tx.outputs : [])
    for (const name of output.assets.get(policy)?.keys() ?? []) {
      const match = DAAT_NAME.exec(name);
      if (match !== null) headers.add(match[1]!);
    }
  for (const header of headers)
    await tx.query(
      `INSERT INTO ${WATCHER_DA_ATTESTATIONS_TABLE} (header_hash, tx_hash, block_height, from_slot, to_slot) VALUES (?, ?, ?, ?, NULL)`,
      [
        Buffer.from(header, "hex"),
        entry.tx.hash,
        block.height,
        block.point.slot,
      ],
    );
};
