import type { DialectName } from "../sql/backend.js";
import type { MigrationSet } from "./migrate.js";

/**
 * The §5.2 fact schema. Every table carries the
 * `-- class: <class>; retention: <rule>` header the schema lint requires
 * (§5.1, §11). The SQLite form mirrors the Postgres form logically: `bytea[]`
 * becomes a JSON array of hex `TEXT`, `jsonb` and `numeric` become `TEXT`,
 * `boolean` becomes a 0/1 `INTEGER`.
 *
 * Deltas from the §5.2 draft, all additive: `l1_outputs_script_ref` (the
 * `l1_scripts` retention check and its foreign key) and
 * `l1_event_keys_slot` (the rewind deletes keys above the target, §5.4),
 * `l1_protocol_init` (the §5.3 step 3 fact that the tx spending the
 * manifest's `hubOracleOneShot` outref landed; it outlives the pruned tx),
 * and `l1_follower_cursor.pruned_through_slot`: the highest slot at or below
 * which pruning may have removed spent outputs, closed temporal rows or
 * blocks. Facts are complete for every slot at or above it, so it bounds
 * reads at a point, rewind targets (R1) and the INV5 chain-linkage window.
 */
export const FOLLOWER_MIGRATION_NAMESPACE = "l1-follower";

const POSTGRES_0001 = `
-- class: A; retention: kept while height > tip - k - 1, or referenced by a retained l1_txs or l1_outputs row; every 1,000th height and the origin are kept forever as intersection checkpoints
CREATE TABLE l1_blocks (
  slot         bigint NOT NULL PRIMARY KEY,
  hash         bytea  NOT NULL UNIQUE,
  height       bigint NOT NULL UNIQUE,
  parent_hash  bytea,
  qualifying_tx_count integer NOT NULL
);

-- class: A; retention: kept while any output it created is retained (R1b), it spent a retained output, a registered pin references it, or its block is within k; pruned after (O4)
CREATE TABLE l1_txs (
  tx_hash          bytea   PRIMARY KEY,
  block_slot       bigint  NOT NULL REFERENCES l1_blocks(slot) ON DELETE CASCADE,
  block_tx_index   integer NOT NULL,
  is_valid         boolean NOT NULL,
  inputs           bytea[] NOT NULL,
  reference_inputs bytea[] NOT NULL,
  collaterals      bytea[] NOT NULL,
  output_count     integer NOT NULL,
  has_collateral_return boolean NOT NULL,
  mint             jsonb   NOT NULL,
  withdrawals      jsonb   NOT NULL,
  redeemers        jsonb   NOT NULL,
  invalid_before   numeric(20,0),
  invalid_after    numeric(20,0),
  body_cbor        bytea   NOT NULL,
  witness_cbor     bytea   NOT NULL,
  aux_cbor         bytea,
  UNIQUE (block_slot, block_tx_index)
);

-- class: A; retention: with its l1_txs row (cascade)
CREATE TABLE l1_tx_mint_policies (
  tx_hash   bytea NOT NULL REFERENCES l1_txs(tx_hash) ON DELETE CASCADE,
  policy_id bytea NOT NULL,
  PRIMARY KEY (tx_hash, policy_id)
);

-- class: C; retention: kept while a retained l1_outputs row references it (R1b); pruned when unreferenced
CREATE TABLE l1_scripts (
  script_hash bytea PRIMARY KEY,
  script_type text NOT NULL,
  bytes bytea NOT NULL
);

-- class: A; retention: live rows forever (R1b); spent rows until the spend is more than k blocks deep (O4)
CREATE TABLE l1_outputs (
  tx_hash bytea NOT NULL,
  output_index integer NOT NULL,
  address bytea NOT NULL,
  payment_cred bytea,
  payment_cred_is_script boolean,
  stake_cred bytea,
  lovelace numeric(20,0) NOT NULL,
  assets jsonb NOT NULL,
  datum_hash bytea,
  datum bytea,
  script_ref_hash bytea REFERENCES l1_scripts(script_hash),
  created_slot bigint REFERENCES l1_blocks(slot) ON DELETE CASCADE,
  created_tx_index integer,
  spent_slot bigint REFERENCES l1_blocks(slot) ON DELETE SET NULL,
  spent_tx bytea REFERENCES l1_txs(tx_hash) ON DELETE SET NULL,
  seed_slot bigint,
  PRIMARY KEY (tx_hash, output_index),
  CHECK ((created_slot IS NULL) = (created_tx_index IS NULL)),
  CHECK ((created_slot IS NULL) = (seed_slot IS NOT NULL)),
  CHECK ((spent_slot IS NULL) = (spent_tx IS NULL)),
  CHECK (datum_hash IS NULL OR datum IS NULL)
);

-- class: A; retention: with its l1_outputs row (cascade)
CREATE TABLE l1_output_assets (
  tx_hash bytea NOT NULL,
  output_index integer NOT NULL,
  policy_id bytea NOT NULL,
  asset_name bytea NOT NULL,
  quantity numeric(40,0) NOT NULL,
  PRIMARY KEY (tx_hash, output_index, policy_id, asset_name),
  FOREIGN KEY (tx_hash, output_index) REFERENCES l1_outputs ON DELETE CASCADE
);

-- class: A; retention: one row forever
CREATE TABLE l1_follower_cursor (
  id boolean PRIMARY KEY DEFAULT true CHECK (id),
  slot bigint NOT NULL,
  hash bytea NOT NULL,
  height bigint NOT NULL,
  generation bigint NOT NULL,
  origin_slot bigint NOT NULL,
  origin_hash bytea NOT NULL,
  pruned_through_slot bigint NOT NULL
);

-- class: A; retention: the last 1,000 rows by generation
CREATE TABLE l1_rollbacks (
  generation bigint PRIMARY KEY,
  from_slot bigint NOT NULL,
  from_hash bytea NOT NULL,
  to_slot bigint NOT NULL,
  to_hash bytea NOT NULL,
  depth_blocks integer NOT NULL
);

-- class: A; retention: forever while canonical (the never-reuse set, O4); a rewind removes keys first seen above its target (§5.4)
CREATE TABLE l1_event_keys (
  kind text NOT NULL,
  key bytea NOT NULL,
  origin_outref bytea NOT NULL,
  first_canonical_slot bigint NOT NULL,
  PRIMARY KEY (kind, key)
);

-- class: A; retention: forever while canonical; prune never removes it, a rewind below its slot removes it
CREATE TABLE l1_protocol_init (
  one_shot bytea PRIMARY KEY,
  tx_hash bytea NOT NULL,
  slot bigint NOT NULL
);

CREATE INDEX l1_outputs_address_live ON l1_outputs (address) WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_payment_live ON l1_outputs (payment_cred) WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_stake_live ON l1_outputs (stake_cred) WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_spent_tx ON l1_outputs (spent_tx) WHERE spent_tx IS NOT NULL;
CREATE INDEX l1_outputs_created_slot ON l1_outputs (created_slot);
CREATE INDEX l1_outputs_seed_slot ON l1_outputs (seed_slot) WHERE seed_slot IS NOT NULL;
CREATE INDEX l1_outputs_spent_slot ON l1_outputs (spent_slot) WHERE spent_slot IS NOT NULL;
CREATE INDEX l1_outputs_script_ref ON l1_outputs (script_ref_hash) WHERE script_ref_hash IS NOT NULL;
CREATE INDEX l1_output_assets_unit ON l1_output_assets (policy_id, asset_name);
CREATE INDEX l1_tx_mint_policy ON l1_tx_mint_policies (policy_id);
CREATE INDEX l1_event_keys_slot ON l1_event_keys (first_canonical_slot);
`;

const SQLITE_0001 = `
-- class: A; retention: kept while height > tip - k - 1, or referenced by a retained l1_txs or l1_outputs row; every 1,000th height and the origin are kept forever as intersection checkpoints
CREATE TABLE l1_blocks (
  slot         INTEGER NOT NULL PRIMARY KEY,
  hash         BLOB    NOT NULL UNIQUE,
  height       INTEGER NOT NULL UNIQUE,
  parent_hash  BLOB,
  qualifying_tx_count INTEGER NOT NULL
);

-- class: A; retention: kept while any output it created is retained (R1b), it spent a retained output, a registered pin references it, or its block is within k; pruned after (O4)
CREATE TABLE l1_txs (
  tx_hash          BLOB    PRIMARY KEY,
  block_slot       INTEGER NOT NULL REFERENCES l1_blocks(slot) ON DELETE CASCADE,
  block_tx_index   INTEGER NOT NULL,
  is_valid         INTEGER NOT NULL CHECK (is_valid IN (0, 1)),
  inputs           TEXT    NOT NULL,
  reference_inputs TEXT    NOT NULL,
  collaterals      TEXT    NOT NULL,
  output_count     INTEGER NOT NULL,
  has_collateral_return INTEGER NOT NULL CHECK (has_collateral_return IN (0, 1)),
  mint             TEXT    NOT NULL,
  withdrawals      TEXT    NOT NULL,
  redeemers        TEXT    NOT NULL,
  invalid_before   TEXT,
  invalid_after    TEXT,
  body_cbor        BLOB    NOT NULL,
  witness_cbor     BLOB    NOT NULL,
  aux_cbor         BLOB,
  UNIQUE (block_slot, block_tx_index)
);

-- class: A; retention: with its l1_txs row (cascade)
CREATE TABLE l1_tx_mint_policies (
  tx_hash   BLOB NOT NULL REFERENCES l1_txs(tx_hash) ON DELETE CASCADE,
  policy_id BLOB NOT NULL,
  PRIMARY KEY (tx_hash, policy_id)
);

-- class: C; retention: kept while a retained l1_outputs row references it (R1b); pruned when unreferenced
CREATE TABLE l1_scripts (
  script_hash BLOB PRIMARY KEY,
  script_type TEXT NOT NULL,
  bytes BLOB NOT NULL
);

-- class: A; retention: live rows forever (R1b); spent rows until the spend is more than k blocks deep (O4)
CREATE TABLE l1_outputs (
  tx_hash BLOB NOT NULL,
  output_index INTEGER NOT NULL,
  address BLOB NOT NULL,
  payment_cred BLOB,
  payment_cred_is_script INTEGER CHECK (payment_cred_is_script IN (0, 1)),
  stake_cred BLOB,
  lovelace TEXT NOT NULL,
  assets TEXT NOT NULL,
  datum_hash BLOB,
  datum BLOB,
  script_ref_hash BLOB REFERENCES l1_scripts(script_hash),
  created_slot INTEGER REFERENCES l1_blocks(slot) ON DELETE CASCADE,
  created_tx_index INTEGER,
  spent_slot INTEGER REFERENCES l1_blocks(slot) ON DELETE SET NULL,
  spent_tx BLOB REFERENCES l1_txs(tx_hash) ON DELETE SET NULL,
  seed_slot INTEGER,
  PRIMARY KEY (tx_hash, output_index),
  CHECK ((created_slot IS NULL) = (created_tx_index IS NULL)),
  CHECK ((created_slot IS NULL) = (seed_slot IS NOT NULL)),
  CHECK ((spent_slot IS NULL) = (spent_tx IS NULL)),
  CHECK (datum_hash IS NULL OR datum IS NULL)
);

-- class: A; retention: with its l1_outputs row (cascade)
CREATE TABLE l1_output_assets (
  tx_hash BLOB NOT NULL,
  output_index INTEGER NOT NULL,
  policy_id BLOB NOT NULL,
  asset_name BLOB NOT NULL,
  quantity TEXT NOT NULL,
  PRIMARY KEY (tx_hash, output_index, policy_id, asset_name),
  FOREIGN KEY (tx_hash, output_index) REFERENCES l1_outputs ON DELETE CASCADE
);

-- class: A; retention: one row forever
CREATE TABLE l1_follower_cursor (
  id INTEGER PRIMARY KEY CHECK (id = 1),
  slot INTEGER NOT NULL,
  hash BLOB NOT NULL,
  height INTEGER NOT NULL,
  generation INTEGER NOT NULL,
  origin_slot INTEGER NOT NULL,
  origin_hash BLOB NOT NULL,
  pruned_through_slot INTEGER NOT NULL
);

-- class: A; retention: the last 1,000 rows by generation
CREATE TABLE l1_rollbacks (
  generation INTEGER PRIMARY KEY,
  from_slot INTEGER NOT NULL,
  from_hash BLOB NOT NULL,
  to_slot INTEGER NOT NULL,
  to_hash BLOB NOT NULL,
  depth_blocks INTEGER NOT NULL
);

-- class: A; retention: forever while canonical (the never-reuse set, O4); a rewind removes keys first seen above its target (§5.4)
CREATE TABLE l1_event_keys (
  kind TEXT NOT NULL,
  key BLOB NOT NULL,
  origin_outref BLOB NOT NULL,
  first_canonical_slot INTEGER NOT NULL,
  PRIMARY KEY (kind, key)
);

-- class: A; retention: forever while canonical; prune never removes it, a rewind below its slot removes it
CREATE TABLE l1_protocol_init (
  one_shot BLOB PRIMARY KEY,
  tx_hash BLOB NOT NULL,
  slot INTEGER NOT NULL
);

CREATE INDEX l1_outputs_address_live ON l1_outputs (address) WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_payment_live ON l1_outputs (payment_cred) WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_stake_live ON l1_outputs (stake_cred) WHERE spent_slot IS NULL;
CREATE INDEX l1_outputs_spent_tx ON l1_outputs (spent_tx) WHERE spent_tx IS NOT NULL;
CREATE INDEX l1_outputs_created_slot ON l1_outputs (created_slot);
CREATE INDEX l1_outputs_seed_slot ON l1_outputs (seed_slot) WHERE seed_slot IS NOT NULL;
CREATE INDEX l1_outputs_spent_slot ON l1_outputs (spent_slot) WHERE spent_slot IS NOT NULL;
CREATE INDEX l1_outputs_script_ref ON l1_outputs (script_ref_hash) WHERE script_ref_hash IS NOT NULL;
CREATE INDEX l1_output_assets_unit ON l1_output_assets (policy_id, asset_name);
CREATE INDEX l1_tx_mint_policy ON l1_tx_mint_policies (policy_id);
CREATE INDEX l1_event_keys_slot ON l1_event_keys (first_canonical_slot);
`;

export const followerMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: FOLLOWER_MIGRATION_NAMESPACE,
  migrations: [
    {
      id: "0001_fact_schema",
      sql: dialect === "postgres" ? POSTGRES_0001 : SQLITE_0001,
    },
  ],
});
