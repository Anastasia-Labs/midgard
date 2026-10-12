-- In-order landed-block processing (plan §5.5 P3/P5, N3) replaces the
-- event-history foreign census, foreign native adoption and the per-commit
-- foreign-base verify. Their tables go; nothing reads them any more.
DROP TABLE foreign_native_adoptions;
DROP TABLE foreign_verified_segments;
DROP TABLE foreign_confirmed_frontier;
DROP TABLE event_history_census_frontier;
DROP TABLE event_history_census_blocks;

-- class: D-x; retention: a row lives while its header is a landed queue node not yet folded into confirmed_ledger, or (state 'removed') until the working-ledger rebase reverts it; folded and reverted rows are deleted
-- One row per landed state-queue block the node processed, exactly once: its
-- net ledger delta (spent and produced outrefs against its parent's ledger)
-- and the events it included. `applied` is whether the working ledger and the
-- native MPF hold it. A foreign row's delta comes from replaying its DA
-- payload against its parent's ledger; an own row's from the node's journal.
CREATE TABLE node_landed_blocks (
  header_hash bytea PRIMARY KEY CHECK (octet_length(header_hash) = 28),
  parent_header_hash bytea NOT NULL CHECK (octet_length(parent_header_hash) = 28),
  parent_utxos_root text NOT NULL CHECK (parent_utxos_root ~ '^[0-9a-f]{64}$'),
  utxos_root text NOT NULL CHECK (utxos_root ~ '^[0-9a-f]{64}$'),
  kind text NOT NULL CHECK (kind IN ('own', 'foreign')),
  state text NOT NULL CHECK (state IN ('processed', 'removed')),
  applied boolean NOT NULL,
  spent bytea[] NOT NULL,
  produced_outrefs bytea[] NOT NULL,
  produced_outputs bytea[] NOT NULL,
  deposit_ids bytea[] NOT NULL,
  withdrawals jsonb NOT NULL CHECK (jsonb_typeof(withdrawals) = 'array'),
  forced_ids bytea[] NOT NULL,
  tx_ids bytea[] NOT NULL,
  processed_at timestamptz NOT NULL DEFAULT NOW(),
  CHECK (cardinality(produced_outrefs) = cardinality(produced_outputs)),
  CHECK (state = 'processed' OR (kind = 'foreign' AND applied))
);
CREATE UNIQUE INDEX uniq_node_landed_blocks_processed_parent
  ON node_landed_blocks (parent_header_hash) WHERE state = 'processed';

-- class: D-x; retention: one row, advanced by every merge folded into confirmed_ledger
-- The landed header whose post-state `confirmed_ledger` holds: the merged
-- queue root, once every block up to it is folded in.
CREATE TABLE node_confirmed_ledger_frontier (
  id boolean PRIMARY KEY DEFAULT true CHECK (id),
  header_hash bytea NOT NULL CHECK (octet_length(header_hash) = 28),
  utxos_root text NOT NULL CHECK (utxos_root ~ '^[0-9a-f]{64}$'),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);
