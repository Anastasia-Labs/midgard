-- `confirmed_ledger` becomes temporal (plan §10.5, §5.5 P10, N5): every
-- landed merge folded into it keeps what an unfold needs, so a rolled-back
-- merge is rewound in O(delta) and the frontier never leaves the merged
-- root's lineage. `confirmed_ledger` itself keeps its columns.

-- class: D-x; retention: a row lives while its merge can still be rolled back: it is deleted once the follower's prune boundary reaches its merge slot, or when the unfold of its block rewinds it
-- One row per landed block folded into `confirmed_ledger`, by header: the
-- block's landed row as processing recorded it (so an unfold re-inserts it
-- verbatim), the events the fold marked terminal (so an unfold reopens
-- exactly those), and the slot and output of the merge that made it the
-- queue root (null until landed-block processing read it from the queue
-- history: a merge the merge fiber folded first).
CREATE TABLE node_confirmed_merges (
  header_hash bytea PRIMARY KEY CHECK (octet_length(header_hash) = 28),
  parent_header_hash bytea NOT NULL CHECK (octet_length(parent_header_hash) = 28),
  parent_utxos_root text NOT NULL CHECK (parent_utxos_root ~ '^[0-9a-f]{64}$'),
  utxos_root text NOT NULL CHECK (utxos_root ~ '^[0-9a-f]{64}$'),
  kind text NOT NULL CHECK (kind IN ('own', 'foreign')),
  applied boolean NOT NULL,
  spent bytea[] NOT NULL,
  produced_outrefs bytea[] NOT NULL,
  produced_outputs bytea[] NOT NULL,
  deposit_ids bytea[] NOT NULL,
  withdrawals jsonb NOT NULL CHECK (jsonb_typeof(withdrawals) = 'array'),
  forced_ids bytea[] NOT NULL,
  tx_ids bytea[] NOT NULL,
  consumed_deposit_ids bytea[] NOT NULL,
  finalized_withdrawal_ids bytea[] NOT NULL,
  finalized_forced_ids bytea[] NOT NULL,
  merge_slot bigint CHECK (merge_slot >= 0),
  merge_out_ref text,
  folded_at timestamptz NOT NULL DEFAULT NOW(),
  CHECK (cardinality(produced_outrefs) = cardinality(produced_outputs)),
  CHECK ((merge_slot IS NULL) = (merge_out_ref IS NULL))
);
CREATE INDEX idx_node_confirmed_merges_merge_slot
  ON node_confirmed_merges (merge_slot);

-- class: D-x; retention: a row lives exactly as long as the `node_confirmed_merges` row of the fold that spent it (deleted with it)
-- The `confirmed_ledger` rows a fold spent, verbatim, so the unfold of that
-- fold restores them.
CREATE TABLE node_confirmed_ledger_spent (
  header_hash bytea NOT NULL REFERENCES node_confirmed_merges (header_hash)
    ON DELETE CASCADE,
  tx_id bytea NOT NULL,
  outref bytea NOT NULL,
  output bytea NOT NULL,
  address text NOT NULL,
  time_stamp_tz timestamptz NOT NULL,
  PRIMARY KEY (header_hash, outref)
);
