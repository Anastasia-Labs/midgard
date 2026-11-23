-- class: D-x; retention: an index of `node_landed_blocks`, kept with its rows
-- The node's ledger store reads the output at an out-ref a landed block
-- produced and `confirmed_ledger` does not hold yet (a replay on a ledger the
-- unfolded rows lead to): one index probe per out-ref, never a scan of every
-- unfolded row.
CREATE INDEX idx_node_landed_blocks_produced_outrefs
  ON node_landed_blocks USING gin (produced_outrefs);
