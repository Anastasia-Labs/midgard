-- class: D-x; retention: an index of `node_confirmed_ledger_spent`, kept with its rows
-- The node's ledger store reads the output at an out-ref a fold spent from
-- `confirmed_ledger` (a confirmed root older than the frontier, served to a
-- peer fetching a ledger snapshot while the node folds): one index probe per
-- out-ref, never a scan of every fold's spent rows.
CREATE INDEX idx_node_confirmed_ledger_spent_outref
  ON node_confirmed_ledger_spent (outref);
