-- class: D-x; retention: a mark lives while the block it names holds the row (landed and processed, or this node's block between its local finalization and its processing); the fold of that block deletes the row, and the rollback that takes the block off the landed chain clears the mark
-- A pending-table row a block includes stays in its table, marked by that
-- block's header hash (plan §7.3, N3), until the block folds into
-- `confirmed_ledger`; the rollback that takes the block off the landed
-- chain clears the mark, so the row is pending again. Pending means
-- unmarked: selection, commit building and every pending read skip a
-- marked row.
ALTER TABLE mempool
  ADD COLUMN included_by bytea CHECK (octet_length(included_by) = 28);
ALTER TABLE processed_mempool
  ADD COLUMN included_by bytea CHECK (octet_length(included_by) = 28);
CREATE INDEX idx_mempool_included_by
  ON mempool (included_by) WHERE included_by IS NOT NULL;
CREATE INDEX idx_processed_mempool_included_by
  ON processed_mempool (included_by) WHERE included_by IS NOT NULL;
