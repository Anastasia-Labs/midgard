-- class: D-x; retention: unchanged (`node_landed_blocks`)
-- A processed own row the working ledger holds stays as `removed` when a
-- rollback takes its block off the landed chain, as an applied foreign row
-- does: the working-ledger rebase disposes of its journal and reverts it. A
-- removed row of either kind is one the working ledger holds.
ALTER TABLE node_landed_blocks DROP CONSTRAINT node_landed_blocks_check1;
ALTER TABLE node_landed_blocks
  ADD CONSTRAINT node_landed_blocks_removed_applied
  CHECK (state = 'processed' OR applied);
