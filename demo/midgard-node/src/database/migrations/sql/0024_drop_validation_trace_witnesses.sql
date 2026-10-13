-- The own-block journal's retained validation-witness members are deleted
-- (plan B1). Every validation descriptor is a two-state placeholder derived
-- from the event and its verdict, and the DA payload's
-- `validation_trace_witnesses` section is always empty, so nothing writes or
-- reads this table. Rows it holds belong to journals built before the change
-- and are dropped with it; the parent `pending_block_finalizations` row keeps
-- its descriptors in `pending_block_finalization_validation_traces`.
DROP TABLE pending_block_finalization_validation_trace_witnesses;
