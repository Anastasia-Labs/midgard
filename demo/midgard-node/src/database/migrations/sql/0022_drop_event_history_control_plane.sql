-- The node's event-history control plane is deleted (plan §13.1, N1-close):
-- the history owner, its authority lease, the Ogmios ChainSync journal (its
-- cursor, replay receipts, block applications, live outputs and event
-- incarnations), the L2 ledger receipts with their settled and rejected
-- members, and the native recovery plans. The L1 follower's facts and the
-- follower-change driver (`follower_event_ingestion`,
-- `node_follower_write_gate`, `node_deployment`) took every duty they had, so
-- nothing writes or reads these tables.
--
-- Rows they hold are dropped with them. No kept table has a foreign key into
-- them (0008 dropped the event rows' history associations), and the
-- settlement enqueue trigger reads `node_deployment` since 0021. Children go
-- before their parents, so no drop needs CASCADE and an unexpected dependent
-- fails the migration instead of being dropped silently.
DROP TABLE event_history_l2_ledger_receipt_rejections;
DROP TABLE event_history_l2_ledger_receipt_settlements;
DROP TABLE event_history_l2_ledger_receipts;
DROP TABLE event_history_recovery_plans;
DROP TABLE event_history_incarnations;
DROP TABLE event_history_live_outputs;
DROP TABLE event_history_block_applications;
DROP TABLE event_history_cursor;
DROP TABLE event_history_replay_receipts;
DROP TABLE event_history_authority;
