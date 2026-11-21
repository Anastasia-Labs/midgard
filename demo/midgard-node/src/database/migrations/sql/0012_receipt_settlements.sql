-- class: D-x; retention: a row lives while the landed block it names stays on the landed chain (processed, or folded into confirmed_ledger); the rollback that takes that block off the chain deletes it
-- One row per acceptance-receipt member a landed block includes (plan §7.3,
-- N3), naming that block: the working-ledger rebuild that applies the block
-- writes it, and the batch closure of that and every later rebuild reads the
-- member as settled by the base. Receipts are never deleted, so the receipt
-- sequence carries no foreign key.
CREATE TABLE event_history_l2_ledger_receipt_settlements (
  receipt_sequence bigint NOT NULL,
  tx_id bytea NOT NULL,
  settled_by bytea NOT NULL CHECK (octet_length(settled_by) = 28),
  PRIMARY KEY (receipt_sequence, tx_id)
);
CREATE INDEX idx_receipt_settlements_settled_by
  ON event_history_l2_ledger_receipt_settlements (settled_by);
