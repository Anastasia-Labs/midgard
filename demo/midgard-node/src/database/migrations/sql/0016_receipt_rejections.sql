-- class: D-x; retention: a row lives while its receipt is unreversed; the working-ledger write that reverses the receipt deletes it
-- One row per acceptance-receipt member that has a terminal rejection
-- (`tx_rejections`) while the receipt is unreversed (plan §7.3, N3). An
-- earlier commit-stage rejection recorded the member rejected and left its
-- receipt unreversed. The batch closure reads a recorded member as decided:
-- the next working-ledger rebuild rejects the receipt's pending members as
-- batch members and reverses the receipt, unless a member is undecided.
-- Nothing writes this table after this migration. Receipts are never
-- deleted, so the receipt sequence carries no foreign key.
CREATE TABLE event_history_l2_ledger_receipt_rejections (
  receipt_sequence bigint NOT NULL,
  tx_id bytea NOT NULL,
  PRIMARY KEY (receipt_sequence, tx_id)
);
INSERT INTO event_history_l2_ledger_receipt_rejections (receipt_sequence, tx_id)
SELECT DISTINCT r.sequence, ids.tx_id
FROM event_history_l2_ledger_receipts r, unnest(r.tx_ids) AS ids(tx_id)
WHERE r.reversed_at_revision IS NULL
  AND EXISTS (SELECT 1 FROM tx_rejections t WHERE t.tx_id = ids.tx_id);
