-- class: D-x; retention: a row lives while its receipt is unreversed; the working-ledger write that reverses the receipt deletes it
-- One row per acceptance-receipt member that has a terminal rejection
-- (`tx_rejections`) while the receipt is unreversed (plan §7.3, N3), with
-- that rejection's code and detail. An earlier commit-stage rejection
-- recorded the member rejected and left its receipt unreversed, its
-- admission accepted and its address history in place. The batch closure
-- reads a recorded member as decided: the next working-ledger rebuild
-- rejects the receipt's pending members as batch members and reverses the
-- receipt, unless a member is undecided, and gives the member the admission
-- (rejected, with the recorded code) and address history (none) of a
-- rejected transaction. Nothing writes this table after this migration.
-- Receipts are never deleted, so the receipt sequence carries no foreign
-- key.
CREATE TABLE event_history_l2_ledger_receipt_rejections (
  receipt_sequence bigint NOT NULL,
  tx_id bytea NOT NULL,
  reject_code text NOT NULL,
  reject_detail text,
  PRIMARY KEY (receipt_sequence, tx_id)
);
INSERT INTO event_history_l2_ledger_receipt_rejections
  (receipt_sequence, tx_id, reject_code, reject_detail)
SELECT DISTINCT r.sequence, ids.tx_id, t.reject_code, t.reject_detail
FROM event_history_l2_ledger_receipts r, unnest(r.tx_ids) AS ids(tx_id)
JOIN tx_rejections t ON t.tx_id = ids.tx_id
WHERE r.reversed_at_revision IS NULL;
