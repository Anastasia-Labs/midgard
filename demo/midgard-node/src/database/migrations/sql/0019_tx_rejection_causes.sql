-- class: D-x; retention: a row lives while its rejection does (it cascades with the `tx_rejections` row)
-- One row per rejected transaction a "dependent" or "batch" rejection
-- follows from: the producer of a rejected output the transaction spends,
-- or a rejected member of the batch it was accepted in. A rejection with no
-- row here is not traced to another transaction. When a landed own block's
-- journal is revived, its members' rejections are deleted, and so is every
-- rejection whose causes are all deleted ones, transitively.
CREATE TABLE tx_rejection_causes (
  tx_id bytea NOT NULL REFERENCES tx_rejections (tx_id) ON DELETE CASCADE,
  cause_tx_id bytea NOT NULL,
  PRIMARY KEY (tx_id, cause_tx_id)
);
CREATE INDEX idx_tx_rejection_causes_cause ON tx_rejection_causes (cause_tx_id);
