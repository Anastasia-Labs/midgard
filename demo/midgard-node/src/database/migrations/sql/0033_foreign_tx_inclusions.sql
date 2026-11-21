-- class: D; retention: as `immutable` keeps this node's own committed txs: no age or retention prune; a row is written once the including block's fold is final, which no rollback reaches, so nothing deletes it
-- A transaction this node accepted that another operator's block included.
-- Its pending row stays, marked by that block (`included_by`), until the
-- block's fold into `confirmed_ledger` is final; the release that deletes the
-- row then (`releaseFinalFolds`) records the inclusion here, so `/tx-status`
-- and the committed-tx count keep answering for it, as `immutable` does for a
-- tx of this node's own blocks.
CREATE TABLE node_foreign_tx_inclusions (
  tx_id bytea PRIMARY KEY CHECK (octet_length(tx_id) = 32),
  header_hash bytea NOT NULL CHECK (octet_length(header_hash) = 28)
);
