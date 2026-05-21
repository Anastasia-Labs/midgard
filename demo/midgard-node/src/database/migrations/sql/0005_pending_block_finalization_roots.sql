-- Records the UTxO and transaction Merkle roots committed into each block
-- header at submission time. Local-finalization recovery compares the
-- confirmed L1 block header against these stored roots instead of recomputing
-- them over the live mempool, which drifts as new transactions are admitted
-- after submission.
ALTER TABLE pending_block_finalizations
  ADD COLUMN utxo_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN tx_root TEXT NOT NULL DEFAULT '';
