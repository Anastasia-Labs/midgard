CREATE INDEX idx_address_history_address_tx_id ON address_history (address, tx_id);
CREATE INDEX idx_mempool_ledger_address_outref ON mempool_ledger (address, outref);
CREATE INDEX idx_blocks_header_hash_tx_id ON blocks (header_hash, tx_id);

ALTER TABLE withdrawal_utxos ADD COLUMN l2_ledger_outref BYTEA;
ALTER TABLE withdrawal_utxos ADD COLUMN l2_ledger_outref_state TEXT NOT NULL DEFAULT 'unknown';
ALTER TABLE withdrawal_utxos ADD CONSTRAINT withdrawal_ledger_outref_state CHECK (
  (l2_ledger_outref_state = 'decoded' AND l2_ledger_outref IS NOT NULL)
  OR (l2_ledger_outref_state IN ('unknown', 'undecodable') AND l2_ledger_outref IS NULL));
CREATE INDEX idx_withdrawal_ledger_outref_pending ON withdrawal_utxos (l2_ledger_outref)
  WHERE l2_ledger_outref_state = 'decoded' AND status IN ('awaiting', 'projected')
    AND (validity IS NULL OR validity = 'WithdrawalIsValid');
CREATE INDEX idx_withdrawal_ledger_outref_unknown ON withdrawal_utxos (event_id)
  WHERE l2_ledger_outref_state = 'unknown';
CREATE INDEX idx_withdrawal_ledger_outref_unknown_pending ON withdrawal_utxos (event_id)
  WHERE l2_ledger_outref_state = 'unknown' AND status IN ('awaiting', 'projected')
    AND (validity IS NULL OR validity = 'WithdrawalIsValid');
