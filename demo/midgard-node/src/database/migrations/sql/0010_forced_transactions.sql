CREATE TABLE forced_transaction_utxos (
  tx_order_id BYTEA PRIMARY KEY,
  tx_order_l1_tx_hash BYTEA NOT NULL CHECK (octet_length(tx_order_l1_tx_hash) = 32),
  tx_order_l1_output_index INTEGER NOT NULL CHECK (tx_order_l1_output_index >= 0),
  asset_name BYTEA NOT NULL CHECK (octet_length(asset_name) BETWEEN 1 AND 32),
  raw_datum BYTEA NOT NULL,
  tx_id BYTEA NOT NULL CHECK (octet_length(tx_id) = 32),
  tx_compact BYTEA NOT NULL,
  forced_inclusion_value BYTEA NOT NULL,
  operator_validity TEXT NOT NULL CHECK (operator_validity IN (
    'TxIsValid',
    'NonExistentInputUtxo',
    'InvalidSignature',
    'FailedScript',
    'FeeTooLow',
    'UnbalancedTx'
  )),
  inclusion_time TIMESTAMPTZ NOT NULL,
  projected_header_hash BYTEA,
  status TEXT NOT NULL CHECK (status IN ('awaiting', 'projected', 'finalized')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT unique_forced_transaction_l1_ref UNIQUE (tx_order_l1_tx_hash, tx_order_l1_output_index),
  CHECK (status <> 'awaiting' OR projected_header_hash IS NULL)
);

CREATE INDEX idx_forced_transaction_utxos_status_inclusion_time_tx_order_id
  ON forced_transaction_utxos (status, inclusion_time, tx_order_id);

CREATE INDEX idx_forced_transaction_utxos_projected_header_hash
  ON forced_transaction_utxos (projected_header_hash);

CREATE INDEX idx_forced_transaction_utxos_tx_id
  ON forced_transaction_utxos (tx_id);

CREATE TABLE pending_block_finalization_forced_transactions (
  header_hash BYTEA NOT NULL REFERENCES pending_block_finalizations(header_hash) ON DELETE CASCADE,
  member_id BYTEA NOT NULL,
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  payload_cbor BYTEA NOT NULL,
  payload_sha256 BYTEA NOT NULL CHECK (octet_length(payload_sha256) = 32),
  source_table TEXT NOT NULL,
  source_id BYTEA NOT NULL,
  source_time_stamp_tz TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (header_hash, member_id),
  CONSTRAINT unique_pending_block_finalization_forced_transaction_ordinal UNIQUE (header_hash, ordinal)
);

ALTER TABLE pending_block_finalizations
  ADD COLUMN base_forced_transactions_root TEXT NOT NULL DEFAULT '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8',
  ADD COLUMN expected_forced_transactions_root TEXT NOT NULL DEFAULT '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8';

ALTER TABLE pending_block_finalizations
  ALTER COLUMN base_forced_transactions_root DROP DEFAULT,
  ALTER COLUMN expected_forced_transactions_root DROP DEFAULT;
